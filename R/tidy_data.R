################################################################################
#                                                                              #
# Functions that call the API functions in "R/query_data.R".                   #
# Maps over lists of query items and extracts data from the API response       #
# JSONs. These functions acts somewhat as filters since what is being kept     #
# from the JSONs is determined here (plucks and selects).                      #
# Binds resulting purrr::map lists row's and turns them into tibbles.          #          #
#                                                                              #
# Aims to create a relational datastructure with the following primary keys:   #
# (Table   -   Primary Key)                                                    #
#  artists -   artist_id                                                       #
#  albums  -   album_id                                                        #
#  tracks  -   track_id                                                        #
#  audio_features - track_id                                                   #
#                                                                              #
################################################################################

source("./R/query_data.R")

library(tidyverse)

# Repeated procedure.
rows_to_tibble <- function(list) {
  list %>%
    bind_rows() %>%
    as_tibble()
}

# Create chunk of size "size" from "idx" in "column" or size < "size" if less
# than "size" items remain to the end ("max_idx").
# Use:
# seq(1, length(column), size) %>%
#   map(~ chunks(column, size, .))
# For batch API calls.
chunks <- function(column, size, idx) {
  max_idx <- length(column)
  if (idx + size - 1 < max_idx) {
    column[idx:(idx + size - 1)] %>%
      str_flatten(collapse = ",")
  } else {
    column[idx:max_idx] %>%
      str_flatten(collapse = ",")
  }
}

# Create data frame with artist ids and names.
# Use artist id as primary key.
get_artist_ids <- function(artists) {
  artists %>%
    map(\(artist)
      spotify_search(artist) %>%
        pluck("artists") %>%
        pluck("items") %>%
        keep(names(.) %in% c("id", "name"))
    ) %>% 
    rows_to_tibble()
}

# Create data frame with album ids, names and release dates.
# Use album id as primary key.
# Attach artist id (artists key) to each album.
get_album_ids <- function(artist_ids) {
  artist_ids %>%
    map(\(id)
      spotify_albums(id) %>%
        pluck("items") %>%
        list_modify(artist_id = id) %>%
        keep(
          names(.) %in% c(
          "artist_id",
          "id",
          "name",
          "release_date",
          "release_date_precision"
          )
        )
    ) %>%
    rows_to_tibble()
}

# Create data frame with track ids and names.
# Use track id as key.
# I want each track to be paired with its respective album id.
# Problem: We're passing 20 album IDs at a time and can therefore
# not use list_modify as in get_album_ids() to add the album id
# to each individual album item. The response does contain each
# album id in every list item but at a different position from the
# track information. Using nested maps and map_depth with pluck got hairy so
# resorting to the tibble approach for the moment. Not pretty but works.
get_album_tracks_ids <- function(album_ids) {
  album_ids %>%
    map(\(ids)
      spotify_album_tracks(ids) %>%
        as_tibble() %>%
        unnest(cols = c(albums)) %>%
        select("id", "tracks") %>%
        unnest(cols = c(tracks)) %>%
        rename(album_id = id) %>%
        select("album_id", "items") %>%
        unnest(cols = c(items)) %>%
        select(
          "id",
          "name",
          "track_number",
          "explicit",
          "album_id"
          )
    ) %>%
    bind_rows()
}

# Create data frame with tracks' audio features.
# Use track id as key.
get_track_audio_features <- function(track_ids) {
  track_ids %>%
    map(\(ids)
      spotify_tracks_audio_features(ids) %>%
        pluck("audio_features") %>%
        keep(names(.) %in% c(
          "id",
          "energy",
          "key",
          "mode",
          "valence",
          "tempo",
          "danceability",
          "loudness",
          "speechiness",
          "acousticness",
          "instrumentalness",
          "liveness",
          "duration_ms",
          "time_signature"
          )
        )
    ) %>%
    rows_to_tibble()
}
