################################################################################
#                                                                              #
# Calls functions in "R/tidy_data.R" to get all the data from the API calls.   #
#                                                                              #
# Clean:                                                                       #                                                                    
# Filter the recieved artists against the scraped band names from Wikipedia.   #
# Set unique key names across all tables.                                      #
# Fix album release dates.                                                     #
#                                                                              #
# Save:                                                                        #
# Each intermediate data set is saved to a Rdata file.                         #
#                                                                              #
################################################################################

source("./R/tidy_data.R")

library(tidyverse)
library(lubridate)

# Load band_names from scrape script and clean for queries.
load("./data/wiki_metal_band_names.Rdata")
band_names <- band_names %>%
  distinct(artist_name, .keep_all = TRUE)

# Only keep artists found on Wikipedia.
artists <- get_artist_ids(band_names$artist_name) %>%
  # We don't know if we remove metal bands here or not. Possible errors in
  # later analysis. What if there are two artists named "All That Remains"
  # where one is a metal band and the other a dansband. Which one is removed
  # by distinct()? Possible to use the "genre" attribute in API response?
  distinct(name, .keep_all = TRUE) %>%
  mutate(name = tolower(name)) %>%
  semi_join(band_names, by = c("name" = "artist_name")) %>%
  rename(artist_id = id, artist_name = name)

albums <- get_album_ids(artists$artist_id) %>%
  distinct(id, .keep_all = TRUE) %>%
  filter(!is.na(name)) %>%  # Not all artists have albums.
  rename(album_id = id, album_name = name) %>%
  mutate(
    release_date = case_when(
      release_date_precision == "year" ~ as_date(str_c(release_date, "-01-01")),
      release_date_precision == "month" ~ as_date(str_c(release_date, "-01")),
      release_date_precision == "day" ~ as_date(release_date)
    )
  ) %>%
  select(-release_date_precision) %>%
  filter(year(release_date) != 2023)  # A bit to soon.

tracks <- seq(1, length(albums$album_id), 20) %>%
  map(\(idx) chunks(albums$album_id, 20, idx)) %>%
  get_album_tracks_ids() %>%
  distinct(id, .keep_all = TRUE) %>%
  mutate(explicit = as_factor(explicit)) %>%
  rename(track_id = id, track_name = name)

circle_of_fifths <- c(
  "0", "7", "2", "9", "4", "11", "6", "1", "8", "3", "10", "5"
)
standard_notation <- c(
  C = "0",  G = "7",  D = "2",    A = "9",
  E = "4",  B = "11", "F#" = "6", Db = "1",
  Ab = "8", Eb = "3", Bb = "10",  "F" = "5"
)

audio_features <- seq(1, length(tracks$track_id), 100) %>%
  map(\(idx) chunks(tracks$track_id, 100, idx)) %>%
  get_track_audio_features() %>%
  drop_na() %>%  # Not all tracks have audio features.
  rename(track_id = id) %>%
  # Relevel keys according to circle of fifths and recode keys to standard
  # notation from pitch class notation.
  mutate(
    key = as_factor(key),
    key = fct_relevel(key, circle_of_fifths),
    key = fct_recode(key, !!!standard_notation)
  )

# Attempt to filter duplicate songs.
#distinct_track_ids <- artists %>%
#  inner_join(albums, by = "artist_id") %>%
#  inner_join(tracks, by = "album_id") %>%
#  group_by(artist_name, track_name) %>%
#  filter(release_date == min(release_date)) %>%
#  select(track_id)

# Write data to disk.
c("artists", "albums", "tracks", "audio_features") %>%
  walk(
    \(datum) save(
      datum,
      list = datum,
      file = str_c("./data/spotify_", datum, ".Rdata")
    )
  )
