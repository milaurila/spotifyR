library(tidyverse)
library(lubridate)
library(gganimate)
theme_set(theme_bw())

# Load data.
load("./data/wiki_metal_band_names.Rdata")
c("artists", "albums", "tracks", "audio_features") %>%
  walk(\(datum) load(str_c("./data/spotify_", datum, ".Rdata"), .GlobalEnv))

# Lets aggregate!

### Look into
# Tracks width identical names.

# Number of items.
list(artists = artists, albums = albums, tracks = tracks) %>%
  map_df(nrow) %>%
  knitr::kable()

# Add release dates to tracks.
# Assuming that a track was released when the album it appears on was released.
tracks <- tracks %>% 
  inner_join(albums %>% select(release_date, album_id), by = "album_id")

# Number and proportion of songs per year.
songs_per_year <- tracks %>%
  mutate(year = year(release_date)) %>%
  group_by(year) %>%
  tally() %>%
  mutate(prop = proportions(n))

# Number and proportion of explicit songs.
explicit_songs_per_year <- tracks %>%
  mutate(year = year(release_date)) %>%
  group_by(year, explicit) %>%
  tally() %>%
  mutate(prop = proportions(n)) %>%
  ungroup()

# Numbers of tracks with different keys and modes, per year. 
keys_modes <- audio_features %>%
  inner_join(tracks %>% select(track_id, release_date), by = "track_id") %>%
  mutate(key = as_factor(key)) %>%
  mutate(
    key = fct_relevel(
      key,
      "0",
      "7",
      "2",
      "9",
      "4",
      "11",
      "6",
      "1",
      "8",
      "3",
      "10",
      "5"
      ),
    key = fct_recode(
      key,
      C = "0",
      G = "7",
      D = "2",
      A = "9",
      E = "4",
      B = "11",
      "F#" = "6",
      Db = "1",
      Ab = "8",
      Eb = "3",
      Bb = "10",
      "F" = "5"
      )
    ) %>%
  select(release_date, key, mode) %>% 
  drop_na() %>%
  group_by(year(release_date), key, mode) %>%
  count() %>%
  mutate(n = if_else(mode == 0, -n, n)) %>%
  rename(year = `year(release_date)`)

# gganimation of the number of tracks in key and mode over the years.
p <- keys_modes %>% ggplot(aes(key, n, fill = n)) +
  geom_col() +
  coord_polar() +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 12)
  ) +
  scale_fill_viridis_c(option = "magma") +
  labs(
    x = "Key",
    y = "Number of songs",
    title = "The keys to metal",
  ) 
# anim <- p + transition_time(year)
# anim_save("gganim_test", anim, width = 640, height = 640)

# Attach track number (as it appears on its album) to audio_features.
# E.g. how does the audio features change over the course of an album?

tracks %>%
  select(track_id, track_number, album_id) %>%
  inner_join(audio_features, by = "track_id") %>%
  inner_join(albums %>% select(album_id, album_name), by = "album_id") %>%
  select(-track_id, -liveness) %>%
  group_by(track_number) %>%
  summarize(
    across(
      c(
        danceability,
        loudness,
        speechiness,
        acousticness,
        instrumentalness,
        duration_ms,
        time_signature,
        valence,
        tempo,
        energy,
        key,
        mode
        ),
      mean
      )
    )



# Turn minor key counts to negative to emulate the inside of the circle of
# fifths (major keys on the outside, positive).

circle_of_fifths <- c(
  "0", "7", "2", "9", "4", "11", "6", "1", "8", "3", "10", "5"
  )
standard_notation <- c(
  C = "0",  G = "7",  D = "2",    A = "9",
  E = "4",  B = "11", "F#" = "6", Db = "1",
  Ab = "8", Eb = "3", Bb = "10",  "F" = "5"
)

features <- artists %>%
  inner_join(albums, by = "artist_id") %>%
  inner_join(tracks, by = "album_id") %>%
# Try to remove duplicate tracks with different ids.
# A track with the same combination of artist_name
# and track_name is considered a duplicate.
  distinct(artist_name, track_name, .keep_all = TRUE) %>%
  inner_join(audio_features, by = "track_id") %>%
  mutate(year = year(release_date)) %>%
# Relevel keys according to circle of fifths and recode keys to standard
# notation from pitch class notation.
  mutate(
    key = as_factor(key),
    key = fct_relevel(key, circle_of_fifths),
    key = fct_recode(key, !!!standard_notation)
    ) %>%
  select(-artist_id, -album_id, -track_id, -release_date)







timbres <- test %>%
  keep(names(.) %in% c("sections", "segments")) %>%
  pluck("segments") %>%
  pluck("timbre") %>%
  reduce(rbind) %>%
  as_tibble()






