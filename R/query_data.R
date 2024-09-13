################################################################################
#                                                                              #
# https://httr.r-lib.org/articles/api-packages.html used as a guide.           #
#                                                                              #
# Imports API token required to make API calls.                                #
# Functions that perform API calls, parses and checks the responses.           #
#                                                                              #
# I would like to create a "backoff and resume" functionality so that querying #
# continues after a server error or rate limit. Avoiding rate limiting is done #
# by the functions that calls the API functions, they do this by sleeping for  #
# a set amount of seconds after a set amount of queries. This seems to work ok #
# but is maybe not the fastest way to get the responses. When a server error   #
# (500+) happens everything stops and the acquired data is lost. Currently     #
# crossing my fingers and hope that each session runs to its end...            #
#                                                                              #
################################################################################

library(httr)
library(jsonlite)
library(tidyverse)

# Application client ID used to request access tokens.
# Obtained by logging in with a Spotify account at
# https://developer.spotify.com/dashboard/applications
# and creating an app. The app needs a redirect URI specified in its settings.
c_id <- read_lines("client_id")

# OAuth2.0 access token valid for 60 minutes.
# Uses the Implicit Grant Flow to request and fetch a token via the
# GET below. Response is a link which prompts a Spotify account to authorize
# the application. Upon authorizing redirects to the specified URI
# (app specific) with the token attached to the adress.
# I am simply copying this token from the URI and pasting it in the "token"
# file. When expired this process is repeated.
# Would like to automate but not a priority at the moment.

# It is possible to just go to the address
# "
# https://accounts.spotify.com/authorize?
# client_id=c_id
# response_type=token&
# redirect_uri=http://localhost:8888/callback
# "
# directly, authorize, and copy the token.

GET(
  "https://accounts.spotify.com",
  path = "authorize",
  query = list(
    response_type = "token",
    client_id = c_id,
    redirect_uri = "http://localhost:8888/callback"
  )
)

access_token <- read_lines("token")

# Request constants.
api_url <- "https://api.spotify.com"
ua <- user_agent("milaurila MT4007 R Project")
auth <- add_headers(Authorization = str_c("Bearer ", access_token))

# Checks response status and turns JSON to list.
json_to_r <- function(response) {
  response %>%
    stop_for_status() %>%
    content("text", encoding = "UTF-8") %>%
    fromJSON()
}

# Get search results for "artist".
spotify_search <- function(artist) {
  RETRY(
    "GET",
    api_url,
    path = "v1/search",
    query = list(
      q = artist,
      type = "artist",
      limit = 10
    ),
    ua,
    auth
  ) %>%
    json_to_r()
}

# Get albums by artist with id "artist_id".
spotify_albums <- function(artist_id) {
  RETRY(
    "GET",
    api_url,
    path = str_c("v1/artists/", artist_id, "/albums"),
    query = list(
      market = "SE",
      limit = 10,
      include_groups = "album,single"
    ),
    ua,
    auth
  ) %>%
    json_to_r()
}

# Get tracks from albums with ids "album_ids".
# Takes a comma separated list of max 20 album ids.
spotify_album_tracks <- function(album_ids) {
  RETRY(
    "GET",
    api_url,
    path = "v1/albums",
    query = list(
      limit = 20,
      market = "SE",
      ids = album_ids
    ),
    ua,
    auth
  ) %>%
    json_to_r()
}

# Get audio features from tracks.
# Takes a comma separated list of max 100 track ids.
# RETURNS TRACK ID (KEY)!
spotify_tracks_audio_features <- function(track_ids) {
  RETRY(
    "GET",
    api_url,
    path = "v1/audio-features",
    query = list(ids = track_ids),
    ua,
    auth
  ) %>%
    json_to_r()
}

spotify_track_audio_analysis <- function(track_id) {
  RETRY(
    "GET",
    api_url,
    path = str_c("v1/audio-analysis/", track_id),
    ua,
    auth
  ) %>%
    json_to_r()
}
