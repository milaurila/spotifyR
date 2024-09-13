################################################################################
#                                                                              #
# Scrapes Wikipedia for bands listed under metal subgenres                     #
# (e.g. heavy metal, black metal, melodic death metal).                        #
#                                                                              #
# Yields 3486 unique "band names" from 31 URLs.                                #
#                                                                              #
# The URL path "wiki/List_of_heavy_metal_bands" contains a list of heavy metal #
# bands as well as links to lists of bands from other metal genres. We begin   #
# by scraping these links and then scrape the bands from each one.             #
#                                                                              #
# Some manual cleaning on the URL scrape is performed where an automated       #
# solution was deemed too time consuming. This includes hardcoding some URLs   #
# and visually inspecting the scraped data (around 30 different genres).       #
#                                                                              #
# The accuracy of the Spotify API "search" endpoint was found to be affected   #
# by characters not part of the artist names. Every scraped "band name"        #
# (possibly some junk as well) is stripped from brackets (footnote links) and  #
# parenthesis, e.g. "Skid Row (band)". No other measures are taken to ensure   #
# that the resulting list contains _only_ band names since these will be fed   #
# to the Spotify API and the responses cross-checked to this list.             #
# If we send a request for the artist "Wikipedia:Citation needed" and the API  #
# returns some artist it deemed fitting to the query, it will probably not     #
# match an artist in our list and thus not be included further down the        #
# pipeline. Not a perfect solution but it does seem to yield a large amount    #
# of band names with little junk.                                              #
#                                                                              #
################################################################################

library(rvest)
library(polite)
library(tidyverse)

# Politely bow to our host.
wiki_bow <- "https://en.wikipedia.org/" %>%
  bow(user_agent = "milaurila MT4007 R Project")
# Scrape heavy metal path for genre URLs.
genre_urls <- wiki_bow %>%
  nod(path = "wiki/List_of_heavy_metal_bands") %>%
  scrape() %>%
  # Extract hyperlinks that reference list URLs.
  html_elements("ul li a") %>%
  html_attr("href") %>%
  str_subset("^/wiki/List_of_")

# Large genres black- and death metal are split into two separate lists.
# This is not accounted for by our genre URL scrape and therefore added
# manually.
extra_urls <- c(
  "/wiki/List_of_black_metal_bands,_0%E2%80%93K",
  "/wiki/List_of_black_metal_bands,_L%E2%80%93Z",
  "/wiki/List_of_death_metal_bands,_!%E2%80%93K",
  "/wiki/List_of_death_metal_bands,_L%E2%80%93Z"
)

# Combine scraped and hardcoded urls.
# The "ul li a" path hit some unwanted unordered lists and added a link to
# a list of metal festivals which we do not care about (index 30).
genre_urls <- c(genre_urls[-30], extra_urls) %>%
  unique()

# Lists of bands come in four different forms, these three are the most
# common. Two genres (metalcore and new wave of British heavy metal)
# present their bands list in the form of unordered lists.
# Selecting these elements yields a lot of unwanted data since "ul" items
# are abundant on every page. Perhaps there is a way (maybe with XPATH?) to
# get only what we want. These genre URLs will not yield any band names (some
# are however included in other genre lists).
elements <- list(
  div_col = "div.div-col li a",
  wikitable = "table.wikitable",
  multicol = "table.multicol"
)

# Extract name of genres from URLs.
# Keep duplicates for the moment so that the naming logic below works.
genres <- str_extract(genre_urls, "(?<=of_)(.+)(?=_bands)")

# Get band names.
band_names <- genre_urls %>%
  # Scrape the genre list URLs.
  map(\(url)
    nod(wiki_bow, path = url) %>%
    scrape() %>%
    # Extract the three list forms from each genre URL scrape.
    (\(doc) map(elements, \(css) html_elements(doc, css))) %>%
    # Keep the non-empty list type.
    keep(\(type) length(type) > 0) %>%
    # Extract bands from the html elements according to their type.
    # As mentioned above a few URLs won't yield any names and have length = 0,
    # we keep them (as empty lists) to correctly assign the item names (genres)
    # later.
    (\(nodeset)
      if (length(nodeset) == 0) {
        list()
      } else if (names(nodeset) == "wikitable") {
        html_table(nodeset$wikitable) %>%
          bind_rows() %>%
          # Tables list bands under column header "Band" or "Name".
          pull(matches("Band|Name")) %>%
          str_remove_all("\\s*\\[.*")
      } else if (names(nodeset) == "multicol") {
        html_elements(nodeset$multicol, "li a") %>%
          html_attr("title") %>%
          discard(is.na) %>%
          str_remove_all("\\s*\\(.*")
      } else if (names(nodeset) == "div_col") {
        html_attr(nodeset$div_col, "title") %>%
          discard(is.na) %>%
          str_remove_all("\\s*\\(.*")
      }
    )
  ) %>%
  # Associate each list with its genre.
  set_names(genres) %>%
  # Retain names (genres) in data frame.
  map_dfr(\(list) as_tibble(list), .id = "name") %>%
  rename(genre = name, artist_name = value) %>%
  # Grouping by artist_name and counting reveals 16 instances of "ISBN" and
  # 8 of "Wikipedia:Citation needed. Lets remove those.
  filter(!artist_name %in% c("ISBN", "Wikipedia:Citation needed")) %>%
  mutate(artist_name = tolower(artist_name))

<<<<<<< HEAD
# Save band names to disk.
save(
  "band_names",
  list = "band_names",
  file = "./data/wiki_metal_band_names.Rdata"
  )
=======
# Grouping by artist_name and counting reveals 16 instances of "ISBN" and
# 8 of "Wikipedia:Citation needed. Lets remove those.
band_names <- band_names %>%
  filter(!artist_name %in% c("ISBN", "Wikipedia:Citation needed")) %>%
  mutate(artist_name = tolower(artist_name))
# Save band names to disk.
save(band_names, band_names, file = "./data/wiki_metal_band_names.Rdata")
# Remove duplicate genres and save to disk.
genres <- unique(genres)
save(genres, genres, file =  "./data/wiki_metal_genres.Rdata")
