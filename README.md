---
title: "Metal Up Your R's"
author: "Michael Laurila"
date: "`r Sys.Date()`"
output: html_document
---

```{r setup, include=FALSE}
library(tidyverse)
library(ggridges)
library(gganimate)
```

```{r load data, include=FALSE, cache=TRUE}
# Load data.
load("./data/wiki_metal_band_names.Rdata")
c("artists", "albums", "tracks", "audio_features") %>%
  walk(\(datum) load(str_c("./data/spotify_", datum, ".Rdata"), .GlobalEnv))
```

`r knitr::asis_output("\U1F50A")` _Metal music has an average loudness of_
`r round(mean(audio_features$loudness))` _on a scale from -60 to 0. Mind your volume settings._ `r knitr::asis_output("\U1F50A")`

```{r combine tables, echo=FALSE, message=FALSE, cache=TRUE}
# Attach genre to each track in audio_features.
genre_features <- band_names %>%
  left_join(artists, by = "artist_name", multiple = "all", relationship = "many-to-many") %>%
  left_join(albums, by = "artist_id", multiple = "all", relationship = "many-to-many") %>%
  left_join(tracks, by = "album_id", multiple = "all", relationship = "many-to-many") %>%
  left_join(audio_features, by = "track_id") %>%
  drop_na() %>%
  mutate(
  genre = str_replace_all(genre, "_", " "),
  genre = str_to_title(genre)
  )
```

# Classifying music is hard (but metal is harder)

> _... the_ musical _boundaries of the genre have sprawled so far<br>&nbsp;&nbsp;&nbsp; that today the phrase_ heavy metal _is ridiculously vague._
>
> -- Kim Neely in Rolling Stone, October 4th 1990

To file the entirety of metal music under _heavy metal_ would be even more
ridiculous today. The evolution of music, paired with our unrelenting need
to categorize, has granulated the genre space to a point where the mere presence
of an electric guitar doesn't cut it anymore. Even the phrase _death metal_, at
the time of inception thought of as an extreme subgenre, is
approaching ridiculous on the scale of vaguness. Several (sub-)subgenres with
distinct sounds has emerged since the 90's: do you prefer the complex rhythmic
structures
of [technical death metal](https://en.wikipedia.org/wiki/Technical_death_metal)
or the high-pitched shrieks and harmonized guitars in
[melodic death metal](https://en.wikipedia.org/wiki/Melodic_death_metal)?
(The answer is **yes**.)

Putting aside distorted guitars and heavy riffs; fashion, attitude, and ideology
also serves as defining characteristics of some genres. Notable examples beeing
the coiffures of [glam metal](https://en.wikipedia.org/wiki/Glam_metal) and the
Christian messages in
[Christian metal](https://en.wikipedia.org/wiki/Christian_metal). Besides having
one of the lowest proportions of explicit songs in its body of work, pulling a
random song out of the Christian metal catalogue gives little promise as to the
musical experience unless our holy grail is solely the absence of
profanity. Neither does a banging hairdo generally speak volumes about the
style of music that comes out of it. Since we will not be writing neural
networks that detects hairspray levels nor do word analysis on lyrics in search
of heathens, our focus will be aimed at the _sound_ of music.

```{r plot explicit songs, echo=FALSE, message=FALSE, fig.align='center'}
# Find and plot proportion of explicit songs.
genre_features %>%
  count(genre, explicit) %>%
  group_by(genre) %>%
  mutate(prop = proportions(n)) %>%
  filter(
    genre %in% c("Deathcore","Black Metal", "Christian Metal"),
    explicit == TRUE
  ) %>% 
  mutate(prop = round(prop * 100, 1)) %>%
  select(genre, prop) %>%
  knitr::kable(
    col.names = c("Genre", "Explicit songs (%)"),
    caption = "Explicit songs in selected genres."
  ) %>%
  kableExtra::kable_styling(full_width = FALSE)
```

A common, and natural, metric for music classification is _tempo_. Metal is
often considered a highly energetic and fast type of music; several genres has
"fast tempo" explicitly mentioned as a defining characteristic, or implicitly in
the term "[blast beat drumming](https://en.wikipedia.org/wiki/Blast_beat)".
Slower genres such as [Doom](https://en.wikipedia.org/wiki/Doom_metal) and
[Stoner metal](https://en.wikipedia.org/wiki/Stoner_rock) break the stereotype
and offers a home to the sluggish. Suppose that tempo acted as the sorting hat
for metal, in which house of genres would it put the band
[TOOL](https://sv.wikipedia.org/wiki/Tool)?

```{r plot tool tempo, echo=FALSE, message=FALSE, fig.align='center'}
genre_features %>%
  # Latest release Opiate^2 not an album.
  filter(artist_name == "tool", album_name != "Opiate²") %>%
  # Remove duplicates due to several genres.
  select(-genre) %>%
  distinct(.keep_all = TRUE) %>%
  ggplot(aes(x = tempo, y = album_name, fill = after_stat(x))) +
  geom_density_ridges_gradient(rel_min_height = 0.05) +
  scale_fill_viridis_c(name = "Tempo [BPM]", option = "C") +
  labs(
    title = "Tempo Distribution in TOOL Albums",
    x = "Tempo",
    y = "Album"
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  coord_cartesian(clip = "off") + 
  theme_ridges(grid = FALSE, center_axis_labels = TRUE)
```

```{=html}
<center>
<iframe src="https://open.spotify.com/embed/track/2ae6mkuD2gJnJQADl488et?utm_source=generator&theme=0" width="30%" height="85" frameBorder="0" allowtransparency="true" allow="encrypted-media" data-external="1"></iframe>
<iframe src="https://open.spotify.com/embed/track/7tvuLLroI0n6uYBWuFig5d?utm_source=generator&theme=0" width="30%" height="85" frameBorder="0" allowtransparency="true" allow="encrypted-media" data-external="1"></iframe>
</center>
<br>
<center>
<i>Intension</i> from <i>10,000 Days</i> with 95 BPM and <i>Lateralus</i>
(the orange bump) from <i>Lateralus</i> with 173 BPM.
</center>
<br>
```

One could probably find a way to use the tempo information to make a definitive
decision but it is not clear-cut which method should be used. Now add the other
factors which go into describing the sound of music: key, mode, instruments etc.
How can we ever hope to label any band as belonging to such and such a genre?

# Let someone else do the work

```{r, echo=FALSE}
num_of_bands <- n_distinct(band_names$artist_name)
num_of_genres <- n_distinct(band_names$genre)
num_of_albums <- n_distinct(albums)
num_of_tracks <- n_distinct(tracks)
years_of_metal <- round(
  sum(audio_features$duration_ms) / 1000 / 60 / 60 / 24 / 365, 1
)
```

Between writing a thesis about musical genres and letting my mom list every
growl over a distorted guitar as "Efter Döden-musik" (really, that's what
she calls it, bless her heart),
[Wikipedia's lists of bands under various metal subgenres](https://en.wikipedia.org/wiki/List_of_heavy_metal_bands) serves as a
middle ground. Out of the `r num_of_bands` artists found
in `r num_of_genres` subgenre lists,
`r count(band_names, artist_name) %>% filter(n >= 2) %>% nrow()`
(~ `r round(count(band_names, artist_name) %>% filter(n >= 2) %>% nrow() / n_distinct(band_names$artist_name) * 100)`%)
of them are found in _at least_ two different genres (the norwegian band
Vardøger is listed
in `r band_names %>% filter(artist_name == "vardøger") %>% nrow()` different
genres)! If this is a product of the musical agility of the bands, their ability
to compose music which spans over the genres, or a demonstration of the fuzzy
lines which constitutes the genre boundaries, is a question for another day.
We're just happy that we got a list of genre classified bands, because bands
make songs and songs is data.

# Put your numbers where your music is

```{r, echo=FALSE}
least_energy <- genre_features %>%
  group_by(genre) %>%
  summarize(en = mean(energy)) %>%
  filter(en == min(en)) %>%
  mutate(en = round(en, 2))
most_energy <- genre_features %>%
  group_by(genre) %>%
  summarize(en = mean(energy)) %>%
  filter(en == max(en)) %>%
  mutate(en = round(en, 2))

least_danceable <- genre_features %>%
  group_by(genre) %>%
  summarise(dance = mean(danceability)) %>%
  arrange(dance) %>%
  slice(1:3) %>%
  pull(genre)
most_danceable <- genre_features %>%
  group_by(genre) %>%
  summarise(dance = mean(danceability)) %>%
  arrange(desc(dance)) %>%
  slice(1:3) %>%
  pull(genre)
danceable <- c(least_danceable, most_danceable)
```

Querying the
[Spotify API](https://developer.spotify.com/documentation/web-api/reference/#/)
about the `r num_of_bands` bands found on Wikipedia yielded `r num_of_albums`
albums containing `r num_of_tracks` tracks combining to `r years_of_metal`
years of metal music. Each track comes with a set of audio features including
amongst others tempo, key, mode, and the high level metrics _danceability_ and
_energy_. While the energy in metal music typically is high,
the averages between the least and most energetic genres is not insignificant.
`r least_energy$genre` has an average energy level of `r least_energy$en`, on
the other side sits `r most_energy$genre` with `r most_energy$en`.

```{=html}
<center>
<iframe src="https://open.spotify.com/embed/track/62qFE9s9Z0PQWtQHh8ZvCQ?utm_source=generator&theme=0" width="30%" height="85" frameBorder="0" allowtransparency="true" allow="encrypted-media" data-external="1"></iframe>
<iframe src="https://open.spotify.com/embed/track/5X40Iz02QMpeB76qOI4l9H?utm_source=generator&theme=0" width="30%" height="85" frameBorder="0" allowtransparency="true" allow="encrypted-media" data-external="1"></iframe>
</center>
<br>
<center>
<i>The Solace</i> by Doom metal band <i>My Dying Bride</i> with an energy of 0.205 and <i>So Far Too Long</i> by Deathcore band <i>All Shall Perish</i> with an energy
of 0.949.
</center>
<br>
```

Calling yourself a dancer because of your ability to headbang and attending the
occasional mosh pit is somewhat of a stretch. Metal music is seldom
associated with dancing and this is reflected in every genre's danceability
average. Some bands blur the lines between metal and pop and does permit moves
that could be considered dancing.

```{r plot danceability, echo=FALSE, message=FALSE, fig.align='center'}
genre_features %>%
  filter(genre %in% danceable) %>%
  ggplot(aes(x = danceability, y = genre, fill = after_stat(x))) +
  geom_density_ridges_gradient(rel_min_height = 0.05) +
  scale_fill_viridis_c(name = "Danceability", option = "C") +
  labs(
    title = "Three Least and Most Danceable Genres",
    x = "Danceability",
    y = "Genre"
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  coord_cartesian(clip = "off") + 
  theme_ridges(grid = FALSE, center_axis_labels = TRUE)
```

```{=html}
<center>
<iframe src="https://open.spotify.com/embed/track/47lHaN4JicIXU2c6YLDWKM?utm_source=generator&theme=0" width="30%" height="85" frameBorder="0" allowtransparency="true" allow="encrypted-media" data-external="1"></iframe>
<iframe src="https://open.spotify.com/embed/track/5cjBGeuaCFju8JrtBqMbaZ?utm_source=generator&theme=0" width="30%" height="85" frameBorder="0" allowtransparency="true" allow="encrypted-media" data-external="1"></iframe>
</center>
<br>
<center>
<i>Perfection or Vanity</i> by Symphonic black metal band <i>Dimmu Borgir</i> with a danceability of 0.0687 and <i>Pimpin'</i> by Nu metal band <i>Hollywood Undead</i>
with a danceability of 0.857.
</center>
<br>
```

A more direct notion of the sound of a song is which key and mode it is played
in. By finding the proportion of songs in a certain key and mode for each genre
we see a surprising variety between them.

```{r circle of fifths, echo=FALSE}
# Find keys and mode of genres.
# Turn minor key counts to negative to emulate the inside of the circle of
# fifths (major keys on the outside, positive)
genre_keys <- genre_features %>%
  select(genre, key, mode) %>%
  group_by(genre, key, mode) %>%
  mutate(num_keys = n()) %>%
  ungroup() %>%
  group_by(genre) %>% 
  mutate(num_genre = n()) %>%
  ungroup() %>%
  mutate(
    prop = if_else(mode == 0, -(num_keys / num_genre), num_keys / num_genre),
    mode = if_else(mode == 0, "minor", "major")
  ) %>%
  distinct() %>%
  drop_na()

# 
keys_plot <- genre_keys %>% 
  ggplot(aes(key, prop, fill = as_factor(mode))) +
  geom_col() +
  ylim(-0.15, 0.15) +
  coord_polar() +
  theme_minimal() +
  scale_fill_manual(values = c("#F9A242FF", "#440154FF")) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  ) +
  labs(
    title = "The Keys to Metal",
    fill = "Mode"
  ) 
```

```{r animate_keys_plot, echo=FALSE, cache=TRUE}
anim <- keys_plot +
  transition_states(genre, transition_length = 1, state_length = 1) +
  ease_aes('cubic-in-out') +
  ggtitle("The Keys to Metal", subtitle = "Genre: {closest_state}") +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 18, face = "bold"),
    legend.title = element_text(size=16),
    legend.text = element_text(size=14)
  )

#anim_save(
#  "gganim_test.gif",
#  anim,
#  width = 400,
#  height = 400,
#  duration = 30,
#  nframes = 300,
#  renderer = gifski_renderer()
#)
```

```{r insert gif, echo=FALSE, out.width="400px", out.height="400px", fig.align='center'}
#knitr::include_graphics("gganim_test.gif")
```

Notible examples include the high number of songs in D-flat in Grindcore and
the high proportion of songs in minor keys in Gothic metal.

```{r plot keys, echo=FALSE}
grindcore_keys <- genre_keys %>%
  filter(genre == "Grindcore") %>%
  ggplot(aes(key, prop, fill = as_factor(mode))) +
  geom_col() +
  ylim(-0.15, 0.15) +
  coord_polar() +
  theme_minimal() +
  scale_fill_manual(values = c("#440154FF", "#F9A242FF")) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  ) +
  labs(
    title = "Grindcore",
    fill = "Mode"
  ) 

gothic_keys <- genre_keys %>%
  filter(genre == "Gothic Metal") %>%
  ggplot(aes(key, prop, fill = as_factor(mode))) +
  geom_col() +
  ylim(-0.15, 0.15) +
  coord_polar() +
  theme_minimal() +
  scale_fill_manual(values = c("#440154FF", "#F9A242FF")) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 16, face = "bold"),
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank()
  ) +
  labs(
    title = "Gothic Metal",
    fill = "Mode"
  )
```

```{r side by side plots, echo=FALSE, fig.show='hold', out.width="50%"}
grindcore_keys
gothic_keys
```

```{=html}
<center>
<iframe src="https://open.spotify.com/embed/track/31lTNpFIbRcOelTet5l6c2?utm_source=generator&theme=0" width="30%" height="85" frameBorder="0" allowtransparency="true" allow="encrypted-media" data-external="1"></iframe>
<iframe src="https://open.spotify.com/embed/track/0TheeArj9ATH89cbyxAGsU?utm_source=generator&theme=0" width="30%" height="85" frameBorder="0" allowtransparency="true" allow="encrypted-media" data-external="1"></iframe>
</center>
<br>
<center>
<i>Wound 91</i> in D-flat by Grindcore band <i>Godflesh</i> and <i>You're So Vain</i> in A minor by Gothic metal band <i>Marilyn Manson</i>.
</center>
<br>
```

# Does it all sound the same?

Pulling songs based on the extremes of data shows that metal as a whole has a
lot of breadth. When entering new genres it can often be difficult to navigate,
most of the songs sounds the same. Using this data driven search for new music
enables one to look at different aspects of a genre, choosing which artist
or song to listen to next. This can be a tad tedious when browsing
in the Spotify GUI since its "Related Artists" and "Fans Also Like" features
lack the specificity that raw numbers provide. In using Spotify for over 10 years
I have never experienced this level of control in my music browsing, this
process can certainly be expanded to more genres, and functions written to
tailor the needs of the listener.

[About the title](https://www.revolvermag.com/culture/metal-your-ass-how-metallicas-kill-em-all-got-its-iconic-art-and-title)<br>
[Kim Neely reviewing Anthrax's album Persistence of Time, October 4th 1990](https://web.archive.org/web/20080108121842/http://www.rollingstone.com/artists/anthrax/albums/album/302316/review/5946559/persistence_of_time)
