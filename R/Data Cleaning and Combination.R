# DATA MUNGING AND CLEANING PERFORMED TO COMBINE OUR 3 DATASETS
library(readr)
library(tidyverse)
library(dbplyr)
songs <- read_csv("enchonest_2.csv")
songs_2 <- as.data.frame(read_csv("hit_songs_09_19.csv"))
songs_3 <- read_csv("echonest_3.csv")

deduped_songs <- songs %>%
  arrange(desc(popularity)) %>% 
  distinct(artists, track_name, .keep_all = TRUE) %>%
  select(artists, track_name, duration_ms, popularity, danceability, energy, key, loudness, mode, speechiness, acousticness,instrumentalness, liveness, valence, tempo)

songs_2 <- songs_2 %>%
  select(artist, song, duration_ms, popularity, danceability, energy, key, loudness, mode, speechiness, acousticness,instrumentalness, liveness, valence, tempo)

deduped_songs2 <- songs_3 %>%
  arrange(desc(popularity)) %>% 
  mutate(
    artist = str_remove_all(artists, "\\[|\\]|'"),   
    artist = str_trim(artist),
    track_genre = NA) %>%
  distinct(artists, name, .keep_all = TRUE) %>%
  select(artist, name, duration_ms, popularity, danceability, energy, key, loudness, mode, speechiness, acousticness,instrumentalness, liveness, valence, tempo)

colnames(deduped_songs2) <- c("artists", "track_name", "duration_ms", "popularity", "danceability", "energy", "key", "loudness", "mode", "speechiness", "acousticness","instrumentalness", "liveness", "valence", "tempo")

colnames(songs_2) <- c("artists", "track_name", "duration_ms", "popularity", "danceability", "energy", "key", "loudness", "mode", "speechiness", "acousticness","instrumentalness", "liveness", "valence", "tempo")

df_new_only <- anti_join(deduped_songs2, songs_2)
df_combined <- bind_rows(songs_2, df_new_only)
df_new_only2 <- anti_join(deduped_songs, df_combined)
df_combined <- bind_rows(df_combined, df_new_only2)

# This line was added due to computation constraints -- Our computes couldn't handle the full dataset. 50 was chosen arbitrarily 
df_combined <- df_combined %>% arrange(desc(popularity)) %>% filter(popularity >50) %>% distinct(artists, track_name, .keep_all = TRUE) %>% distinct(duration_ms, track_name, .keep_all = TRUE) 

write.csv(df_combined, "df_combined.csv")