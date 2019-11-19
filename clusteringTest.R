
library(tidyverse)
library(tibble)
library(lubridate)
library(GGally)
library(cluster)
library(VIM)
library(fpc)

spotify.clustering <- read_csv("inst/edited_spotify.csv")

clean_data <- spotify.clustering %>%
  mutate(AlbumReleaseDate = parse_date_time(AlbumReleaseDate, orders = c("y", "ym","ymd"))) %>%
  #Old-school grepl method
  mutate(Artist = ifelse(grepl("Beyonc*", Artist), 'Beyonce', Artist)) %>%
  #Tidyverse str_detect method
  mutate(Artist = ifelse(Artist %>% 
                           str_detect("Janelle Mon*"), 'Janelle Monae', Artist)) %>%
  mutate(Artist = ifelse(AlbumBestChartPosition %>% 
                           str_detect("#N/A"), 0, Artist)) %>%
  na.omit() %>%
  mutate(id = row_number()) %>%
  mutate(id = as.character(id))
sapply(data, class)

aggr(clean_data) # checks for missing data

test_data <- subset(clean_data, ((AlbumName == "A Girl Called Dusty") | 
                                 (AlbumName == "Action!") |
                                 (AlbumName == "Selling England By The Pound") |
                                 (AlbumName == "Carpenters") | 
                                 (AlbumName == "Ride On") |
                                 (AlbumName == "Autoamerican") |
                                 (AlbumName == "Selected Ambient Works 85-92") |    
                                 (AlbumName == "Different Class") |
                                 (AlbumName == "O") |
                                 (AlbumName == "The Elder Scrolls IV: Oblivion: Original Game Soundtrack") |
                                 (AlbumName == "AM") |
                                 (AlbumName == "An Awesome Wave")))

training_data <- subset(clean_data, ((AlbumName != "A Girl Called Dusty") & 
                                     (AlbumName != "Action!") &
                                     (AlbumName != "Selling England By The Pound") &
                                     (AlbumName != "Carpenters") & 
                                     (AlbumName != "Ride On") &
                                     (AlbumName != "Autoamerican") &
                                     (AlbumName != "Selected Ambient Works 85-92") &    
                                     (AlbumName != "Different Class") &
                                     (AlbumName != "O") &
                                     (AlbumName != "The Elder Scrolls IV: Oblivion: Original Game Soundtrack") &
                                     (AlbumName != "AM") &
                                     (AlbumName != "An Awesome Wave")))

training_data <- training_data[c("TrackDuration", "TrackDanceability",
                                 "TrackEnergy", "TrackKey", "TrackLoudness",
                                 "TrackSpeechiness", "TrackAcousticness",
                                 "TrackInstrumentalness", "TrackLiveness", "TrackValence",
                                 "TrackTempo")]

clusters <- pam(training_data, 6)

plotcluster(training_data, clusters$cluster, color = TRUE, shade = TRUE, labels = 2, lines = 0)


"

Things I want to do:

  - figure out which factors are relevant and how to remove the useless ones;
  - try and figure out a number of clusters (maybe use pamk() in the  fpc package),
    or maybe by using the genre strings to create a set of theoretical clusters;
  - test out the nstart option in kmeans();
  - use pam() instead of kmeans();
  - try other clustering algorithms that don't require a number of clusters;
  - write a weighting function to use on songs.  
  
"