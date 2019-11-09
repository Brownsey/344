
# NOTE FROM STEPHEN: use ggpairs for variable correlations

library(tidyr)
library(dplyr)
library(knitr)
library(ggplot2)
library(rio)

spotify.genres <- import("edited_spotify.xlsx", setclass = "tibble") %>% 
  mutate(ArtistGenres = strsplit(ArtistGenres, ","))

genres <- na.omit(unique(unlist(spotify.genres$ArtistGenres)))

split_genres <- NULL

for(i in 1:length(genres)) {
  split_genres[i] <- strsplit(genres[i], " ")
}

genre_words <- unlist(split_genres)

unique_genre_words <- unique(genre_words)

genre_words_table <- subset(as.data.frame(table(genre_words)), (Freq > 3))

genre_freq_plot <- ggplot(data = genre_words_table, aes(x=genre_words, y=Freq)) + geom_col()

print(genre_freq_plot)

# remove adjectives

