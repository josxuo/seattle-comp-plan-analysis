# LIBRARIES----
library(tidyverse)
library(tidytext)
library(wordcloud)
library(textdata)
library(readtext)
library(tm)
library(quanteda)
library(rebus)
library(stringi)
library(tidygraph)
library(ggraph)


bio.dic <- read_csv("biodiversity_terms/biodiversity_terms.csv")
custom_stop <- data.frame(word = c("lu", "cw", "en", "gs", "ce" , "cf", "ac", "ci", "ed", "tg"),
                          lexicon = rep("custome", 10))
stop_words <- rbind(stop_words, custom_stop)

folder <- "C:/My Files/Learning/DataCamp/Text Analysis/CompPlanTextAnalysis/data/processedTextFiles"

corpus <- VCorpus(DirSource(directory = folder, ## gather text files into corpus
                            pattern = "*.txt"))



tidy_corpus <- tidy(corpus) ## create a tidy dataframe
#view(tidy_corpus) # inspect data
str(tidy_corpus)

unique(tidy_corpus$id)

d <- tidy_corpus %>%
  filter(!id %in% c("2022_[15]_citywide_planning_shoreline_areas_pro.txt",
                         "2022_[14]_citywide_planning_container_port_pro.txt")) %>% ## create year and section columns
  mutate(year = str_extract(id, pattern = or("2022", "2024")),
         section = str_extract(id, pattern = "planning_(.*?).txt"),
         section = str_replace(section, pattern = "planning_", replacement = ""),
         section = str_replace(section, pattern = ".txt", replacement = ""),
         section = str_replace_all(section, pattern = "_pro", replacement = ""),
         section = str_replace_all(section, pattern = "_", replacement = " ")) %>%
  select(year, section, text)

unique(d$section)

## view(d)

bigrams <- d %>%  ## create bigrams, excluding stop words,  digits, and null values
  unnest_tokens(output = ngram, text, n = 2, token = "ngrams") %>%
  separate(ngram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word, 
         !is.na(word1), 
         !is.na(word2),
         !str_detect(word1, pattern = DGT),
         !str_detect(word2, pattern = DGT)) %>%
  mutate(bigram = paste(word1, word2))

trigrams <- d %>%  ## create trigrams, excluding stop words and null values
  unnest_tokens(output = ngram, text, n = 3, token = "ngrams") %>%
  separate(ngram, into = c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word,
         !is.na(word1), 
         !is.na(word2),
         !is.na(word3)) %>%
  mutate(trigram = paste(word1, word2, word3))

biplot <- bigrams %>% group_by(year, section) %>%  ## Count bigrams by year, by section, filter for word in biodiversity terms list
  count(bigram, sort = TRUE) %>%
  ungroup() %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  mutate(bigram = paste(word1, word2)) %>%
  filter(word1 %in% bio.dic$term | word2 %in% bio.dic$term | bigram %in% bio.dic$term)

view(biplot)

## Might add confusion to separate by section. Don't filter on biodiversity terms

biplot <- bigrams %>%
  group_by(year) %>%
  count(bigram, sort = TRUE)


delta <- biplot %>% pivot_wider(names_from = year, values_from = n) %>%
  rename(old = `2022`, new = `2024`) %>%
  mutate(old = case_when(is.na(old) == TRUE ~ -9999, 
                         TRUE ~ old),
         new = case_when(is.na(new) == TRUE ~ -9999, 
                       TRUE ~ new)) %>%
  group_by(bigram) %>%
  summarise(delta = new - old) %>%
  ungroup() %>%
  mutate(bigram = fct_reorder(bigram, delta))

view(delta)

## Terms used in old and not used in new
retired <- filter(delta, delta < -9999)

view(retired)

ggplot(filter(delta, (delta >= 1 | delta <= -1)), aes(x = bigram, y = delta)) +
  geom_col() +
  coord_flip()


view(bigrams)

view(filter(bigrams, word1 == "ad"))

view(bigrams[(str_length(bigrams$word2) == 2), ])

str_length(bigrams$word2) == 4
