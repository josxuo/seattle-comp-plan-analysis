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

# Useful terms and stop words

bio.dic <- read_csv("biodiversity_terms/biodiversity_terms.csv")
custom_stop <- data.frame(word = c("lu", "cw", "en", "gs", "ce" , "cf", "ac", "ci", "ed", "tg"),
                          lexicon = rep("custom", 10))
stop_words <- rbind(stop_words, custom_stop)

# Data and corpus

folder <- "C:/My Files/Learning/DataCamp/Text Analysis/CompPlanTextAnalysis/data/processedTextFiles"

corpus <- VCorpus(DirSource(directory = folder, ## gather text files into corpus
                            pattern = "*.txt"))



tidy_corpus <- tidy(corpus) ## create a tidy dataframe
# view(tidy_corpus) # inspect data
# str(tidy_corpus)

# Tidy data
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


## Analyze bigrams
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
# if 10000 or greater, then term is new to 2024. If -10000 or greater, term is retired


## Terms used in old and not used in new
retired <- filter(delta, delta < -9999) %>%
  mutate(freq = -9999-delta)
view(retired)

## retired word cloud
wordcloud(retired$bigram, freq = retired$freq, min.freq = 4)


ggplot(filter(delta, (delta >= 1 | delta <= -1)), aes(x = bigram, y = delta)) +
  geom_col() +
  coord_flip()


view(bigrams)

view(filter(bigrams, word1 == "ad"))

view(bigrams[(str_length(bigrams$word2) == 2), ])

str_length(bigrams$word2) == 4
view(biplot)

## word clouds for bigrams
wordcloud(words = biplot[biplot$year == 2022, ]$bigram, 
          freq = biplot[biplot$year == 2022, ]$n, min.freq = 10)

wordcloud(words = biplot[biplot$year == 2024, ]$bigram, 
          freq = biplot[biplot$year == 2024, ]$n, min.freq = 10)

?wordcloud

## Explore trigrams

trigrams <- d %>%  ## create trigrams, excluding stop words and null values
  unnest_tokens(output = ngram, text, n = 3, token = "ngrams") %>%
  separate(ngram, into = c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word,
         !is.na(word1), 
         !is.na(word2),
         !is.na(word3),
         !str_detect(word1, pattern = DGT),
         !str_detect(word2, pattern = DGT),
         !str_detect(word3, pattern = DGT)) %>%
  mutate(trigram = paste(word1, word2, word3))

triplot <- trigrams %>%
  group_by(year) %>%
  count(trigram, sort = TRUE) %>%
  mutate(trigram = fct_reorder(trigram, n)) %>%
  ungroup()

view(triplot)

wordcloud(triplot$trigram, freq = triplot$n, min.freq = 7)

ggplot(triplot[triplot$n >= 7, ], aes(x = trigram, y = n)) +
  geom_col() + 
  coord_flip() + 
  facet_wrap(~ year, scales = "free")

levels(triplot$trigram)[9100:9133]

## Most common trigram in 2022 = urban village strategy followed by manufacturing industrial centers
## Most common trigram in 2024 = greenhouse gas emissions followed by income restricted affordable
## third most for both is lower income households

tridelta <- triplot %>% pivot_wider(names_from = year, values_from = n) %>%
  rename(old = `2022`, new = `2024`) %>%
  mutate(old = case_when(is.na(old) == TRUE ~ -9999, 
                         TRUE ~ old),
         new = case_when(is.na(new) == TRUE ~ -9999, 
                         TRUE ~ new)) %>%
  group_by(trigram) %>%
  summarise(delta = new - old) %>%
  ungroup() %>%
  mutate(bigram = fct_reorder(trigram, delta))

view(tridelta)

## Trigrams more frequently used

ggplot(tridelta[tridelta$delta >= 10003, ], aes(x = trigram, y = delta-9999)) +
  geom_col() + 
  coord_flip()

## Greater reference to the Seattle Transportation Plan; greater consideration
## for sea level rise and other climate impacts

unigrams <- trigrams <- d %>%  ## create unigrams (i.e., words), excluding stop words and null values
  unnest_tokens(output = word, text) %>%
  filter(!word %in% stop_words$word,
         !is.na(word), 
         !str_detect(word, pattern = DGT)) %>%
  group_by(year) %>%
  count(word, sort = TRUE)
view(unigrams)

view(filter(unigrams, year == 2022))

d <- unigrams %>%
  filter(year == 2022) %>%
 # slice_max(., n, n = 30) %>%
  left_join(., bio.dic, join_by("word"=="term")) %>%
  replace(is.na(.), "other") %>%
  mutate(word = fct_reorder(word, n))

ggplot(d, aes(x = word, y = n, fill = category)) +
  geom_col() + 
  coord_flip() + 
  scale_fill_manual(breaks = c("other", "climate"),
                    values = c("lightblue", "salmon")) +
  ylab("Word count") + xlab("") +
  ggtitle("30 most frequently used words in the 2022 comp plan") +
  theme_minimal()

d2 <- unigrams %>%
  filter(year == 2024) %>%
 # slice_max(., n, n = 30) %>%
  left_join(., bio.dic, join_by("word" == "term")) %>%
  replace(is.na(.), "other") %>%
  mutate(word = fct_reorder(word, n))

ggplot(d2, aes(x = word, y = n, fill = category)) +
  geom_col() + 
  coord_flip() + 
  ylab("Word count") + xlab("") +
  scale_fill_manual(breaks = c("other", "climate"),
                    values = c("lightblue", "salmon")) +
  ggtitle("30 most frequently used words in the draft 2024 comp plan") +
  theme_minimal()

view(d2)

slice_max(d2, n, n = 30)

shared <- inner_join(d, d2,  join_by("word" == "word"))
view(shared)

view(full_join(d, d2) %>%
       pivot_wider(names_from = year, values_from = n))

filter(unigrams, word == "affordable")

delta <- full_join(d, d2) %>%
  pivot_wider(names_from = year,
              values_from = n) %>%
  rename(old = `2022`,
         new = `2024`) %>%
  replace(is.na(.), 0) %>%
  mutate(delta = new - old) %>%
  left_join(., bio.dic, join_by("word"=="term")) %>%
  replace(is.na(.), "other") %>%
  select(word, delta, category.y) %>%
  rename(category = category.y)

view(delta)


ggplot(slice_max(delta, delta, n = 30) %>%
         mutate(word = fct_reorder(word, delta)), aes(x = word, y = delta)) +
  geom_col() +
  coord_flip() +
  theme_minimal()

ggplot(slice_min(delta, delta, n = 30) %>%
         mutate(word = fct_reorder(word, abs(delta))), aes(x = word, y = delta)) +
  geom_col() +
  coord_flip() +
  theme_minimal()
