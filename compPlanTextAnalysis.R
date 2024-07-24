# TEXT ANALYSIS OF SEATTLE'S COMPREHENSIVE PLAN

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

# RAW DATA----
bio.dic <- read_csv("biodiversity_terms/biodiversity_terms.csv")
d22 <- stri_read_lines("texts/CouncilAdopted2022FullPlan.txt")
d24 <- stri_read_lines("texts/OneSeattlePlanDraftPlan2024.txt")

# TIDY DATA
# I want to combine the documents but note where they came from
d22 <- as_tibble(d22) %>%
  mutate(source = "d22")
d24 <- as_tibble(d24) %>%
  mutate(source = "d24")

d <- rbind(d22, d24)


# NGRAM ANALYSIS 2024 CLIMATE AND NATURE TERMS

ngrams <- d %>%
  unnest_tokens(output = ngram, value, n = 2, token = "ngrams") %>%
  separate(ngram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word, 
         !is.na(word1), 
         !is.na(word2)) %>%
  mutate(bigram = paste(word1, word2))

nplot <- ngrams %>% group_by(source) %>%
  count(bigram, sort = TRUE) %>%
  ungroup() %>%
  pivot_wider(names_from = source, values_from = n) %>%
  mutate(d22 = replace_na(d22, 0),
         d24 = replace_na(d24, 0), 
         delta = d24-d22,
         bigram = fct_reorder(bigram, delta)) %>%
  arrange(desc(delta))

nplotmaxmin <- nplot %>%
  filter(delta > 20 | delta < -20)

ggplot(nplotmaxmin, aes(x = bigram, y = delta)) +
  geom_col() +
  coord_flip()

## We see a clear increased use of terms around public space and public spaces
## We see a clear increase use of climate change and carbon pollution
## capital facilities. There is a move away from talking about 
## urban villages/centers and the term "marinalized populations"

## What are new terms in d24?
view(nplot %>% filter(d22 == 0 & d24 > 0))

## Terms including "indigenous" are new, as well as new terms regarding
## climate hazards. There is use of "underserve communities", housing crisis


## What crises?
bis <- as.character(nplot$bigram)
str_view(bis, pattern = "crisis", match = TRUE, html = TRUE)

nplot[str_detect(bis, pattern = "crisis"),]

### homelessness, housing, climate, displacement, affordability
### the only crisis present in previous draft was affordability.

### Let's look at trigrams
ngrams <- d %>%
  unnest_tokens(output = ngram, value, n = 3, token = "ngrams") %>%
  separate(ngram, into = c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word,
         !is.na(word1), 
         !is.na(word2),
         !is.na(word3)) %>%
  mutate(trigram = paste(word1, word2, word3))

nplot <- ngrams %>% group_by(source) %>%
  count(trigram, sort = TRUE) %>%
  ungroup() %>%
  pivot_wider(names_from = source, values_from = n) %>%
  mutate(d22 = replace_na(d22, 0),
         d24 = replace_na(d24, 0), 
         delta = d24-d22,
         trigram = fct_reorder(trigram, delta)) %>%
  arrange(desc(delta))

view(nplot)

nplotmaxmin <- nplot %>%
  filter(delta >= 5 | delta < -5)

ggplot(nplotmaxmin, aes(x = trigram, y = delta)) +
  geom_col() +
  coord_flip()

## much more talk about greenhouse gas emissions, sound regional council
## seattle transportation plan, sea level rise, again much less use of terms
## around urban village strategy.

# New terms in 2024
view(nplot %>%
       filter(d22 == 0 & d24 > 0))

## climate impacts from SLR and others a notable and significant add as is covid19 
## and anti-displacement

## Terms retired from 2022
view(nplot %>%
       filter(d24 == 0))

## African american; 

## How are we talking about justice?

view(nplot[str_detect(as.character(nplot$trigram), pattern = "justice"), ])

## criminal down, environmental, climate, and historical up; social steady to down

## How are we talking about wildlife?

view(nplot[str_detect(as.character(nplot$trigram), pattern = "wildlife"), ])

## Generally talking about wildlife less;

view(nplot[str_detect(as.character(nplot$trigram), pattern = "environment"), ])

## What do we learn from the chart? New top terms for 2024:
## tree canopy; community based; gas emmissions; 

bidelta <- nplot %>% pivot_wider(names_from = source, values_from = n) %>%
  mutate(d22 = replace_na(d22, 0),
         d24 = replace_na(d24, 0), 
         delta = d24-d22,
         bigram = fct_reorder(bigram, delta)) %>%
  arrange(desc(delta))
  
view(bidelta)


ngrams24 <- as_tibble(d24) %>%
  unnest_tokens(output = ngram, value, n = 2, token = "ngrams") %>%
  separate(ngram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word, 
         !is.na(word1),
         !is.na(word2)) %>%
  mutate(bigram = str_c(word1, word2, sep = " "))

view(ngrams24[1:100, ])

ngrams24 %>% count(bigram, sort = TRUE)

bigrams24 <- ngrams24 %>%
  filter((word1 %in% bio.dic$term | word2 %in% bio.dic$term | bigram %in% bio.dic$term)) %>%
  count(word1, word2, sort = TRUE)

bigram24_graph <- bigrams24 %>%
  #filter(n > 1) %>%
  as_tbl_graph()

bigram24_graph

ggraph(bigram24_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

arrow <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram24_graph, layout = "fr") + 
  geom_edge_link(aes(alpha = n), show.legend = F, 
                 arrow = arrow, end_cap = circle(0.07, "inches")) + 
  geom_node_point(color = "lightblue", size = 5) + 
  geom_node_text(aes(label = name), size = 2, vjust = 1, hjust = 1)



# NGRAM ANALYSIS 2022 CLIMATE AND NATURE TERMS
ngrams22 <- as_tibble(d22) %>%
  unnest_tokens(output = ngram, value, n = 2, token = "ngrams") %>%
  separate(ngram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word, 
         !is.na(word1),
         !is.na(word2)) %>%
  mutate(bigram = str_c(word1, word2, sep = " "))

view(ngrams24[1:100, ])

ngrams22 %>% count(bigram, sort = TRUE)

bigrams22 <- ngrams22 %>%
  filter((word1 %in% bio.dic$term | word2 %in% bio.dic$term | bigram %in% bio.dic$term)) %>%
  count(word1, word2, sort = TRUE)

bigram22_graph <- bigrams22 %>%
  #filter(n > 1) %>%
  as_tbl_graph()

bigram22_graph

ggraph(bigram22_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

arrow <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram22_graph, layout = "fr") + 
  geom_edge_link(aes(alpha = n), show.legend = F, 
                 arrow = arrow, end_cap = circle(0.07, "inches")) + 
  geom_node_point(color = "lightblue", size = 5) + 
  geom_node_text(aes(label = name), size = 2, vjust = 1, hjust = 1)


## The network analysis shows a much more complex network
## of interacting terms, indicating more thinking and planning specifically around climate change

## comparing 24 and 22 env/climate bigrams

c22 <- bigrams22 %>%
  mutate(bigram = paste(word1, word2, sep = " ")) %>%
  dplyr::rename(count22 = n) %>%
  select(bigram, count22)

c24 <- bigrams24 %>%
  mutate(bigram = paste(word1, word2, sep = " ")) %>%
  dplyr::rename(count24 = n) %>%
  select(bigram, count24)

compare <- dplyr::full_join(c22, c24)

view(compare)

compare[is.na(compare)] <- 0

compare <- compare %>%
  mutate(delta = (count24-count22)/count22) %>%
  arrange(desc(delta)) %>%
  mutate(bigram = fct_reorder(bigram, delta))

ggplot(compare[compare$delta >= -1 & compare$delta <= 15 & compare$delta != 0, ], aes(x = bigram, y = delta)) +
  geom_col() +
  coord_flip()

bigrams22 %>%
  filter(word1 == "conservation" | word2 == "conservation")

view(bigrams24)

view(words24)
str(stop_words)

words22 <- d22 %>%
  unnest_tokens(word, value) %>%
  anti_join(stop_words) %>%
  count(word) %>%
  arrange(desc(n))

view(words22)

wordcloud(
  words = words$word,
  freq = words$n
)

d22.sentiments <- words22 %>%
  inner_join(get_sentiments("loughran"))

d24.sentiments <- words24 %>%
  inner_join(get_sentiments("loughran"))

sent22 <- d22.sentiments %>%
  group_by(sentiment) %>%
  summarise(sum = sum(n)) %>%
  ungroup() %>%
  mutate(sentiment = fct_reorder(sentiment, sum))

ggplot(sent22, aes(x = sentiment, y = sum)) +
  geom_col()

sent24 <- d24.sentiments %>%
  group_by(sentiment) %>%
  summarise(sum = sum(n)) %>%
  ungroup() %>%
  mutate(sentiment = fct_reorder(sentiment, sum))

ggplot(sent24, aes(x = sentiment, y = sum)) +
  geom_col()

table(sent22)
table(sent24)
ratio22 <- (731 / (467 + 250 + 156 + 110))
ratio24 <- (888 / (667 +298 + 178 + 126))


## Search for biodiversity words
terms22 <- words22 %>%
  filter(word %in% bio.dic$term) %>%
  rename(n22 = n)

terms24 <- words24 %>% 
  filter(word %in% bio.dic$term) %>%
  rename(n24 = n)

terms_all <- full_join(terms22, terms24) %>%
  pivot_longer(2:3, names_to = "Year", values_to = "n")

terms_all[is.na(terms_all)] <- 0

ggplot(terms_all, aes(x = word, y = n, group = Year, color = Year)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme(axis.text.x = element_text(angle = 90))

## But how to search for specific terms?

# Network analysis on "crisis"?

str_count(d24, pattern = "crisis")

## REMOVE EMPTY LINES

d24 <- as.data.frame(d24[apply(d24, 2, str_length) >= 1])
d22 <- d22[apply(d22, 2, str_length) >= 1]

d24.dfm <- as.dfm(d24)

toks <- tokens(d22, remove_punct = TRUE)
toks_nram <- tokens_ngrams(toks, n = 2)

?slice_max
rtext <- readtext("C:/My Files/Learning/DataCamp/Text Analysis")
corp <- corpus(rtext)

view(corp)

str(rtext)
view(rtext)

comp <- tm::Corpus(VectorSource(rtext[["text"]]))
str(comp)

words <- comp %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word) %>%
  arrange(desc(n))


words <- comp %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word) %>%
  arrange(desc(n))

textstat_summary(comp)
chap <- corpus_segment(corp, pattern = "Introduction")

## Exploring the text of George Ebers "Cleopatra" 

text <- stri_read_lines("C:/My Files/Learning/DataCamp/Text Analysis/Cleopatra.txt")

start <- str_which(text, pattern = "START OF THE PROJECT")
end <- str_which(text, pattern = "END OF THE PROJECT")

cleo <- text[(start + 1):(end - 1)]
cleo.tibx <- as_tibble(cleo)

view(cleo.tibx)

?unnest_tokens

words <- cleo.tibx %>%
  unnest_tokens(word, value) %>%
  anti_join(stop_words) %>%
  count(word) %>%
  arrange(desc(n))

cleo_sentiments <- words %>%
  inner_join(get_sentiments("loughran"))

view(cleo_sentiments)

sent <- cleo_sentiments %>%
  group_by(sentiment) %>%
  summarise(sum = sum(n)) %>%
  ungroup() %>%
  mutate(sentiment = fct_reorder(sentiment, sum))

ggplot(sent, aes(x = sentiment, y = sum)) +
  geom_col()
