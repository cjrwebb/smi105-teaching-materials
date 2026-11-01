install.packages("localLLM")

library(tidyverse)
library(localLLM)

install_localLLM()

# Clear cached models
cache_dir <- file.path(tools::R_user_dir("localLLM", "cache"), "models")
unlink(cache_dir, recursive = TRUE)
list_cached_models()

# https://huggingface.co/ggml-org/gemma-3-1b-it-GGUF

resp <- quick_llama("What is the sentiment of the following text? I thought the service was bad. I wish they were faster.", 
                    model_path = "week-06/handout/gemma-3-1b-it-Q4_K_M.gguf",
                    system_prompt = "Only reply with responses to the question and nothing else.", 
                    verbosity = 0, clean = TRUE, max_tokens = 100, temperature = 0.9)

cat(resp)


resp <- quick_llama(prompt = "Summarise the following responses to a survey: I felt anxious before I arrive but now I'm here everyone is nice I was worried about starting but now I feel okay. I still don't know if I made the right choice but so far everything is okay.", 
                    system_prompt = "Only reply with responses to the question and nothing else.", 
                    verbosity = 0, clean = TRUE, max_tokens = 100)

cat(resp)


survey_resps <- c("I would recommend everyone goes to the Rutland Arms for a proper Sunday Lunch and to Red Brick Market as well. The afterwards I might go to Pearl and drink a cocktail while lookng out over the city.",
                  "I would visit the Peak District to go hiking and then maybe go to the Nottingham House for a pie and chips afterwards",
                  "Probably to go to a metal gig at Corporation and then get a kebab from Aslans on the way home.")


resp <- quick_llama(prompt = survey_resps, 
                    system_prompt = "Extract only the place or activity names and provide them in a response, do not include any extraneous information. Separate the activities/places with commas. Only reply with responses to the question and nothing else.", 
                    verbosity = 0, clean = TRUE, max_tokens = 100, temperature = 0.5)

paste(resp)

library(googlesheets4)

gs4_auth()
survey_resp <- read_sheet("https://docs.google.com/spreadsheets/d/1y6Ep8KZcmTY_Itso3p5Lbw6QPo2Viy0zaXkQrCFO5Bo/edit?usp=sharing", 
           skip = 1, na = "",
           col_names = c("time", "resident", "feel_before", "know_before", "feel_now", "perfect_day"))

survey_resp

resp <- quick_llama(prompt = survey_resp$perfect_day, 
                    system_prompt = "Extract only the place or activity names and provide them in a response, do not include any extraneous information. Separate the activities/places with commas. Ensure all places/activities have consistent capitalisation. Only reply with responses to the question and nothing else.", 
                    verbosity = 0, clean = TRUE, max_tokens = 100, temperature = 0.5)

paste(resp)


library(tidytext)
library(gutenbergr)
library(textdata)
library(ggwordcloud)
library(googlesheets4)

gs4_auth()
survey_resp <- read_sheet("https://docs.google.com/spreadsheets/d/1y6Ep8KZcmTY_Itso3p5Lbw6QPo2Viy0zaXkQrCFO5Bo/edit?usp=sharing", 
                          skip = 1, na = "",
                          col_names = c("time", "resident", "feel_before", "know_before", "feel_now", "perfect_day"))

survey_resp

dracula <- gutenberg_download(345)
frankenstein <- gutenberg_download(84)

# Word Cloud and Counts

# Use in workbook
survey_resp %>%
  select(resident, perfect_day) %>%
  unnest_tokens(word, perfect_day) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

survey_resp %>%
  select(resident, perfect_day) %>%
  unnest_tokens(word, perfect_day) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  slice(1:100) %>%
  ggplot() +
  aes(label = word, size = n) +
  geom_text_wordcloud() +
  theme_void() +
  scale_size_area(max_size = 10) 

survey_resp %>%
  select(resident, perfect_day) %>%
  unnest_tokens(word, perfect_day) %>%
  anti_join(stop_words) %>%
  count(resident, word, sort = TRUE) %>%
  slice(1:100) %>%
  ggplot() +
  aes(label = word, size = n) +
  geom_text_wordcloud() +
  facet_wrap(~resident) +
  theme_void() +
  scale_size_area(max_size = 10) 





dracula %>%
  slice(-1:-85) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) 

dracula %>%
  slice(-1:-85) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE) %>%
  slice(1:100) %>%
  ggplot() +
  aes(label = word, size = n) +
  geom_text_wordcloud() +
  theme_void()

# Bi-grams


# Sentiment 
afinn <- get_sentiments("afinn")

# Use in workbook
survey_resp %>%
  select(feel_before, feel_now) %>%
  pivot_longer(cols = c(feel_before, feel_now), names_to = "when", values_to = "response") %>%
  unnest_tokens(word, response) %>%
  count(when, word, sort = TRUE) %>%
  inner_join(afinn, by = "word") %>%
  mutate(
    sentiment = n*value
  ) %>% 
  ggplot() +
  aes(x = fct_reorder(word, sentiment), y = sentiment, fill = when) +
  geom_col() +
  coord_flip() 

# Feeling/sentiment wordcloud
survey_resp %>%
  select(feel_before, feel_now) %>%
  pivot_longer(cols = c(feel_before, feel_now), names_to = "when", values_to = "response") %>%
  unnest_tokens(word, response) %>%
  count(when, word, sort = TRUE) %>%
  inner_join(afinn, by = "word") %>%
  mutate(
    sentiment = n*value
  ) %>% 
  ggplot() +
  aes(label = word, size = sentiment, colour = sentiment) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 30) + 
  scale_colour_viridis_c(option = "C")


dracula %>%
  slice(-1:-85) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

# Use bigrams to add context to sentiment analysis
negation_words <- c("no", "not", "don't", "never", "without")

# Use in workbook
survey_resp %>%
  select(feel_before, feel_now) %>%
  pivot_longer(cols = c(feel_before, feel_now), names_to = "when", values_to = "response") %>%
  unnest_tokens(word, response, token = "ngrams", n = 2) %>%
  drop_na(word) %>%
  separate(word, c("word1", "word2"), sep = " ") %>%
  inner_join(afinn, by = c("word2"="word")) %>%
  mutate(value = ifelse(word1 %in% negation_words, value*-1, value)) %>%
  group_by(when, word2) %>%
  summarise(sentiment = sum(value)) %>%
  ggplot() +
  aes(x = fct_reorder(word2, sentiment), y = sentiment, fill = when) +
  geom_col() +
  coord_flip() 


dracula %>%
  slice(-1:-85) %>%
  unnest_tokens(word, text, token = "ngrams", n = 2) %>%
  drop_na(word) %>%
  separate(word, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE) 

drac_neg_words <- dracula %>%
  slice(-1:-85) %>%
  mutate(line = row_number()) %>%
  unnest_tokens(word, text, token = "ngrams", n = 2) %>%
  drop_na(word) %>%
  separate(word, c("word1", "word2"), sep = " ") %>%
  #count(word1, word2, sort = TRUE) %>%
  filter(word1 %in% negation_words) %>%
  inner_join(afinn, by = c(word2 = "word")) %>%
  mutate(
    value = -1*value
  )

drac_reg_words <- dracula %>%
  slice(-1:-85) %>%
  mutate(line = row_number()) %>%
  unnest_tokens(word, text, token = "ngrams", n = 2) %>%
  drop_na(word) %>%
  separate(word, c("word1", "word2"), sep = " ") %>%
  #count(word1, word2, sort = TRUE) %>%
  filter(!word1 %in% negation_words) %>%
  inner_join(afinn, by = c(word2 = "word")) 


# Use in workbook
dracula %>%
  slice(-1:-85) %>%
  mutate(line = row_number()) %>%
  unnest_tokens(word, text, token = "ngrams", n = 2) %>%
  drop_na(word) %>%
  separate(word, c("word1", "word2"), sep = " ") %>%
  inner_join(afinn, by = c(word2 = "word")) %>%
  mutate(
    value = ifelse(word1 %in% negation_words, value*-1, value)
  ) %>%
  arrange(line) %>%
  mutate(
    line_group = cut_interval(line, 100, labels = FALSE)
  ) %>%
  group_by(line_group) %>%
  summarise(sentiment = sum(value)) %>% 
  ggplot() +
  aes(x = line_group, y = sentiment) +
  geom_col() +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  labs(x = "Percentage of Book")


bind_rows(drac_neg_words, drac_reg_words) %>%
  arrange(line) %>%
  mutate(
    line_group = cut_interval(line, 100, labels = FALSE)
  ) %>%
  group_by(line_group) %>%
  summarise(sentiment = sum(value)) %>% 
  ggplot() +
  aes(x = line_group, y = sentiment) +
  geom_col() +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  labs(x = "Percentage of Book")


# Homework

frank_neg <- frankenstein %>%
  slice(-1:-44) %>%
  mutate(line = row_number()) %>%
  unnest_tokens(word, text, token = "ngrams", n = 2) %>%
  drop_na(word) %>%
  separate(word, c("word1", "word2"), sep = " ") %>%
  #count(word1, word2, sort = TRUE) %>%
  filter(word1 %in% negation_words) %>%
  inner_join(afinn, by = c(word2 = "word")) %>%
  mutate(
    value = -1*value
  )

frank_reg <- frankenstein %>%
  slice(-1:-44) %>%
  mutate(line = row_number()) %>%
  unnest_tokens(word, text, token = "ngrams", n = 2) %>%
  drop_na(word) %>%
  separate(word, c("word1", "word2"), sep = " ") %>%
  #count(word1, word2, sort = TRUE) %>%
  filter(!word1 %in% negation_words) %>%
  inner_join(afinn, by = c(word2 = "word")) 

bind_rows(frank_neg, frank_reg) %>%
  arrange(line) %>%
  mutate(
    line_group = cut_interval(line, 100, labels = FALSE)
  ) %>%
  group_by(line_group) %>%
  summarise(sentiment = sum(value)) %>% 
  ggplot() +
  aes(x = line_group, y = sentiment) +
  geom_col() +
  scale_x_continuous(labels = scales::percent_format(scale = 1)) +
  labs(x = "Percentage of Book")


# Using LLMs to extract information from free text


