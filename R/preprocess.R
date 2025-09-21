# preprocess.R
library(tidyverse)
library(tidytext)
library(SnowballC)
library(stopwords)

# ---- Helpers ----
get_stopwords <- function() {
  tibble(word = unique(c(stop_words$word, stopwords::stopwords("en"))))
}

clean_text <- function(text) {
  text %>%
    str_replace_all("http\\S+|www\\S+", " ") %>%
    str_replace_all("[\r\n]", " ") %>%
    str_replace_all("[^\\x01-\\x7F]", " ") %>%
    str_replace_all("[[:punct:]]", " ") %>%
    str_replace_all("[[:digit:]]+", " ") %>%
    str_squish() %>%
    tolower()
}

# ---- Tokenize & Count ----
tokenize_and_count <- function(df, text_col = "text_body", time_col = "created_utc", platform_col = "platform") {
  df2 <- df %>%
    mutate(clean = clean_text(.data[[text_col]]),
           created_date = as.Date(.data[[time_col]])) %>%
    select(created_date, platform = all_of(platform_col), clean)
  
  tokens <- df2 %>%
    unnest_tokens(word, clean) %>%
    anti_join(get_stopwords(), by = "word") %>%
    mutate(word = wordStem(word, language = "en")) %>%
    count(created_date, platform, word, sort = TRUE)
  
  return(tokens)
}

