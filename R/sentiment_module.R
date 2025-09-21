# sentiment_module.R
library(tidyverse)
library(syuzhet)

score_sentiment <- function(df, text_col = "text_body") {
  s <- df %>%
    mutate(clean = clean_text(.data[[text_col]])) %>%
    mutate(sentiment = get_sentiment(clean, method = "syuzhet")) %>%
    group_by(platform) %>%
    summarise(mean_sentiment = mean(sentiment, na.rm = TRUE), .groups = "drop")
  return(s)
}
