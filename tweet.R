library(tidyverse)
library(tidytext)
library(textdata)
library(wordcloud)

tweets <- read_csv("tweets.csv")

# View the first few rows
head(tweets)

clean_tweets <- tweets %>%
  mutate(text = str_replace_all(text, "https?://\\S+\\s*", "")) %>%
  mutate(text = str_replace_all(text, "[^[:alnum:]\\s]", "")) %>%
  mutate(text = tolower(text))
words <- clean_tweets %>%
  unnest_tokens(word, text)

# Remove common stop words
data("stop_words")
words_cleaned <- words %>%
  anti_join(stop_words)

# Load sentiment lexicon
bing <- get_sentiments("bing")

# Join words with sentiments
sentiment_words <- words_cleaned %>%
  inner_join(bing, by = "word")

# Count sentiments
sentiment_summary <- sentiment_words %>%
  count(sentiment)

print(sentiment_summary)

ggplot(sentiment_summary, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Sentiment Analysis of Tweets",
       x = "Sentiment",
       y = "Word Count")

wordcloud(words = words_cleaned$word, max.words = 100, colors = brewer.pal(8, "Dark2"))