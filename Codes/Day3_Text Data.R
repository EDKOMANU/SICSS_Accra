
# Load Libraries
library(readr)
library(tidyverse)
library(tidytext)
library(readxl)


GHtweets_covid <- read_csv("Day3/GHtweets_covid.csv")

ghTweets<- GHtweets_covid %>%
  #select(tweet_id, user_name, source, possibly_sensitive, text)  %>%
  select(created_at, text, source)  %>%
  mutate(device = fct_collapse(source,
                               "Apple" = c("Tweetbot for iΟS", "Twitter for iPad", "Twitter for iPhone"),
                               "Android" = c("Twitter for Android"),
                               "Other" = c("Instagram")
  )) %>%
  mutate(text.ori = text) %>%
  unnest_tokens("word", text) %>% 
  mutate(word2 = fct_collapse(word,
                              "covid19" = c("covid", "19", "covid19", "coronavirus", "corona", "virus"),
                              "cure" = c("cure", "treatment", "treat", "treating")
  )
  ) %>% 
  mutate(yearMonth = format(as.Date(created_at), "%Y-%m"))


unwantedWords <- c('dey', 'https', 'rt', 't.co', 'amp', 'dey')

data("stop_words")
top_words <- ghTweets %>%
  anti_join(stop_words) %>%
  filter(!word2 %in% unwantedWords) %>% 
  count(word2) %>%
  arrange(desc(n)) %>% 
  top_n(20)


top50 <- c('covid19', 'cure', 'people', 'ghana', 'patients', 'health')  #, 'hospital', 'vacine', 'madagascar', 'herbal', 'malaria', 'god', 'medicine', 'president')
top50_words <- ghTweets %>%
  anti_join(stop_words) %>%
  filter(word2 %in% top50)  %>% 
  select(device, word2, yearMonth) %>% 
  group_by(yearMonth,  word2)  %>% 
  summarise( totW = n())


library(ggplot2)
topWords.plot <-  ggplot(top_words, aes(x=reorder(word2, -n), y=n, fill=word2))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size=13),
        plot.title =  element_text(hjust = 0.5, size=18))+
  ylab("Count") + 
  xlab("") +
  ggtitle("Number of Unique words Tweeted in Ghana") +
  guides(fill=FALSE)+
  coord_flip
topWords.plot
ggsave("Top word Tweets.jpg", topWords.plot )

table(ghTweets$device)
table(top_words$word2)


party_pal <- c("#1482EE","#228B22","#E9967A","#686868","#FF3300","#EEC900")

# reading score chart
ggplot(top50_words, aes(x = yearMonth, y = word2, color = word2, size = totW)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = party_pal) +
  theme_minimal(base_size = 10) +
  xlab("") +
  ylab("Reading level") +
  guides(col = guide_legend(ncol = 2, override.aes = list(size = 4))) +
  theme(legend.position = c(0.5,0.5),
        legend.text = element_text(color="#909090", size = 16),
        panel.grid.minor = element_blank()
  )
















tidy_trump_tfidf<- GHtweets_covid %>%
  select(created_at,text) %>%
  unnest_tokens("word", text) %>%
  anti_join(stop_words) %>%
  count(word, created_at) %>%
  bind_tf_idf(word, created_at, n)


top_tfidf<-tidy_trump_tfidf %>%
  arrange(desc(tf_idf))

top_tfidf$word[1]


economic_dictionary<-c("covid19","hospital", "ghana")
economic_dictionary<-c("kwasia","gyimii","aboa")


library(stringr)
economic_tweets<-GHtweets_covid[str_detect(GHtweets_covid$text, paste(economic_dictionary, collapse="|")),]



## Sentiment Analysis ----

library(tidytext)
head(get_sentiments("bing"))

trump_tweet_sentiment <- GHtweets_covid %>%
  cross_join(get_sentiments("bing")) %>%
  count(created_at, sentiment) 

head(trump_tweet_sentiment)

GHtweets_covid$date<-as.Date(GHtweets_covid$created_at, 
                             format="%Y-%m-%d %x")


trump_sentiment_plot <-
  GHtweets_covid %>%
  cross_join(get_sentiments("bing")) %>% 
  filter(sentiment=="negative") %>%
  count(date, sentiment)

ggplot(trump_sentiment_plot, aes(x=date, y=n)) +
  geom_line(color="red", linewidth=.5) +
  theme_minimal() +
  theme(axis.text.x = 
          element_text(angle = 60, hjust = 1, size=13)) +
  theme(plot.title = 
          element_text(hjust = 0.5, size=18)) +
  ylab("Number of Negative Words") +
  xlab("") +
  ggtitle("Negative Sentiment in Trump Tweets") +
  theme(aspect.ratio=1/4)

