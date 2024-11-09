setwd("C:/Users/PROXIMITY REPORT/Desktop/SICSS2024")

library(tidyverse)
library(tidytext)
GHtweets_covid <- read_csv("GHtweets_covid.csv")
KEtweets_covid <- read_csv("KEtweets_covid.csv")
NGtweets_covid <- read_csv("NGtweets_covid.csv")


view(GHtweets_covid )
view(KEtweets_covid)
view(NGtweets_covid)


ghTweets<- GHtweets_covid |> 
  #select(tweet_id, user_name, source, possibly_sensitive, text)  %>%
  select(tweet_id,created_at, text, source)  |> 
  mutate(device = fct_collapse(source,
                               "Apple" = c("Tweetbot for iΟS", "Twitter for iPad", "Twitter for iPhone"),
                               "Android" = c("Twitter for Android"),
                               "Other" = c("Instagram")
  )) |> 
  mutate(text.ori = text) |> 
  unnest_tokens("word", text) |>  
  mutate(word2 = fct_collapse(word,
                              "covid19" = c("covid", "19", "covid19", "coronavirus", "corona", "virus"),
                              "cure" = c("cure", "treatment", "treat", "treating")
  )
  ) %>% 
  mutate(yearMonth = format(as.Date(created_at), "%Y-%m"))
view(ghTweets)

data('stop_words')

ghTweets_new<-ghTweets |> 
  anti_join(stop_words) 

   count(word2) |> 
  arrange(desc(n))
  



nrc_count<-ghTweets_new |> 
  mutate(word=word2) |> 
  inner_join(nrc, relationship = 'many-to-many') |> 
  rename(nrc=sentiment) |> 
  group_by(yearMonth,nrc, .add=TRUE) |> 
  mutate(nwords=n()) |>
  ungroup() |> 
  mutate(nrc = fct_reorder(nrc, nwords))
  
  
view(nrc_count)


bing_count<-ghTweets_new |>
  inner_join(bing) |> 
  rename(bing=sentiment) 





afinn_count<-ghTweets_new |>
  inner_join(afinn) 



lough_count<-ghTweets_new |>
  inner_join(loughran) |> 
  rename(lough=sentiment)



nrc_plot_tweet<-nrc_count |> 
  ggplot(aes(y=nwords, x=yearMonth,  color=nrc, group=nrc))+
  geom_line()+theme_minimal()+
  theme(
    text = element_text(size = 10),       # General text size
    axis.text = element_text(size = 8),   # Axis text size
    axis.title = element_text(size = 10), # Axis title size
    legend.text = element_text(size = 8), # Legend text size
    plot.title = element_text(size = 12))
  facet_wrap(~nrc, scales='free_x')

plotly::ggplotly(nrc_plot_tweet)

# Increase plot height and save the plot
ggsave("nrc_plot_sentiment.jpg", plot = nrc_plot_tweet, width = 10, height = 8, dpi = 300)
  

nrc_plot_tweet1<-nrc_count |> 
  plotly::plot_ly(aes(y=nwords, x=yearMonth, size=nwords, color=nrc))+
  plotly::add_line()







view(nrc_count)


  
new_bing<-




install.packages('textdata')
library(textdata)
nrc<-get_sentiments('nrc') |> 
  select(everything())



loughran<-get_sentiments('loughran') |> 
  select(everything())
afinn <- get_sentiments('afinn') |> 
  select(everything())

bing <- get_sentiments('bing') |> 
  select(everything())
