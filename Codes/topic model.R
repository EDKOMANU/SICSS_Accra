install.packages("topicmodels")
library(topicmodels)





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



nrc_count<-ghTweets_new |> 
  mutate(word=word2) |> 
  inner_join(nrc, relationship = 'many-to-many') |> 
  rename(nrc=sentiment) |> 
  group_by(yearMonth,nrc, .add=TRUE) |> 
  mutate(nwords=n()) |>
  ungroup() |> 
  mutate(nrc = fct_reorder(nrc, nwords)) |> 
  cast_dtm(yearMonth, word, nwords)

View(nrc_count)

covid_topic<- nrc_count |> 
  LDA(k=3, method="Gibbs", 
      control= list(seed=50)) |> 
  tidy('beta') |> 
  arrange(desc(beta)) |> 
  group_by(topic) |> 
  slice_max(beta, n=15) |> 
  mutate(term= fct_reorder(term, beta))

class(auth_topic)


ggplot(covid_topic, aes(x=term, y= beta, fill= as.factor(topic)))+
  geom_col()+
  facet_wrap(~topic, scales='free')+
  coord_flip()

cite_countDF |> 
  sum(citations >= seq_along(citations))




summary(covid_topic)
