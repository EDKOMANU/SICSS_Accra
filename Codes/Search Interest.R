install.packages('gtrendsR')


library(gtrendsR)
library(tidyverse)


keywords<-c( 'Hommie', 'Rich Hommie', 'Rich Hommie Quan', 'Quan')
countries<-c('GH')



GH_trends<- gtrends(keyword=keywords, 
                 gprop="web",
                 geo= "GH", 
                 time="2010-01-01 2024-09-08",
                  
                 compared_breakdown = FALSE,
                 low_search_volume = FALSE,
                 cookie_url = "http://trends.google.com/Cookies/NID")



NG_trends<- gtrends(keyword=keywords, 
                    gprop=c("web"),
                    geo= c("GH"), 
                    time="2024-01-01 2024-09-08",
                    
                    compared_breakdown = FALSE,
                    low_search_volume = FALSE,
                    cookie_url = "http://trends.google.com/Cookies/NID")






over_time<-GH_trends$interest_over_time |> 
  rename(countries=geo,
         artiste=keyword) |> 
  mutate(
         hits=if_else(hits=='<1', '1', hits),
         hits=as.numeric(hits))
  
 
  
NGover_time<-NG_trends$interest_over_time |> 
  rename(countries=geo,
         artiste=keyword) |> 
  mutate(
         hits=if_else(hits=='<1', '1', hits),
         hits=as.numeric(hits))

New<-rbind(over_time, NGover_time) |> 
  mutate(year = format(as.Date(date)))
  
 

set.seed(2022)
new<-mosaic::shuffle(NEW,groups = c(NEW$countries, NEW$artiste))
new<-new$data

library(ggplot2)


NEW<- ggplot(NGover_time, mapping=aes(x=date,y=hits, group=artiste, colour=artiste))+
  geom_line() +
  labs(title = "Trend of Hits Over Time",
       x = "Year",
       y = "Hits") +
  theme_minimal()+
  facet_wrap(~countries)

ggplot(New, aes(x = date, y = hits, 
                color=artiste,group = artiste)) +
  geom_step() +
  labs(title = "Trend of Hits Over Time",
       x = "Year",
       y = "Hits") +
  
  facet_wrap(~countries)
  

