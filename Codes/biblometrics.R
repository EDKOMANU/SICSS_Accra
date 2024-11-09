
install.packages('bibliometrix')
library(bibliometrix)
library(pubmedR)
library(easyPubMed)
library(tidyverse)
library(citecorp)
library(countrycode)
library(ggraph)
library(tidygraph)
library(tidytext)
library(lda)



new_query<- '(Media[TIAB] AND Social[TIAB] OR Influencers[TIAB] AND Ghana[TIAB] AND (2014[PDAT]:2024[PDAT])'

id_query<- epm_query(new_query)

database<- epm_fetch(id_query)
article_list<- epm_parse(database, compact_output=TRUE)
article_df<- get_epm_data(article_list)
view(article_df)

DOIS = article_df$doi
oc_coci_meta(doi = DOIS[10])

author_list <- epm_parse(database,
                         compact_output = FALSE)
auth_df <- get_epm_data(author_list)


## A robust code for pulling records for multiple dois.
cite_count = DOIS %>% map( ~ tryCatch({
  cat(paste0("completed • ", .x, "\n"))
  oc_coci_meta(doi = .x)},
  error = function(e) {
    cat(paste0("•Error• ", .x, "\n"))
  }))
cite_count[sapply(cite_count, is.null)] <- NULL

view(DOIS)

## We can classify article as based on whether they are open access or subscription only.
cite_countDF <- do.call("rbind", cite_count) %>% 
  mutate(opA = ifelse(oa_link != "", "Open Access", "Subscription"))


auth_df<- auth_df |> 
  mutate(affil=affiliation, 
         ) |> 
  mutate(country= gsub(".*,\\s*", "\\1", affil), 
         country1 = countryname(country, destination='country.name.en', nomatch=NA ),
         country=if_else(str_detect(affil, pattern='.Dakar') & is.na(country1), "Senegal", 
                  if_else(str_detect(affil,pattern=".Sant") & is.na(country1), 'Burkina',
                          if_else(country=="" & is.na(country1),'USA',country) )),
  country1 = countryname(country, destination='country.name.en', nomatch=NA ), 
  school=str_extract(pattern= "(?<=,\\s)[^,]+(?=,)", affil)) |> 
  mutate(University = str_extract(affiliation, pattern = "([A-Za-z][^\\s,.]+[.]?\\s[(]?)*(University|Institute|Academy|Hospital|Research)[^,\\d]*(?=,|\\d|\\s)"),
         authors= if_else(duplicated(pmid), "Others", "Main"),
         authors= if_else(!is.na(email) & authors == "Others",'Co-Author', authors )) 



by_year<-auth_df |> 
  group_by(year) |> 
  summarise(number=n()) |> 
  ungroup() |> 
  mutate(prop = number/sum(number)) |> 
  mutate( cumlative= cumsum(number),
         lg = log(number, base = exp(1)))


plot(by_year$year, by_year$number, type = "s", col = "blue", main = "Original Growth")
plot(by_year$year, by_year$lg, type = "s", col = "red", main = "Log-Transformed Growth")


ggplot(by_year, aes(x=year, y=lg ) )+
  geom_line()



auth_df<-auth_df |> 
  group_by(country1) |> 
  mutate(aut2 = (n()/2^2),
         aut3 = (n()/2^3))
View(auth_df)

other_nat<- auth_df |> 
  select(country,aut2, aut3) |> 
  distinct(country1, .keep_all = T) |> 
  filter(country1 !='Ghana')

other_nat<-other_nat |> 
  pivot_longer(cols=c(aut2, aut3), names_to = 'n_aut', values_to = 'numbers' ) |> 
  mutate(country1=fct_reorder())
  
ggplot(other_nat, aes(reorder(x=country1,numbers), y=numbers))+
  geom_col()+
  coord_flip() +
  facet_wrap(~n_aut, scales='free_y')

cou<- auth_topic$country1


nan<- auth_df |> 
  filter()
data('stop_words')

auth_topic<- auth_df |> 
  unnest_tokens( 'word', title) |> 
  anti_join(stop_words) |> 
  filter(!word %in% cou) |> 
  filter(!word %in% c('ghana', 'uganda')) |> 
  filter(!word %in% 1:2500) |> 
  as_tibble()
  
auth_topic2<- auth_topic |> 
 group_by(country1, word) |> 
  summarise(number=n()) |> 
  arrange(desc(number)) |> 
  ungroup() |> 
  group_by(country1) |> 
  slice_max(number, n=5) |> 
  ungroup() |> 
  arrange(desc(number)) 
  
  
#-----Institution Ghana-----------  
instGhana<- auth_df |> 
  count(University) |>
  filter(country1=='Ghana') |> 
  
  group_by(country1) |>
  slice_max(n, n=15) |> 
  ungroup() |> 
  mutate(University= fct_reorder(University, n))


ggplot(instGhana, aes(x=University, y=n)) +
  geom_col()+
  coord_flip()
#Other Institutions-----
instfiltered<- auth_df |>
  mutate(country1, as.factor(country1)) |> 
  filter(country1!='Ghana') |> 
  select(-country1) |> 
  count(University) |>
  group_by(country1) |>
  slice_max(n, n=5) |> 
  ungroup() |>
  arrange(desc(n)) |> 
  mutate(University= fct_reorder(University, n)) |> 
  head(15)


ggplot(instfiltered, aes(x=University, y=n)) +
  geom_col()+
  coord_flip()
  
cite<- cite_countDF |>
  
  
sum(citations >= seq_along(citations))
  
 


#-----------word#------------- country count----------

link_counts <- vv |> 
  group_by(country1) |> 
  summarise(link_count = n()) |> 
  ungroup() |> 
  mutate(country1= fct_reorder(country1, link_count))  
view(auth_df)

ggplot(link_counts, aes(x=country1, y=link_count))+
  geom_col()+
  coord_flip()




#--------------Nodes and Link Graph-------------
vv<- auth_df |> 
  select(country1) |> 
  mutate(to='Ghana')


graph <- as_tbl_graph(vv)

graph <- graph %>%
  mutate(is_ghana = ifelse(name == "Ghana", "Ghana", "Other"))

ggraph(graph, layout = "fr") + 
  geom_edge_link(aes(color = "gray"), alpha = 0.5, width = 1) +  # Gray links
  geom_node_point(aes(fill = is_ghana), size = 20, shape = 21, color = "black") +  # Larger node for Ghana
  geom_node_text(aes(label = name), vjust = 0.5, hjust = 0.5, color = "black") +  # Text color
  scale_fill_manual(values = c("Ghana" = "red")) +  # Red for Ghana, blue for others
  theme_void() +
  theme(legend.position = "none")  # Hide the legend
ggplot(author, aes(x=year,y=num_pubs, color=country1))+
  geom_line()

#-------------





author<-auth_df |> 
  group_by(country1, year) |> 
  summarise(num_pubs= n())
 

ggraph(graph, layout = "fr") +  
  geom_edge_link() + 
  geom_node_point(size = 10) +
  geom_node_text(aes(label = name, color='gray'), vjust = 0.5, hjust = 0.5) +
  theme_void()




author1<-auth_df |> 
  group_by(pmid, authors) |> 
  summarise(num_pubs= n())

view(author1)


view(auth_df)


other<-auth_df |> 
  group_by(pmid) |> 
  summarise(other_auth = n()-1)
  
  
auth_na<-auth_df |> 
  filter(is.na(country1)) |> 
  group_by(affil) |> 
  summarise(count= n())



view(auth_na)
auth_na
?countryname


auth_df<-auth_df |> 
  mutate(authors= if_else(duplicated(pmid), "duplicate", "unique"))

distinct<-auth_df |>
  distinct(pmid, .keep_all=TRUE)
view(distinct)

new_author <- 
inner_join(distinct,other, by='pmid' ) 


vv <- auth_df |>
  select(country1) |> 
  mutate(to = 'Ghana')

# Count the number of links (connections) from each country to Ghana


# Create a tidygraph object
graph <- as_tbl_graph(vv |> 
                        inner_join(link_counts, by = c("country1" = "country1")))

# Add a column to identify "Ghana" and assign a unique color to each node
graph <- graph %>%
  mutate(color = ifelse(name == "Ghana", "Ghana", name))

# Plot the network with customized node and link appearances
ggraph(graph, layout = "fr") +  # 'fr' stands for Fruchterman-Reingold layout
  geom_edge_link(aes(width = link_count), alpha = 0.7, color = "gray", size=link_counts) +  # Link width reflects the count
  geom_node_point(aes(fill = color), size = link_counts, shape = 21, color = "black") +  # Color nodes based on their name
  geom_node_text(aes(label = name), vjust = 1.5, hjust = 0.5, color = "black") +  # Text color
  scale_fill_manual(values = c("Ghana" = "red", 
                               "OtherColor1" = "blue", 
                               "OtherColor2" = "green",
                               "OtherColor3" = "orange",
                               "OtherColor4" = "purple")) +  # Customize the colors for each node
  theme_void() +
  theme(legend.position = "none")  # Hide the legend


