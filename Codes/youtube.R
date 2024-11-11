library(tidyverse)
library(tuber)
library(jsonlite)
library(httr)
library(vosonSML)


options(stringsAsFactors = FALSE) # setting strings as characters so that they appear as they were in the dataset

youtube_auth <- Authenticate("youtube", apiKey = "your API", ) #Activating your youtube API

galampsey = c("71hNxnjAfn0", "frJhTkX3E6M", "kj2S5RcfvKc", "1b4C3ORyDR4", "WGhU8s2wp_Q","fCulR9-6AV8", "Yuy-ScBotvA",
              "u7oOUwroT0o", "Pq3QkXD_g18","n87dumuGbI", "caGclk8yEHk", 
              "S4hwWtRlNqQ") #Video ID
comments <- youtube_auth  |> # Collecting the comments from the video
  Collect(videoIDs = galampsey,
          maxComments = 4300,
          verbose = T)

Both_ids = c("-sPcTO2mv6o&t=2s", "R6VKnOXyITk")
Both_comments <- youtube_auth  |> 
  Collect(videoIDs = Both_ids,
          maxComments = 100000,
          verbose=T)
