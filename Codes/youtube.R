library(tidyverse)
library(tuber)
library(jsonlite)
library(httr)
library(vosonSML)


options(stringsAsFactors = FALSE) # setting strings as characters so that they appear as they were in the dataset

youtube_auth <- Authenticate("youtube", apiKey = "AIzaSyBIZcKqKB47tlZr9DkkBUvuRw4AzV-HAc0") #Activating your youtube API

video_ids = c("71hNxnjAfn0") #Video ID
comments <- youtube_auth  |> # Collecting the comments from the video
  Collect(videoIDs = video_ids,
          maxComments = 4300,
          verbose = FALSE)

Both_ids = c("-sPcTO2mv6o&t=2s", "R6VKnOXyITk")
Both_comments <- youtube_auth  |> 
  Collect(videoIDs = Both_ids,
          maxComments = 100000,
          verbose=T)
