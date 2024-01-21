# SOCS0100-Web-API
## Overview
This study is for analysing music taste by using Spotify streaming data. The code use Spotify for Developers platform and the "spotifyr" package to access audio features in selected playlists. A interactive dashboard is created to present the data visualisation using Shiny App in R.

## Set Up
```
library(spotifyr)
library(tidyverse)
library(knitr)
library(scales)
library(ggjoy)
library(ggplot2)
library(shiny)
library(wordcloud)
```
These packages will be used in the code. 

## Spotify API data collection
```
Sys.setenv(SPOTIFY_CLIENT_ID = 'a2d7a26c672a48bbad504a4457ce7b4d')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '7cf618358d4241739aea91db68cd1d81')
access_token <- get_spotify_access_token()
username <- 'Spotify'
```
Please change the SPOTIFY_CLIENT_ID and SPOTIFY_CLIENT_SECRET into your personal registered account information on Spotify for Developers.

## Data Wrangling 
Change playlist by using the unique URI. 
```
#get top 50 playlists data
usa <- get_playlist_audio_features(username, '37i9dQZEVXbLRQDuF5jeBp', authorization = access_token)
japan <- get_playlist_audio_features(username, '37i9dQZEVXbKXQ4mDTEBXq', authorization = access_token)
uk <- get_playlist_audio_features(username, '37i9dQZEVXbLnolsZ8PSNw', authorization = access_token)
germany <- get_playlist_audio_features(username, '37i9dQZEVXbJiZcmkrIHGU', authorization = access_token)
france <- get_playlist_audio_features(username, '37i9dQZEVXbIPWwFssbupI', authorization = access_token)
korea <- get_playlist_audio_features(username, '37i9dQZEVXbNxXF4SkHj9F', authorization = access_token)
global <- get_playlist_audio_features(username, '37i9dQZEVXbMDoHDwVN2tF', authorization = access_token)


#tidy data, choose variables and rename
USA <- select(usa, danceability, valence, track.id, track.name, track.album.name, track.album.release_date)%>%
  mutate(region="USA")
Japan <- select(japan, danceability, valence, track.id, track.name, track.album.name, track.album.release_date)%>%
  mutate(region="Japan")
UK <- select(uk, danceability, valence, track.id, track.name, track.album.name, track.album.release_date)%>%
  mutate(region="UK")
Germany <- select(germany, danceability, valence, track.id, track.name, track.album.name, track.album.release_date)%>%
  mutate(region="Germany")
France <- select(france, danceability, valence, track.id, track.name, track.album.name, track.album.release_date)%>%
  mutate(region="France")
South_korea <- select(korea, danceability, valence, track.id, track.name, track.album.name, track.album.release_date)%>%
  mutate(region="South Korea")
Global <- select(global, danceability, valence, track.id, track.name, track.album.name, track.album.release_date)%>%
  mutate(region="Global")

#combine in to one large dataset
data <- rbind(USA,Japan,UK,Germany,France,South_korea)
#rename the variables and add a column that extract the year of album release
data1 <- rename(data, name=track.name, album=track.album.name, date=track.album.release_date)%>%
  mutate(date = as.Date(date),year = format(date, "%Y"))
global_data <- rename(Global, name=track.name, album=track.album.name, date=track.album.release_date)%>%
  mutate(date = as.Date(date),year = format(date, "%Y"))
```

## Interactive dashboard (shiny)
This part total generates 7 different plots, user interface and server function can be changed as need.
```
#define ui - set sidebar and choices in shiny, sidebar is in the side panel, the plot is in the main panel
ui <- sidebarLayout(
  sidebarPanel(
    selectInput("taste", "Music taste on:", choices = c("Danceability", "Joy", "Retro Taste","Most Popular Track")),
    selectInput("scope", "Scope", choices = c("Regional", "Global"))
  ),
  mainPanel(plotOutput("plot"))
)
```
```
#define server
server <- function(input, output) {
#load data as reactive
data1_reactive <- reactive({data1})
global_reactive <- reactive({global_data})
#define outputs
output$plot <- renderPlot({
  #visualisation 1 - joy plot, which region prefer danceable songs?
  if (input$taste == 'Danceability') {
    if (input$scope == 'Regional'){
      p <- ggplot(data1_reactive(), aes(x = danceability, y = region)) + 
        geom_joy() + 
        theme_joy() + 
        ggtitle("Joyplot of top 50 tracks' danceability distributions", subtitle = "Comparison among 6 regions based on Spotify API data")+
        labs(x = "Danceability", y = "Region")
    }
    else {
      p <- ggplot(global_reactive(), aes(x = danceability, y = region)) + 
        geom_joy() + 
        theme_joy() +
        ggtitle("Joyplot of top 50 tracks' danceability distribution", subtitle = "Global context based on Spotify API data")+
        labs(x = "Danceability", y = "Region")
    }
  } 
  #visualisation 2 - joy plot, which region prefer joy songs?
  else if (input$taste == 'Joy'){
    if (input$scope == 'Regional'){
      p <- ggplot(data1_reactive(), aes(x = valence, y = region)) + 
        geom_joy() + 
        theme_joy() +
        ggtitle("Joyplot of top 50 tracks' joy distributions", subtitle = "Comparison among 6 regions based on Spotify API data")+
        labs(x = "Joy", y = "Region")
    }
    else {
      p <- ggplot(global_reactive(), aes(x = valence, y = region)) + 
        geom_joy() + 
        theme_joy() +
        ggtitle("Joyplot of top 50 tracks' joy distribution", subtitle = "Global context based on Spotify API data")+
        labs(x = "Joy", y = "Region")
    }
  }
  #visualisation 3 - bar plot, distribution of release year
  else if (input$taste == 'Retro Taste'){
    if (input$scope == 'Regional'){
      p <- ggplot(data1_reactive(), aes(x = factor(year))) + 
        geom_bar(fill = "orange") +
        ggtitle("Barplot of top 50 tracks' album release year", subtitle = "Comparison among 6 regions based on Spotify API data")+
        labs(x = "Album Release Year", y = "Frequency")
    }
    else {
      p <- ggplot(global_reactive(), aes(x = factor(year))) + 
        geom_bar(fill = "orange") +
        ggtitle("Barplot of top 50 tracks' album release year", subtitle = "Global context based on Spotify API data")+
        labs(x = "Album Release Year", y = "Frequency")
    }
  }
  #visualisation 4 - word cloud, which songs are the most popular?
  else if (input$taste == "Most Popular Track"){
    if (input$scope == 'Regional'){
      text <- data1_reactive()$name
      word_freq <-table(text)
      par(mar = c(2, 2, 2, 2)) 
      options(repr.plot.width = 8, repr.plot.height = 2)
      p <- wordcloud(words = names(word_freq),
                     freq = word_freq,
                     min.freq = 2,
                     scale = c(2.4, 0.2),
                     colors = brewer.pal(8, "Dark2"))
    } 
  }
  
  p
})
}
```
```
# Run the Shiny app
shinyApp(ui, server)
```
