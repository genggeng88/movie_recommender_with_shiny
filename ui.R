library(shiny)
library(shinyjs)
# install.packages("DT")
library(dplyr)
library(ggplot2)
library(recommenderlab)
library(DT)
library(reshape2)
library(devtools)
# devtools::install_github("stefanwilhelm/ShinyRatingInput")
library(usethis)
library(ShinyRatingInput)
library(shinydashboard)

source('functions/helpers.R')

ui <- fluidPage(
  
  dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Movie Recommender"),
    dashboardSidebar(
      # Define the sidebar menu items
      sidebarMenu(
        menuItem("Recommender by Genre", tabName = "genre", icon = icon("film")),
        menuItem("Recommender by Rating", tabName = "rating", icon = icon("star"))
      )
    ),
    dashboardBody(includeCSS("css/movies.css"),
                  tabItems(
                    tabItem(
                      tabName = "genre",
                      fluidRow(
                        box(
                          width = 12, title = "Step 1: Select your favorite genre", status = "info", 
                          solidHeader = TRUE, collapsible = TRUE,
                          div(class = "rateitems", style = "height: 800px; overflow-y: auto;",
                              selectInput("favorite_genre", "select a single genre from the dropdown menu:",
                                          choices = c("Action", "Adventure", "Animation", 
                                                      "Children's", "Comedy", "Crime",
                                                      "Documentary", "Drama", "Fantasy",
                                                      "Film-Noir", "Horror", "Musical", 
                                                      "Mystery", "Romance", "Sci-Fi", 
                                                      "Thriller", "War", "Western")
                              ),
                              uiOutput('favorite')
                          )
                        )
                      )
                    ),
                    
                    tabItem(
                      tabName = "rating",
                      fluidRow(
                        useShinyjs(),
                        box(
                          width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
                          title = "Step 2: Discover movies you might like",
                          br(),
                          div(class = "get_ratings",
                              uiOutput('ratings')
                          ),
                          div(class = "ratings",
                              withBusyIndicatorUI(
                                actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                              )
                          ),
                          br(),
                          uiOutput("results")
                        )
                      )
                    )
                  )
    )
  )
)