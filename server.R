library(shiny)
library(ShinyRatingInput)
library(data.table)


# read in data
myurl = "https://liangfgithub.github.io/MovieData/"
# movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
file_path <- "movies.dat"
movies <- readLines(file_path, warn = FALSE)

movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))


# load the genre_rating matrix
genre_rating <- read.csv("my_table.csv", header = TRUE)


# load the ratings matrix
ratings_matrix <- read.csv("Rmat.csv", header = TRUE)

matrix_size=dim(ratings_matrix)[2]

# load the similarity matrix from the .cvs file I saved earlier
file_path <- "save_S.csv"
S <- as.matrix(read.csv("save_S.csv", header = TRUE, row.names = 1))



get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}


myIBCF <- function(w) {
  
  ibcf <- numeric(matrix_size)
  top <- 30
  
  for (i in 1:matrix_size) {
    row_values <- S[i, ]
    top_indices <- order(row_values, decreasing = TRUE)[1:top]
    rated_indices <- which(!is.na(w))
    
    if (i %in% rated_indices) {
      ibcf[i] <- 0
      next
    }
    
    s_u <- as.numeric(S[i, top_indices])
    w_v <- as.numeric(w[top_indices])
    
    I <- which(!is.na(s_u) & !is.na(w_v))
    s_u <- s_u[I]
    w_v <- w_v[I]
    
    # print(length(s_u))
    # print(length(w_v))
    
    ibcf[i] <- (s_u %*% w_v) / sum(s_u)
  }
  
  return(ibcf)
}


server <- function(input, output) {
  # show the movies to be rated
  output$favorite <- renderUI({
    num_rows <- 2
    num_movies <- 5 # movies per row
    
    selected_genre <- input$favorite_genre
    
    recommend_movieID <- genre_rating[which(genre_rating$genre == selected_genre),][2:11]
    
    recommend_movies <- movies[which(movies$MovieID %in% recommend_movieID),]
    
    len_movies <- sum(!is.na(recommend_movieID))
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        if((i - 1) * num_movies + j <= len_movies){
          list(box(width = 2,
              div(style = "text-align:center", img(src = recommend_movies$image_url[(i - 1) * num_movies + j], height = 150)),
              div(style = "text-align:center", strong(recommend_movies$Title[(i - 1) * num_movies + j]))
          )) 
        }
      })))
    })
  })
  
  
  # show the books to be rated
  output$ratings <- renderUI({
    num_rows <- 3
    num_movies <- 5 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 #div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  
  # Calculate recommendations when the sbumbutton is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      # print(value_list)
      user_ratings <- get_user_ratings(value_list)
      
      idx <- which(sub("^m", "", colnames(ratings_matrix)) %in% user_ratings$MovieID)
      
      w <- vector("logical", length = matrix_size)
      w[] <- NA 
      w[idx] <- user_ratings$Rating
      
      ibcf = myIBCF(w)
      
      top_recommand <- order(ibcf, decreasing = TRUE)[1:10]
      
      recom_results <- data.table(Rank = 1:10, 
                                  MovieID = movies$MovieID[top_recommand], 
                                  Title = movies$Title[top_recommand], 
                                  Predicted_rating =  ibcf[top_recommand])
      
    }) # still busy
    
  }) # clicked on button
  
  
  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
}