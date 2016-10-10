extract_reviews <- function(lines){
  rev <- vector("character")
  rating <- vector("numeric")
  sentiment <- vector("character")
  aux <- vector("character")
  
  j <- 1
  
  while(j <= length(lines)){
    #j = j+1
    if(lines[j]=="<review>"){
      #j <- j+1
      while(lines[j]!="</review>"){
        if(lines[j]=="<rating>"){
          rating <- c(rating, as.numeric(lines[j+1]))
          if(as.numeric(lines[j+1])<4){
            sentiment <- c(sentiment, "negative")
          }
          else{
            sentiment <- c(sentiment, "positive")
          }
        }
        if(lines[j]=="<review_text>"){
          while(lines[j]!="</review_text>"){
            j <- j+1
            if(lines[j]!="</review_text>") aux <- c(aux, lines[j])
          }
        }
        
        if(lines[j]=="</review_text>"){
          rev <- c(rev, paste(aux, collapse = " "))
          aux <- ""
        }
        
        if(lines[j]!="</review>") j = j+1
        
      }### bucle de review
    }
    j <- j+1
  }
  
  df <- data.frame(REVIEW = rev, RATING = rating, SENTIMENT = sentiment)
  
  df
  
}

more_df <- function(x) {
  file <- tempfile()
  sink(file); on.exit(sink())
  print(x)
  file.show(file, delete.file = T)
}

more <- function(myfile){
  x <- readLines(con = file(myfile))
  more_df(x)
  close.connection(con = file(myfile))
}