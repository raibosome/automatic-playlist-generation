library(rhdf5)   # reading hdf5 files
library(beepr)   # beep sound
library(MASS)    # save matrices
library(R.utils) # binary numbers
library(quadprog)# finding Beta
library(optimx)  # optimisation
library(jsonlite)# AoTM dataset 
library(Matrix)  # nearPD?
library(matrixcalc)#check P.D.

filename   <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Basic\\filename.txt"))
trackname  <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Basic\\trackname.txt"))
song_id    <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Basic\\song_id.txt"))
albumname  <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Basic\\albumname.txt"))
songtitle  <- h5read("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Basic\\songtitle.h5", "songtitle")
artistname <- h5read("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Basic\\artistname.h5", "artistname")
genre_names <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Features\\genre_names.txt"))

#####################
##   Functions     ##
#####################

displaysongdetails <- function(song_indices) {
  i <- 1
  l <- length(song_indices)
  while (i <= l) {
    cat("(", song_indices[i], ")\n", sep="")
    cat("Title:  ", songtitle[song_indices[i]], "\n")
    cat("Artist: ", artistname[song_indices[i]], "\n")
    cat("Genre:  ", genre_names[song_indices[i]], "\n")
    cat("Year:   ", year[song_indices[i]], "\n\n")
    i <- i+1
  }
}

randomsongs <- function (no_of_songs,song_indices) {
  response <- "yup"
  while(response=="yup") {    
    #     for (i in 1:1000) {
    #       cat(sample(song_indices),"\t",sample(song_indices),"\t")
    #       cat(sample(song_indices),"\t",sample(song_indices),"\t")
    #       cat(sample(song_indices),"\t",sample(song_indices),"\n")
    #     }
    #     for (i in 1:700) {
    #       cat(sample(0:1,1),"   ",sample(0:1,1),"   ",sample(0:1,1),"   ",sample(0:1,1),"   ",sample(0:1,1),"   ",sample(0:1,1),"   ")
    #       cat(sample(0:1,1),"   ",sample(0:1,1),"   ",sample(0:1,1),"   ",sample(0:1,1),"   ",sample(0:1,1),"   ",sample(0:1,1),"\n")
    #     }
    cat("\n\014")
    rnd <- sample(song_indices, no_of_songs, replace = F)
    displaysongdetails(rnd)
    response <- tolower(readline("Generate again? (yup/nah): "))
  }
}

artistname_letter <- NULL
for (i in 1:10000) {
  artistname_letter[i] <- strsplit(artistname[i],"")[[1]][1]
}
search.query <- function() {
  cat("\n\014")
  beep("facebook")
  cat("Good day, Mr. Raimi. Search for songs in the MSD here.\n")
  cat("\nSearch by\n (a) full artist name\n (b) first letter of artist name\n (c) song title\n (d) year\n (e) song_id\n ")
  cat("\nOr \n (f) get songs randomly\n")
  mode <- readline(prompt = "\nEnter response: ")
  mode <- tolower(mode)
  if (mode == "a" || mode == "b") {
    if (mode == "b") {
      cat("You have selected to search artist name by its first letter.\n")
      Query <- readline(prompt = "Enter artist name's first letter: ")
      found_artists <- which(artistname_letter %in% Query)
      print(sort(unique(artistname[found_artists])))
      cat("\nNumber of artists found for names starting with \"",Query,"\": ",
          length(unique(artistname[found_artists])),sep="")
    }
    Query <- readline(prompt = "Enter artist name: ")
    query <- tolower(Query)
    song_index <- which(artistname %in% query)
    no_of_songs <- length(song_index)
    if (no_of_songs==0)
      return(cat("No results found for \"", Query, "\".", sep=""))
    else
      cat("Number of songs found: ",no_of_songs,"\n")
  }
  
  else if (mode == "c") {
    cat("You have selected to search by song title.\n")
    Query <- readline(prompt = "Enter song title: ")
    query <- tolower(Query)
    song_index <- which(songtitle %in% query)
    no_of_songs <- length(song_index)
    if (no_of_songs==0)
      return(cat("No results found for \"", Query, "\".", sep=""))
    else
      cat("Number of songs found: ",no_of_songs,"\n")
  }
  else if (mode == "d") {
    cat("You have selected to search songs by year.\n")
    query <- readline(prompt = "Enter year (between 1926-2010 inclusive): ")
    song_index <- which(year %in% query)
    no_of_songs <- length(song_index)
    cat("Number of songs found: ",no_of_songs,"\n")
    
    query2 <- readline(prompt = "Enter number of songs to be generated: ")
    randomsongs(query2,song_index)
    youtube()
    return()
  }
  else if (mode == "e") {
    cat("You have selected to search by song_id.\n")
    query <- readline(prompt = "Enter song_id: ")
    song_index <- as.integer(query)
    displaysongdetails(song_index)
    return()
  }
  
  else if (mode == "f") {
    cat("You have selected to get songs randomly.\n")
    query <- readline(prompt = "Enter number of songs to be generated: ")
    randomsongs(query,1:10000)
    youtube()
    return()
  }
  else
    return(cat("You have entered an invalid response. Goodbye.\n"))
  
  toproceed <- readline(prompt = "Display details of the song(s)? Yes/No: ")
  toproceed <- tolower(toproceed)
  cat("\n\014")
  cat(no_of_songs)
  cat(" song(s) found by \"", Query, "\":\n", sep="")
  if (toproceed == "yes")
    displaysongdetails(song_index)
  else if (toproceed == "no")
    return("See you soon, sir.")
  else
    return(cat("You have entered an invalid response. Goodbye.\n"))
  
  youtube()
}

MSD_song_index <- function(song_id_input) {
  which(song_id %in% song_id_input)
}

explore.song <- function() {
  cat("\n\014")
  song_id <- readline(prompt = "Enter song_id: ")
  song_id <- as.integer(song_id)
  mainmenu <- h5ls(filename[song_id])
  print(mainmenu)
  
  submenu <- readline(prompt = "Enter submenu (eg. /analysis/songs): ")
  outputfile <- h5read(filename[song_id],submenu)
  
  return(outputfile)
}

youtube <- function() {
  response <- "yes"
  while(response=="yes") {
    song_id <- readline(prompt = "Enter song_id of song to be played on YouTube: ")
    song_id <- as.integer(song_id)
    
    query <- paste(artistname[song_id],songtitle[song_id])
    query <- gsub(" ", "+", query)
    searchquery <- paste("https://www.youtube.com/results?search_query=",query,sep="")
    browseURL(searchquery)
    
    cat("Play another song?")
    response <- tolower(readline("Yes/No: "))
  }
}

plot.timbre <- function() {
  song_id <- readline(prompt = "Enter song_id: ")
  song_id <- as.integer(song_id)
  displaysongdetails(song_id)
  #   timbs <- h5read(filename[song_id],"/analysis/segments_timbre")
  #   timings <- h5read(filename[song_id],"/analysis/segments_start")
  #   
  timbs <- h5read(filename_schoolcom[song_id],"/analysis/segments_timbre")
  timings <- h5read(filename_schoolcom[song_id],"/analysis/segments_start")
  
  
  namesdim <- c("Average loudness","Brightness","Flatness","Sounds with a stronger attack")
  cat("Plot graph of:\n1) Average loudness\n2) Brightness\n3) Flatness\n4) Sounds with a stronger attack\n")
  while(TRUE) {
    
    whichdim <- readline(prompt="Enter selection: ")
    whichdim <- as.integer(whichdim)
    
    plot(x=timings,
         y=timbs[whichdim,],
         main=c(artistname[song_id],songtitle[song_id]),
         xlab = "Time (s)", ylab=namesdim[whichdim],
         col="green3",type="l")
    
    cat("\nPlot another graph of:\n1) Average loudness\n2) Brightness\n3) Flatness\n4) Sounds with a stronger attack\n")
  }
}

meanvar <- function(x) {
  c(mean(x),var(x))
}

# # list
# displaysongdetails(c(1,9999))
# randomsongs(2,1:10000)
# search.query()
# MSD_song_index()
# explore.song()
# youtube()
# plot.timbre()
# meanvar(c(1,2,3))


