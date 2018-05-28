library(rhdf5)   # reading hdf5 files
library(beepr)   # beep sound
library(MASS)    # save matrices
library(R.utils) # binary numbers
library(quadprog)# finding Beta
library(optimx)  # optimisation
library(jsonlite)# AoTM dataset
library(Matrix)  # nearPD?
library(matrixcalc)#check P.D.

MSDS_duplicates_tracks <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/MSDS_duplicates_tracks.txt"))

### Reading the list of tracks
tracklist <- readLines("/Users/binKarim/Desktop/FYP Datasets/MSD/MillionSongSubset/AdditionalFiles/subset_unique_tracks.txt",
                  encoding="UTF-8")
tracklist <- as.matrix(tracklist)
dim(tracklist)
head(tracklist)

# splitting
trackname_initial  <- rep("NIL",10000)
song_id    <- rep("NIL",10000)
artistname <- rep("NIL",10000)
songtitle  <- NULL
albumname  <- NULL
for (i in 1:10000) {
  t <- strsplit(tracklist[i,], split = "<SEP>")
  trackname_initial[i]  <- t[[1]][1]
  song_id[i]    <- t[[1]][2]
  artistname[i] <- tolower(t[[1]][3])
}

MSDS_duplicates_indices <- which(trackname_initial %in% MSDS_duplicates_tracks)

# naming the files using trackname (files are named as such)
filename <- NULL
for (i in 1:10000) {
  filename[i] <- paste("/Users/binKarim/Desktop/FYP Datasets/MSD/MillionSongSubset/datafiles/",
                       trackname_initial[i],".h5",sep = "")
}
head(filename)

# list of song titles & albums
(start.time <- Sys.time())
for (i in 1:10000) {      # 2.8 mins
  song <- h5read(filename[i],"/metadata/songs")
  songtitle[i] <- tolower(song$title)
  albumname[i] <- song$release
}
end.time <- Sys.time()
(difftime(end.time,start.time,units = c("auto")))
for (i in 1:3)
  beep("facebook")

filename  <- filename[-MSDS_duplicates_indices]
trackname <- trackname_initial[-MSDS_duplicates_indices]
song_id   <- song_id[-MSDS_duplicates_indices]
albumname <- albumname[-MSDS_duplicates_indices]
songtitle <- songtitle[-MSDS_duplicates_indices]
artistname <- artistname[-MSDS_duplicates_indices]

write.table(filename,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Basic/filename.txt",row.names = F, col.names = F)
write.matrix(trackname_initial,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Basic/trackname_initial.txt")
write.matrix(trackname,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Basic/trackname.txt")
write.matrix(song_id,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Basic/song_id.txt")
write.table(albumname,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Basic/albumname.txt",row.names = F, col.names = F)

filename <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Basic/filename.txt"))
trackname_initial <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Basic/trackname_initial.txt"))
trackname <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Basic/trackname.txt"))
song_id <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Basic/song_id.txt"))
albumname <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Basic/albumname.txt"))

h5createFile("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Basic/songtitle.h5") 
attr(songtitle, "scale") <- "liter" 
h5write(songtitle, "/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Basic/songtitle.h5", "songtitle")
songtitle <- h5read("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Basic/songtitle.h5", "songtitle")
songtitle <- h5read("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\kmt_lastfm.h5", "songtitle")

h5createFile("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Basic/artistname.h5") 
attr(artistname, "scale") <- "liter" 
h5write(artistname, "/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Basic/artistname.h5", "artistname")
artistname <- h5read("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Basic/artistname.h5", "artistname")
artistname <- h5read("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\kmt_lastfm.h5", "artistname")

# list
trackname
song_id
albumname
songtitle
artistname
filename



################################################################
# Functions
################################################################

displaysongdetails <- function(song_indices) {
  i <- 1
  l <- length(song_indices)
  while (i <= l) {
    cat("(", song_indices[i], ")\n", sep="")
    cat("Title:  ", songtitle[song_indices[i]], "\n")
    cat("Artist: ", artistname[song_indices[i]], "\n")
    cat("Genre:  ", genre[song_indices[i]], "\n")
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

MSD_trackname <- function(trackname_input) {
  song_id <- which(trackname %in% trackname_input)
  displaysongdetails(song_id)
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


# list
displaysongdetails(c(1,9999))
randomsongs(2,1:10000)
search.query()
explore.song()
youtube()
plot.timbre()
meanvar(c(1,2,3))
