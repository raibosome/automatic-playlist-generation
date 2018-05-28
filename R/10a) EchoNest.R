library(digest)
apikey <- "az8ztag25p2kku4cwjjqabge"
secret <- "qknc8VkWPZ"
string <- paste(apikey,
                secret,
                as.character(as.integer(as.POSIXct(Sys.time()))),
                sep="")
(sig <- digest(object = string,algo = "md5",serialize="F"))
sig="a99818ce282b830cbd0408ccf58e99d1"
(target <- paste("http://api.rovicorp.com/data/v1.1/album/tracks?album=california+gurls&count=0&offset=0&country=US&language=en&format=json&apikey=",apikey,
                "&sig=",sig,sep=""))
fromJSON(target)

http://api.rovicorp.com/data/v1.1/album/tracks?album=california+gurls&count=0&offset=0&country=US&language=en&format=json&apikey=

# library(RJSONIO)
# api_key = "XLUSPWGQKMBLGGJYI"

meanlogvar <- function(x) {
  c(mean(x),log(var(x)))
}

echonest.features <- function(title,artist) {
  cat("Title:  ",title,"\n",sep="")
  cat("Artist: ",artist,"\n",sep="")
  #   genre <- find_genre(artist)
  
  original_title <- title
  
  if (grepl(x = title,pattern = "&") || grepl(x = artist,pattern = "&") || 
        grepl(x = title,pattern = "-")) {
    artist <- URLencode(paste("artist",artist,sep="="))
    title  <- URLencode(paste("title",title,sep="="))
    artist <- gsub(pattern = "&",replacement = "%26",x = artist)
    title  <- gsub(pattern = "&",replacement = "%26",x = title)
    title  <- gsub(pattern = "-",replacement = "",x = title)
  }
  else {
    artist <- URLencode(paste("artist",artist,sep="="))
    title  <- URLencode(paste("title",title,sep="="))
  }
  target <- paste("http://developer.echonest.com/api/v4/song/search?api_key=XLUSPWGQKMBLGGJYI&format=json&results=20",artist,title,
                  "bucket=id:7digital-US&bucket=audio_summary&bucket=tracks",sep="&")
  results <- fromJSON(target)
  
  song <- results$response$songs
  
  l <- length(song$title)
  
  if (l == 0) {
    title <- strsplit(original_title," \\(feat.")[[1]][1]
    original_title <- title
    if (grepl(x = title,pattern = "&") || grepl(x = artist,pattern = "&") || 
          grepl(x = title,pattern = "-")) {
      title  <- URLencode(paste("title",title,sep="="))
      title  <- gsub(pattern = "&",replacement = "%26",x = title)
      title  <- gsub(pattern = "-",replacement = "",x = title)
    }
    else
      title  <- URLencode(paste("title",title,sep="="))
    target <- paste("http://developer.echonest.com/api/v4/song/search?api_key=XLUSPWGQKMBLGGJYI&format=json&results=20",artist,title,
                    "bucket=id:7digital-US&bucket=audio_summary&bucket=tracks",sep="&")
    results <- fromJSON(target)
    
    song <- results$response$songs
    
    l <- length(song$title)
  }

  if (l == 0) {
    beep("facebook")  
    cat("Song not found. Enter title and artist manually:\n")
    title  <- readline("Title: ")
    original_title <- title
    artist <- readline("Artist: ")
    if (grepl(x = title,pattern = "&") || grepl(x = artist,pattern = "&")) {
      artist <- URLencode(paste("artist",artist,sep="="))
      title  <- URLencode(paste("title",title,sep="="))
      artist <- gsub(pattern = "&",replacement = "%26",x = artist)
      title  <- gsub(pattern = "&",replacement = "%26",x = title)
    }
    else {
      artist <- URLencode(paste("artist",artist,sep="="))
      title  <- URLencode(paste("title",title,sep="="))
    }
    target <- paste("http://developer.echonest.com/api/v4/song/search?api_key=XLUSPWGQKMBLGGJYI&format=json&results=20",artist,title,
                    "bucket=id:7digital-US&bucket=audio_summary&bucket=tracks",sep="&")
    results <- fromJSON(target)
    
    song <- results$response$songs
    l <- length(song$title)
    if (l == 0) {
      cat("Song still not found. Skipped.\n")
      return(NA)
    }
  }
  
  if (l == 1 || tolower(song$title[1]) == tolower(original_title))
    song_index = 1
  else if (tolower(song$title[2]) == tolower(original_title))
    song_index = 2
  else if (tolower(song$title[3]) == tolower(original_title))
    song_index = 3
  else {
    beep(1)
    for (i in 1:l)   #no of songs
      cat(i,")\t",song$title[i],"\n",sep="")
    song_index <- readline(prompt = "Select song no.: \n")
    if (song_index=="")
      song_index = 1
    song_index <- as.integer(song_index)
  }
  
  mode <- song$audio_summary$mode[song_index]
  tempo <- song$audio_summary$tempo[song_index]
  time_signature <- song$audio_summary$time_signature[song_index]
  analysis_url <- song$audio_summary$analysis_url[song_index]
  
  analysis <- fromJSON(analysis_url)
  names(analysis)
  analysis$segments$timbre
  length(analysis$segments$timbre)
  analysis$segments$timbre[[90]] # 90th segment
  
  bigtimbre <- matrix(0,ncol=7,nrow=length(analysis$segments$timbre))
  for (i in 1:length(analysis$segments$timbre))
    bigtimbre[i,] <- analysis$segments$timbre[[i]][1:7]
  
  timbre <- apply(bigtimbre,MARGIN = 2,meanlogvar) #1st row = mean, 2nd row=logvar
  
  # tempo
  if (tempo <= 84)
    tempo = 1
  else if (tempo >  84 & tempo <= 112)
    tempo = 2
  else if (tempo > 112 & tempo <= 146)
    tempo = 3
  else if (tempo > 146 & tempo <= 176)
    tempo = 4
  else if (tempo > 176)
    tempo = 5
  
  loudness <-   find_loudness(timbre[1,1],timbre[2,1])
  brightness <- find_brightness(timbre[1,2],timbre[2,2])
  flatness <-   find_flatness(timbre[1,3],timbre[2,3])
  attack <-     find_attack(timbre[1,4],timbre[2,4])
  timbre5 <-    find_timbre5(timbre[1,5],timbre[2,5])
  timbre6 <-    find_timbre6(timbre[1,6],timbre[2,6])
  timbre7 <-    find_timbre7(timbre[1,7],timbre[2,7])
#   genre <- readline("Enter genre: ")
  
  return(c(#genre,
           mode,
           tempo,
           time_signature,
           loudness,
           brightness,
           flatness,
           attack,
           timbre5,
           timbre6,
           timbre7
  ))
}

find_loudness <- function(timb_mean,timb_logvar) {
  
  if (timb_mean <= 38.89 & timb_logvar <= 2.885)
    loudness = 11
  else if (timb_mean <= 38.89 & timb_logvar >  2.885 & timb_logvar <= 3.334)
    loudness = 12
  else if (timb_mean <= 38.89 & timb_logvar >  3.334 & timb_logvar <= 3.733)
    loudness = 13
  else if (timb_mean <= 38.89 & timb_logvar >  3.733)
    loudness = 14
  
  else if (timb_mean >  38.89 & timb_mean <= 43.34 & timb_logvar <= 2.885)
    loudness = 21
  else if (timb_mean >  38.89 & timb_mean <= 43.34 & timb_logvar >  2.885 & timb_logvar <= 3.334)
    loudness = 22
  else if (timb_mean >  38.89 & timb_mean <= 43.34 & timb_logvar >  3.334 & timb_logvar <= 3.733)
    loudness = 23
  else if (timb_mean >  38.89 & timb_mean <= 43.34 & timb_logvar >  3.733)
    loudness = 24
  
  else if (timb_mean >    43.34 & timb_mean <= 46.89 & timb_logvar <= 2.885)
    loudness = 31
  else if (timb_mean >    43.34 & timb_mean <= 46.89 & timb_logvar >  2.885 & timb_logvar <= 3.334)
    loudness = 32
  else if (timb_mean >    43.34 & timb_mean <= 46.89 & timb_logvar >  3.334 & timb_logvar <= 3.733)
    loudness = 33
  else if (timb_mean >    43.34 & timb_mean <= 46.89 & timb_logvar >  3.733)
    loudness = 34
  
  else if (timb_mean >   46.89 & timb_logvar <= 2.885)
    loudness = 41
  else if (timb_mean >   46.89 & timb_logvar >  2.885 & timb_logvar <= 3.334)
    loudness = 42
  else if (timb_mean >   46.89 & timb_logvar >  3.334 & timb_logvar <= 3.733)
    loudness = 43
  else if (timb_mean >   46.89 & timb_logvar >  3.733)
    loudness = 44
  
  return(loudness)
}

find_brightness <- function(brightness_mean,brightness_logvar) {
  if (brightness_mean <= -29.87 & brightness_logvar <= 7.233)
    brightness = 11
  else if (brightness_mean <= -29.87 & brightness_logvar >  7.233 & brightness_logvar <= 7.624)
    brightness = 12
  else if (brightness_mean <= -29.87 & brightness_logvar >  7.624 & brightness_logvar <= 7.996)
    brightness = 13
  else if (brightness_mean <= -29.87 & brightness_logvar >  7.996)
    brightness = 14
  
  else if (brightness_mean >  -29.87 & brightness_mean <= 6.533 & brightness_logvar <= 7.233)
    brightness = 21
  else if (brightness_mean >  -29.87 & brightness_mean <= 6.533 & brightness_logvar >  7.233 & brightness_logvar <= 7.624)
    brightness = 22
  else if (brightness_mean >  -29.87 & brightness_mean <= 6.533 & brightness_logvar >  7.624 & brightness_logvar <= 7.996)
    brightness = 23
  else if (brightness_mean >  -29.87 & brightness_mean <= 6.533 & brightness_logvar >  7.996)
    brightness = 24
  
  else if (brightness_mean >    6.533 & brightness_mean <= 35.16 & brightness_logvar <= 7.233)
    brightness = 31
  else if (brightness_mean >    6.533 & brightness_mean <= 35.16 & brightness_logvar >  7.233 & brightness_logvar <= 7.624)
    brightness = 32
  else if (brightness_mean >    6.533 & brightness_mean <= 35.16 & brightness_logvar >  7.624 & brightness_logvar <= 7.996)
    brightness = 33
  else if (brightness_mean >    6.533 & brightness_mean <= 35.16 & brightness_logvar >  7.996)
    brightness = 34
  
  else if (brightness_mean >   35.16 & brightness_logvar <= 7.233)
    brightness = 41
  else if (brightness_mean >   35.16 & brightness_logvar >  7.233 & brightness_logvar <= 7.624)
    brightness = 42
  else if (brightness_mean >   35.16 & brightness_logvar >  7.624 & brightness_logvar <= 7.996)
    brightness = 43
  else if (brightness_mean >   35.16 & brightness_logvar >  7.996)
    brightness = 44
  
  return(brightness)
}

find_flatness <- function(flatness_mean,flatness_logvar) {
  if (flatness_mean <= -10.11 & flatness_logvar <= 7.125)
    flatness = 11
  else if (flatness_mean <= -10.11 & flatness_logvar >  7.125 & flatness_logvar <= 7.496)
    flatness = 12
  else if (flatness_mean <= -10.11 & flatness_logvar >  7.496 & flatness_logvar <= 7.86)
    flatness = 13
  else if (flatness_mean <= -10.11 & flatness_logvar >  7.86)
    flatness = 14
  
  else if (flatness_mean >  -10.11 & flatness_mean <= 12.78 & flatness_logvar <= 7.125)
    flatness = 21
  else if (flatness_mean >  -10.11 & flatness_mean <= 12.78 & flatness_logvar >  7.125 & flatness_logvar <= 7.496)
    flatness = 22
  else if (flatness_mean >  -10.11 & flatness_mean <= 12.78 & flatness_logvar >  7.496 & flatness_logvar <= 7.86)
    flatness = 23
  else if (flatness_mean >  -10.11 & flatness_mean <= 12.78 & flatness_logvar >  7.86)
    flatness = 24
  
  else if (flatness_mean >   12.78 & flatness_mean <= 33.52 & flatness_logvar <= 7.125)
    flatness = 31
  else if (flatness_mean >   12.78 & flatness_mean <= 33.52 & flatness_logvar >  7.125 & flatness_logvar <= 7.496)
    flatness = 32
  else if (flatness_mean >   12.78 & flatness_mean <= 33.52 & flatness_logvar >  7.496 & flatness_logvar <= 7.86)
    flatness = 33
  else if (flatness_mean >   12.78 & flatness_mean <= 33.52 & flatness_logvar >  7.86)
    flatness = 34
  
  else if (flatness_mean >   33.52 & flatness_logvar <= 7.125)
    flatness = 41
  else if (flatness_mean >   33.52 & flatness_logvar >  7.125 & flatness_logvar <= 7.496)
    flatness = 42
  else if (flatness_mean >   33.52 & flatness_logvar >  7.496 & flatness_logvar <= 7.86)
    flatness = 43
  else if (flatness_mean >   33.52 & flatness_logvar >  7.86)
    flatness = 44
  
  return(flatness)
}

find_attack <- function(attack_mean,attack_logvar) {
  if (attack_mean <= -8.069 & attack_logvar <= 6.834)
    attack = 11
  else if (attack_mean <= -8.069 & attack_logvar >  6.834 & attack_logvar <= 7.245)
    attack = 12
  else if (attack_mean <= -8.069 & attack_logvar >  7.245 & attack_logvar <= 7.649)
    attack = 13
  else if (attack_mean <= -8.069 & attack_logvar >  7.649)
    attack = 14
  
  else if (attack_mean >  -8.069 & attack_mean <= 0.1181 & attack_logvar <= 6.834)
    attack = 21
  else if (attack_mean >  -8.069 & attack_mean <= 0.1181 & attack_logvar >  6.834 & attack_logvar <= 7.245)
    attack = 22
  else if (attack_mean >  -8.069 & attack_mean <= 0.1181 & attack_logvar >  7.245 & attack_logvar <= 7.649)
    attack = 23
  else if (attack_mean >  -8.069 & attack_mean <= 0.1181 & attack_logvar >  7.649)
    attack = 24
  
  else if (attack_mean > 0.1181 & attack_mean <= 10.01 & attack_logvar <= 6.834)
    attack = 31
  else if (attack_mean > 0.1181 & attack_mean <= 10.01 & attack_logvar >  6.834 & attack_logvar <= 7.245)
    attack = 32
  else if (attack_mean > 0.1181 & attack_mean <= 10.01 & attack_logvar >  7.245 & attack_logvar <= 7.649)
    attack = 33
  else if (attack_mean > 0.1181 & attack_mean <= 10.01 & attack_logvar >  7.649)
    attack = 34
  
  else if (attack_mean >  10.01 & attack_logvar <= 6.834)
    attack = 41
  else if (attack_mean >  10.01 & attack_logvar >  6.834 & attack_logvar <= 7.245)
    attack = 42
  else if (attack_mean >  10.01 & attack_logvar >  7.245 & attack_logvar <= 7.649)
    attack = 43
  else if (attack_mean >  10.01 & attack_logvar >  7.649)
    attack = 44
  
  return(attack)
}

find_timbre5 <- function(timbre5_mean,timbre5_logvar) {
  if (timbre5_mean <= -17.83 & timbre5_logvar <= 6.473)
    timbre5 = 11
  else if (timbre5_mean <= -17.83 & timbre5_logvar >  6.473 & timbre5_logvar <= 6.757)
    timbre5 = 12
  else if (timbre5_mean <= -17.83 & timbre5_logvar >  6.757 & timbre5_logvar <= 7.046)
    timbre5 = 13
  else if (timbre5_mean <= -17.83 & timbre5_logvar >  7.046)
    timbre5 = 14
  
  else if (timbre5_mean >  -17.83 & timbre5_mean <= -2.7 & timbre5_logvar <= 6.473)
    timbre5 = 21
  else if (timbre5_mean >  -17.83 & timbre5_mean <= -2.7 & timbre5_logvar >  6.473 & timbre5_logvar <= 6.757)
    timbre5 = 22
  else if (timbre5_mean >  -17.83 & timbre5_mean <= -2.7 & timbre5_logvar >  6.757 & timbre5_logvar <= 7.046)
    timbre5 = 23
  else if (timbre5_mean >  -17.83 & timbre5_mean <= -2.7 & timbre5_logvar >  7.046)
    timbre5 = 24
  
  else if (timbre5_mean > -2.7 & timbre5_mean <= 10.5 & timbre5_logvar <= 6.473)
    timbre5 = 31
  else if (timbre5_mean > -2.7 & timbre5_mean <= 10.5 & timbre5_logvar >  6.473 & timbre5_logvar <= 6.757)
    timbre5 = 32
  else if (timbre5_mean > -2.7 & timbre5_mean <= 10.5 & timbre5_logvar >  6.757 & timbre5_logvar <= 7.046)
    timbre5 = 33
  else if (timbre5_mean > -2.7 & timbre5_mean <= 10.5 & timbre5_logvar >  7.046)
    timbre5 = 34
  
  else if (timbre5_mean >  10.5 & timbre5_logvar <= 6.473)
    timbre5 = 41
  else if (timbre5_mean >  10.5 & timbre5_logvar >  6.473 & timbre5_logvar <= 6.757)
    timbre5 = 42
  else if (timbre5_mean >  10.5 & timbre5_logvar >  6.757 & timbre5_logvar <= 7.046)
    timbre5 = 43
  else if (timbre5_mean >  10.5 & timbre5_logvar >  7.046)
    timbre5 = 44
  
  return(timbre5)
}

find_timbre6 <- function(timbre6_mean,timbre6_logvar) {
  if (timbre6_mean <= -16.2 & timbre6_logvar <= 6.279)
    timbre6 = 11
  else if (timbre6_mean <= -16.2 & timbre6_logvar >  6.279 & timbre6_logvar <= 6.692)
    timbre6 = 12
  else if (timbre6_mean <= -16.2 & timbre6_logvar >  6.692 & timbre6_logvar <= 7.086)
    timbre6 = 13
  else if (timbre6_mean <= -16.2 & timbre6_logvar >  7.086)
    timbre6 = 14
  
  else if (timbre6_mean >  -16.2 & timbre6_mean <= -7.73 & timbre6_logvar <= 6.279)
    timbre6 = 21
  else if (timbre6_mean >  -16.2 & timbre6_mean <= -7.73 & timbre6_logvar >  6.279 & timbre6_logvar <= 6.692)
    timbre6 = 22
  else if (timbre6_mean >  -16.2 & timbre6_mean <= -7.73 & timbre6_logvar >  6.692 & timbre6_logvar <= 7.086)
    timbre6 = 23
  else if (timbre6_mean >  -16.2 & timbre6_mean <= -7.73 & timbre6_logvar >  7.086)
    timbre6 = 24
  
  else if (timbre6_mean > -7.73 & timbre6_mean <= 2.193 & timbre6_logvar <= 6.279)
    timbre6 = 31
  else if (timbre6_mean > -7.73 & timbre6_mean <= 2.193 & timbre6_logvar >  6.279 & timbre6_logvar <= 6.692)
    timbre6 = 32
  else if (timbre6_mean > -7.73 & timbre6_mean <= 2.193 & timbre6_logvar >  6.692 & timbre6_logvar <= 7.086)
    timbre6 = 33
  else if (timbre6_mean > -7.73 & timbre6_mean <= 2.193 & timbre6_logvar >  7.086)
    timbre6 = 34
  
  else if (timbre6_mean >  2.193 & timbre6_logvar <= 6.279)
    timbre6 = 41
  else if (timbre6_mean >  2.193 & timbre6_logvar >  6.279 & timbre6_logvar <= 6.692)
    timbre6 = 42
  else if (timbre6_mean >  2.193 & timbre6_logvar >  6.692 & timbre6_logvar <= 7.086)
    timbre6 = 43
  else if (timbre6_mean >  2.193 & timbre6_logvar >  7.086)
    timbre6 = 44
  
  return(timbre6)
}

find_timbre7 <- function(timbre7_mean,timbre7_logvar) {
  if (timbre7_mean <= -13.38 & timbre7_logvar <= 6.064)
    timbre7 = 11
  else if (timbre7_mean <= -13.38 & timbre7_logvar >  6.064 & timbre7_logvar <= 6.392)
    timbre7 = 12
  else if (timbre7_mean <= -13.38 & timbre7_logvar >  6.392 & timbre7_logvar <= 6.683)
    timbre7 = 13
  else if (timbre7_mean <= -13.38 & timbre7_logvar >  6.683)
    timbre7 = 14
  
  else if (timbre7_mean >  -13.38 & timbre7_mean <= -3.963 & timbre7_logvar <= 6.064)
    timbre7 = 21
  else if (timbre7_mean >  -13.38 & timbre7_mean <= -3.963 & timbre7_logvar >  6.064 & timbre7_logvar <= 6.392)
    timbre7 = 22
  else if (timbre7_mean >  -13.38 & timbre7_mean <= -3.963 & timbre7_logvar >  6.392 & timbre7_logvar <= 6.683)
    timbre7 = 23
  else if (timbre7_mean >  -13.38 & timbre7_mean <= -3.963 & timbre7_logvar >  6.683)
    timbre7 = 24
  
  else if (timbre7_mean > -3.963 & timbre7_mean <= 4.893 & timbre7_logvar <= 6.064)
    timbre7 = 31
  else if (timbre7_mean > -3.963 & timbre7_mean <= 4.893 & timbre7_logvar >  6.064 & timbre7_logvar <= 6.392)
    timbre7 = 32
  else if (timbre7_mean > -3.963 & timbre7_mean <= 4.893 & timbre7_logvar >  6.392 & timbre7_logvar <= 6.683)
    timbre7 = 33
  else if (timbre7_mean > -3.963 & timbre7_mean <= 4.893 & timbre7_logvar >  6.683)
    timbre7 = 34
  
  else if (timbre7_mean >  4.893 & timbre7_logvar <= 6.064)
    timbre7 = 41
  else if (timbre7_mean >  4.893 & timbre7_logvar >  6.064 & timbre7_logvar <= 6.392)
    timbre7 = 42
  else if (timbre7_mean >  4.893 & timbre7_logvar >  6.392 & timbre7_logvar <= 6.683)
    timbre7 = 43
  else if (timbre7_mean >  4.893 & timbre7_logvar >  6.683)
    timbre7 = 44
  
  return(timbre7)
}

find_genre <- function(artist) {
    query <- URLencode(artist)
    if (grepl(x = query,pattern = "&"))
      query  <- gsub(pattern = "&",replacement = "%26",x = query)
    target <- paste("http://www.allmusic.com/search/artists/",query,sep="")
    browseURL(target)  
}


echonest.song <- function() {
  title <- URLencode(readline("Enter title: \n"))
  title <- paste("title",title,sep="=")
  
  cat("Wait ah...\n")
  
  target <- paste("http://developer.echonest.com/api/v4/song/search?api_key=XLUSPWGQKMBLGGJYI&format=json&results=30",
                  title,sep="&")
  x <- fromJSON(target)
  cat("\n")
  #x[[1]][2][[1]]
  for (i in 1:length(x[[1]][2][[1]]) )   #no of songs
    cat(i,")\t",x[[1]][2][[1]][[i]][4],"\n",sep="") #song no.i, 4 is for title
}

echonest.artist <- function() {
  artist <- URLencode(readline("Enter artist: \n"))
  artist <- paste("artist",artist,sep="=")
  
  target <- paste("http://developer.echonest.com/api/v4/song/search?api_key=XLUSPWGQKMBLGGJYI&format=json&results=30",
                  artist,sep="&")
  x <- fromJSON(target)
  
  titles <- x$response$songs["title"][[1]]
  if (length(titles)==0)
    return(cat("Aucun resultat."))
  for (i in 1:length(titles))
    cat(i,")\t",titles[i],"\n",sep="")
}

# side working
# names(results$response$songs)
# results$response$songs[1:2] # title & artist name
# results$response$songs$audio_summary$key[song]
# results$response$songs$audio_summary$mode
# results$response$songs$audio_summary$tempo
# results$response$songs$audio_summary$time_signature
# results$response$songs$audio_summary$analysis_url