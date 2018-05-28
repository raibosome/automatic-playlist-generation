# required: 5) Mercer

au2seed <- function() {
  
  ### Read csv file
  (friendlib <- as.matrix(read.csv(paste("/Users/binKarim/Dropbox/FYP/The Dataset/User Libraries/",
                                         (name<-readline("Enter name: ")),".csv",sep=""), header=F)))
  colnames(friendlib) <- c("SONG TITLE","ARTIST")
  as.data.frame(friendlib[-1,])
  
  ### No. of songs
  (no_of_songs <- length(friendlib[,1])-1)
  
  ### No. of songs in playlist
  (no_of_songs_playlist <- as.integer(friendlib[1,1]))
  (playlisttitle <- friendlib[1,2])
  cat("No. of songs in the playlist: ",no_of_songs_playlist,"\n",sep="")
  cat("Playlist title              : ",playlisttitle,"\n\n",sep="")
  
  ### Features extraction
  # features <- matrix(0,ncol=10,nrow=no_of_songs) # 10 features
  # for (i in 23:no_of_songs) {
  #   cat("---------")
  #   cat(i," of ",no_of_songs,sep="")
  #   cat("---------\n")
  #   features[i,] <- echonest.features(title = friendlib[i+1,1], artist = friendlib[i+1,2])
  # }
  # i = 16
  # features[i,] <- echonest.features(title = friendlib[i+1,1], artist = friendlib[i+1,2])
  # features[i,] <- echonest.features(title = readline("Enter title: "), artist = readline("Enter artist: "))
  # echonest.artist()
  # features; cat("---Songs from ",name,"---\n",sep="")
  # table(features[,1])
  # write.table(x = features,row.names = F,col.names = F,
  #             paste("/Users/binKarim/Dropbox/FYP/The Dataset/User Libraries/",
  #                   readline("Enter name: "),"_features.csv",sep=""))
  (userfeatures <- as.matrix(read.table(paste("/Users/binKarim/Dropbox/FYP/The Dataset/User Libraries/",
                                              name,"_features.csv",sep=""), header=F)))
  colnames(userfeatures) <- c("mode","tempo","time_signature","loudness","brightness","flatness","attack","timbre5","timbre6","timbre7")
  
  ### Beta
  # Beta <- as.matrix(read.table(paste("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Beta_v",
  #                                    readline("Enter version: v"),".txt",sep="")))
  # featurenames <- rep(0,7)
  # for (i in 1:7) {# enter feature names to be used
  #   featurenames[i] <- readline(cat("Feature #",i,": ",sep=""))
  #   if (i==7)
  #     (features <- features[,featurenames])
  # }
  Beta <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Beta_v3.6.txt"))
  features <- userfeatures[,c("time_signature","loudness","brightness","flatness","attack","timbre5","timbre6")]
  
  ### Features processing
  (toremove <- which(is.na(userfeatures[,1]))) 
  (no_of_songs_left <- no_of_songs - length(toremove))
  
  if (length(toremove) > 0) {
    (features <- features[-toremove,])
    final_friendlib <- friendlib[-c(1,(toremove+1)),]
  }
  else
    final_friendlib <- friendlib[-1,]
  
  ### Kernel
  user_kernel <- get_user_PSI(Beta,7,no_of_songs_left,features)
  user_kernel <- user_kernel + t(user_kernel)               # non-diagonal entries
  user_kernel <- user_kernel + diag(sum(Beta),no_of_songs_left)  # diagonal entries
  
  ### Playlist
  generate.userplaylist(final_friendlib,user_kernel,no_of_songs_left,no_of_songs_playlist,name)
}
au2seed.mine <- function() {
  name <- "Raimi"
  ### Read csv file
  (friendlib <- as.matrix(read.csv(paste("/Users/binKarim/Dropbox/FYP/The Dataset/User Libraries/",
                                         name,".csv",sep=""), header=F)))
  colnames(friendlib) <- c("SONG TITLE","ARTIST")
  as.data.frame(friendlib)
  
  ### No. of songs
  (no_of_songs <- length(friendlib[,1]))
  
  (userfeatures <- as.matrix(read.table(paste("/Users/binKarim/Dropbox/FYP/The Dataset/User Libraries/",
                                              name,"_features.csv",sep=""), header=F)))
  colnames(userfeatures) <- c("mode","tempo","time_signature","loudness","brightness","flatness","attack","timbre5","timbre6","timbre7")
  
  Beta <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Beta_v3.6.txt"))
  features <- userfeatures[,c("time_signature","loudness","brightness","flatness","attack","timbre5","timbre6")]
  
  ### Features processing
  (toremove <- which(is.na(userfeatures[,1]))) 
  (no_of_songs_left <- no_of_songs - length(toremove))
  
  if (length(toremove) > 0) {
    (features <- features[-toremove,])
    final_friendlib <- friendlib[-toremove,]
  }
  
  ### Kernel
  user_kernel <- get_user_PSI(Beta,7,no_of_songs_left,features)
  user_kernel <- user_kernel + t(user_kernel)               # non-diagonal entries
  user_kernel <- user_kernel + diag(sum(Beta),no_of_songs_left)  # diagonal entries
  
  ### Playlist
  generate.userplaylist.mine(final_friendlib,user_kernel,no_of_songs_left,name)
}
find_matrix_A <- function(sigma,userpref_songs,user_kernel) {
  matrix_A <- NULL
  no_userpref <- length(userpref_songs)
  matrix_A <- user_kernel[userpref_songs,userpref_songs] +
    diag(sigma^2,no_userpref)
  matrix_A <- solve(matrix_A)
  return(matrix_A)
}
loglikelihood <- function(sigma,userpref_songs,userpref_obs,user_kernel) {
  matrix_A <- find_matrix_A(sigma,userpref_songs,user_kernel)
  
  no_userpref <- length(userpref_songs)
  value <- 0.5*log(det(matrix_A)) -
    0.5*t(userpref_obs)%*%matrix_A%*%userpref_obs -
    0.5 * no_userpref * log(2*pi)
  
  return(as.numeric(value))
}
generate.userplaylist <- function(final_friendlib,user_kernel,no_of_songs_left,no_of_songs_playlist,name) {
  print(as.data.frame(final_friendlib))
  cat("\n\n")
  cat("#######################################################\n")
  cat("###  Hello, ",name,".\n",sep="")
  cat("###  This is the playlist you have given me previously.\n")
  cat("#######################################################\n\n")
  
  print(as.data.frame(final_friendlib[1:no_of_songs_playlist,]))
  
  # seed
  if (readline("\nSample? (y/n): ")=="y") {
    userpref_songs <- sample(1:no_of_songs_playlist,2)
    userpref_obs   <- c(1,1)
  }
  else {
    userpref_songs <- as.integer(strsplit(readline("Song indices: "), " ")[[1]])
    userpref_obs   <- as.integer(strsplit(readline("1 or 0      : "), " ")[[1]])
  }
  
  # sigma
  sigma_guess   <- 0.5
  loglikelihood(sigma_guess,userpref_songs,userpref_obs,user_kernel)
  results_sigma <- optimx(par=sigma_guess, fn=loglikelihood,lower=0,
                          control=list(maximize=T),method="L-BFGS-B",
                          userpref_songs=userpref_songs,userpref_obs=userpref_obs,
                          user_kernel=user_kernel)
  matrix_A <- find_matrix_A(results_sigma$p1,userpref_songs,user_kernel)  
  
  # posterior mean
  candidates <- c(1:no_of_songs_left)[-userpref_songs]
  scores     <- user_kernel[candidates,userpref_songs] %*% matrix_A %*% userpref_obs
  unordered  <- matrix(c(candidates,scores),ncol=2)
  newrule    <- rev(order(scores))
  ordered    <- matrix(c(candidates[newrule],scores[newrule]),ncol=2)
  
  # permute randomly for songs with the same score???
  uniquescores <- unique(ordered[,2])
  l <- length(uniquescores)
  (cutoff <- which(ordered[,2] %in% uniquescores[l])[1])
  for (i in 1:l) {
    (indices <- which(uniquescores[i] == ordered[,2]))
    if (length(indices)==1)
      next
    if (i == l) {
      (ordered[indices,1] <- ordered[sample(indices),1])
    }
    else
      (ordered[indices,1] <- sample(x=ordered[indices,1],size=length(indices)))
  }
  
  toplay <- ordered[1:cutoff,1]
  cat("\n\n")
  cat("##########################################\n")
  cat("###  I sampled 2 songs from that playlist: \n")
  cat("##########################################\n\n")
  print(as.data.frame(final_friendlib[userpref_songs,]))
  cat("\n\n")
  cat("#########################################\n")
  cat("###  And generated this playlist for you: \n")
  cat("#########################################\n\n")
  return(as.data.frame(final_friendlib[toplay,]))
}
generate.userplaylist.mine <- function(final_friendlib,user_kernel,no_of_songs_left,name) {
  print(as.data.frame(final_friendlib))
  cat("\n\n")
  cat("#######################################################\n")
  cat("###  Hello, ",name,".\n",sep="")
  #cat("###  This is the playlist you have given me previously.\n")
  cat("#######################################################\n\n")
  
  print(as.data.frame(final_friendlib))
  
  # seed
  userpref_songs <- as.integer(strsplit(readline("Song indices: "), " ")[[1]])
  userpref_obs   <- as.integer(strsplit(readline("1 or 0      : "), " ")[[1]])
  
  # sigma
  sigma_guess   <- 0.5
  loglikelihood(sigma_guess,userpref_songs,userpref_obs,user_kernel)
  results_sigma <- optimx(par=sigma_guess, fn=loglikelihood,lower=0,
                          control=list(maximize=T),method="L-BFGS-B",
                          userpref_songs=userpref_songs,userpref_obs=userpref_obs,
                          user_kernel=user_kernel)
  matrix_A <- find_matrix_A(results_sigma$p1,userpref_songs,user_kernel)  
  
  # posterior mean
  candidates <- c(1:no_of_songs_left)[-userpref_songs]
  scores     <- user_kernel[candidates,userpref_songs] %*% matrix_A %*% userpref_obs
  unordered  <- matrix(c(candidates,scores),ncol=2)
  newrule    <- rev(order(scores))
  ordered    <- matrix(c(candidates[newrule],scores[newrule]),ncol=2)
  
  # permute randomly for songs with the same score???
  uniquescores <- unique(ordered[,2])
  l <- length(uniquescores)
  (cutoff <- which(ordered[,2] %in% uniquescores[l])[1])
  for (i in 1:l) {
    (indices <- which(uniquescores[i] == ordered[,2]))
    if (length(indices)==1)
      next
    if (i == l) {
      (ordered[indices,1] <- ordered[sample(indices),1])
    }
    else
      (ordered[indices,1] <- sample(x=ordered[indices,1],size=length(indices)))
  }
  cutoff <- 20
  toplay <- ordered[1:cutoff,1]
  cat("\n\n")
  cat("#################################\n")
  cat("###  Your selected seed songs: \n")
  cat("#################################\n\n")
  print(as.data.frame(final_friendlib[userpref_songs,]))
  readline("Press any key to continue:")
  cat("\n\n")
  cat("#########################################\n")
  cat("###  And generated this playlist for you: \n")
  cat("#########################################\n\n")
  print(as.data.frame(final_friendlib[toplay,]))
  
  # review
  toreview <- readline("Add/remove songs? (y/n): ")
  while (toreview == "y") {
    
  }
}
au2seed()

# play.friends.songs <- function(final_friendlib) {
#   leng <- length(final_friendlib[,1])
#   for (i in 1:leng) {
#     cat("---Song ",i,"---\n")
#     artist <- final_friendlib[i,1]
#     title  <- final_friendlib[i,2]
#     cat("Artist: ",artist,"\n",sep="")
#     cat("Title:  ",title,"\n",sep="")
#     
#     readline("Press enter to play.")
#     query <- paste(artist,title)
#     query <- gsub(" ", "+", query)
#     searchquery <- paste("https://www.youtube.com/results?search_query=",query,sep="")
#     browseURL(searchquery)
#     
#     cat("Play next song?")
#     response <- tolower(readline("Yes/No: "))
#   }
#   
# }
# play.friends.songs(final_friendlib)



# target <- "http://gdata.youtube.com/feeds/api/videos?q=+taylor+swift+&format=5&max-results=1&v=2&alt=jsonc"
# x <- fromJSON(target)
# length(x$data$items)
# names(x$data$items)
# browseURL(x$data$items$player$default)




### Read csv file
(friendlib <- as.matrix(read.csv(paste("/Users/binKarim/Dropbox/FYP/The Dataset/User Libraries/",
                                       (name<-readline("Enter name: ")),".csv",sep=""), header=F)))
colnames(friendlib) <- c("SONG TITLE","ARTIST")
as.data.frame(friendlib)

### No. of songs
(no_of_songs <- length(friendlib[,1]))

### No. of songs in playlist
(no_of_songs_playlist <- as.integer(friendlib[1,1]))
(playlisttitle <- friendlib[1,2])
cat("No. of songs in the playlist: ",no_of_songs_playlist,"\n",sep="")
cat("Playlist title              : ",playlisttitle,"\n\n",sep="")

## Features extraction
(features <- as.matrix(read.table(paste("/Users/binKarim/Dropbox/FYP/The Dataset/User Libraries/",
                                            name,"_features.csv",sep=""), header=F)))
features <- matrix(0,ncol=10,nrow=no_of_songs) # 10 features
for (j in :117) {
  cat("---------")
  cat(j," of 117",sep="") #cat(i," of ",no_of_songs,sep="")
  cat("---------\n")
  i <- which(is.na(features[,1]))[j]
  features[i,] <- echonest.features(title = friendlib[i,1], artist = friendlib[i,2])
}
i = 88
features[i,] <- echonest.features(title = friendlib[i,1], artist = friendlib[i,2])
features[i,] <- echonest.features(title = readline("Enter title: "), artist = readline("Enter artist: "))
echonest.artist()
features; cat("---Songs from ",name,"---\n",sep="")
table(features[,1])
write.table(x = features,row.names = F,col.names = F,
            paste("/Users/binKarim/Dropbox/FYP/The Dataset/User Libraries/",
                  readline("Enter name: "),"_features.csv",sep=""))

