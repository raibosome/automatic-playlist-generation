### MATCHING AOTM & 10K MSD
#
# http://bmcfee.github.io/data/aotm2011.html

# 4mins
aotm <- fromJSON("/Users/binKarim/Desktop/FYP Datasets/aotm2011_playlists.json")
names(aotm)
attach(aotm)

aotm$category
aotm$filtered_lists # messy
aotm$mix_id
aotm$playlist       # messy

# test$playlist[x]
#
# playlist x
#
# [[1]][[1]]
# [[1]][[1]][[1]]
# [1] "song_1 artist"
# [2] "song_1 title"
#
# [[1]][[1]][[2]]
# [1] "song_1_id"
#
#
# [[1]][[i]]
# [[1]][[i]][[1]]
# [1] "song_i artist"
# [2] "song_i title"

# [[1]][[i]][[2]]
# [1] "song_i_id"

length(aotm$playlist)  # 101,343 in total
playlist_index       <- 169 
songinplaylist_index <- 23
songinfo_or_songid   <- 2   # songinfo=1, songid=2

# songid = {SGWJ57BJ6VK, NULL}

aotm$playlist[playlist_index][[1]]
aotm$playlist[playlist_index][[1]][[songinplaylist_index]]
aotm$playlist[playlist_index][[1]][[songinplaylist_index]][[songinfo_or_songid]]

##########################################################
##   Create an aotm_matched matrix/list to store info   ##
##########################################################

# initialise
# 101343 playlists in total
# i is the playlist index
# j is the song index within each playlist

MSD_song_index <- function(song_id_input) {
  which(song_id %in% song_id_input)
}

working_playlists <- seq(1,101343,1)
faulty_playlists <- NULL
h <- 1
for (i in 1:101343) {
  if (length(aotm$playlist[i][[1]]) == 0) {
    faulty_playlists[h] <- i
    h <- h+1
  }
}
working_playlists <- working_playlists[-faulty_playlists]
length(working_playlists) # 101,264 working playlists

#playlist lengths
aotm_initial_lengths <- NULL
for (i in 1:101264) 
  aotm_initial_lengths[i] <- length(aotm$playlist[working_playlists[i]][[1]])
table(aotm_initial_lengths)

useful_songs <- function(playlist,no_of_songs) {
  wantedsongs <- NULL
  j <- 1
  for (i in 1:no_of_songs) {
    identify <- playlist[[i]][[2]]
    if (is.character(identify)==TRUE)
      if (length(MSD_song_index(identify)) != 0) {
        wantedsongs[j] <- identify
        j <- j+1
      }
  }
  return(wantedsongs)
}

# 7min
aotm_matched <- matrix(NA,ncol=2,nrow=101343)
p <- 1 
for (i in working_playlists) {
  if (i %% 1000 == 0)
    print(i)
  playlist <- aotm$playlist[i][[1]]
  no_of_songs <- length(playlist)
  
  if (no_of_songs <= 1)
    next
  
  wanted <- useful_songs(playlist,no_of_songs)
  if (length(wanted) <= 1)
    next
  
  for (j in 1:length(wanted)) {
    MSD_index <- MSD_song_index(wanted[j])
    aotm_matched[p,] <- c(MSD_index,i)
    p <- p+1
  }
}
head(aotm_matched)

# length of aotm_matched is p-1
p-1 #1744
aotm_matched[(p-2):(p+2),]
# matched database:
aotm_matched <- aotm_matched[1:p-1,]
dim(aotm_matched) #1744,2

# no. of unique songs
length(unique(aotm_matched[,1])) # 506
# no. of unique playlists
length(unique(aotm_matched[,2])) # 833



#######################################
##  Cleaning  ##
#######################################

aotm_freq <- table(aotm_matched[,2])
aotm_lengthcount <- rep(0,833)
count_2 <- 0
count_3 <- 0
count_4 <- 0
count_5ormore <- 0
j <- 1
for (i in 1:833) {
  count <- aotm_freq[[i]][1]
  aotm_lengthcount[i] <- count
  if (count==2)
    count_2 <- count_2+1
  else if (count==3)
    count_3 <- count_3+1
  else if (count==4)
    count_4 <- count_4+1
  else if (count>=5) {
    count_5ormore <- count_5ormore+1
  }
}
count_2 # 778
count_3 # 49
count_4 # 5
count_5ormore #1. playlist 79466. 21 britney songs. this playlist has to be deleted. super crazy britney fan

crazybritneyfan <- which(aotm_matched[,2]==79466)
aotm_matched <- aotm_matched[-crazybritneyfan,]

# re-process
aotm_freq <- table(aotm_matched[,2])
leaotm_lengthcount <- rep(0,832) #minus 1
count_2 <- 0
count_3 <- 0
count_4 <- 0
count_5ormore <- 0
j <- 1
for (i in 1:832) {
  count <- aotm_freq[[i]][1]
  aotm_lengthcount[i] <- count
  if (count==2)
    count_2 <- count_2+1
  else if (count==3)
    count_3 <- count_3+1
  else if (count==4)
    count_4 <- count_4+1
  else if (count>=5) {
    count_5ormore <- count_5ormore+1
  }
}
count_2 # 778
count_3 # 49
count_4 # 5
count_5ormore #0

tail(aotm_matched)

# no. of unique songs
length(unique(aotm_matched[,1])) # 506
# no. of unique playlists
length(unique(aotm_matched[,2])) # 832

#######################################
## Further cleaning  ##
#######################################

aotm_freq <- table(aotm_matched[,2])

# list of playlists
list_aotm <- list(NULL)
i <- 1
for (k in 1:832) {
  list_aotm[[k]] <- aotm_matched[i:(i+aotm_freq[[k]]-1),1]
  i <- i+aotm_freq[[k]]
}

# further cleaning (duplicates within a playlist)
for (i in 1:832) {
  repeated <- which(duplicated(list_aotm[[i]]))
  if (length(repeated) != 0)
    list_aotm[[i]] <- list_aotm[[i]][-repeated]
}

# further cleaning (no. of songs in playlist = 1)
todelete <- NULL
m <- 1
for (i in 1:832) {
  if (length(list_aotm[[i]])==1) {
    print("ALERT")
    print(i)
    todelete[m] <- i
    m <- m+1
  }
}
todelete
length(todelete)  # 17
head(list_aotm)
list_aotm <- list_aotm[-todelete]

# no. of unique playlists
length(list_aotm) # 815

detach("package:jsonlite", unload=TRUE)
library("RJSONIO")
exportJSON <- toJSON(list_aotm)
write(exportJSON,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/list_aotm.json")
list_aotm <- fromJSON("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/list_aotm.json")
detach("package:RJSONIO", unload=TRUE)
library("jsonlite")

# no. of unique songs
all_aotm_songs <- NULL
for (i in 1:815) {
  songs <- list_aotm[[i]]
  all_aotm_songs <- c(all_aotm_songs,songs)
}
unique_aotm_songs <- unique(all_aotm_songs)
length(unique_aotm_songs) # 503

# no. of songs in each playlist
aotm_lengthcount <- rep(0,815)
for (i in 1:815)
  aotm_lengthcount[i] <- length(list_aotm[[i]])
table(aotm_lengthcount)



############################
##   VALIDATION DATASET   ##
############################

# take out 3 from the 47 3-song playlists
set.seed(4199)
(validation_aotm <- sample(which(aotm_lengthcount == 3),3,replace=F))
(validation_aotm <- validation_aotm[order(validation_aotm)])

write.table(validation_aotm,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/validation_aotm.txt",col.names = F,row.names = F)
validation_aotm <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\validation_aotm.txt"))
validation_aotm <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/validation_aotm.txt"))



#########################
##   prepare for KMT   ##
#########################

aotm_left <- c(1:815)[-validation_aotm]
length(aotm_left) # 812 playlists left

kmt_aotm <- matrix(0,ncol=9939,nrow=9939)
for (i in aotm_left) {
  print(i)
  (playlist <- list_aotm[[i]])
  (playlist <- playlist[order(playlist)])
  (playlist_length <- length(playlist))
  for (u in 1:(playlist_length-1)) {
    v <- u+1
    while (v <= playlist_length) {
      (x <- playlist[u])
      (y <- playlist[v])
      kmt_aotm[x,y] <- kmt_aotm[x,y]+1
      v <- v+1
    }
  }
}
length(which(diag(kmt_aotm)!=0)) # should be 0
length(which(lower.triangle(kmt_aotm)!=0)) # should be 0
length(which(kmt_aotm!=0)) # 772
length(which(kmt_aotm==1)) # 687
length(which(kmt_aotm==2)) # 58
length(which(kmt_aotm==3)) # 12
length(which(kmt_aotm==5))
which(kmt_aotm==5,arr.ind = T)

kmt_aotm <- kmt_aotm + t(kmt_aotm)
for (i in aotm_left) {
  (playlist <- list_aotm[[i]])
  (playlist_length <- length(playlist))
  for (j in 1:playlist_length) {
    x <- playlist[j]
    kmt_aotm[x,x] <- kmt_aotm[x,x]+1
  } 
}
table(diag(kmt_aotm))
which(diag(kmt_aotm)==36,arr.ind = T) 
length(which(kmt_aotm!=0)) # 2047

h5createFile("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kmt_aotm.h5") 
attr(kmt_aotm, "scale") <- "liter" 
h5write(kmt_aotm, "/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kmt_aotm.h5", "kmt_aotm")
kmt_aotm <- h5read("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kmt_aotm.h5", "kmt_aotm")
kmt_aotm <- h5read("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\kmt_aotm.txt", "kmt_aotm")