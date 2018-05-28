# TASTE PROFILE DATASET

# ~5mins
tasteprofile <- read.csv("/Users/binKarim/Desktop/FYP Datasets/train_triplets.txt",sep = "\t",header=F)
tasteprofile <- as.matrix(tasteprofile)

dim(tasteprofile) # 48,373,586 by 3
length(unique(tasteprofile[,2])) # 384,546 unique MSD songs
length(which(song_id %in% unique(tasteprofile[,2]))) # 3660 unique MSDS songs

# sieve out those not in MSDS
tokeep <- which((tasteprofile[,2]) %in% song_id)
length(tokeep) # 767,438
tasteprofile_matched <- tasteprofile[tokeep,]
length(unique(tasteprofile_matched[,2])) # should be 3660 unique songs
dim(tasteprofile_matched) # 767,438

# MATCHING ERROR
tasteprofile_error <- readLines("/Users/binKarim/Desktop/FYP Datasets/sid_mismatches.txt")
tasteprofile_error <- tasteprofile_error[1:5752]
# a list of song_id and tracknames
tasteprofile_error_pairs <- matrix(0,nrow=5752,ncol=2)
for (i in 1:5752) {
  splits <- strsplit(tasteprofile_error[i],split = " ")[[1]]
  songid <- substr(splits[2],2,19)
  track  <- substr(splits[3],1,18)
  tasteprofile_error_pairs[i,] <- c(songid,track)
}
tasteprofile_error_pairs[4981,]; tasteprofile_error[4981]
# find in tasteprofile
todelete <- which(tasteprofile_matched[,2] %in% tasteprofile_error_pairs[,1])
length(todelete) # 2075
tasteprofile_matched <- tasteprofile_matched[-todelete,]
dim(tasteprofile_matched) # 765,363
length(unique(tasteprofile_matched[,2])) # 3637 unique songs

# sort users
tasteprofile_matched <- tasteprofile_matched[order(tasteprofile_matched[,1]),]
tasteprofile_matched <- as.matrix(tasteprofile_matched)

# delete users with only one entry
taste_freq <- table(tasteprofile_matched[,1])
length(unique(tasteprofile_matched[,1])) # 415,697
length(taste_freq) # 415,697
head(taste_freq)
userstodelete <- names(taste_freq)[which(taste_freq==1)]
length(userstodelete) # 244,601
todelete_ind <- which(tasteprofile_matched[,1] %in% userstodelete)
tasteprofile_matched <- tasteprofile_matched[-todelete_ind,]
length(tasteprofile_matched[,1])         # 520,762 entries
length(unique(tasteprofile_matched[,1])) # 171,096 unique profiles
length(unique(tasteprofile_matched[,2])) #    3499 unique songs

write.table(tasteprofile_matched,"/Users/binKarim/Desktop/tasteprofile_matched.txt",row.names = F,col.names = F)
tasteprofile_matched <- read.table("/Users/binKarim/Desktop/tasteprofile_matched.txt")

unique_users <- unique(tasteprofile_matched[,1])

# convert into a list_tasteprofile[userindex][MSDS]
all(order(tasteprofile_matched[,1]) == 1:520762) # correct order
taste_freq <- table(tasteprofile_matched[,1])
list_tasteprofile <- list(NULL)
for (i in 1:171096) {
  if (i %% 100 == 0)
    print(i)
  (no_of_songs   <- taste_freq[[i]])
  (taste_indices <- which(tasteprofile_matched[,1] %in% unique_users[i]))
  (MSDS_indices  <- which(song_id %in% tasteprofile_matched[taste_indices,2]))
  (list_tasteprofile[[i]] <- c(MSDS_indices))
}

for (i in 1:171096) {
  print(i)
  songs <- list_tasteprofile[[i]]
  if (length(which(duplicated(songs))) > 0) {
    break
  }
}
# no duplicates!

detach("package:jsonlite", unload=TRUE)
library("RJSONIO")
exportJSON <- toJSON(list_tasteprofile)
write(exportJSON,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/list_tasteprofile.json")
list_tasteprofile <- fromJSON("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/list_tasteprofile.json")
list_tasteprofile <- fromJSON("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\list_tasteprofile.json")
detach("package:RJSONIO", unload=TRUE)
library("jsonlite")

# song count. song_count[tasteprofile_index] = no. of songs
taste_lengthcount <- rep(0,171096)
for (i in 1:171096) {
  taste_lengthcount[i] <- length(list_tasteprofile[[i]])
}
table(taste_lengthcount)
table(taste_lengthcount)[-1] # profiles with 3 to 48 songs
round(60/(171096-38882)*table(taste_lengthcount)[-1]) # profiles with 2 to 8 songs



############################
##   VALIDATION DATASET   ##
############################

(c(815,3856,171096)/sum(c(815,3856,171096))*100)
#     0      2     68
#     1      2     97
#     3      5     97 = 105 validation_playlist
# aotm, lastfm, taste

# take 10:18, in proportion
sum <- 0
for (i in 10:18)
  sum <- sum+table(taste_lengthcount)[[i-1]]
sum #1972

validation_taste <- NULL
for (i in 10:18) {
  (howmany <- round(table(taste_lengthcount)[[i-1]]/sum*105))
  validation_taste <- c(validation_taste,
                        sample(which(taste_lengthcount==i),howmany,replace=F))
}
length(validation_taste) # 104
(validation_taste <- validation_taste[order(validation_taste)])

# threshold = 109
# while (threshold > 108) {
#   validation_dataset <- NULL
#   for (i in 2:8) {
#     (howmany  <- round(60/171096*table(taste_lengthcount))[[i-1]])
#     (candidates <- which(taste_lengthcount==i))
#     (profiles <- sample(candidates,howmany,replace=F))
#     validation_dataset <- c(validation_dataset,profiles)
#   }
#   validation_dataset <- validation_dataset[order(validation_dataset)]
#   validation_dataset_songs <- NULL
#   for (i in validation_dataset) {
#     songs <- list_tasteprofile[[i]]
#     validation_dataset_songs <- c(validation_dataset_songs,songs)
#   }
#   threshold <- length(unique(validation_dataset_songs))
#   print(threshold)
# }
# threshold # min unique songs is 104
# length(validation_dataset) # 59

write.table(validation_taste,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/validation_taste.txt",col.names = F,row.names = F)
validation_taste <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\validation_taste.txt"))
validation_taste <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/validation_taste.txt"))


#########################
##   prepare for KMT   ##
#########################

profiles_left <- c(1:171096)[-validation_taste]
length(profiles_left) # 170,992 unique profiles left
kmt_tasteprofile <- matrix(0,ncol=9939,nrow=9939)
for (i in profiles_left) {
  if (i %% 100 == 0)
    print(i)
  (taste <- list_tasteprofile[[i]])
  (taste <- taste[order(taste)])
  (taste_length <- length(taste))
  for (u in 1:(taste_length-1)) {
    v <- u+1
    while (v <= taste_length) {
      (x <- taste[u])
      (y <- taste[v])
      kmt_tasteprofile[x,y] <- kmt_tasteprofile[x,y]+1
      v <- v+1
    }
  }
}
length(which(diag(kmt_tasteprofile)!=0)) # should be 0 i.e. no duplicates
length(which(lower.triangle(kmt_tasteprofile)!=0)) # should be 0
length(which(kmt_tasteprofile!=0)) # 196,787
length(which(kmt_tasteprofile==1))
length(which(kmt_tasteprofile==2))
length(which(kmt_tasteprofile>3))
length(which(kmt_tasteprofile>10)) # 10,548

kmt_tasteprofile <- kmt_tasteprofile + t(kmt_tasteprofile)
for (i in profiles_left) {
  (taste <- list_tasteprofile[[i]])
  (taste_length <- length(taste))
  for (j in 1:taste_length) {
    x <- taste[j]
    kmt_tasteprofile[x,x] <- kmt_tasteprofile[x,x]+1
  } 
}
table(diag(kmt_tasteprofile))

h5createFile("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kmt_tasteprofile.h5") 
attr(kmt_tasteprofile, "scale") <- "liter" 
h5write(kmt_tasteprofile, "/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kmt_tasteprofile.h5", "kmt_tasteprofile")
kmt_tasteprofile2 <- h5read("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kmt_tasteprofile.h5", "kmt_tasteprofile")
kmt_tasteprofile <- h5read("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\kmt_tasteprofile.h5", "kmt_tasteprofile")



