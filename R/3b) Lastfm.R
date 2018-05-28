# keywords: mother song, child songs

lastfm_similartracks <- read.table(file="C:\\Users\\a0082893\\Desktop\\FYP Datasets\\Lastfm\\tracks_with_similar.txt",
                                   header = F)
lastfm_similartracks <- read.table(file="/Users/binKarim/Desktop/FYP Datasets/Lastfm/tracks_with_similar.txt",
                                   header = F)
lastfm_similartracks <- as.matrix(lastfm_similartracks) 
length(lastfm_similartracks) #584,897 'mother tracks' with at least one similar track

# MSD indices of lastfm songs
matches_MSDindex <- which(trackname %in% lastfm_similartracks)
length(matches_MSDindex) #5801
matches_tracknames <- trackname[matches_MSDindex]

filename_lastfm <- NULL
for (i in 1:5801)
  filename_lastfm[i] <- paste("C:\\Users\\a0082893\\Desktop\\FYP Datasets\\Lastfm\\lastfm_subset\\",
                              matches_tracknames[i],".json",sep = "")
for (i in 1:5801)
  filename_lastfm[i] <- paste("/Users/binKarim/Desktop/FYP Datasets/Lastfm/lastfm_subset/",
                       matches_tracknames[i],".json",sep = "")

# first is songindex in MSD of 'mother track', second is how many songs are in the similarity list
lastfm_matrix <- matrix(0,nrow=5801,ncol=2)
for (i in 1:5801) {
  if(i%%100==0)
    print(i)
  (lastfm_matrix[i,1] <- matches_MSDindex[i])
  (song <- fromJSON(txt=filename_lastfm[i],flatten = TRUE))
  (lastfm_matrix[i,2] <- length(which(trackname %in% song[[3]][,1])))
}
table(lastfm_matrix[,2])

# delete lists with 0 child songs
similar0 <- which(lastfm_matrix[,2]==0)

matches_MSDindex <- matches_MSDindex[-similar0]
length(matches_MSDindex) ##3856 lists (after deleting lists with 0 child songs)
matches_tracknames <- trackname[matches_MSDindex]

#### re-process
filename_lastfm <- NULL
for (i in 1:3856)
  filename_lastfm[i] <- paste("C:\\Users\\a0082893\\Desktop\\FYP Datasets\\Lastfm\\lastfm_subset\\",
                              matches_tracknames[i],".json",sep = "")
for (i in 1:3856)
  filename_lastfm[i] <- paste("/Users/binKarim/Desktop/FYP Datasets/Lastfm/lastfm_subset/",
                              matches_tracknames[i],".json",sep = "")

# store as matrix: 1stcol=MSD songindex in MSD (of 'mother track'), 2ndcol=how many songs in the similarity list (including mother track)
lastfm_matrix <- matrix(0,nrow=3856,ncol=2)
for (i in 1:3856) {
  if(i%%10==0)
    print(i)
  lastfm_matrix[i,1] <- matches_MSDindex[i]
  song <- fromJSON(txt=filename_lastfm[i],flatten = TRUE)
  lastfm_matrix[i,2] <- length(which(trackname %in% song[[3]][,1])) + 1 #toinclude mother song as well
}
rule <- order(lastfm_matrix[,2]) # order in terms of no of songs in each list
lastfm_matrix[,1] <- lastfm_matrix[rule,1]
lastfm_matrix[,2] <- lastfm_matrix[rule,2]
head(lastfm_matrix)
table(lastfm_matrix[,2])

matches_MSDindex <- matches_MSDindex[rule]
filename_lastfm <- filename_lastfm[rule]

# store as list of [[similarity list no]][mother track & its similar songs]
list_lastfm <- list(NULL)
for (i in 1:3856) {
  if(i%%10==0)
    print(i)
  (mothersong <- matches_MSDindex[i])
  (song <- fromJSON(txt=filename_lastfm[i],flatten = TRUE))
  (indices <- which(trackname %in% song[[3]][,1]))
  list_lastfm[[i]] <- c(mothersong,indices)
}
head(list_lastfm)
tail(list_lastfm)

# no. of unique songs
all_lastfm_songs <- NULL
for (i in 1:3856) {
  songs <- list_lastfm[[i]]
  all_lastfm_songs <- c(all_lastfm_songs,songs)
}
unique_lastfm_songs <- unique(all_lastfm_songs)
length(unique_lastfm_songs) # 4517

detach("package:jsonlite", unload=TRUE)
library("RJSONIO")
exportJSON <- toJSON(list_lastfm)
write(exportJSON,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/list_lastfm.json")
list_lastfm <- fromJSON("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/list_lastfm.json")
detach("package:RJSONIO", unload=TRUE)
library("jsonlite")

# no. of songs in each list
lastfm_lengthcount <- rep(0,3856)
for (i in 1:3856)
  lastfm_lengthcount[i] <- length(list_lastfm[[i]])
table(lastfm_lengthcount)



############################
##   VALIDATION DATASET   ##
############################

table(lastfm_lengthcount)
c(305,183,103)/sum(c(305,183,103))*5
table(lastfm_lengthcount)[[3-1]]

# take one each
set.seed(4199)
validation_lastfm <- 
  c(sample(which(lastfm_lengthcount == 5),1,replace=F),
    sample(which(lastfm_lengthcount == 6),1,replace=F),
    sample(which(lastfm_lengthcount == 7),1,replace=F),
    sample(which(lastfm_lengthcount == 8),1,replace=F),
    sample(which(lastfm_lengthcount == 9),1,replace=F))
(validation_lastfm <- validation_lastfm[order(validation_lastfm)])
# length = 5

write.table(validation_lastfm,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/validation_lastfm.txt",col.names = F,row.names = F)
validation_lastfm <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\validation_lastfm.txt"))
validation_lastfm <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/validation_lastfm.txt"))



#########################################
###           TRAINING SET            ###
#########################################

lastfm_left <- which(!(1:3856 %in% validation_lastfm))
length(lastfm_left) #3851 = 3856-5 sim lists left

kmt_lastfm <- matrix(0,ncol=9939,nrow=9939)
for (i in lastfm_left) {
  print(i)
  (simlist <- list_lastfm[[i]])
  (simlist <- simlist[order(simlist)])
  (simlist_length <- length(simlist))
  for (u in 1:(simlist_length-1)) {
    v <- u+1
    while (v <= simlist_length) {
      (x <- simlist[u])
      (y <- simlist[v])
      kmt_lastfm[x,y] <- kmt_lastfm[x,y]+1
      v <- v+1
    }
  }
}
length(which(diag(kmt_lastfm)!=0)) # should be 0
length(which(lower.triangle(kmt_lastfm)!=0)) # should be 0
length(which(kmt_lastfm!=0)) #14,274
length(which(kmt_lastfm==1))
length(which(kmt_lastfm==2))
length(which(kmt_lastfm==3))
length(which(kmt_lastfm==14))
which(kmt_lastfm==14,arr.ind = T)

kmt_lastfm <- kmt_lastfm + t(kmt_lastfm)
for (i in lastfm_left) {
  (simlist <- list_lastfm[[i]])
  (simlist_length <- length(simlist))
  for (j in 1:simlist_length) {
    x <- simlist[j]
    kmt_lastfm[x,x] <- kmt_lastfm[x,x]+1
  } 
}
table(diag(kmt_lastfm))
which(diag(kmt_lastfm)==53,arr.ind = T) 
kmt_lastfm[20:30,20:30]

h5createFile("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kmt_lastfm.h5") 
attr(kmt_lastfm, "scale") <- "liter" 
h5write(kmt_lastfm, "/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kmt_lastfm.h5", "kmt_lastfm")
kmt_lastfm <- h5read("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kmt_lastfm.h5", "kmt_lastfm")
kmt_lastfm <- h5read("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\kmt_lastfm.h5", "kmt_lastfm")