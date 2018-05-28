trackname_initial <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Basic/trackname_initial.txt"))

msd_duplicates <- readLines("/Users/binKarim/Dropbox/FYP/The Dataset/msd_duplicates.txt")
msd_duplicates[1:10]
msd_duplicates <- as.matrix(msd_duplicates)
msd_duplicates <- msd_duplicates[-c(1:6),]
msd_duplicates <- as.matrix(msd_duplicates)
head(msd_duplicates)
dim(msd_duplicates)  #185,132 duplicates in MSD

title_indices <- NULL
for (i in 1:185132) {
  if (i %% 100 == 0)
    print(i)
  (x <- msd_duplicates[i])
  chars <- (strsplit(x,split = "")[[1]][1:2])
  if (chars[1]=="%")
    title_indices <- c(title_indices,i)
}
tail(msd_duplicates[title_indices])
tail(msd_duplicates)
length(title_indices) # 53,471

title_indices
tail(title_indices)
title_indices[1:7]
head(title_indices)
head(title_indices[-1]-1)

list_MSDS_duplicates <- list(NULL)
k <- 1 # list counter
for (i in 1:53470) {  #53,471 is total length
  print(i)
  (start <- c(title_indices+1)[i])  # plus 1 to exclude title
  (end <- c(title_indices[-1]-1)[i])
  (dupli_list <- msd_duplicates[start:end])
  
  (MSD_index <- which(trackname_initial %in% dupli_list))
  if (length(MSD_index) >= 2) {
    list_MSDS_duplicates[[k]] <- MSD_index
    k <- k+1
  }
}
msd_duplicates[185130:185132]  # manually check last index (i=53471)
length(which(trackname_initial %in% msd_duplicates[185131:185132]))  # no duplicates
length(list_MSDS_duplicates) # 56

detach("package:jsonlite", unload=TRUE)
library("RJSONIO")
exportJSON <- toJSON(list_MSDS_duplicates)
write(exportJSON,"/Users/binKarim/Dropbox/FYP/The Dataset/list_MSDS_duplicates.json")
list_MSDS_duplicates <- fromJSON("/Users/binKarim/Dropbox/FYP/The Dataset/list_MSDS_duplicates.json")
detach("package:RJSONIO", unload=TRUE)
library("jsonlite")

# for (i in 1:56) {
#   (songs <- list_MSDS_duplicates[[i]])
#   (songs_length <- length(songs))
#   (tracks <- trackname_initial[songs])
#   if (length(which(genrelist_mc[,1] %in% tracks)) != songs_length) {
#     print("ALERT")
#     print(i)
#     print(which(tracks %in% genrelist_mc[,1]))
# #     which(genrelist_allmusic %in% tracks)
#   }
# }

MSDS_duplicates_indices <- NULL
 #56 unique songs with duplicates
for (i in 1:56) {
  (songs <- list_MSDS_duplicates[[i]])
  (MSDS_duplicates_indices <- c(MSDS_duplicates_indices,songs[2:length(songs)]))
}
length(MSDS_duplicates_indices) # 61
MSDS_duplicates_indices
MSDS_duplicates_tracks <- trackname_initial[MSDS_duplicates_indices]

write.table(MSDS_duplicates_tracks,"/Users/binKarim/Dropbox/FYP/The Dataset/MSDS_duplicates_tracks.txt",row.names = F, col.names = F)