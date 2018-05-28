# SONG FEATURES EXTRACTION

# EXCLUDED
# loudness_class <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/loudness_class.txt"))
# loudness_class <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Features\\loudness_class.txt"))
tempo    <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/tempo.txt"))

# INCLUDED
genre          <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/genre.txt"))
key            <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/key.txt"))
mode           <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/mode.txt"))
section_class  <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/section_class.txt"))
tempo_class    <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/tempo_class.txt"))
time_signature <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/time_signature.txt"))
year           <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/year.txt"))

loudness_timb_class <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/loudness_timb_class.txt"))
brightness_class    <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/brightness_class.txt"))
flatness_class      <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/flatness_class.txt"))
attack_class        <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/attack_class.txt"))
timbre5_class       <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/timbre5_class.txt"))
timbre6_class       <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/timbre6_class.txt"))
timbre7_class       <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/timbre7_class.txt"))

genre          <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Features\\genre.txt"))
key            <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Features\\key.txt"))
mode           <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Features\\mode.txt"))
section_class  <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Features\\section_class.txt"))
tempo_class    <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Features\\tempo_class.txt"))
time_signature <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Features\\time_signature.txt"))
year           <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Features\\year.txt"))

loudness_timb_class <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Features\\loudness_timb_class.txt"))
brightness_class    <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Features\\brightness_class.txt"))
flatness_class      <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Features\\flatness_class.txt"))
attack_class        <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Features\\attack_class.txt"))
timbre5_class       <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Features\\timbre5_class.txt"))
timbre6_class       <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Features\\timbre6_class.txt"))
timbre7_class       <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Features\\timbre7_class.txt"))

mastertimb <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/mastertimb.txt"))
mastertimb <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Features\\mastertimb.txt"))

tempo_class
mode
loudness_class
time_signature
genre
loudness_timb_class
brightness_class
flatness_class
attack_class
timbre5_class
timbre6_class

(start.time <- Sys.time())
length_MSDS <- 9939
blank_vector <- rep(0,length_MSDS)

key            <- blank_vector
section        <- blank_vector
year           <- blank_vector
tempo          <- blank_vector
mode           <- blank_vector
# loudness       <- blank_vector
time_signature <- blank_vector

for (i in 1:length_MSDS) {      # 6.8 mins
     analysis.songs <- h5read(filename[i],"/analysis/songs")
  analysis.sections <- h5read(filename[i],"/analysis/sections_start")
   musicbrainz.tags <- h5read(filename[i],"/musicbrainz/songs")

    time_signature[i] <- analysis.songs$time_signature
           tempo[i] <- analysis.songs$tempo  
            mode[i] <- analysis.songs$mode
#         loudness[i] <- analysis.songs$loudness
             key[i] <- analysis.songs$key
         section[i] <- length(analysis.sections)
            year[i] <- musicbrainz.tags$year
}
end.time <- Sys.time()
(difftime(end.time,start.time,units = c("auto")))
(difftime(end.time,start.time,units = c("secs")))
for (i in 1:3)
  beep("facebook")



########################################################

# Allmusic dataset
genrelist_allmusic <- read.table("/Users/binKarim/Desktop/FYP Datasets/Allmusic/msd-topMAGD-genreAssignment.txt",sep="\t")
genrelist_allmusic <- as.matrix(genrelist_allmusic)
dim(genrelist_allmusic) # 406,427
head(genrelist_allmusic)

genre_allmusic <- matrix(0,length_MSDS)
genrelist_found2 <- which(genrelist_allmusic[,1] %in% trackname)
length(genrelist_found2)  #3680
genre_allmusic <- matrix(0,length_MSDS) # col: MSDS index, genre
for (i in genrelist_found2) {
  print(i)
  (MSDS_ind <- which(trackname %in% genrelist_allmusic[i,1]))
  (genre    <- genrelist_allmusic[i,2])
  genre_allmusic[MSDS_ind] <- genre
}
genre_allmusic[genre_allmusic=="Pop_Rock"] <- "Pop/Rock"
genre_allmusic[genre_allmusic=="RnB"] <- "R&B"
table(genre_allmusic)

# MSD Genre Tags by Yajie Hu
genrelist_mc <- read.table("/Users/binKarim/Desktop/FYP Datasets/Genre Tags Yajie/GenreTags clean.txt")
genrelist_mc <- as.matrix(genrelist_mc)
dim(genrelist_mc) # 990,013
head(genrelist_mc)

genrelist_found <- which(genrelist_mc[,1] %in% trackname)
length(genrelist_found) #9878
genre_yajie <- matrix(0,length_MSDS) # col: MSDS index, genre
for (i in genrelist_found) {
  print(i)
  (MSDS_ind <- which(trackname %in% genrelist_mc[i,1]))
  (genre    <- genrelist_mc[i,2])
  genre_yajie[MSDS_ind] <- genre
}
table(genre_yajie)

# Yajie's accuracy
ind_to_compare  <- which(genre_yajie != 0) # pick songs that have genre tags
max <- 0; score <- 0
for (i in ind_to_compare) {
  if (genre_allmusic[i] == 0) # exclude songs that do not have tag in allmusic
    next
  max <- max+1
  if (genre_yajie[i] == genre_allmusic[i])
    score <- score+1
}
score/max*100  # 50.36% accuracy for yajie's machine learning

# Combining both
genre_hybrid <- genre_allmusic
length(which(genre_hybrid==0))
ind_nogenre <- which(genre_hybrid=="0")
for (i in ind_nogenre)
  genre_hybrid[i] <- genre_yajie[i]
genre_hybrid[genre_hybrid=="0"] <- "Miscellaneous"
length(genre_hybrid[genre_hybrid=="Miscellaneous"])
table(genre_hybrid)

genre_names <- genre_hybrid
# write.table(genre_names,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/genre_names.txt",row.names = F, col.names = F)
genre_names <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Features\\genre_names.txt"))
genre_names <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/genre_names.txt"))

genre <- genre_hybrid
table(genre)
genre[genre=="Blues"] <- 1
genre[genre=="Country"] <- 2
genre[genre=="Electronic"] <- 3
genre[genre=="Folk"] <- 4 
genre[genre=="International"] <- 5
genre[genre=="Jazz"] <- 6
genre[genre=="Latin"] <- 7
genre[genre=="New Age"] <- 8
genre[genre=="Pop/Rock"] <- 9
genre[genre=="R&B"] <- 10
genre[genre=="Rap"] <- 11
genre[genre=="Reggae"] <- 12
genre[genre=="Vocal"] <- 13
genre[genre=="Miscellaneous"] <- 14

# write.table(genre,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/genre.txt",row.names = F, col.names = F)
genre <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Features\\genre.txt"))
genre <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/genre.txt"))

######################################################

hist(log(tempo),col="orange",nclass=150,xlim=c(3.5,5.6))
abline(v=log(c(84,112,142,176)),lwd=3,col="blue")
hist(tempo,col="orange",nclass=150,xlim=c(25,250))
abline(v=c(84,112,142,176),lwd=3,col="blue")
abline(v=summary(tempo)[c(2,3,5)],lwd=3,col="limegreen")
# tempo_clusters <- kmeans(tempo,centers = 2)$clusters
# table(tempo_clusters)
# hist(tempo_clusters,col="orange",nclass=150,xlim=c(25,250))
# abline(v=centers,lwd=2)
summary(tempo)
tempo_class <- rep(0,length_MSDS)
class1 <- which(tempo <= 84)
class2 <- which(tempo >  84 & tempo <= 112)
class3 <- which(tempo > 112 & tempo <= 146)
class4 <- which(tempo > 146 & tempo <= 176)
class5 <- which(tempo > 176)
tempo_class[class1] <- 1
tempo_class[class2] <- 2
tempo_class[class3] <- 3
tempo_class[class4] <- 4
tempo_class[class5] <- 5
table(tempo_class)
hist(tempo_class,col="orange")

write.table(tempo,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/tempo.txt",row.names = F, col.names = F)
write.table(tempo_class,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/tempo_class.txt",row.names = F, col.names = F)

######################################################

table(key)
hist(key,col="orange")
write.table(key,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/key.txt",row.names = F, col.names = F)

######################################################

table(time_signature)
which(time_signature==0)
# 1282 5584 7258
songtitle[which(time_signature==0)]
# "genuine"           "black market hell" "bereit"
artistname[which(time_signature==0)]
# "five fingers of funk" "aiden"                "panzer ag"
song_id[which(time_signature==0)]
# "SOHODRU12A81C230CE" "SOCQLKZ12AB0183796" "SOEROGE12A6D4FACDD"
# 4 4 4
time_signature[1282] <- 4
time_signature[5584] <- 4
time_signature[7258] <- 4
table(time_signature)
length(time_signature)

write.table(time_signature,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/time_signature.txt",row.names = F, col.names = F)

######################################################

table(mode)
hist(mode,col="orange")
write.table(mode,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/mode.txt",row.names = F, col.names = F)

######################################################

table(section)
summary(section)[c(2,3,5)]
hist(section,col="orange",nclass=100,xlim=c(0,25))
hist(log(section),col="orange")
section_class <- rep(0,length_MSDS)
class1  <- which(section <= 6)
class2  <- which(section >= 7  & section <= 9)
class3  <- which(section >= 10  & section <= 12)
class4  <- which(section >= 13)
section_class[class1]  <- 1
section_class[class2]  <- 2
section_class[class3]  <- 3
section_class[class4]  <- 4
table(section_class)
write.table(section_class,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/section_class.txt",row.names = F, col.names = F)

######################################################

length(year[year=="0"]) # 5297
hist(year[-which(year==0)],col="orange",seq(1920,2010,5))
min(year[-which(year==0)]) #1926
max(year[-which(year==0)]) #2010
summary(year[-which(year==0)])
class30    <- which(year >  0 & year <= 1930)
class40    <- which(year >= 1931 & year <= 1940)
class50    <- which(year >= 1941 & year <= 1950)
class60    <- which(year >= 1951 & year <= 1960)
class70    <- which(year >= 1961 & year <= 1970)
class80    <- which(year >= 1971 & year <= 1980)
class90    <- which(year >= 1981 & year <= 1990)
class2000  <- which(year >= 1991 & year <= 2000)
class2010  <- which(year >= 2001 & year <= 2010)
year_class <- rep(0,length_MSDS)
year_class[class30]   <- 30
year_class[class40]   <- 40
year_class[class50]   <- 50
year_class[class60]   <- 60
year_class[class70]   <- 70
year_class[class80]   <- 80
year_class[class90]   <- 90
year_class[class2000] <- 2000
year_class[class2010] <- 2010
table(year_class)

write.table(year,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/year.txt",row.names = F, col.names = F)
write.table(year_class,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/year_class.txt",row.names = F, col.names = F)

######################################################

# hist(loudness,col="orange",nclass=100)
# summary(loudness)
# class25 <-  which(loudness <= -13.16)
# class50 <-  which(loudness >  -13.16 & loudness <= -9.38)
# class75 <-  which(loudness >   -9.38 & loudness <= -6.532)
# class100 <- which(loudness >  -6.532)
# loudness_class <- rep(0,length_MSDS)
# loudness_class[class25] <- 1
# loudness_class[class50] <- 2
# loudness_class[class75] <- 3
# loudness_class[class100] <- 4
# table(loudness_class)
# 
# write.table(loudness_class,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/loudness_class.txt",row.names = F, col.names = F)

#########################################################
### TIMBRE 
#########################################################


# # 4120 sos
# timbre[,1:10]
# segments_start[1:10]
# beats_start[1:10]
# 
# # 1238 everytime
# timbre1238
# segments_start1238
# 
# endtime <- 200
# t <- 0
# Sys.time()
# Sys.sleep(1)
# Sys.time()
# Sys.sleep(1)
# Sys.time()
# Sys.sleep(1)
# Sys.time()
# Sys.sleep(1)
# while(t <= endtime) {
#   plot(x=segments_start1238,
#        y=timbre1238[4,],
#        xlab = "Time (s)",
#        xlim=c(0,50),
#        ylim=c(-75,100),
#        col="limegreen",type="l")
#   abline(v=t)
#   t <- t+1
#   Sys.sleep(1)
# }

#####
# computing mean & var of brightness, flatness & sounds with stronger attack values
# brightness.mean, brightness.var, 
meanvar <- function(x) {
  c(mean(x),var(x))
}
mastertimb <- matrix(0,ncol=24,nrow=9939)
for (i in 1:9939) {
  if (i%%100==0)
    print(i)
  timbs <- h5read(filename[i],"/analysis/segments_timbre")
  timbs <- timbs[1:12,]
  timbsstats <- apply(timbs,1,meanvar)
  dim(timbsstats) <- c(1,24)
  mastertimb[i,] <- timbsstats
}
write.table(mastertimb,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/mastertimb.txt",row.names = F, col.names = F)

# loudness_timb (mean)
hist(mastertimb[,1],col="limegreen",nclass=100)
summary(mastertimb[,1])[c(2,3,5)]
loudness_timb.mean_class <- rep(0,length_MSDS)
class25 <-  which(mastertimb[,1] <= 38.89)
class50 <-  which(mastertimb[,1] >  38.89 & mastertimb[,1] <= 43.34)
class75 <-  which(mastertimb[,1] >  43.34 & mastertimb[,1] <= 46.89)
class100 <- which(mastertimb[,1] >   46.89)
loudness_timb.mean_class[class25] <- 1
loudness_timb.mean_class[class50] <- 2
loudness_timb.mean_class[class75] <- 3
loudness_timb.mean_class[class100] <- 4
table(loudness_timb.mean_class)

# loudness_timb (var)
summary(mastertimb[,2])
summary(log(mastertimb[,2]))[c(2,3,5)]
hist(mastertimb[,2],col="limegreen",nclass=150)
hist(log(mastertimb[,2]),col="limegreen",nclass=150)
loudness_timb.var_class <- rep(0,length_MSDS)
class25 <-  which(log(mastertimb[,2]) <= 2.885)
class50 <-  which(log(mastertimb[,2]) >  2.885 & log(mastertimb[,2]) <= 3.334)
class75 <-  which(log(mastertimb[,2]) >  3.334 & log(mastertimb[,2]) <= 3.733)
class100 <- which(log(mastertimb[,2]) >  3.733)
loudness_timb.var_class[class25] <- 1
loudness_timb.var_class[class50] <- 2
loudness_timb.var_class[class75] <- 3
loudness_timb.var_class[class100] <- 4
table(loudness_timb.var_class)

# loudness_timb mean+var combined
loudness_timb_class <- rep(0,length_MSDS)
class11 <-  which(mastertimb[,1] <= 38.89 & log(mastertimb[,2]) <= 2.885)
class12 <-  which(mastertimb[,1] <= 38.89 & log(mastertimb[,2]) >  2.885 & log(mastertimb[,2]) <= 3.334)
class13 <-  which(mastertimb[,1] <= 38.89 & log(mastertimb[,2]) >  3.334 & log(mastertimb[,2]) <= 3.733)
class14 <-  which(mastertimb[,1] <= 38.89 & log(mastertimb[,2]) >  3.733)

class21 <-  which(mastertimb[,1] >  38.89 & mastertimb[,1] <= 43.34 & log(mastertimb[,2]) <= 2.885)
class22 <-  which(mastertimb[,1] >  38.89 & mastertimb[,1] <= 43.34 & log(mastertimb[,2]) >  2.885 & log(mastertimb[,2]) <= 3.334)
class23 <-  which(mastertimb[,1] >  38.89 & mastertimb[,1] <= 43.34 & log(mastertimb[,2]) >  3.334 & log(mastertimb[,2]) <= 3.733)
class24 <-  which(mastertimb[,1] >  38.89 & mastertimb[,1] <= 43.34 & log(mastertimb[,2]) >  3.733)

class31 <-  which(mastertimb[,1] >    43.34 & mastertimb[,1] <= 46.89 & log(mastertimb[,2]) <= 2.885)
class32 <-  which(mastertimb[,1] >    43.34 & mastertimb[,1] <= 46.89 & log(mastertimb[,2]) >  2.885 & log(mastertimb[,2]) <= 3.334)
class33 <-  which(mastertimb[,1] >    43.34 & mastertimb[,1] <= 46.89 & log(mastertimb[,2]) >  3.334 & log(mastertimb[,2]) <= 3.733)
class34 <-  which(mastertimb[,1] >    43.34 & mastertimb[,1] <= 46.89 & log(mastertimb[,2]) >  3.733)

class41 <- which(mastertimb[,1] >   46.89 & log(mastertimb[,2]) <= 2.885)
class42 <- which(mastertimb[,1] >   46.89 & log(mastertimb[,2]) >  2.885 & log(mastertimb[,2]) <= 3.334)
class43 <- which(mastertimb[,1] >   46.89 & log(mastertimb[,2]) >  3.334 & log(mastertimb[,2]) <= 3.733)
class44 <- which(mastertimb[,1] >   46.89 & log(mastertimb[,2]) >  3.733)

test <- 9238 # sanity check
c(mastertimb[test,1],log(mastertimb[test,2]))

loudness_timb_class[class11] <- 11
loudness_timb_class[class12] <- 12
loudness_timb_class[class13] <- 13
loudness_timb_class[class14] <- 14
loudness_timb_class[class21] <- 21
loudness_timb_class[class22] <- 22
loudness_timb_class[class23] <- 23
loudness_timb_class[class24] <- 24
loudness_timb_class[class31] <- 31
loudness_timb_class[class32] <- 32
loudness_timb_class[class33] <- 33
loudness_timb_class[class34] <- 34
loudness_timb_class[class41] <- 41
loudness_timb_class[class42] <- 42
loudness_timb_class[class43] <- 43
loudness_timb_class[class44] <- 44
table(loudness_timb_class)

write.table(loudness_timb_class,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/loudness_timb_class.txt",row.names = F, col.names = F)

# brightness (mean)
hist(mastertimb[,3],col="limegreen",nclass=100)
summary(mastertimb[,3])[c(2,3,5)]
brightness.mean_class <- rep(0,length_MSDS)
class25 <-  which(mastertimb[,3] <= -29.87)
class50 <-  which(mastertimb[,3] >  -29.87 & mastertimb[,3] <= 6.533)
class75 <-  which(mastertimb[,3] >   6.533 & mastertimb[,3] <= 35.16)
class100 <- which(mastertimb[,3] >   35.16)
brightness.mean_class[class25] <- 1
brightness.mean_class[class50] <- 2
brightness.mean_class[class75] <- 3
brightness.mean_class[class100] <- 4
table(brightness.mean_class)

# brightness (var)
summary(mastertimb[,4])
summary(log(mastertimb[,4]))[c(2,3,5)]
hist(mastertimb[,4],col="limegreen",nclass=150)
hist(log(mastertimb[,4]),col="limegreen",nclass=150)
brightness.var_class <- rep(0,length_MSDS)
class25 <-  which(log(mastertimb[,4]) <= 7.233)
class50 <-  which(log(mastertimb[,4]) >  7.233 & log(mastertimb[,4]) <= 7.624)
class75 <-  which(log(mastertimb[,4]) >  7.624 & log(mastertimb[,4]) <= 7.996)
class100 <- which(log(mastertimb[,4]) >  7.996)
brightness.var_class[class25] <- 1
brightness.var_class[class50] <- 2
brightness.var_class[class75] <- 3
brightness.var_class[class100] <- 4
table(brightness.var_class)

# brightness mean+var combined
brightness_class <- rep(0,length_MSDS)
class11 <-  which(mastertimb[,3] <= -29.87 & log(mastertimb[,4]) <= 7.233)
class12 <-  which(mastertimb[,3] <= -29.87 & log(mastertimb[,4]) >  7.233 & log(mastertimb[,4]) <= 7.624)
class13 <-  which(mastertimb[,3] <= -29.87 & log(mastertimb[,4]) >  7.624 & log(mastertimb[,4]) <= 7.996)
class14 <-  which(mastertimb[,3] <= -29.87 & log(mastertimb[,4]) >  7.996)

class21 <-  which(mastertimb[,3] >  -29.87 & mastertimb[,3] <= 6.533 & log(mastertimb[,4]) <= 7.233)
class22 <-  which(mastertimb[,3] >  -29.87 & mastertimb[,3] <= 6.533 & log(mastertimb[,4]) >  7.233 & log(mastertimb[,4]) <= 7.624)
class23 <-  which(mastertimb[,3] >  -29.87 & mastertimb[,3] <= 6.533 & log(mastertimb[,4]) >  7.624 & log(mastertimb[,4]) <= 7.996)
class24 <-  which(mastertimb[,3] >  -29.87 & mastertimb[,3] <= 6.533 & log(mastertimb[,4]) >  7.996)

class31 <-  which(mastertimb[,3] >    6.533 & mastertimb[,3] <= 35.16 & log(mastertimb[,4]) <= 7.233)
class32 <-  which(mastertimb[,3] >    6.533 & mastertimb[,3] <= 35.16 & log(mastertimb[,4]) >  7.233 & log(mastertimb[,4]) <= 7.624)
class33 <-  which(mastertimb[,3] >    6.533 & mastertimb[,3] <= 35.16 & log(mastertimb[,4]) >  7.624 & log(mastertimb[,4]) <= 7.996)
class34 <-  which(mastertimb[,3] >    6.533 & mastertimb[,3] <= 35.16 & log(mastertimb[,4]) >  7.996)

class41 <- which(mastertimb[,3] >   35.16 & log(mastertimb[,4]) <= 7.233)
class42 <- which(mastertimb[,3] >   35.16 & log(mastertimb[,4]) >  7.233 & log(mastertimb[,4]) <= 7.624)
class43 <- which(mastertimb[,3] >   35.16 & log(mastertimb[,4]) >  7.624 & log(mastertimb[,4]) <= 7.996)
class44 <- which(mastertimb[,3] >   35.16 & log(mastertimb[,4]) >  7.996)

test <- 9238 # sanity check
c(mastertimb[test,1],log(mastertimb[test,2]))

brightness_class[class11] <- 11
brightness_class[class12] <- 12
brightness_class[class13] <- 13
brightness_class[class14] <- 14
brightness_class[class21] <- 21
brightness_class[class22] <- 22
brightness_class[class23] <- 23
brightness_class[class24] <- 24
brightness_class[class31] <- 31
brightness_class[class32] <- 32
brightness_class[class33] <- 33
brightness_class[class34] <- 34
brightness_class[class41] <- 41
brightness_class[class42] <- 42
brightness_class[class43] <- 43
brightness_class[class44] <- 44
table(brightness_class)

write.table(brightness_class,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/brightness_class.txt",row.names = F, col.names = F)

# flatness (mean)
hist(mastertimb[,5],col="limegreen",nclass=100)
summary(mastertimb[,5])[c(2,3,5)]
flatness.mean_class <- rep(0,length_MSDS)
class25 <-  which(mastertimb[,5] <= -10.11)
class50 <-  which(mastertimb[,5] >  -10.11 & mastertimb[,5] <= 12.78)
class75 <-  which(mastertimb[,5] >   12.78 & mastertimb[,5] <= 33.52)
class100 <- which(mastertimb[,5] >   33.52)
flatness.mean_class[class25] <- 1
flatness.mean_class[class50] <- 2
flatness.mean_class[class75] <- 3
flatness.mean_class[class100] <- 4
table(flatness.mean_class)

# flatness (var)
summary(log(mastertimb[,6]))[c(2,3,5)]
hist(mastertimb[,6],col="limegreen",nclass=150)
hist(log(mastertimb[,6]),col="limegreen",nclass=150)
flatness.var_class <- rep(0,length_MSDS)
class25 <-  which(log(mastertimb[,6]) <= 7.125)
class50 <-  which(log(mastertimb[,6]) >  7.125 & log(mastertimb[,6]) <= 7.496)
class75 <-  which(log(mastertimb[,6]) >  7.496 & log(mastertimb[,6]) <= 7.86)
class100 <- which(log(mastertimb[,6]) >  7.86)
flatness.var_class[class25] <- 1
flatness.var_class[class50] <- 2
flatness.var_class[class75] <- 3
flatness.var_class[class100] <- 4
table(flatness.var_class)

# flatness mean+var combined
flatness_class <- rep(0,length_MSDS)
class11 <- which(mastertimb[,5] <= -10.11 & log(mastertimb[,6]) <= 7.125)
class12 <- which(mastertimb[,5] <= -10.11 & log(mastertimb[,6]) >  7.125 & log(mastertimb[,6]) <= 7.496)
class13 <- which(mastertimb[,5] <= -10.11 & log(mastertimb[,6]) >  7.496 & log(mastertimb[,6]) <= 7.86)
class14 <- which(mastertimb[,5] <= -10.11 & log(mastertimb[,6]) >  7.86)

class21 <- which(mastertimb[,5] >  -10.11 & mastertimb[,5] <= 12.78 & log(mastertimb[,6]) <= 7.125)
class22 <- which(mastertimb[,5] >  -10.11 & mastertimb[,5] <= 12.78 & log(mastertimb[,6]) >  7.125 & log(mastertimb[,6]) <= 7.496)
class23 <- which(mastertimb[,5] >  -10.11 & mastertimb[,5] <= 12.78 & log(mastertimb[,6]) >  7.496 & log(mastertimb[,6]) <= 7.86)
class24 <- which(mastertimb[,5] >  -10.11 & mastertimb[,5] <= 12.78 & log(mastertimb[,6]) >  7.86)

class31 <- which(mastertimb[,5] >   12.78 & mastertimb[,5] <= 33.52 & log(mastertimb[,6]) <= 7.125)
class32 <- which(mastertimb[,5] >   12.78 & mastertimb[,5] <= 33.52 & log(mastertimb[,6]) >  7.125 & log(mastertimb[,6]) <= 7.496)
class33 <- which(mastertimb[,5] >   12.78 & mastertimb[,5] <= 33.52 & log(mastertimb[,6]) >  7.496 & log(mastertimb[,6]) <= 7.86)
class34 <- which(mastertimb[,5] >   12.78 & mastertimb[,5] <= 33.52 & log(mastertimb[,6]) >  7.86)

class41 <- which(mastertimb[,5] >   33.52 & log(mastertimb[,6]) <= 7.125)
class42 <- which(mastertimb[,5] >   33.52 & log(mastertimb[,6]) >  7.125 & log(mastertimb[,6]) <= 7.496)
class43 <- which(mastertimb[,5] >   33.52 & log(mastertimb[,6]) >  7.496 & log(mastertimb[,6]) <= 7.86)
class44 <- which(mastertimb[,5] >   33.52 & log(mastertimb[,6]) >  7.86)

flatness_class[class11] <- 11
flatness_class[class12] <- 12
flatness_class[class13] <- 13
flatness_class[class14] <- 14
flatness_class[class21] <- 21
flatness_class[class22] <- 22
flatness_class[class23] <- 23
flatness_class[class24] <- 24
flatness_class[class31] <- 31
flatness_class[class32] <- 32
flatness_class[class33] <- 33
flatness_class[class34] <- 34
flatness_class[class41] <- 41
flatness_class[class42] <- 42
flatness_class[class43] <- 43
flatness_class[class44] <- 44
table(flatness_class)

write.table(flatness_class,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/flatness_class.txt",row.names = F, col.names = F)

# sounds with stronger attack (mean)
hist(mastertimb[,7],col="limegreen",nclass=100)
summary(mastertimb[,7])[c(2,3,5)]
attack.mean_class <- rep(0,length_MSDS)
class25 <-  which(mastertimb[,7] <= -8.069)
class50 <-  which(mastertimb[,7] >  -8.069 & mastertimb[,7] <= 0.1181)
class75 <-  which(mastertimb[,7] > 0.1181 & mastertimb[,7] <= 10.01)
class100 <- which(mastertimb[,7] >  10.01)
attack.mean_class[class25] <- 1
attack.mean_class[class50] <- 2
attack.mean_class[class75] <- 3
attack.mean_class[class100] <- 4
table(attack.mean_class)

# sounds with stronger attack (var)
summary(log(mastertimb[,8]))[c(2,3,5)]
hist(mastertimb[,8],col="limegreen",nclass=150)
hist(log(mastertimb[,8]),col="limegreen",nclass=150)
attack.var_class <- rep(0,length_MSDS)
class25 <-  which(log(mastertimb[,8]) <= 6.834)
class50 <-  which(log(mastertimb[,8]) >  6.834 & log(mastertimb[,8]) <= 7.245)
class75 <-  which(log(mastertimb[,8]) >  7.245 & log(mastertimb[,8]) <= 7.649)
class100 <- which(log(mastertimb[,8]) >  7.649)
attack.var_class[class25] <- 1
attack.var_class[class50] <- 2
attack.var_class[class75] <- 3
attack.var_class[class100] <- 4
table(attack.var_class)

# sounds with stronger attack mean+var combined
attack_class <- rep(0,length_MSDS)
class11 <-  which(mastertimb[,7] <= -8.069 & log(mastertimb[,8]) <= 6.834)
class12 <-  which(mastertimb[,7] <= -8.069 & log(mastertimb[,8]) >  6.834 & log(mastertimb[,8]) <= 7.245)
class13 <-  which(mastertimb[,7] <= -8.069 & log(mastertimb[,8]) >  7.245 & log(mastertimb[,8]) <= 7.649)
class14 <-  which(mastertimb[,7] <= -8.069 & log(mastertimb[,8]) >  7.649)

class21 <-  which(mastertimb[,7] >  -8.069 & mastertimb[,7] <= 0.1181 & log(mastertimb[,8]) <= 6.834)
class22 <-  which(mastertimb[,7] >  -8.069 & mastertimb[,7] <= 0.1181 & log(mastertimb[,8]) >  6.834 & log(mastertimb[,8]) <= 7.245)
class23 <-  which(mastertimb[,7] >  -8.069 & mastertimb[,7] <= 0.1181 & log(mastertimb[,8]) >  7.245 & log(mastertimb[,8]) <= 7.649)
class24 <-  which(mastertimb[,7] >  -8.069 & mastertimb[,7] <= 0.1181 & log(mastertimb[,8]) >  7.649)

class31 <-  which(mastertimb[,7] > 0.1181 & mastertimb[,7] <= 10.01 & log(mastertimb[,8]) <= 6.834)
class32 <-  which(mastertimb[,7] > 0.1181 & mastertimb[,7] <= 10.01 & log(mastertimb[,8]) >  6.834 & log(mastertimb[,8]) <= 7.245)
class33 <-  which(mastertimb[,7] > 0.1181 & mastertimb[,7] <= 10.01 & log(mastertimb[,8]) >  7.245 & log(mastertimb[,8]) <= 7.649)
class34 <-  which(mastertimb[,7] > 0.1181 & mastertimb[,7] <= 10.01 & log(mastertimb[,8]) >  7.649)

class41 <- which(mastertimb[,7] >  10.01 & log(mastertimb[,8]) <= 6.834)
class42 <- which(mastertimb[,7] >  10.01 & log(mastertimb[,8]) >  6.834 & log(mastertimb[,8]) <= 7.245)
class43 <- which(mastertimb[,7] >  10.01 & log(mastertimb[,8]) >  7.245 & log(mastertimb[,8]) <= 7.649)
class44 <- which(mastertimb[,7] >  10.01 & log(mastertimb[,8]) >  7.649)

attack_class[class11] <- 11
attack_class[class12] <- 12
attack_class[class13] <- 13
attack_class[class14] <- 14
attack_class[class21] <- 21
attack_class[class22] <- 22
attack_class[class23] <- 23
attack_class[class24] <- 24
attack_class[class31] <- 31
attack_class[class32] <- 32
attack_class[class33] <- 33
attack_class[class34] <- 34
attack_class[class41] <- 41
attack_class[class42] <- 42
attack_class[class43] <- 43
attack_class[class44] <- 44
table(attack_class)

write.table(attack_class,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/attack_class.txt",row.names = F, col.names = F)

# timbre5 (mean)
hist(mastertimb[,9],col="limegreen",nclass=100)
summary(mastertimb[,9])[c(2,3,5)]
timbre5.mean_class <- rep(0,length_MSDS)
class25 <-  which(mastertimb[,9] <= -17.83)
class50 <-  which(mastertimb[,9] >  -17.83 & mastertimb[,9] <= -2.7)
class75 <-  which(mastertimb[,9] >  -2.7 & mastertimb[,9] <=  10.5)
class100 <- which(mastertimb[,9] >   10.5)
timbre5.mean_class[class25] <- 1
timbre5.mean_class[class50] <- 2
timbre5.mean_class[class75] <- 3
timbre5.mean_class[class100] <- 4
table(timbre5.mean_class)

# timbre5 (var)
summary(mastertimb[,10])
summary(log(mastertimb[,10]))[c(2,3,5)]
hist(mastertimb[,10],col="limegreen",nclass=150)
hist(log(mastertimb[,10]),col="limegreen",nclass=150)
timbre5.var_class <- rep(0,length_MSDS)
class25 <-  which(log(mastertimb[,10]) <= 6.473)
class50 <-  which(log(mastertimb[,10]) >  6.473 & log(mastertimb[,10]) <= 6.757)
class75 <-  which(log(mastertimb[,10]) >  6.757 & log(mastertimb[,10]) <= 7.046)
class100 <- which(log(mastertimb[,10]) >  7.046)
timbre5.var_class[class25] <- 1
timbre5.var_class[class50] <- 2
timbre5.var_class[class75] <- 3
timbre5.var_class[class100] <- 4
table(timbre5.var_class)

# timbre5 mean+var combined
timbre5_class <- rep(0,length_MSDS)
class11 <-  which(mastertimb[,9] <= -17.83 & log(mastertimb[,10]) <= 6.473)
class12 <-  which(mastertimb[,9] <= -17.83 & log(mastertimb[,10]) >  6.473 & log(mastertimb[,10]) <= 6.757)
class13 <-  which(mastertimb[,9] <= -17.83 & log(mastertimb[,10]) >  6.757 & log(mastertimb[,10]) <= 7.046)
class14 <-  which(mastertimb[,9] <= -17.83 & log(mastertimb[,10]) >  7.046)

class21 <-  which(mastertimb[,9] >  -17.83 & mastertimb[,9] <= -2.7 & log(mastertimb[,10]) <= 6.473)
class22 <-  which(mastertimb[,9] >  -17.83 & mastertimb[,9] <= -2.7 & log(mastertimb[,10]) >  6.473 & log(mastertimb[,10]) <= 6.757)
class23 <-  which(mastertimb[,9] >  -17.83 & mastertimb[,9] <= -2.7 & log(mastertimb[,10]) >  6.757 & log(mastertimb[,10]) <= 7.046)
class24 <-  which(mastertimb[,9] >  -17.83 & mastertimb[,9] <= -2.7 & log(mastertimb[,10]) >  7.046)

class31 <-  which(mastertimb[,9] >  -2.7 & mastertimb[,9] <= 10.5 & log(mastertimb[,10]) <= 6.473)
class32 <-  which(mastertimb[,9] >  -2.7 & mastertimb[,9] <= 10.5 & log(mastertimb[,10]) >  6.473 & log(mastertimb[,10]) <= 6.757)
class33 <-  which(mastertimb[,9] >  -2.7 & mastertimb[,9] <= 10.5 & log(mastertimb[,10]) >  6.757 & log(mastertimb[,10]) <= 7.046)
class34 <-  which(mastertimb[,9] >  -2.7 & mastertimb[,9] <= 10.5 & log(mastertimb[,10]) >  7.046)

class41 <- which(mastertimb[,9] >   10.5 & log(mastertimb[,10]) <= 6.473)
class42 <- which(mastertimb[,9] >   10.5 & log(mastertimb[,10]) >  6.473 & log(mastertimb[,10]) <= 6.757)
class43 <- which(mastertimb[,9] >   10.5 & log(mastertimb[,10]) >  6.757 & log(mastertimb[,10]) <= 7.046)
class44 <- which(mastertimb[,9] >   10.5 & log(mastertimb[,10]) >  7.046)

test <- 9238 # sanity check
c(mastertimb[test,9],log(mastertimb[test,10]))

timbre5_class[class11] <- 11
timbre5_class[class12] <- 12
timbre5_class[class13] <- 13
timbre5_class[class14] <- 14
timbre5_class[class21] <- 21
timbre5_class[class22] <- 22
timbre5_class[class23] <- 23
timbre5_class[class24] <- 24
timbre5_class[class31] <- 31
timbre5_class[class32] <- 32
timbre5_class[class33] <- 33
timbre5_class[class34] <- 34
timbre5_class[class41] <- 41
timbre5_class[class42] <- 42
timbre5_class[class43] <- 43
timbre5_class[class44] <- 44
table(timbre5_class)

write.table(timbre5_class,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/timbre5_class.txt",row.names = F, col.names = F)

# timbre6 (mean)
hist(mastertimb[,11],col="limegreen",nclass=100)
summary(mastertimb[,11])[c(2,3,5)]
timbre6.mean_class <- rep(0,length_MSDS)
class25 <-  which(mastertimb[,11] <= -16.2)
class50 <-  which(mastertimb[,11] >  -16.2 & mastertimb[,11] <= -7.73)
class75 <-  which(mastertimb[,11] >   -7.73 & mastertimb[,11] <= 2.193)
class100 <- which(mastertimb[,11] >   2.193)
timbre6.mean_class[class25] <- 1
timbre6.mean_class[class50] <- 2
timbre6.mean_class[class75] <- 3
timbre6.mean_class[class100] <- 4
table(timbre6.mean_class)

# timbre6 (var)
summary(mastertimb[,12])
summary(log(mastertimb[,12]))[c(2,3,5)]
hist(mastertimb[,12],col="limegreen",nclass=150)
hist(log(mastertimb[,12]),col="limegreen",nclass=150)
timbre6.var_class <- rep(0,length_MSDS)
class25 <-  which(log(mastertimb[,12]) <= 6.279)
class50 <-  which(log(mastertimb[,12]) >  6.279 & log(mastertimb[,12]) <= 6.692)
class75 <-  which(log(mastertimb[,12]) >  6.692 & log(mastertimb[,12]) <= 7.086)
class100 <- which(log(mastertimb[,12]) >  7.086)
timbre6.var_class[class25] <- 1
timbre6.var_class[class50] <- 2
timbre6.var_class[class75] <- 3
timbre6.var_class[class100] <- 4
table(timbre6.var_class)

# timbre6 mean+var combined
timbre6_class <- rep(0,length_MSDS)
class11 <-  which(mastertimb[,11] <= -16.2 & log(mastertimb[,12]) <= 6.279)
class12 <-  which(mastertimb[,11] <= -16.2 & log(mastertimb[,12]) >  6.279 & log(mastertimb[,12]) <= 6.692)
class13 <-  which(mastertimb[,11] <= -16.2 & log(mastertimb[,12]) >  6.692 & log(mastertimb[,12]) <= 7.086)
class14 <-  which(mastertimb[,11] <= -16.2 & log(mastertimb[,12]) >  7.086)

class21 <-  which(mastertimb[,11] >  -16.2 & mastertimb[,11] <= -7.73 & log(mastertimb[,12]) <= 6.279)
class22 <-  which(mastertimb[,11] >  -16.2 & mastertimb[,11] <= -7.73 & log(mastertimb[,12]) >  6.279 & log(mastertimb[,12]) <= 6.692)
class23 <-  which(mastertimb[,11] >  -16.2 & mastertimb[,11] <= -7.73 & log(mastertimb[,12]) >  6.692 & log(mastertimb[,12]) <= 7.086)
class24 <-  which(mastertimb[,11] >  -16.2 & mastertimb[,11] <= -7.73 & log(mastertimb[,12]) >  7.086)

class31 <-  which(mastertimb[,11] >  -7.73 & mastertimb[,11] <= 2.193 & log(mastertimb[,12]) <= 6.279)
class32 <-  which(mastertimb[,11] >  -7.73 & mastertimb[,11] <= 2.193 & log(mastertimb[,12]) >  6.279 & log(mastertimb[,12]) <= 6.692)
class33 <-  which(mastertimb[,11] >  -7.73 & mastertimb[,11] <= 2.193 & log(mastertimb[,12]) >  6.692 & log(mastertimb[,12]) <= 7.086)
class34 <-  which(mastertimb[,11] >  -7.73 & mastertimb[,11] <= 2.193 & log(mastertimb[,12]) >  7.086)

class41 <- which(mastertimb[,11] >   2.193 & log(mastertimb[,12]) <= 6.279)
class42 <- which(mastertimb[,11] >   2.193 & log(mastertimb[,12]) >  6.279 & log(mastertimb[,12]) <= 6.692)
class43 <- which(mastertimb[,11] >   2.193 & log(mastertimb[,12]) >  6.692 & log(mastertimb[,12]) <= 7.086)
class44 <- which(mastertimb[,11] >   2.193 & log(mastertimb[,12]) >  7.086)

test <- 9238 # sanity check
c(mastertimb[test,1],log(mastertimb[test,2]))

timbre6_class[class11] <- 11
timbre6_class[class12] <- 12
timbre6_class[class13] <- 13
timbre6_class[class14] <- 14
timbre6_class[class21] <- 21
timbre6_class[class22] <- 22
timbre6_class[class23] <- 23
timbre6_class[class24] <- 24
timbre6_class[class31] <- 31
timbre6_class[class32] <- 32
timbre6_class[class33] <- 33
timbre6_class[class34] <- 34
timbre6_class[class41] <- 41
timbre6_class[class42] <- 42
timbre6_class[class43] <- 43
timbre6_class[class44] <- 44
table(timbre6_class)

write.table(timbre6_class,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/timbre6_class.txt",row.names = F, col.names = F)

# timbre7 (mean)
hist(mastertimb[,13],col="limegreen",nclass=100)
summary(mastertimb[,13])[c(2,3,5)]
timbre7.mean_class <- rep(0,length_MSDS)
class25 <-  which(mastertimb[,13] <= -13.38)
class50 <-  which(mastertimb[,13] >  -13.38 & mastertimb[,13] <= -3.963)
class75 <-  which(mastertimb[,13] >  -3.963 & mastertimb[,13] <= 4.893)
class100 <- which(mastertimb[,13] >   4.893)
timbre7.mean_class[class25] <- 1
timbre7.mean_class[class50] <- 2
timbre7.mean_class[class75] <- 3
timbre7.mean_class[class100] <- 4
table(timbre7.mean_class)

# timbre7 (var)
summary(mastertimb[,14])
summary(log(mastertimb[,14]))[c(2,3,5)]
hist(mastertimb[,14],col="limegreen",nclass=150)
hist(log(mastertimb[,14]),col="limegreen",nclass=150)
timbre7.var_class <- rep(0,length_MSDS)
class25 <-  which(log(mastertimb[,14]) <= 6.064)
class50 <-  which(log(mastertimb[,14]) >  6.064 & log(mastertimb[,14]) <= 6.392)
class75 <-  which(log(mastertimb[,14]) >  6.392 & log(mastertimb[,14]) <= 6.683)
class100 <- which(log(mastertimb[,14]) >  6.683)
timbre7.var_class[class25] <- 1
timbre7.var_class[class50] <- 2
timbre7.var_class[class75] <- 3
timbre7.var_class[class100] <- 4
table(timbre7.var_class)

# timbre7 mean+var combined
timbre7_class <- rep(0,length_MSDS)
class11 <-  which(mastertimb[,13] <= -13.38 & log(mastertimb[,14]) <= 6.064)
class12 <-  which(mastertimb[,13] <= -13.38 & log(mastertimb[,14]) >  6.064 & log(mastertimb[,14]) <= 6.392)
class13 <-  which(mastertimb[,13] <= -13.38 & log(mastertimb[,14]) >  6.392 & log(mastertimb[,14]) <= 6.683)
class14 <-  which(mastertimb[,13] <= -13.38 & log(mastertimb[,14]) >  6.683)

class21 <-  which(mastertimb[,13] >  -13.38 & mastertimb[,13] <= -3.963 & log(mastertimb[,14]) <= 6.064)
class22 <-  which(mastertimb[,13] >  -13.38 & mastertimb[,13] <= -3.963 & log(mastertimb[,14]) >  6.064 & log(mastertimb[,14]) <= 6.392)
class23 <-  which(mastertimb[,13] >  -13.38 & mastertimb[,13] <= -3.963 & log(mastertimb[,14]) >  6.392 & log(mastertimb[,14]) <= 6.683)
class24 <-  which(mastertimb[,13] >  -13.38 & mastertimb[,13] <= -3.963 & log(mastertimb[,14]) >  6.683)

class31 <-  which(mastertimb[,13] >    -3.963 & mastertimb[,13] <= 4.893 & log(mastertimb[,14]) <= 6.064)
class32 <-  which(mastertimb[,13] >    -3.963 & mastertimb[,13] <= 4.893 & log(mastertimb[,14]) >  6.064 & log(mastertimb[,14]) <= 6.392)
class33 <-  which(mastertimb[,13] >    -3.963 & mastertimb[,13] <= 4.893 & log(mastertimb[,14]) >  6.392 & log(mastertimb[,14]) <= 6.683)
class34 <-  which(mastertimb[,13] >    -3.963 & mastertimb[,13] <= 4.893 & log(mastertimb[,14]) >  6.683)

class41 <- which(mastertimb[,13] >   4.893 & log(mastertimb[,14]) <= 6.064)
class42 <- which(mastertimb[,13] >   4.893 & log(mastertimb[,14]) >  6.064 & log(mastertimb[,14]) <= 6.392)
class43 <- which(mastertimb[,13] >   4.893 & log(mastertimb[,14]) >  6.392 & log(mastertimb[,14]) <= 6.683)
class44 <- which(mastertimb[,13] >   4.893 & log(mastertimb[,14]) >  6.683)

test <- 9238 # sanity check
c(mastertimb[test,1],log(mastertimb[test,2]))

timbre7_class[class11] <- 11
timbre7_class[class12] <- 12
timbre7_class[class13] <- 13
timbre7_class[class14] <- 14
timbre7_class[class21] <- 21
timbre7_class[class22] <- 22
timbre7_class[class23] <- 23
timbre7_class[class24] <- 24
timbre7_class[class31] <- 31
timbre7_class[class32] <- 32
timbre7_class[class33] <- 33
timbre7_class[class34] <- 34
timbre7_class[class41] <- 41
timbre7_class[class42] <- 42
timbre7_class[class43] <- 43
timbre7_class[class44] <- 44
table(timbre7_class)

write.table(timbre7_class,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Features/timbre7_class.txt",row.names = F, col.names = F)


###################################

features_v3.0 <- matrix(c(genre,
                          key,
                          mode,
                          section_class,
                          tempo_class,
                          time_signature,
                          loudness_timb_class),byrow=F,nrow=9939)

features_v3.1 <- matrix(c(genre,
                          mode,
                          tempo_class,
                          loudness_timb_class,
                          brightness_class,
                          flatness_class,
                          attack_class),byrow=F,nrow=9939)

features_v3.2 <- matrix(c(genre,
                          tempo_class,
                          time_signature,
                          loudness_timb_class,
                          brightness_class,
                          flatness_class,
                          attack_class),byrow=F,nrow=9939)

features_v3.3 <- matrix(c(loudness_timb_class,
                          brightness_class,
                          flatness_class,
                          attack_class,
                          timbre5_class,
                          timbre6_class,
                          timbre7_class),byrow=F,nrow=9939)

features_v3.4 <- matrix(c(mode,
                          tempo_class,
                          time_signature,
                          loudness_timb_class,
                          brightness_class,
                          flatness_class,
                          attack_class),byrow=F,nrow=9939)

features_v3.5 <- matrix(c(tempo_class,
                          time_signature,
                          loudness_timb_class,
                          brightness_class,
                          flatness_class,
                          attack_class),byrow=F,nrow=9939)

features_v3.6 <- matrix(c(genre,
                          loudness_timb_class,
                          brightness_class,
                          flatness_class,
                          attack_class,
                          timbre5_class,
                          timbre6_class),byrow=F,nrow=9939)

features_v3.7 <- matrix(c(mode,
                          loudness_timb_class,
                          brightness_class,
                          flatness_class,
                          attack_class,
                          timbre5_class,
                          timbre6_class),byrow=F,nrow=9939)

features_v3.8 <- matrix(c(tempo_class,
                          loudness_timb_class,
                          brightness_class,
                          flatness_class,
                          attack_class,
                          timbre5_class,
                          timbre6_class),byrow=F,nrow=9939)

features_v3.9 <- matrix(c(time_signature,
                          loudness_timb_class,
                          brightness_class,
                          flatness_class,
                          attack_class,
                          timbre5_class,
                          timbre6_class),byrow=F,nrow=9939)


genre,
mode,
tempo_class,
time_signature,

loudness_timb_class,
brightness_class,
flatness_class,
attack_class,
timbre5_class,
timbre6_class,
timbre7_class