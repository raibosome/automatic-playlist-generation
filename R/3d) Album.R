albumname
no_unique_albums <- 7799 # length(unique(albumname))

unique_albums <- unique(albumname)
albums <- rep(0,7799) # matrix of albums[uniquealbumno.]=no.of songs
for (i in 1:7799) {
  albums[i] <- length(which(albumname %in% unique_albums[i]))  
}

kmt_album <- matrix(0,nrow = 9939,ncol = 9939)
for (i in 1:9938) {
  if (i %% 100 == 0)
    print(i)
  j <- i+1
  while (j <= 9939) {
    if (albumname[i] == albumname[j])
      kmt_album[i,j] <- 1
    j <- j+1
  }
}
dim(which(kmt_album!=0,arr.ind = T)) # 3183

kmt_album <- kmt_album + t(kmt_album) # nondiagonal entries
kmt_album <- kmt_album + diag(1,9939) 
all(kmt_album == t(kmt_album))

h5createFile("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kmt_album.h5") 
attr(kmt_album, "scale") <- "liter" 
h5write(kmt_album, "/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kmt_album.h5", "kmt_album")
kmt_album <- h5read("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kmt_album.h5", "kmt_album")
kmt_album <- h5read("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\kmt_album.h5", "kmt_album")