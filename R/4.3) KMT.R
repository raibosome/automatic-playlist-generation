########################################################
#  Obtain KMT v3: MSDS + aotm + lastfm + tasteprofile  #
########################################################

# you need
# kmt_album
# kmt_aotm
# kmt_lastfm
# kmt_tasteprofile

       kmt_album <- h5read("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kmt_album.h5", "kmt_album")
        kmt_aotm <- h5read("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kmt_aotm.h5", "kmt_aotm")
      kmt_lastfm <- h5read("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kmt_lastfm.h5", "kmt_lastfm")
kmt_tasteprofile <- h5read("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kmt_tasteprofile.h5", "kmt_tasteprofile")
       kmt_album <- h5read("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\kmt_album.h5", "kmt_album")
      kmt_lastfm <- h5read("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\kmt_lastfm.h5", "kmt_lastfm")
        kmt_aotm <- h5read("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\kmt_aotm.txt", "kmt_aotm")
kmt_tasteprofile <- h5read("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\kmt_tasteprofile.h5", "kmt_tasteprofile")

       no_unique_albums <- 7799 # length(unique(albumname))
no_unique_aotmplaylists <- 812
no_unique_lastfmlists   <- 3851
no_unique_tasteprofiles <- 170992

##############################################################
#  Version 3

KMT_v3 <- kmt_album +
  kmt_aotm +
  kmt_lastfm +
  kmt_tasteprofile
dim(which(KMT_v3!=0,arr.ind = T)) # 436,497
436497/(9939^2) * 100 # 0.442% sparsity
all(KMT_v3 == t(KMT_v3))

#scaling 1
KMT_v3 <- KMT_v3/(no_unique_albums+no_unique_aotmplaylists+no_unique_lastfmlists+no_unique_tasteprofiles)

h5createFile("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/KMT_v3.h5") 
attr(KMT_v3, "scale") <- "liter" 
h5write(KMT_v3, "/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/KMT_v3.h5", "KMT_v3")
KMT_v3 <- h5read("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/KMT_v3.h5", "KMT_v3")
KMT_v3 <- h5read("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\KMT_v3.h5", "KMT_v3")

# obtain nonzero indices of UPPER TRIANGLE of KMT_v3
nonzero <- KMT_v3
nonzero <- upper.triangle(nonzero) # leaves lower.tri to be 0
diag(nonzero) <- rep(0,9939)
table(nonzero[which(upper.tri(nonzero))])
KMT_v3_nonzeroentries <- which(nonzero != 0,arr.ind = T)
head(KMT_v3_nonzeroentries)
(length_nonzero <- length(KMT_v3_nonzeroentries[,1])) # 213,279. Prev 16,892 (in the KMT_v3 matrix where diag & upper triangle entries = 0)
length(which(KMT_v3!=0))/((9939)^2)*100
# KMT_v3 is less sparse. Now 0.442% sparse. prev 0.0443 %

h5createFile("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/KMT_v3_nonzeroentries.h5") 
attr(KMT_v3_nonzeroentries, "scale") <- "liter" 
h5write(KMT_v3_nonzeroentries, "/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/KMT_v3_nonzeroentries.h5", "KMT_v3_nonzeroentries")
KMT_v3_nonzeroentries <- h5read("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/KMT_v3_nonzeroentries.h5", "KMT_v3_nonzeroentries")
KMT_v3_nonzeroentries <- h5read("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\KMT_v3_nonzeroentries.h5", "KMT_v3_nonzeroentries")