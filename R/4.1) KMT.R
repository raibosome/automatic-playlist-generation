#####################
#  Obtain matrix K  #
#####################

# v1.0: only MSD albums
# v1.1: MSD albums + AoTM playlists

albumname <- album[,1]
unique_albumname <- unique(albumname)
no_unique_albums <- length(unique_albumname)
no_unique_playlists <- length(unique(aotm_matched[,2]))
songs_from_aotm <- unique(aotm_matched[,1])

# justify algorithm
getmatrixK_v1.0 <- function(N,S) {  
  
  matrix_K <- matrix(0,S,S)
  for (i in 1:(S-1)) {
    j <- i+1
    while (j <= S) {
      if (albumname[i] == albumname[j])
        matrix_K[i,j] <- 1
      j <- j+1
    }
  }
  
  matrix_K <- matrix_K + t(matrix_K) # nondiagonal entries
  
  matrix_K <- matrix_K + diag(1,S)   # diagonal
  
  return(matrix_K/no_unique_albums)
}

getmatrixK_v1.1 <- function(N,S) {  
  
  matrix_K <- matrix(0,S,S)
  for (i in 1:(S-1)) {
    print(i)
    j <- i+1
    while (j <= S) {
      if (is.element(i,songs_from_aotm) && is.element(j,songs_from_aotm))
        matrix_K[i,j] <- is.from.aotm(i,j)
      else if (albumname[i] == albumname[j])
        matrix_K[i,j] <- 1
      j <- j+1
    }
  }
  
  matrix_K <- matrix_K + t(matrix_K) # nondiagonal entries
  
  matrix_K <- matrix_K + diag(1,S)   # diagonal
  
  return(matrix_K/(no_unique_albums+no_unique_playlists))
}

##############################################################
#  Version 1

(start.time <- Sys.time())
matrix_K_v1.0 <- getmatrixK_v1.0(N,S=10000)
start.time <- Sys.time()
(end.time <- Sys.time())
(difftime(end.time,start.time,units = c("auto")))
(difftime(end.time,start.time,units = c("mins")))
(difftime(end.time,start.time,units = c("secs")))
for (i in 1:3)
  beep("facebook")

h5createFile("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\matrix_K_v1.0.h5")
attr(matrix_K_v1.0,"scale") <- "liter"
h5write(matrix_K_v1.0,"C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\matrix_K_v1.0.h5","matrix_K_v1.0")

matrix_K_v1.0 <- h5read(file = "C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\matrix_K_v1.0.h5","matrix_K_v1.0")

dim(matrix_K_v1.0)

# 1000 : 1.332255 secs
# 2000 : 5.232433 secs
# 5000 : 29.02656 secs
#  10k : *150 secs = 2.5 min
# schoool com: 1.519678 mins

##############################################################
#  Version 3

(start.time <- Sys.time())
matrix_K_v1.1 <- getmatrixK_v1.1(N,S=10000)
(start.time)
(end.time <- Sys.time())
(difftime(end.time,start.time,units = c("auto")))
(difftime(end.time,start.time,units = c("mins")))
(difftime(end.time,start.time,units = c("secs")))
for (i in 1:3)
  beep("facebook")

h5createFile("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\matrix_K_v1.1.h5")
attr(matrix_K_v1.1,"scale") <- "liter"
h5write(matrix_K_v1.1,"C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\matrix_K_v1.1.h5","matrix_K_v1.1")

matrix_K_v1.1 <- h5read(file = "C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\matrix_K_v1.1.h5","matrix_K_v1.1")

# 1000 :
# 2000 :
# 5000 : 
#  10k :
# schoool com:

dim(matrix_K_v1.1)
