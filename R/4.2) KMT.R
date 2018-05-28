#####################
#  Obtain matrix K  #
#####################

# v2.0: MSD albums + lastfm
# v2.1: MSD albums + 10% lastfm
# v2.2: MSD albums + 20% lastfm

dim(lastfm_matrix_final)
matches_MSDindex2

lastfm[-totrain10_MSDind]
lastfm[-totrain20_MSDind]

albumname <- album[,1]
unique_albumname <- unique(albumname)
no_unique_albums <- length(unique_albumname)
no_unique_similarsongs <- length(matches_MSDindex2)




# justify algorithm

getmatrixK_v2.0 <- function(N,S) {  
  
  matrix_K <- matrix(0,S,S)
  for (i in 1:(S-1)) {
    if (i %% 2 == 0)
      print(i)
    j <- i+1
    while (j <= S) {
      if (albumname[i] == albumname[j])
        matrix_K[i,j] <- 1
      else {
        if (is.element(el = i,set = totrain10_MSDind)==T)
          break
        else if (is.element(el = j,set = totrain10_MSDind)==T)
          break
      }
        matrix_K[i,j] <- are.similar.songs(i,j)
      j <- j+1
    }
  }
  
  matrix_K <- matrix_K + t(matrix_K) # nondiagonal entries
  
  matrix_K <- matrix_K + diag(1,S)   # diagonal
  
  no_unique_similarsongs <- length(matches_MSDindex2)
  return(matrix_K/(no_unique_albums+no_unique_similarsongs))
}

getmatrixK_v2.1 <- function(N,S) {  
  
  matrix_K <- matrix(0,S,S)
  for (i in 1:(S-1)) {
    if (i %% 2 == 0)
      print(i)
    j <- i+1
    while (j <= S) {
      if (albumname[i] == albumname[j])
        matrix_K[i,j] <- 1
      else {
        if (is.element(el = i,set = totrain10_MSDind)==F && 
              is.element(el = j,set = totrain10_MSDind)==F)
          matrix_K[i,j] <- are.similar.songs(i,j)
      }  
      j <- j+1
    }
  }
  
  matrix_K <- matrix_K + t(matrix_K) # nondiagonal entries
  
  matrix_K <- matrix_K + diag(1,S)   # diagonal
  
  no_unique_similarsongs <- length(matches_MSDindex2)-length(totrain10_MSDind)
  return(matrix_K/(no_unique_albums+no_unique_similarsongs))
}

getmatrixK_v2.2 <- function(N,S) {  
  
  matrix_K <- matrix(0,S,S)
  for (i in 1:(S-1)) {
    if (i %% 2 == 0)
      print(i)
    j <- i+1
    while (j <= S) {
      if (albumname[i] == albumname[j])
        matrix_K[i,j] <- 1
      else {
        if (is.element(el = i,set = totrain20_MSDind)==F && 
              is.element(el = j,set = totrain20_MSDind)==F)
          matrix_K[i,j] <- are.similar.songs(i,j)
      }  
      j <- j+1
    }
  }
  
  matrix_K <- matrix_K + t(matrix_K) # nondiagonal entries
  
  matrix_K <- matrix_K + diag(1,S)   # diagonal
  
  no_unique_similarsongs <- length(matches_MSDindex2)-length(totrain20_MSDind)
  return(matrix_K/(no_unique_albums+no_unique_similarsongs))
}

##############################################################
#  Version 2.0

(start.time <- Sys.time())
matrix_K_v2.0 <- getmatrixK_v2.0(N,S=10000)
(end.time <- Sys.time())
(difftime(end.time,start.time,units = c("auto")))
(difftime(end.time,start.time,units = c("mins")))
(difftime(end.time,start.time,units = c("secs")))
for (i in 1:3)
  beep("facebook")

h5createFile("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\matrix_K_v2.0.h5")
attr(matrix_K_v2.0,"scale") <- "liter"
h5write(matrix_K_v2.0,"C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\matrix_K_v2.0.h5","matrix_K_v2.0")
matrix_K_v2.0 <- h5read(file = "C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\matrix_K_v2.0.h5","matrix_K_v2.0")

h5createFile("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/matrix_K_v2.0.h5")
attr(matrix_K_v2.0,"scale") <- "liter"
h5write(matrix_K_v2.0,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/matrix_K_v2.0.h5","matrix_K_v2.0")
matrix_K_v2.0 <- h5read(file = "/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/matrix_K_v2.0.h5","matrix_K_v2.0")

dim(matrix_K_v2.0)
length(which(matrix_K_v2.0!=0))

# 10k : 67.1 mins
# schoool com: 

##############################################################
#  Version 2.1

(start.time <- Sys.time())
matrix_K_v2.1 <- getmatrixK_v2.1(N,S=10000)
(end.time <- Sys.time())
(difftime(end.time,start.time,units = c("auto")))
(difftime(end.time,start.time,units = c("mins")))
(difftime(end.time,start.time,units = c("secs")))
for (i in 1:3)
  beep("facebook")

h5createFile("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\matrix_K_v2.1.h5")
attr(matrix_K_v2.1,"scale") <- "liter"
h5write(matrix_K_v2.1,"C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\matrix_K_v2.1.h5","matrix_K_v2.1")
matrix_K_v2.1 <- h5read(file = "C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\matrix_K_v2.1.h5","matrix_K_v2.1")

h5createFile("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/matrix_K_v2.1.h5")
attr(matrix_K_v2.1,"scale") <- "liter"
h5write(matrix_K_v2.1,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/matrix_K_v2.1.h5","matrix_K_v2.1")
matrix_K_v2.1 <- h5read(file = "/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/matrix_K_v2.1.h5","matrix_K_v2.1")

dim(matrix_K_v2.1)
length(which(matrix_K_v2.1!=0))

# 10k : 84.9 mins
# schoool com: 

##############################################################
#  Version 2.2

(start.time <- Sys.time())
matrix_K_v2.2 <- getmatrixK_v2.2(N,S=10000)
(end.time <- Sys.time())
(difftime(end.time,start.time,units = c("auto")))
(difftime(end.time,start.time,units = c("mins")))
(difftime(end.time,start.time,units = c("secs")))
for (i in 1:3)
  beep("facebook")

h5createFile("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\matrix_K_v2.2.h5")
attr(matrix_K_v2.2,"scale") <- "liter"
h5write(matrix_K_v2.2,"C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\matrix_K_v2.2.h5","matrix_K_v2.2")
matrix_K_v2.2 <- h5read(file = "C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\matrix_K_v2.2.h5","matrix_K_v2.2")

h5createFile("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/matrix_K_v2.2.h5")
attr(matrix_K_v2.2,"scale") <- "liter"
h5write(matrix_K_v2.2,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/matrix_K_v2.2.h5","matrix_K_v2.2")
matrix_K_v2.2 <- h5read(file = "/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/matrix_K_v2.2.h5","matrix_K_v2.2")

dim(matrix_K_v2.2)
length(which(matrix_K_v2.2!=0))

# 10k : 93.40281 mins
# schoool com: 

########################################################
