####### Mercer Kernel, psi(n,x,y) #######
library(R.utils) # for binary function

no_of_features <- 7
N <- 2^no_of_features
# Beta <- rep(1,N) # initial Beta values. to be maximised later

# integer to binary
intseq <- NULL
binseq <- NULL
intseq <- seq(0,N-1,1)       # integer
binseq <- intToBin(intseq)   # binary
binseq

bit <- NULL
for (i in 1:N) {
  (temp <- strsplit(binseq[i], split=""))
  (part1 <- charToInt(temp[[1]])-48)  # ASCII to integer
  (part2 <- which(part1 %in% 1))
  (bit[[i]] <- list(part1,part2))
}
head(bit)

# scalar. mercer = {0,1}
# number: integer number that represents the binary form bit[[number]]
mercer <- function(number,song1_features,song2_features) {
  if (number==1)
    return(1)
  
  (components_to_check <- bit[[number]][[2]])
  for (i in components_to_check)
    if (song1_features[i] != song2_features[i])
      return(0)
  return(1)
}

# scalar. (i,j)th component of PSI; given song1 and song2
sum_mercer <- function(N,song1_features,song2_features,Beta) {
  value <- 0
  for (i in 1:N) {
    if (Beta[i]==0)
      next
    mercervalue <- mercer(number=i,song1_features,song2_features)
    if (mercervalue == 1)
      value <- value + Beta[i]
  }
  return(value)
}

# PSI: basically getting the covariance kernel.
get_PSI <- function(Beta,N,S_range) {
  PSI <- matrix(0,9939,9939)
  for (i in S_range) {
    cat("get_PSI i = ",i,". Start time = ")
    print(start.time)
    j <- i+1
    while (j <= 9939) {
      PSI[i,j] <- sum_mercer(N,features[i,],features[j,],Beta)
      j <- j+1
    }
  }
  return(PSI)
}


# User PSI: basically getting the covariance kernel.
get_user_PSI <- function(Beta,N,no_of_songs,features) {
  PSI <- matrix(0,no_of_songs,no_of_songs)
  for (i in 1:(no_of_songs-1)) {
#     cat("get_user_PSI i = ",i,". Start time = ")
#     print(start.time)
    j <- i+1
    while (j <= no_of_songs) {
      PSI[i,j] <- sum_mercer(N,features[i,],features[j,],Beta)
      j <- j+1
    }
  }
  return(PSI)
}
