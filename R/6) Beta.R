# Computes Dmat, dvec & Beta

no_of_features <- 7
N <- 2^no_of_features

       no_unique_albums <- 7799 # length(unique(albumname))
no_unique_aotmplaylists <- 812
no_unique_lastfmlists   <- 3851
no_unique_tasteprofiles <- 170992

KMT_v3_nonzeroentries <- h5read("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/KMT_v3_nonzeroentries.h5", "KMT_v3_nonzeroentries")
KMT_v3_nonzeroentries <- h5read("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\KMT_v3_nonzeroentries.h5", "KMT_v3_nonzeroentries")

features <- features_v3.0
features <- features_v3.1
features <- features_v3.2
features <- features_v3.3
features <- features_v3.4
features <- features_v3.5
features <- features_v3.6
features <- features_v3.7
features <- features_v3.8
features <- features_v3.9

######################################
### Amat
Amat <- diag(1,N)

######################################
### bvec
bvec <- rep(0,N)

######################################
### Dmat
Dmat_entry <- function(u,v) {
  sum <- 0
  if (u==v) {
    for (i in 1:S) {
      j_samples <- rand_subset[i,]
      for (j in j_samples) {
        mercer_val <- mercer(number=u,features[i,],features[j,])
        if (mercer_val == 1)
          sum <- sum + 1
      }
    }
  }
  else {
    for (i in 1:S) {
      j_samples <- rand_subset[i,]
      for (j in j_samples) {
        value_u <- mercer(number=u,features[i,],features[j,])
        if (value_u == 1) {
          value_v <- mercer(number=v,features[i,],features[j,])
          if (value_v == 1)
            sum <- sum+1 
        }        
      }
    }
  }
  return(sum)
}

S <- 9939
samplesize <- 100
rand_subset <- matrix(0,S,samplesize)
set.seed(4199)
for (i in 1:S)
  rand_subset[i,] <- sample(1:S,samplesize,replace = F)
Dmat <- matrix(0,N,N)
(start.time <- Sys.time())
for (u in 1:36) {          # v > u entries  #1:36,37:128
  v <- u+1
  while (v <= N) {
    cat("Dmat | u = ",u,". v = ",v,". Start time: ",sep="")
    print(start.time)
    Dmat[u,v] <- Dmat_entry(u,v)
    v <- v+1
  }
}
(start.time)
(end.time <- Sys.time())
(difftime(end.time,start.time,units = c("auto")))
(difftime(end.time,start.time,units = c("mins")))
(difftime(end.time,start.time,units = c("secs")))
for (i in 1:3)
  beep("facebook")

# 7h (parallellised by 2)
Dmat_v3_i <- Dmat
Dmat_v3_ii <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Dmat_v3_ii.txt"))

Dmat <- Dmat_v3_i + Dmat_v3_ii

Dmatbackup <- Dmat
Dmat <- Dmat + t(Dmat)    # non-diagonal entries
system.time(
  for (u in 1:N) {  
    print(u)         # diagonal entries
    Dmat[u,u] <- Dmat_entry(u,u)
  }
)
# 677 secs

all(Dmat == t(Dmat))

Dmat_v3.0 <- Dmat
Dmat_v3.1 <- Dmat
Dmat_v3.2 <- Dmat
Dmat_v3.3 <- Dmat
Dmat_v3.4 <- Dmat
Dmat_v3.5 <- Dmat
Dmat_v3.6 <- Dmat
Dmat_v3.7 <- Dmat
Dmat_v3.8 <- Dmat
Dmat_v3.9 <- Dmat

write.table(Dmat_v3.0,"C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Dmat_v3.0.txt",col.names = F,row.names = F)
write.table(Dmat_v3.1,"C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Dmat_v3.1.txt",col.names = F,row.names = F)
write.table(Dmat_v3.2,"C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Dmat_v3.2.txt",col.names = F,row.names = F)
write.table(Dmat_v3.3,"C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Dmat_v3.3.txt",col.names = F,row.names = F)
write.table(Dmat_v3.4,"C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Dmat_v3.4.txt",col.names = F,row.names = F)
write.table(Dmat_v3.5,"C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Dmat_v3.5.txt",col.names = F,row.names = F)
write.table(Dmat_v3.6,"C:\\Users\\a0082893\\Desktop\\Dmat_v3.6.txt",col.names = F,row.names = F)
write.table(Dmat_v3.7,"C:\\Users\\a0082893\\Desktop\\Dmat_v3.7.txt",col.names = F,row.names = F)
write.table(Dmat_v3.8,"C:\\Users\\a0082893\\Desktop\\Dmat_v3.8.txt",col.names = F,row.names = F)
write.table(Dmat_v3.9,"C:\\Users\\a0082893\\Desktop\\Dmat_v3.9.txt",col.names = F,row.names = F)

write.table(Dmat_v3.0,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Dmat_v3.0.txt",col.names = F,row.names = F)
write.table(Dmat_v3.1,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Dmat_v3.1.txt",col.names = F,row.names = F)
write.table(Dmat_v3.2,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Dmat_v3.2.txt",col.names = F,row.names = F)
write.table(Dmat_v3.3,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Dmat_v3.3.txt",col.names = F,row.names = F)
write.table(Dmat_v3.4,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Dmat_v3.4.txt",col.names = F,row.names = F)
write.table(Dmat_v3.5,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Dmat_v3.5.txt",col.names = F,row.names = F)

Dmat_v3.0 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Dmat_v3.0.txt"))
Dmat_v3.1 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Dmat_v3.1.txt"))
Dmat_v3.2 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Dmat_v3.2.txt"))
Dmat_v3.3 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Dmat_v3.3.txt"))
Dmat_v3.4 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Dmat_v3.4.txt"))
Dmat_v3.5 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Dmat_v3.5.txt"))
Dmat_v3.6 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Dmat_v3.6.txt"))
Dmat_v3.7 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Dmat_v3.7.txt"))
Dmat_v3.8 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Dmat_v3.8.txt"))
Dmat_v3.9 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Dmat_v3.9.txt"))

Dmat_v3.0 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Dmat_v3.0.txt"))
Dmat_v3.1 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Dmat_v3.1.txt"))
Dmat_v3.2 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Dmat_v3.2.txt"))
Dmat_v3.3 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Dmat_v3.3.txt"))
Dmat_v3.4 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Dmat_v3.4.txt"))
Dmat_v3.5 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Dmat_v3.5.txt"))

######################################
### dvec_v3

(length_nonzero <- length(KMT_v3_nonzeroentries[,1])) # 213,279 entries in upper triangle

dvec_v3 <- rep(0,N)
dvec_v3[1] <- 2*length_nonzero+9939 
system.time(for (m in 2:128) {
  cat("dvec_v3 iteration ",m,"\n")
  
  # for diagonals. also a way to initialise 'sum'
  sum <- 9939
  
  # for lower triangle
  for (k in 1:length_nonzero) {
    song1 <- KMT_v3_nonzeroentries[k,1]
    song2 <- KMT_v3_nonzeroentries[k,2]
    value <- mercer(number=m,song1_features = features[song1,],song2_features = features[song2,])
    if (value == 1)
      sum <- sum + 2
  }
  dvec_v3[m] <- sum
})
# 2.8 mins

dvec_v3 <- dvec_v3/(no_unique_albums+no_unique_aotmplaylists+no_unique_lastfmlists+no_unique_tasteprofiles)

dvec_v3.0 <- dvec_v3
dvec_v3.1 <- dvec_v3
dvec_v3.2 <- dvec_v3
dvec_v3.3 <- dvec_v3
dvec_v3.4 <- dvec_v3
dvec_v3.5 <- dvec_v3
dvec_v3.6 <- dvec_v3
dvec_v3.7 <- dvec_v3
dvec_v3.8 <- dvec_v3
dvec_v3.9 <- dvec_v3

write.table(dvec_v3.0,"C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\dvec_v3.0.txt",row.names = F, col.names = F)
write.table(dvec_v3.1,"C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\dvec_v3.1.txt",row.names = F, col.names = F)
write.table(dvec_v3.2,"C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\dvec_v3.2.txt",row.names = F, col.names = F)
write.table(dvec_v3.3,"C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\dvec_v3.3.txt",row.names = F, col.names = F)
write.table(dvec_v3.4,"C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\dvec_v3.4.txt",row.names = F, col.names = F)
write.table(dvec_v3.5,"C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\dvec_v3.5.txt",row.names = F, col.names = F)

write.table(dvec_v3.0,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/dvec_v3.0.txt",row.names = F, col.names = F)
write.table(dvec_v3.1,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/dvec_v3.1.txt",row.names = F, col.names = F)
write.table(dvec_v3.2,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/dvec_v3.2.txt",row.names = F, col.names = F)
write.table(dvec_v3.3,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/dvec_v3.3.txt",row.names = F, col.names = F)
write.table(dvec_v3.4,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/dvec_v3.4.txt",row.names = F, col.names = F)
write.table(dvec_v3.5,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/dvec_v3.5.txt",row.names = F, col.names = F)
write.table(dvec_v3.6,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/dvec_v3.6.txt",row.names = F, col.names = F)
write.table(dvec_v3.7,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/dvec_v3.7.txt",row.names = F, col.names = F)
write.table(dvec_v3.8,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/dvec_v3.8.txt",row.names = F, col.names = F)
write.table(dvec_v3.9,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/dvec_v3.9.txt",row.names = F, col.names = F)

dvec_v3.0 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\dvec_v3.0.txt"))
dvec_v3.1 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\dvec_v3.1.txt"))
dvec_v3.2 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\dvec_v3.2.txt"))
dvec_v3.3 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\dvec_v3.3.txt"))
dvec_v3.4 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\dvec_v3.4.txt"))
dvec_v3.5 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\dvec_v3.5.txt"))
dvec_v3.6 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\dvec_v3.6.txt"))
dvec_v3.7 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\dvec_v3.7.txt"))
dvec_v3.8 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\dvec_v3.8.txt"))
dvec_v3.9 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\dvec_v3.9.txt"))

dvec_v3.0 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/dvec_v3.0.txt"))
dvec_v3.1 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/dvec_v3.1.txt"))
dvec_v3.2 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/dvec_v3.2.txt"))
dvec_v3.3 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/dvec_v3.3.txt"))
dvec_v3.4 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/dvec_v3.4.txt"))
dvec_v3.5 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/dvec_v3.5.txt"))
dvec_v3.6 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/dvec_v3.6.txt"))
dvec_v3.7 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/dvec_v3.7.txt"))
dvec_v3.8 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/dvec_v3.8.txt"))
dvec_v3.9 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/dvec_v3.9.txt"))

# Solve for Beta
Dmat_v3.4 <- as.matrix(nearPD(Dmat_v3.4,corr = F,keepDiag = T)$mat)

Beta <- solve.QP(Dmat_v3.0,dvec_v3.0,Amat,bvec)$solution
Beta <- solve.QP(Dmat_v3.1,dvec_v3.1,Amat,bvec)$solution
Beta <- solve.QP(Dmat_v3.2,dvec_v3.2,Amat,bvec)$solution
Beta <- solve.QP(Dmat_v3.3,dvec_v3.3,Amat,bvec)$solution
Beta <- solve.QP(Dmat_v3.4,dvec_v3.4,Amat,bvec)$solution
Beta <- solve.QP(Dmat_v3.5,dvec_v3.5,Amat,bvec)$solution
Beta <- solve.QP(Dmat_v3.6,dvec_v3.6,Amat,bvec)$solution
Beta <- solve.QP(Dmat_v3.7,dvec_v3.7,Amat,bvec)$solution
Beta <- solve.QP(Dmat_v3.8,dvec_v3.8,Amat,bvec)$solution
Beta <- solve.QP(Dmat_v3.9,dvec_v3.9,Amat,bvec)$solution

length(which(Beta > 1e-11))
length(which(Beta > 1e-10))
Beta[Beta < 1e-11] <- 0
Beta
hist(Beta[which(Beta != 0)])

Beta_v3.0 <- Beta
Beta_v3.1 <- Beta
Beta_v3.2 <- Beta
Beta_v3.3 <- Beta
Beta_v3.4 <- Beta
Beta_v3.5 <- Beta
Beta_v3.6 <- Beta
Beta_v3.7 <- Beta
Beta_v3.8 <- Beta
Beta_v3.9 <- Beta

write.table(Beta_v3.0,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Beta_v3.0.txt",row.names = F, col.names = F)
write.table(Beta_v3.1,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Beta_v3.1.txt",row.names = F, col.names = F)
write.table(Beta_v3.2,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Beta_v3.2.txt",row.names = F, col.names = F)
write.table(Beta_v3.3,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Beta_v3.3.txt",row.names = F, col.names = F)
write.table(Beta_v3.4,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Beta_v3.4.txt",row.names = F, col.names = F)
write.table(Beta_v3.5,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Beta_v3.5.txt",row.names = F, col.names = F)
write.table(Beta_v3.6,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Beta_v3.6.txt",row.names = F, col.names = F)

write.table(Beta_v3.0,"C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Beta_v3.0.txt",row.names = F, col.names = F)
write.table(Beta_v3.1,"C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Beta_v3.1.txt",row.names = F, col.names = F)
write.table(Beta_v3.2,"C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Beta_v3.2.txt",row.names = F, col.names = F)
write.table(Beta_v3.3,"C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Beta_v3.3.txt",row.names = F, col.names = F)
write.table(Beta_v3.4,"C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Beta_v3.4.txt",row.names = F, col.names = F)
write.table(Beta_v3.5,"C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Beta_v3.5.txt",row.names = F, col.names = F)
write.table(Beta_v3.6,"C:\\Users\\a0082893\\Desktop\\Beta_v3.6.txt",row.names = F, col.names = F)
write.table(Beta_v3.7,"C:\\Users\\a0082893\\Desktop\\Beta_v3.7.txt",row.names = F, col.names = F)
write.table(Beta_v3.8,"C:\\Users\\a0082893\\Desktop\\Beta_v3.8.txt",row.names = F, col.names = F)
write.table(Beta_v3.9,"C:\\Users\\a0082893\\Desktop\\Beta_v3.9.txt",row.names = F, col.names = F)

Beta_v3.0 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Beta_v3.0.txt"))
Beta_v3.1 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Beta_v3.1.txt"))
Beta_v3.2 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Beta_v3.2.txt"))
Beta_v3.3 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Beta_v3.3.txt"))
Beta_v3.4 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Beta_v3.4.txt"))
Beta_v3.5 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Beta_v3.5.txt"))

Beta_v3.0 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Beta_v3.0.txt"))
Beta_v3.1 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Beta_v3.1.txt"))
Beta_v3.2 <-  as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Beta_v3.2.txt"))
Beta_v3.3 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Beta_v3.3.txt"))
Beta_v3.4 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Beta_v3.4.txt"))
Beta_v3.5 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Beta_v3.5.txt"))
Beta_v3.6 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Beta_v3.6.txt"))
Beta_v3.7 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Beta_v3.7.txt"))
Beta_v3.8 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Beta_v3.8.txt"))
Beta_v3.9 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Beta_v3.9.txt"))


Beta <- Beta_v3.0
Beta <- Beta_v3.1
Beta <- Beta_v3.2
Beta <- Beta_v3.3
Beta <- Beta_v3.4
Beta <- Beta_v3.5
Beta <- Beta_v3.6
Beta <- Beta_v3.7
Beta <- Beta_v3.8
Beta <- Beta_v3.9

which(Beta!=0)
for (i in which(Beta!=0))
  print(bit[[i]][[1]])
Beta[Beta!=0]

#create a matrix of checked items
(l <- length(Beta[Beta!=0]))
checkbin <- matrix(0,ncol=7,nrow=l)
checkweight <- matrix(0,ncol=7,nrow=l)
for (i in 1:l) {
  checkbin[i,] <- bit[[which(Beta!=0)[i]]][[1]]
  checkweight[i,] <- bit[[which(Beta!=0)[i]]][[1]]*Beta[Beta!=0][i]
}
# x <- apply(checkbin,MARGIN = 2,sum)
# rev(order(x))
x <- apply(checkweight,MARGIN = 2,sum)
rev(order(x))

