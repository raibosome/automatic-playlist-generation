# Standard Collaborative filtering metric
# S={1,2,3,4,5}

validation_aotm   <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/validation_aotm.txt"))
validation_lastfm <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/validation_lastfm.txt"))
validation_taste  <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/validation_taste.txt"))
validation_aotm   <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\validation_aotm.txt"))
validation_lastfm <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\validation_lastfm.txt"))
validation_taste  <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\validation_taste.txt"))
length(validation_aotm)   # 3
length(validation_lastfm) # 5
length(validation_taste)  # 104
# total 112 'playlists'

list_aotm         <- fromJSON("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/list_aotm.json")
list_lastfm       <- fromJSON("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/list_lastfm.json")
list_tasteprofile <- fromJSON("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/list_tasteprofile.json")
list_aotm         <- fromJSON("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\list_aotm.json")
list_lastfm       <- fromJSON("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\list_lastfm.json")
list_tasteprofile <- fromJSON("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\list_tasteprofile.json")

# compile these into a list
validation_list <- list(NULL)
for (i in 1:112) {
  if (i <= 3)
    for (j in 1:3)
      validation_list[[j]] <- list_aotm[[validation_aotm[j]]]
  else if (i <= 8)
    for (j in 1:5)
      validation_list[[j+3]] <- list_lastfm[[validation_lastfm[j]]]
  else
    for (j in 1:104)
      validation_list[[j+8]] <- list_tasteprofile[[validation_taste[j]]]
}
tail(validation_list)

detach("package:jsonlite", unload=TRUE)
library("RJSONIO")
exportJSON <- toJSON(validation_list)
write(exportJSON,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/validation_list.json")
validation_list <- fromJSON("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/validation_list.json")
validation_list <- fromJSON("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\validation_list.json")
detach("package:RJSONIO", unload=TRUE)
library("jsonlite")

# unique songs from validation dataset
validation_unique_songs <- NULL
validation_lengths <- NULL
for (i in 1:112) {
  songs <- validation_list[[i]]
  validation_lengths[i] <- length(songs)
  songs <- unique(songs)
  validation_unique_songs <- c(validation_unique_songs,songs)
}
table(validation_lengths)
validation_unique_songs <- unique(validation_unique_songs)
validation_unique_songs <- validation_unique_songs[order(validation_unique_songs)] #just arranging the order of MSDS indices
length(validation_unique_songs) # 508 unique songs

###################
##   R SCORE     ##
###################

  VERSION <- c("v3.0","v3.1","v3.2","v3.3","v3.4","v3.5","v3.6","v3.7","v3.8","v3.9","random")
   trials <- 1000
 halflife <- 10
    seeds <- 1
topupsize <- 150           # "topupsize" excludes no. of test songs. 100 bad 130 bad. 170 ideal
      RNG <- c(1416,9646,4359,2111,5448)
 leng_RNG <- length(RNG)
  R_score <- matrix(0,nrow=trials,ncol=(length(VERSION)+1)) #include maxscore
for (j in 1:trials) {
  print(j)
  set.seed(NULL)
  
  # find a playlist that has more than the no. of seed songs
        (tochoose <- which(validation_lengths >= (seeds+1)))
  (playlist_index <- sample(tochoose,1))
  (chosenplaylist <- validation_list[[playlist_index]])
  (lengthplaylist <- length(chosenplaylist))
  
  # seed songs = train songs
  (train         <- sample(1:lengthplaylist,seeds))
  (train_songs   <- validation_list[[playlist_index]][train])
  (which(validation_unique_songs %in% train_songs)) # sanity check
  
  # to be tested
  (test          <- which(!(1:lengthplaylist %in% train)))
  (test_songs    <- validation_list[[playlist_index]][test])
  (test_length   <- length(test))
  (which(validation_unique_songs %in% test_songs)) # sanity check
  
  allscores_v3.0 <- rep(0,leng_RNG)
  allscores_v3.1 <- rep(0,leng_RNG)
  allscores_v3.2 <- rep(0,leng_RNG)
  allscores_v3.3 <- rep(0,leng_RNG)
  allscores_v3.4 <- rep(0,leng_RNG)
  allscores_v3.5 <- rep(0,leng_RNG)
  allscores_v3.6 <- rep(0,leng_RNG)
  allscores_v3.7 <- rep(0,leng_RNG)
  allscores_v3.8 <- rep(0,leng_RNG)
  allscores_v3.9 <- rep(0,leng_RNG)
  allscores_rand <- rep(0,leng_RNG)
  allscores_max  <- rep(0,leng_RNG)
  
  for (r in 1:leng_RNG) {
    # userlibrary
    set.seed(RNG[r])
    userlibrary <- sample(c(1:9939)[-validation_unique_songs],topupsize,replace = F)
    userlibrary <- c(userlibrary,test_songs)
    
    (generated_v3.0 <- generate.playlist_v3.0(train_songs,userlibrary))
    (generated_v3.1 <- generate.playlist_v3.1(train_songs,userlibrary))
    (generated_v3.2 <- generate.playlist_v3.2(train_songs,userlibrary))
    (generated_v3.3 <- generate.playlist_v3.3(train_songs,userlibrary))
    (generated_v3.4 <- generate.playlist_v3.4(train_songs,userlibrary))
    (generated_v3.5 <- generate.playlist_v3.5(train_songs,userlibrary))
    (generated_v3.6 <- generate.playlist_v3.6(train_songs,userlibrary))
    (generated_v3.7 <- generate.playlist_v3.7(train_songs,userlibrary))
    (generated_v3.8 <- generate.playlist_v3.8(train_songs,userlibrary))
    (generated_v3.9 <- generate.playlist_v3.9(train_songs,userlibrary))
    (generated_rand <- sample(userlibrary,replace=F))
    
    (ranks_v3.0 <- which(generated_v3.0 %in% test_songs))
    (ranks_v3.1 <- which(generated_v3.1 %in% test_songs))
    (ranks_v3.2 <- which(generated_v3.2 %in% test_songs))
    (ranks_v3.3 <- which(generated_v3.3 %in% test_songs))
    (ranks_v3.4 <- which(generated_v3.4 %in% test_songs))
    (ranks_v3.5 <- which(generated_v3.5 %in% test_songs))
    (ranks_v3.6 <- which(generated_v3.6 %in% test_songs))
    (ranks_v3.7 <- which(generated_v3.7 %in% test_songs))
    (ranks_v3.8 <- which(generated_v3.8 %in% test_songs))
    (ranks_v3.9 <- which(generated_v3.9 %in% test_songs))
    (ranks_rand <- which(generated_rand %in% test_songs))

    score_v3.0 <- 0
    score_v3.1 <- 0
    score_v3.2 <- 0
    score_v3.3 <- 0
    score_v3.4 <- 0
    score_v3.5 <- 0
    score_v3.6 <- 0
    score_v3.7 <- 0
    score_v3.8 <- 0
    score_v3.9 <- 0
    score_rand <- 0
    score_max  <- 0
    for (i in 1:test_length) {
      score_v3.0 <- score_v3.0 + 1/( 2^((ranks_v3.0[i]-1) / (halflife-1)) )
      score_v3.1 <- score_v3.1 + 1/( 2^((ranks_v3.1[i]-1) / (halflife-1)) )
      score_v3.2 <- score_v3.2 + 1/( 2^((ranks_v3.2[i]-1) / (halflife-1)) )
      score_v3.3 <- score_v3.3 + 1/( 2^((ranks_v3.3[i]-1) / (halflife-1)) )
      score_v3.4 <- score_v3.4 + 1/( 2^((ranks_v3.4[i]-1) / (halflife-1)) )
      score_v3.5 <- score_v3.5 + 1/( 2^((ranks_v3.5[i]-1) / (halflife-1)) )
      score_v3.6 <- score_v3.6 + 1/( 2^((ranks_v3.6[i]-1) / (halflife-1)) )
      score_v3.7 <- score_v3.7 + 1/( 2^((ranks_v3.7[i]-1) / (halflife-1)) )
      score_v3.8 <- score_v3.8 + 1/( 2^((ranks_v3.8[i]-1) / (halflife-1)) )
      score_v3.9 <- score_v3.9 + 1/( 2^((ranks_v3.9[i]-1) / (halflife-1)) )
      score_rand <- score_rand + 1/( 2^((ranks_rand[i]-1) / (halflife-1)) )
      score_max  <- score_max  + 1/( 2^((i-1)             / (halflife-1)) )
    }
    allscores_v3.0[r] <- score_v3.0
    allscores_v3.1[r] <- score_v3.1
    allscores_v3.2[r] <- score_v3.2
    allscores_v3.3[r] <- score_v3.3
    allscores_v3.4[r] <- score_v3.4
    allscores_v3.5[r] <- score_v3.5
    allscores_v3.6[r] <- score_v3.6
    allscores_v3.7[r] <- score_v3.7
    allscores_v3.8[r] <- score_v3.8
    allscores_v3.9[r] <- score_v3.9
    allscores_rand[r] <- score_rand
    allscores_max[r]  <- score_max
  }

  (R_score[j,] <- c(
    mean(allscores_v3.0),
    mean(allscores_v3.1),
    mean(allscores_v3.2),
    mean(allscores_v3.3),
    mean(allscores_v3.4),
    mean(allscores_v3.5),
    mean(allscores_v3.6),
    mean(allscores_v3.7),
    mean(allscores_v3.8),
    mean(allscores_v3.9),
    mean(allscores_rand),
    mean(allscores_max)))
}
RSCORE <- 100*apply(R_score,MARGIN = 2,sum)/sum(R_score[,(length(VERSION)+1)])
RSCORE <- formatC(RSCORE,digits=2,format="f")[-(length(VERSION)+1)]
as.data.frame(cbind(VERSION,RSCORE))
beep("facebook")

# RSCORE_table <- matrix(0,ncol=10,nrow=11)
# RSCORE_table <- as.data.frame(RSCORE_table)
# names(RSCORE_table) <- 1:10
# rownames(RSCORE_table) <- VERSION
seeds
RSCORE_table[,seeds] <- as.numeric(RSCORE)
backup <- RSCORE_table
RSCORE_table <- RSCORE_table[,1:5]

write.table(RSCORE_table,"/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/RSCORE_table.txt",row.names = T,col.names = T)
RSCORE_table <- as.data.frame(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/RSCORE_table.txt",col.names = c(1:5)))

#######################
##   BEST PLAYLIST   ##
#######################

RSCORE_table <- as.matrix(RSCORE_table)
wilcox.test(RSCORE_table["v3.0",],
            RSCORE_table["random",],paired=T,
            alternative = "greater")

wilcox.test(RSCORE_table["v3.1",],
            RSCORE_table["v3.9",],paired=T,
            alternative = "greater")

wilcox.test(RSCORE_table["v3.6",],
            RSCORE_table["v3.2",],paired=T,
            alternative = "greater")
# reject null for all except v3.0 and v3.1

wilcox.test(RSCORE_table["v3.5",], RSCORE_table["v3.0",],paired=T,alternative = "less")


############################
##   PLAYLIST FUNCTIONS   ##
############################


generate.playlist_v3.0 <- function(userpref_songs,userlibrary) {
  # seed
  userpref_obs      <- rep(1,length(userpref_songs))
  dim(userpref_obs) <- c(length(userpref_obs),1)
  
  # sigma
  sigma_guess   <- 0.5
  loglikelihood_v3.0(sigma_guess,userpref_songs,userpref_obs)
  results_sigma <- optimx(par=sigma_guess, fn=loglikelihood_v3.0,lower=0,
                          control=list(maximize=T),method="L-BFGS-B",
                          userpref_songs=userpref_songs,userpref_obs=userpref_obs)
  matrix_A_v3.0 <- find_matrix_A_v3.0(results_sigma$p1,userpref_songs)  
  
  # posterior mean 
  scores    <- kernel_K_v3.0[userlibrary,userpref_songs] %*% 
    matrix_A_v3.0 %*% userpref_obs
  unordered <- matrix(c(userlibrary,scores),ncol=2)
  newrule   <- rev(order(scores))
  ordered   <- matrix(c(userlibrary[newrule],scores[newrule]),ncol=2)
  
  return(ordered[,1])
}

generate.playlist_v3.1 <- function(userpref_songs,userlibrary) {
  # seed
  userpref_obs      <- rep(1,length(userpref_songs))
  dim(userpref_obs) <- c(length(userpref_obs),1)
  
  # sigma
  sigma_guess   <- 0.5
  loglikelihood_v3.1(sigma_guess,userpref_songs,userpref_obs)
  results_sigma <- optimx(par=sigma_guess, fn=loglikelihood_v3.1,lower=0,
                          control=list(maximize=T),method="L-BFGS-B",
                          userpref_songs=userpref_songs,userpref_obs=userpref_obs)
  matrix_A_v3.1 <- find_matrix_A_v3.1(results_sigma$p1,userpref_songs)  
  
  # posterior mean 
  scores    <- kernel_K_v3.1[userlibrary,userpref_songs] %*% 
    matrix_A_v3.1 %*% userpref_obs
  unordered <- matrix(c(userlibrary,scores),ncol=2)
  newrule   <- rev(order(scores))
  ordered   <- matrix(c(userlibrary[newrule],scores[newrule]),ncol=2)
  
  return(ordered[,1])
}

generate.playlist_v3.2 <- function(userpref_songs,userlibrary) {
  # seed
  userpref_obs      <- rep(1,length(userpref_songs))
  dim(userpref_obs) <- c(length(userpref_obs),1)
  
  # sigma
  sigma_guess   <- 0.5
  loglikelihood_v3.2(sigma_guess,userpref_songs,userpref_obs)
  results_sigma <- optimx(par=sigma_guess, fn=loglikelihood_v3.2,lower=0,
                          control=list(maximize=T),method="L-BFGS-B",
                          userpref_songs=userpref_songs,userpref_obs=userpref_obs)
  matrix_A_v3.2 <- find_matrix_A_v3.2(results_sigma$p1,userpref_songs)  
  
  # posterior mean 
  scores    <- kernel_K_v3.2[userlibrary,userpref_songs] %*% 
    matrix_A_v3.2 %*% userpref_obs
  unordered <- matrix(c(userlibrary,scores),ncol=2)
  newrule   <- rev(order(scores))
  ordered   <- matrix(c(userlibrary[newrule],scores[newrule]),ncol=2)
  
  return(ordered[,1])
}

generate.playlist_v3.3 <- function(userpref_songs,userlibrary) {
  # seed
  userpref_obs      <- rep(1,length(userpref_songs))
  dim(userpref_obs) <- c(length(userpref_obs),1)
  
  # sigma
  sigma_guess   <- 0.5
  loglikelihood_v3.3(sigma_guess,userpref_songs,userpref_obs)
  results_sigma <- optimx(par=sigma_guess, fn=loglikelihood_v3.3,lower=0,
                          control=list(maximize=T),method="L-BFGS-B",
                          userpref_songs=userpref_songs,userpref_obs=userpref_obs)
  matrix_A_v3.3 <- find_matrix_A_v3.3(results_sigma$p1,userpref_songs)  
  
  # posterior mean 
  scores    <- kernel_K_v3.3[userlibrary,userpref_songs] %*% 
    matrix_A_v3.3 %*% userpref_obs
  unordered <- matrix(c(userlibrary,scores),ncol=2)
  newrule   <- rev(order(scores))
  ordered   <- matrix(c(userlibrary[newrule],scores[newrule]),ncol=2)
  
  return(ordered[,1])
}

generate.playlist_v3.4 <- function(userpref_songs,userlibrary) {
  # seed
  userpref_obs      <- rep(1,length(userpref_songs))
  dim(userpref_obs) <- c(length(userpref_obs),1)
  
  # sigma
  sigma_guess   <- 0.5
  loglikelihood_v3.4(sigma_guess,userpref_songs,userpref_obs)
  results_sigma <- optimx(par=sigma_guess, fn=loglikelihood_v3.4,lower=0,
                          control=list(maximize=T),method="L-BFGS-B",
                          userpref_songs=userpref_songs,userpref_obs=userpref_obs)
  matrix_A_v3.4 <- find_matrix_A_v3.4(results_sigma$p1,userpref_songs)  
  
  # posterior mean 
  scores    <- kernel_K_v3.4[userlibrary,userpref_songs] %*% 
    matrix_A_v3.4 %*% userpref_obs
  unordered <- matrix(c(userlibrary,scores),ncol=2)
  newrule   <- rev(order(scores))
  ordered   <- matrix(c(userlibrary[newrule],scores[newrule]),ncol=2)
  
  return(ordered[,1])
}

generate.playlist_v3.5 <- function(userpref_songs,userlibrary) {
  # seed
  userpref_obs      <- rep(1,length(userpref_songs))
  dim(userpref_obs) <- c(length(userpref_obs),1)
  
  # sigma
  sigma_guess   <- 0.5
  loglikelihood_v3.5(sigma_guess,userpref_songs,userpref_obs)
  results_sigma <- optimx(par=sigma_guess, fn=loglikelihood_v3.5,lower=0,
                          control=list(maximize=T),method="L-BFGS-B",
                          userpref_songs=userpref_songs,userpref_obs=userpref_obs)
  matrix_A_v3.5 <- find_matrix_A_v3.5(results_sigma$p1,userpref_songs)  
  
  # posterior mean 
  scores    <- kernel_K_v3.5[userlibrary,userpref_songs] %*% 
    matrix_A_v3.5 %*% userpref_obs
  unordered <- matrix(c(userlibrary,scores),ncol=2)
  newrule   <- rev(order(scores))
  ordered   <- matrix(c(userlibrary[newrule],scores[newrule]),ncol=2)
  
  return(ordered[,1])
}

generate.playlist_v3.6 <- function(userpref_songs,userlibrary) {
  # seed
  userpref_obs      <- rep(1,length(userpref_songs))
  dim(userpref_obs) <- c(length(userpref_obs),1)
  
  # sigma
  sigma_guess   <- 0.5
  loglikelihood_v3.6(sigma_guess,userpref_songs,userpref_obs)
  results_sigma <- optimx(par=sigma_guess, fn=loglikelihood_v3.6,lower=0,
                          control=list(maximize=T),method="L-BFGS-B",
                          userpref_songs=userpref_songs,userpref_obs=userpref_obs)
  matrix_A_v3.6 <- find_matrix_A_v3.6(results_sigma$p1,userpref_songs)  
  
  # posterior mean 
  scores    <- kernel_K_v3.6[userlibrary,userpref_songs] %*% 
    matrix_A_v3.6 %*% userpref_obs
  unordered <- matrix(c(userlibrary,scores),ncol=2)
  newrule   <- rev(order(scores))
  ordered   <- matrix(c(userlibrary[newrule],scores[newrule]),ncol=2)
  
  return(ordered[,1])
}

generate.playlist_v3.7 <- function(userpref_songs,userlibrary) {
  # seed
  userpref_obs      <- rep(1,length(userpref_songs))
  dim(userpref_obs) <- c(length(userpref_obs),1)
  
  # sigma
  sigma_guess   <- 0.5
  loglikelihood_v3.7(sigma_guess,userpref_songs,userpref_obs)
  results_sigma <- optimx(par=sigma_guess, fn=loglikelihood_v3.7,lower=0,
                          control=list(maximize=T),method="L-BFGS-B",
                          userpref_songs=userpref_songs,userpref_obs=userpref_obs)
  matrix_A_v3.7 <- find_matrix_A_v3.7(results_sigma$p1,userpref_songs)  
  
  # posterior mean 
  scores    <- kernel_K_v3.7[userlibrary,userpref_songs] %*% 
    matrix_A_v3.7 %*% userpref_obs
  unordered <- matrix(c(userlibrary,scores),ncol=2)
  newrule   <- rev(order(scores))
  ordered   <- matrix(c(userlibrary[newrule],scores[newrule]),ncol=2)
  
  return(ordered[,1])
}

generate.playlist_v3.8 <- function(userpref_songs,userlibrary) {
  # seed
  userpref_obs      <- rep(1,length(userpref_songs))
  dim(userpref_obs) <- c(length(userpref_obs),1)
  
  # sigma
  sigma_guess   <- 0.5
  loglikelihood_v3.8(sigma_guess,userpref_songs,userpref_obs)
  results_sigma <- optimx(par=sigma_guess, fn=loglikelihood_v3.8,lower=0,
                          control=list(maximize=T),method="L-BFGS-B",
                          userpref_songs=userpref_songs,userpref_obs=userpref_obs)
  matrix_A_v3.8 <- find_matrix_A_v3.8(results_sigma$p1,userpref_songs)  
  
  # posterior mean 
  scores    <- kernel_K_v3.8[userlibrary,userpref_songs] %*% 
    matrix_A_v3.8 %*% userpref_obs
  unordered <- matrix(c(userlibrary,scores),ncol=2)
  newrule   <- rev(order(scores))
  ordered   <- matrix(c(userlibrary[newrule],scores[newrule]),ncol=2)
  
  return(ordered[,1])
}

generate.playlist_v3.9 <- function(userpref_songs,userlibrary) {
  # seed
  userpref_obs      <- rep(1,length(userpref_songs))
  dim(userpref_obs) <- c(length(userpref_obs),1)
  
  # sigma
  sigma_guess   <- 0.5
  loglikelihood_v3.9(sigma_guess,userpref_songs,userpref_obs)
  results_sigma <- optimx(par=sigma_guess, fn=loglikelihood_v3.9,lower=0,
                          control=list(maximize=T),method="L-BFGS-B",
                          userpref_songs=userpref_songs,userpref_obs=userpref_obs)
  matrix_A_v3.9 <- find_matrix_A_v3.9(results_sigma$p1,userpref_songs)  
  
  # posterior mean 
  scores    <- kernel_K_v3.9[userlibrary,userpref_songs] %*% 
    matrix_A_v3.9 %*% userpref_obs
  unordered <- matrix(c(userlibrary,scores),ncol=2)
  newrule   <- rev(order(scores))
  ordered   <- matrix(c(userlibrary[newrule],scores[newrule]),ncol=2)
  
  return(ordered[,1])
}





for (i in which(Beta!=0)) {
  cat("---",i,"---\n")
  print(bit[[i]])
  readline()
}

