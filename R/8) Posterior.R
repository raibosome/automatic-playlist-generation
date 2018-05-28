kernel_K_v3.0 <- h5read("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kernel_K_v3.0.h5", "kernel_K_v3.0")
kernel_K_v3.1 <- h5read("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kernel_K_v3.1.h5", "kernel_K_v3.1")
kernel_K_v3.2 <- h5read("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kernel_K_v3.2.h5", "kernel_K_v3.2")
kernel_K_v3.3 <- h5read("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kernel_K_v3.3.h5", "kernel_K_v3.3")
kernel_K_v3.4 <- h5read("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kernel_K_v3.4.h5", "kernel_K_v3.4")
kernel_K_v3.5 <- h5read("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kernel_K_v3.5.h5", "kernel_K_v3.5")
kernel_K_v3.6 <- h5read("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kernel_K_v3.6.h5", "kernel_K_v3.6")
kernel_K_v3.7 <- h5read("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kernel_K_v3.7.h5", "kernel_K_v3.7")
kernel_K_v3.8 <- h5read("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kernel_K_v3.8.h5", "kernel_K_v3.8")
kernel_K_v3.9 <- h5read("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kernel_K_v3.9.h5", "kernel_K_v3.9")

find_matrix_A_v3.0 <- function(sigma,userpref_songs) {
  matrix_A <- NULL
  no_userpref <- length(userpref_songs)
  matrix_A <- kernel_K_v3.0[userpref_songs,userpref_songs] +
    diag(sigma^2,no_userpref)
  matrix_A <- solve(matrix_A)
  return(matrix_A)
}
find_matrix_A_v3.1 <- function(sigma,userpref_songs) {
  matrix_A <- NULL
  no_userpref <- length(userpref_songs)
  matrix_A <- kernel_K_v3.1[userpref_songs,userpref_songs] +
    diag(sigma^2,no_userpref)
  matrix_A <- solve(matrix_A)
  return(matrix_A)
}
find_matrix_A_v3.2 <- function(sigma,userpref_songs) {
  matrix_A <- NULL
  no_userpref <- length(userpref_songs)
  matrix_A <- kernel_K_v3.2[userpref_songs,userpref_songs] +
    diag(sigma^2,no_userpref)
  matrix_A <- solve(matrix_A)
  return(matrix_A)
}
find_matrix_A_v3.3 <- function(sigma,userpref_songs) {
  matrix_A <- NULL
  no_userpref <- length(userpref_songs)
  matrix_A <- kernel_K_v3.3[userpref_songs,userpref_songs] +
    diag(sigma^2,no_userpref)
  matrix_A <- solve(matrix_A)
  return(matrix_A)
}
find_matrix_A_v3.4 <- function(sigma,userpref_songs) {
  matrix_A <- NULL
  no_userpref <- length(userpref_songs)
  matrix_A <- kernel_K_v3.4[userpref_songs,userpref_songs] +
    diag(sigma^2,no_userpref)
  matrix_A <- solve(matrix_A)
  return(matrix_A)
}
find_matrix_A_v3.5 <- function(sigma,userpref_songs) {
  matrix_A <- NULL
  no_userpref <- length(userpref_songs)
  matrix_A <- kernel_K_v3.5[userpref_songs,userpref_songs] +
    diag(sigma^2,no_userpref)
  matrix_A <- solve(matrix_A)
  return(matrix_A)
}
find_matrix_A_v3.6 <- function(sigma,userpref_songs) {
  matrix_A <- NULL
  no_userpref <- length(userpref_songs)
  matrix_A <- kernel_K_v3.6[userpref_songs,userpref_songs] +
    diag(sigma^2,no_userpref)
  matrix_A <- solve(matrix_A)
  return(matrix_A)
}
find_matrix_A_v3.7 <- function(sigma,userpref_songs) {
  matrix_A <- NULL
  no_userpref <- length(userpref_songs)
  matrix_A <- kernel_K_v3.7[userpref_songs,userpref_songs] +
    diag(sigma^2,no_userpref)
  matrix_A <- solve(matrix_A)
  return(matrix_A)
}
find_matrix_A_v3.8 <- function(sigma,userpref_songs) {
  matrix_A <- NULL
  no_userpref <- length(userpref_songs)
  matrix_A <- kernel_K_v3.8[userpref_songs,userpref_songs] +
    diag(sigma^2,no_userpref)
  matrix_A <- solve(matrix_A)
  return(matrix_A)
}
find_matrix_A_v3.9 <- function(sigma,userpref_songs) {
  matrix_A <- NULL
  no_userpref <- length(userpref_songs)
  matrix_A <- kernel_K_v3.9[userpref_songs,userpref_songs] +
    diag(sigma^2,no_userpref)
  matrix_A <- solve(matrix_A)
  return(matrix_A)
}

loglikelihood_v3.0 <- function(sigma,userpref_songs,userpref_obs) {
  matrix_A <- find_matrix_A_v3.0(sigma,userpref_songs)
  
  no_userpref <- length(userpref_songs)
  value <- 0.5*log(det(matrix_A)) -
    0.5*t(userpref_obs)%*%matrix_A%*%userpref_obs -
    0.5 * no_userpref * log(2*pi)
  
  return(as.numeric(value))
}
loglikelihood_v3.1 <- function(sigma,userpref_songs,userpref_obs) {
  matrix_A <- find_matrix_A_v3.1(sigma,userpref_songs)
  
  no_userpref <- length(userpref_songs)
  value <- 0.5*log(det(matrix_A)) -
    0.5*t(userpref_obs)%*%matrix_A%*%userpref_obs -
    0.5 * no_userpref * log(2*pi)
  
  return(as.numeric(value))
}
loglikelihood_v3.2 <- function(sigma,userpref_songs,userpref_obs) {
  matrix_A <- find_matrix_A_v3.2(sigma,userpref_songs)
  
  no_userpref <- length(userpref_songs)
  value <- 0.5*log(det(matrix_A)) -
    0.5*t(userpref_obs)%*%matrix_A%*%userpref_obs -
    0.5 * no_userpref * log(2*pi)
  
  return(as.numeric(value))
}
loglikelihood_v3.3 <- function(sigma,userpref_songs,userpref_obs) {
  matrix_A <- find_matrix_A_v3.3(sigma,userpref_songs)
  
  no_userpref <- length(userpref_songs)
  value <- 0.5*log(det(matrix_A)) -
    0.5*t(userpref_obs)%*%matrix_A%*%userpref_obs -
    0.5 * no_userpref * log(2*pi)
  
  return(as.numeric(value))
}
loglikelihood_v3.4 <- function(sigma,userpref_songs,userpref_obs) {
  matrix_A <- find_matrix_A_v3.4(sigma,userpref_songs)
  
  no_userpref <- length(userpref_songs)
  value <- 0.5*log(det(matrix_A)) -
    0.5*t(userpref_obs)%*%matrix_A%*%userpref_obs -
    0.5 * no_userpref * log(2*pi)
  
  return(as.numeric(value))
}
loglikelihood_v3.5 <- function(sigma,userpref_songs,userpref_obs) {
  matrix_A <- find_matrix_A_v3.5(sigma,userpref_songs)
  
  no_userpref <- length(userpref_songs)
  value <- 0.5*log(det(matrix_A)) -
    0.5*t(userpref_obs)%*%matrix_A%*%userpref_obs -
    0.5 * no_userpref * log(2*pi)
  
  return(as.numeric(value))
}
loglikelihood_v3.6 <- function(sigma,userpref_songs,userpref_obs) {
  matrix_A <- find_matrix_A_v3.6(sigma,userpref_songs)
  
  no_userpref <- length(userpref_songs)
  value <- 0.5*log(det(matrix_A)) -
    0.5*t(userpref_obs)%*%matrix_A%*%userpref_obs -
    0.5 * no_userpref * log(2*pi)
  
  return(as.numeric(value))
}
loglikelihood_v3.7 <- function(sigma,userpref_songs,userpref_obs) {
  matrix_A <- find_matrix_A_v3.7(sigma,userpref_songs)
  
  no_userpref <- length(userpref_songs)
  value <- 0.5*log(det(matrix_A)) -
    0.5*t(userpref_obs)%*%matrix_A%*%userpref_obs -
    0.5 * no_userpref * log(2*pi)
  
  return(as.numeric(value))
}
loglikelihood_v3.8 <- function(sigma,userpref_songs,userpref_obs) {
  matrix_A <- find_matrix_A_v3.8(sigma,userpref_songs)
  
  no_userpref <- length(userpref_songs)
  value <- 0.5*log(det(matrix_A)) -
    0.5*t(userpref_obs)%*%matrix_A%*%userpref_obs -
    0.5 * no_userpref * log(2*pi)
  
  return(as.numeric(value))
}
loglikelihood_v3.9 <- function(sigma,userpref_songs,userpref_obs) {
  matrix_A <- find_matrix_A_v3.9(sigma,userpref_songs)
  
  no_userpref <- length(userpref_songs)
  value <- 0.5*log(det(matrix_A)) -
    0.5*t(userpref_obs)%*%matrix_A%*%userpref_obs -
    0.5 * no_userpref * log(2*pi)
  
  return(as.numeric(value))
}

####################################

# userpref_songs <- c(6362)
# length(userpref_songs)
# userpref_obs <- c(1)
# length(userpref_songs)
# (no_userpref <- length(userpref_obs))
# dim(userpref_obs) <- c(no_userpref,1)
# 
# # testing <- function(x)
# #   -x^2 +5*x -1
# # results <- optimx(par = 1, fn = testing, lower=0, control=list(maximize=T),method="L-BFGS-B")
# 
# # sigma
# sigma_guess <- 0.5
# loglikelihood_v3.4(sigma_guess,userpref_songs,userpref_obs)
# results_sigma <- suppressWarnings(suppressMessages(optimx(par=sigma_guess, fn=loglikelihood_v3.4, lower=0,
#                         control=list(dowarn=F,maximize=T),method="L-BFGS-B",
#                         userpref_songs=userpref_songs,userpref_obs=userpref_obs)))
# (sigma <- results_sigma$p1)
# 
# # matrix_A
# matrix_A_v3.4 <- find_matrix_A_v3.4(sigma,userpref_songs)
# 
# ###################
# #    PLAYLIST     #
# ###################
# 
# candidate_index <- (1:9939)[-userpref_songs]
# # posterior mean
# scores <- kernel_K_v3.4[candidate_index,userpref_songs] %*% 
#   matrix_A_v3.4 %*% userpref_obs
# unordered <- matrix(c(candidate_index,scores),ncol=2)
# newrule <- rev(order(scores))
# ordered <- matrix(c(candidate_index[newrule],scores[newrule]),ncol=2)
# 
# top5songs <- ordered[1:5,1]
# displaysongdetails(userpref_songs)
# displaysongdetails(top5songs)
# 
# 
