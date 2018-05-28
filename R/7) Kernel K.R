############################################### 
#              Final Kernel K                 #
###############################################

no_of_features <- 7
N <- 2^no_of_features

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

Beta_v3.0 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Beta_v3.0.txt"))
Beta_v3.1 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Beta_v3.1.txt"))
Beta_v3.2 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Beta_v3.2.txt"))
Beta_v3.3 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Beta_v3.3.txt"))
Beta_v3.4 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Beta_v3.4.txt"))
Beta_v3.5 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\Beta_v3.5.txt"))
Beta_v3.6 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\Beta_v3.6.txt"))
Beta_v3.7 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\Beta_v3.7.txt"))
Beta_v3.8 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\Beta_v3.8.txt"))
Beta_v3.9 <- as.matrix(read.table("C:\\Users\\a0082893\\Desktop\\Beta_v3.9.txt"))

Beta_v3.0 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Beta_v3.0.txt"))
Beta_v3.1 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Beta_v3.1.txt"))
Beta_v3.2 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Beta_v3.2.txt"))
Beta_v3.3 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Beta_v3.3.txt"))
Beta_v3.4 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Beta_v3.4.txt"))
Beta_v3.5 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Beta_v3.5.txt"))
Beta_v3.6 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Beta_v3.6.txt"))
Beta_v3.7 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Beta_v3.7.txt"))
Beta_v3.8 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Beta_v3.8.txt"))
Beta_v3.9 <- as.matrix(read.table("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/Beta_v3.9.txt"))

############
##  code  ##
############

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

(start.time <- Sys.time())
kernel_K <- get_PSI(Beta,N,S_range=1:2910) ##### ATTENTION! range is from 1 to 9939-1. 1-2910;2911-9938
(start.time)
(end.time <- Sys.time())
(difftime(end.time,start.time,units = c("auto")))
(difftime(end.time,start.time,units = c("mins")))
(difftime(end.time,start.time,units = c("secs")))
for (i in 1:3)
  beep("facebook")

# About 2 hours

####    paralellisation     ####
kernel_K_ii <- h5read("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\kernel_K_v3.4_ii.h5", "kernel_K_v3.4_ii")
kernel_K_i  <- kernel_K

kernel_K_i <- kernel_K + kernel_K_ii


#### end of parallelisation ####

kernel_K <- kernel_K + t(kernel_K)          # non-diagonal entries

sumBeta <- sum(Beta)
kernel_K <- kernel_K + diag(sumBeta,9939)  # diagonal entries

kernel_K_v3.0 <- kernel_K
kernel_K_v3.1 <- kernel_K
kernel_K_v3.2 <- kernel_K
kernel_K_v3.3 <- kernel_K
kernel_K_v3.4 <- kernel_K
kernel_K_v3.5 <- kernel_K
kernel_K_v3.6 <- kernel_K
kernel_K_v3.7 <- kernel_K
kernel_K_v3.8 <- kernel_K
kernel_K_v3.9 <- kernel_K

# v3.0
h5createFile("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kernel_K_v3.0.h5") 
attr(kernel_K_v3.0, "scale") <- "liter" 
h5write(kernel_K_v3.0, "/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kernel_K_v3.0.h5", "kernel_K_v3.0")
kernel_K_v3.0 <- h5read("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\kernel_K_v3.0.h5", "kernel_K_v3.0")
kernel_K_v3.0 <- h5read("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kernel_K_v3.0.h5", "kernel_K_v3.0")
# v3.1
h5createFile("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kernel_K_v3.1.h5") 
attr(kernel_K_v3.1, "scale") <- "liter" 
h5write(kernel_K_v3.1, "/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kernel_K_v3.1.h5", "kernel_K_v3.1")
kernel_K_v3.1 <- h5read("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\kernel_K_v3.1.h5", "kernel_K_v3.1")
kernel_K_v3.1 <- h5read("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kernel_K_v3.1.h5", "kernel_K_v3.1")
# v3.2
h5createFile("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kernel_K_v3.2.h5") 
attr(kernel_K_v3.2, "scale") <- "liter" 
h5write(kernel_K_v3.2, "/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kernel_K_v3.2.h5", "kernel_K_v3.2")
kernel_K_v3.2 <- h5read("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\kernel_K_v3.2.h5", "kernel_K_v3.2")
kernel_K_v3.2 <- h5read("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kernel_K_v3.2.h5", "kernel_K_v3.2")
# v3.3
h5createFile("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kernel_K_v3.3.h5") 
attr(kernel_K_v3.3, "scale") <- "liter" 
h5write(kernel_K_v3.3, "/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kernel_K_v3.3.h5", "kernel_K_v3.3")
kernel_K_v3.3 <- h5read("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\kernel_K_v3.3.h5", "kernel_K_v3.3")
kernel_K_v3.3 <- h5read("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kernel_K_v3.3.h5", "kernel_K_v3.3")
# v3.4
h5createFile("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\kernel_K_v3.4.h5") 
attr(kernel_K_v3.4, "scale") <- "liter" 
h5write(kernel_K_v3.4, "C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\kernel_K_v3.4.h5", "kernel_K_v3.4")
kernel_K_v3.4 <- h5read("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\kernel_K_v3.4.h5", "kernel_K_v3.4")
kernel_K_v3.4 <- h5read("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kernel_K_v3.4.h5", "kernel_K_v3.4")
# v3.5
h5createFile("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kernel_K_v3.5.h5") 
attr(kernel_K_v3.5, "scale") <- "liter" 
h5write(kernel_K_v3.5, "/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kernel_K_v3.5.h5", "kernel_K_v3.5")
kernel_K_v3.5 <- h5read("C:\\Users\\a0082893\\Desktop\\FYP\\The Dataset\\Outputs\\kernel_K_v3.5.h5", "kernel_K_v3.5")
kernel_K_v3.5 <- h5read("/Users/binKarim/Dropbox/FYP/The Dataset/Outputs/kernel_K_v3.5.h5", "kernel_K_v3.5")
