library(tourr)
library(lattice)
library(ISLR)
mystery0<-read.csv('~/desktop/mystery.csv')
animate(mystery0[,c(3,7,8,9)])
pairs(mystery0)