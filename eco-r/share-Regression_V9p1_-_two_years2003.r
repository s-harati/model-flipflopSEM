# Code edited to run LR(elev^2) for 2003
# as part of the works of the socioecological model

rm(list=ls(all=TRUE))
assign("last.warning", NULL, envir = baseenv())

library(raster)
library(rgdal)
library(biomod2)
library(parallel)
library(foreach)
library(doParallel)
library(speedglm)
library(gnm)
library(speedglm)
library(ROCR)
library(ranger)
library(reghelper)

# make sure to set working directory correctly!

###################################################
# Some functions are found here.
source("MPB_functions_V15p2.r")

# Initial parameters.
pi <- 3.141592653
threshold.initial <- pnorm(-1)
one.year <- FALSE
years <- ifelse(one.year,"one_year_only","two_years")
time.step <- 1 # this simulation
year.calculation <- 2003 
w.halfsize <- switch(time.step,
                     "1"=50,
                     "2"=100,
                     "3"=150,
					 "4"=200)
###################################################

# Raster stack with predictor variables. Make lulc a separate raster.
r <- get.predictor.rasters()
lulc <- r[["lulc.mask"]]
r <- dropLayer(r,which("lulc.mask"==names(r)))

for (i in year.calculation) {
  cat(paste("Reading ",i-1,", ",i," and ",i+time.step," rasters...\n",sep=""))
  a <- read.cumkill(i,time.step,threshold.initial)
  proj4string(a) <- proj4string(r)

  # Now we compute neighborhood with "focal" function.
  cat(paste("   Calculating neighborhood with focal function...\n"))
  cat(paste("      Neighborhood at t=-1\n"))
  rr <- stack(r,focal.parallel(raster(a,1),w.halfsize,T))
  cat(paste("      Neighborhood at t=0\n"))
  rr <- stack(rr,focal.parallel(raster(a,2),w.halfsize,T))
  rr <- stack(rr,raster(a,3))
  names(rr)[dim(rr)[3]] <- "y"

  # Building a mask that must fulfill four conditions:
  # 1.- only locations that are not infested in t=1 are valid.
  # 2.- only locations that are close to already infested locations are valid.
  # 3.- only locations with appropriate lulc classes can be infested.
  # 4.- only locations where other variables do not have NA are valid.
  cat("   Creating the mask...\n")
  mask <- raster(a,2)==0
  mask <- mask & rr[["identity.2"]]>0
  mask <- mask & lulc==1
  mask <- mask & calc(rr,fun=function(x) all(!is.na(x)))

# Create the dataset for the fit.
  cat(paste("   Selecting training and test datasets...\n"))
  dat <- as.data.frame(rr)
  dat <- dat[which(as.data.frame(mask)==1),]
  
  data.train <- dat
  data.test <- dat

#  cat(paste("   Binomial fit...\n"))
#  binomial <- fit.model(data.train,data.test,type="binomial")
  cat(paste("   Binomial fit with parabollic elevation...\n"))
  binomial.parabollic.elevation <- fit.model(data.train,data.test,type="binomial.parabollic.elevation")
#  cat(paste("   Binomial fit with interactions...\n"))
#  binomial.interactions <- fit.model(data.train,data.test,type="binomial.interactions")
#  cat(paste("   Random forest...\n"))
#  rf <- fit.model(data.train,data.test,type="randomforest",n.threads=12)

 # Saving. # no need to do the saving of regression results. we use them presently.
  cat("   Saving...\n")
  save(time.step,w.halfsize,binomial.parabollic.elevation,compress=T,
       file=paste("Regression_V9p1_-_",i,"_-_time_step_",time.step,"_-_w_halfsize_",w.halfsize,"_-_",years,sep=""))
}
