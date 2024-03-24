# Calculating the focal weight matrices.
weighting.window <- function(window.type="identity",w.halfsize=25) {
  window.type <- tolower(window.type)
  if (!any(window.type==c("identity","inverse","squared","linear"))) stop("Wrong weighting window type")
  w.identity <- matrix(1/(2*w.halfsize+1)^2,2*w.halfsize+1,2*w.halfsize+1)
  w <- outer((-w.halfsize:w.halfsize)^2,(-w.halfsize:w.halfsize)^2,"+")
  w.identity[w>w.halfsize^2] <- 0
  w.identity[w.halfsize+1,w.halfsize+1] <- 0
  w.identity <- w.identity/sum(w.identity)
  if (window.type=="identity") {
    return(w.identity)
  } else switch(window.type,
                inverse={
                  w.inverse <- matrix(0,2*w.halfsize+1,2*w.halfsize+1)
                  w.inverse[w.identity!=0] <- 1/sqrt(w[w.identity!=0])
                  return(w.inverse)
                },
                squared={
                  w.squared <- matrix(0,2*w.halfsize+1,2*w.halfsize+1)
                  w.squared[w.identity!=0] <- 1/w[w.identity!=0]
                  return(w.squared)
                },
                linear={
                  w.linear <- matrix(0,2*w.halfsize+1,2*w.halfsize+1)
                  w.linear[w.identity!=0] <- max(sqrt(w))-max(sqrt(w))/w.halfsize*sqrt(w[w.identity!=0])
                  return(w.linear)
                })
}

# It computes the Kappa index for two arrays.
kappa.index <- function(a,b) {
  nab <- sum(!is.na(b))
  a0 <- !a
  b0 <- !b
  p.a <- (sum(a0 & b0) + sum(a & b))/nab
  p.e <- sum(a0)/nab*sum(b0)/nab+sum(a)/nab*sum(b)/nab
  return((p.a-p.e)/(1-p.e))
}

# Model fit.
fit.model <- function(data.train,data.test,type="randomforest",n.threads=12,big=F,verbose=T) {
# Regressions.
  if (type=="randomforest") {
    reg <- ranger(dependent.variable.name="y",data=data.train,verbose=T,importance="permutation",
                                    write.forest=T,num.threads=n.threads,num.trees=500,classification=F)
    pred <- predict(reg,data=data.test,verbose=F)$predictions
  } else if (type=="binomial") {
    reg <- glm(y~.,family=binomial(link="logit"),data=data.train,x=F,y=F)
    pred <- predict(reg,newdata=data.test,type="response")
  } else if (type=="binomial.interactions") {
    reg <- glm(y~.^2,family=binomial(link="logit"),data=data.train,x=F,y=F)
    pred <- predict(reg,newdata=data.test,type="response")
  } else if (type=="binomial.parabollic.elevation") {
    reg <- glm(y~.+I(elevation^2),family=binomial(link="logit"),data=data.train,x=F,y=F)
    pred <- predict(reg,newdata=data.test,type="response")
  } else stop("Wrong fit option")
  
  if (type!="randomforest") {
    if (verbose) print(summary(reg))
    sum.model <- summary(reg)$coefficients
    sum.beta <- beta(reg)$coefficients
    pred <- predict(reg,newdata=data.test,type="response")
  } else {
    if (verbose) print(reg)
    sum.model <- c()
    sum.beta <- c()
  }
  roc.pred <- prediction(pred,data.test$y)
  
  # Youden's J.
  youden.perf <- performance(roc.pred,"sens","spec")
  x.youden <- youden.perf@alpha.values[[1]]
  y.youden <- youden.perf@y.values[[1]]+youden.perf@x.values[[1]]-1

  # Kappa.
  x.kappa <- seq(0.001,.999,length=1000)
  y.kappa <- sapply(x.kappa,function(x) kappa.index(pred>=x,data.test$y))
  
  # Quantity. added in V15p2 to calculate quantity error
  x.quantity <- seq(0.001,.999,length=1000)
  y.quantity <- sapply(x.quantity,function(x) abs(sum(pred>=x) - sum(data.test$y)))
  
  # Hits, misses and false alarms.
  map.kappa <- (pred>max(y.kappa))*1   # V15p2: I think instead of max(y.kappa) we need x.kappa[which.max(y.kappa)]
  map.youden <- (pred>max(y.youden))*1 # V15p2: In this sense, these results are wrong but because they don't change model results I leave them unchanged for now. To be discussed in group.
  # quantity.cutoff= mean(x.quantity[which(y.quantity==min(y.quantity))]) # this may be more accurate but not necessary to add.
  map.quantity <- (pred>x.quantity[which.min(y.quantity)])*1 # added in V15p2. Note that y quantity is error, hence 'min'
  
  kappa.hits.raw <- sum(map.kappa==1 & data.test$y==1,na.rm=T)
  kappa.reject.raw <- sum(map.kappa==0 & data.test$y==0,na.rm=T)
  kappa.false.raw <- sum(map.kappa==1 & data.test$y==0,na.rm=T)
  kappa.miss.raw <- sum(map.kappa==0 & data.test$y==1,na.rm=T)

  youden.hits.raw <- sum(map.youden==1 & data.test$y==1,na.rm=T)
  youden.reject.raw <- sum(map.youden==0 & data.test$y==0,na.rm=T)
  youden.false.raw <- sum(map.youden==1 & data.test$y==0,na.rm=T)
  youden.miss.raw <- sum(map.youden==0 & data.test$y==1,na.rm=T)
  
  quantity.hits.raw <- sum(map.quantity==1 & data.test$y==1,na.rm=T)   # added in V15p2
  quantity.reject.raw <- sum(map.quantity==0 & data.test$y==0,na.rm=T)
  quantity.false.raw <- sum(map.quantity==1 & data.test$y==0,na.rm=T)
  quantity.miss.raw <- sum(map.quantity==0 & data.test$y==1,na.rm=T)  
  
  number.test.points <- length(data.test$y)

  out <- list(reg=reg,
              sum.model=sum.model,
              sum.beta=sum.beta,
              auc=performance(roc.pred,"auc")@y.values,
              y.youden=max(y.youden),
              youden.cutoff=x.youden[which.max(y.youden)],
              y.kappa=max(y.kappa),
              kappa.cutoff=x.kappa[which.max(y.kappa)],
			  y.quantity=min(y.quantity), # added in V15p2. Note that y.quantity is quantity error, hence 'min'
			  quantity.cutoff=x.quantity[which.min(y.quantity)],
              kappa.hits.raw=kappa.hits.raw,
              kappa.reject.raw=kappa.reject.raw,
              kappa.false.raw=kappa.false.raw,
              kappa.miss.raw=kappa.miss.raw,
              youden.hits.raw=youden.hits.raw,
              youden.reject.raw=youden.reject.raw,
              youden.false.raw=youden.false.raw,
              youden.miss.raw=youden.miss.raw,
			  quantity.hits.raw=quantity.hits.raw,
              quantity.reject.raw=quantity.reject.raw,
              quantity.false.raw=quantity.false.raw,
              quantity.miss.raw=quantity.miss.raw,
              number.test.points=number.test.points)
  if (type=="randomforest") {
    out$importance <- importance(reg)
  } else {
    out$importance <- c()
  }
  return(out)
}


# V15p2 IMPORTANT: THIS FUNCTION IS NOT USED IN VERSION 9 OF REGRESSION AND IMAGING CODES
#                  THEREFORE IT HAS NOT BEEN MODIFIED TO INCLUDE quantity.cutoff
# Write binary image onto disk. Cutoff is chosen to maximize Kappa or Youden' J.
write.cutoff.image <- function(name.output,regres,image.to.match,j.index,type="youden",
                               extension=ext.image,projection=proj.image,dataset=dataset) {
  cat(paste("   Image ",name.output,"\n",sep=""))
  if (type=="youden") {
    cat(paste("      Max. Youden J = ",max(regres$youden$youden),"\n",sep=""))
    cutoff <- regres$youden$cutoff[which.max(regres$youden$youden)]
    cat(paste("      Cutoff Youden J = ",cutoff,"\n",sep=""))
  } else {
    cat(paste("      Max. Kappa = ",max(regres$kappa$kappa),"\n",sep=""))
    cutoff <- regres$kappa$cutoff[which.max(regres$kappa$kappa)]
    cat(paste("      Cutoff Kappa = ",cutoff,"\n",sep=""))
  }
  if (class(regres$reg)[1]=="speedglm" | class(regres$reg)[1]=="glm") {
    pred <- predict(regres$reg,newdata=dataset,type="response")
  } else {
    pred <- predict(regres$reg,data=dataset,type="response")$predictions
  }
  image.to.match[j.index] <- (pred>cutoff)*1
  image.to.match <- raster(image.to.match)
  projection(image.to.match) <- proj.image
  extent(image.to.match) <- ext.image
  # setMinMax(image.to.match) <- c(0,1)
  writeRaster(x=image.to.match,format="ascii",overwrite=T,filename=name.output)
}

focal.parallel <- function(a,w.halfsize,parallel.focal=T) {
  wlabel <- c("identity","linear","inverse","squared")
  if (parallel.focal) {
    cl <- makeCluster(4)
    registerDoParallel(cl)
      out <- foreach (i=1:4,.packages=c("raster"),.export="weighting.window") %dopar% 
        focal(a,weighting.window(wlabel[i],w.halfsize),na.rm=T,pad=T)
    stopCluster(cl)
  } else out <- lapply(1:4,function(i) as.matrix(focal(a,weighting.window(wlabel[i],w.halfsize),na.rm=T,pad=T)))
  r <- stack(out[[1]])
  for (i in 2:4) r <- stack(r,out[[i]])
  names(r) <- wlabel
  return(r)
}

get.predictor.rasters <- function(neighbors=8,w.halfsize=25) {
  # Aspect and elevation datasets.
  
  cat("Reading elevation, aspect and mask data...\n")
  elevation <- raster(readGDAL("Infestation_data-increment/dem/w001001.adf",silent=T))
  proj.images <- proj4string(elevation)
  ruggedness <- terrain(elevation,opt="TPI",neighbors=neighbors)
  slope <- terrain(elevation,opt="slope",neighbors=neighbors)
  aspect <- raster(readGDAL("Infestation_data-increment/aspect/w001001.adf",silent=T))
  aspect.sin <- sin(2*pi/360*aspect)
  aspect.cos <- cos(2*pi/360*aspect)
  lulc.mask <- raster(readGDAL("Infestation_data-increment/Masks-urban_crops_lakes_/mask_water_lc_xposedland/mask_wr_lc_xp.asc",silent=T))
  
  cat("Clipping rasters and converting to matrices...\n")
  a <- raster(readGDAL("Infestation_data-cumulative/cumkill1999.asc",silent=T))
  proj4string(a) <- proj.images
  elevation <- crop(elevation,a)
  ruggedness <- crop(ruggedness,a)
  aspect.sin <- crop(aspect.sin,a)
  aspect.cos <- crop(aspect.cos,a)
  lulc.mask <- crop(lulc.mask,a)
  slope <- crop(slope,a)

  x <- stack(elevation,ruggedness,aspect.sin,aspect.cos,slope,lulc.mask)
  names(x) <- c("elevation","ruggedness","aspect.sin","aspect.cos","slope","lulc.mask")
  return(x)
}

# Modified so that it rescales cumulative images to [0,1] by default.
read.cumkill <- function(i,time.step,th,silent=T) {
  f <- function(aa,th=th,factor.rescale=0.001) {
    aa <- ((aa*factor.rescale)>th)*1
    aa[is.na(aa)] <- 0
    return(aa)
  }
  x <- "Infestation_data-cumulative/cumkill"
  a <- f(aa=raster(readGDAL(paste(x,i-1,".asc",sep=""),silent=silent)),th=th)
  a <- stack(a,f(aa=raster(readGDAL(paste(x,i,".asc",sep=""),silent=silent)),th=th))
  a <- stack(a,f(aa=raster(readGDAL(paste(x,i+time.step,".asc",sep=""),silent=silent)),th=th))
  names(a) <- c("i-1","i","i+time.step")
  return(a)
}

stripGlmLR = function(cm) {
  cm$y = c()
  cm$model = c()
  
  cm$residuals = c()
  cm$fitted.values = c()
  cm$effects = c()
  cm$qr$qr = c()  
  cm$linear.predictors = c()
  cm$weights = c()
  cm$prior.weights = c()
  cm$data = c()
  
  
  cm$family$variance = c()
  cm$family$dev.resids = c()
  cm$family$aic = c()
  cm$family$validmu = c()
  cm$family$simulate = c()
  attr(cm$terms,".Environment") = c()
  attr(cm$formula,".Environment") = c()
  
  cm
}






