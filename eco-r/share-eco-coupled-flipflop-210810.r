# Integrating r and java runs.
# The code is compirsed of a SETUP section and a LOOPS section.
# The LOOPS section contains 2 nested loops,
# taking account of stochasticity and timesteps

# This code is modified to run on Windows.
# It is part of the socioecological model project.
# With 1-year time-setps we simulate 2005 and
# we build on previous simulations onward.

#########
# SETUP #
#########

rm(list = ls(all = TRUE))
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

# Set your working directory correctly!

folder.inbox.java = "your path to the 0-inboxes folder/0-inboxes/java/"
folder.inbox.r    = "your path to the 0-inboxes folder/0-inboxes/r/"
folder.archive.r  = "your path to the 0-inboxes folder/0-inboxes/archive-r/"
folder.workshop.r  = "your path to the 0-inboxes folder/0-inboxes/workshop-r/"
folder.runs = "your path to where the runs should be saved/"

###################################################
# Some functions are found here.
source("MPB_functions_V15p2.r") # modified in V9p1
source("Integrator_functions.r") # check: r or R?

# Initial parameters.

threshold.initial <- pnorm(-1) # TEST&DEBUGGING
one.year <- FALSE
years <- ifelse(one.year, "one_year_only", "two_years")
time.step <- 1
#iterations <- 1 # added in V9p2
year.calculation <- 2003

## this run:
str.algorithm = "Binomial_parabollic_elevation" ; j=2# either keep this line and comment out the next, or vice versa
#str.algorithm = "Random_forest" ; j=4
str.cutoff = "Youden" # choose one of: "Quantity" , "Kappa" , "Youden"

scenario = "SuggestMoore4" # choose one of: "SuggestVonNeumann" , "SuggestMoore", "Null", "EnforceVonNeumann", "EnforceMoore" , "NoHumans" , "EnforceVonNeumann2", "EnforceMoore2" , "SuggestVonNeumann2" , "SuggestMoore2" , "EnforceVonNeumann3", "EnforceMoore3" , "SuggestVonNeumann3" , "SuggestMoore3" , "EnforceVonNeumann4", "EnforceMoore4" , "SuggestVonNeumann4" , "SuggestMoore4"
num.iters.preparation = 10 # suggested values: 0, 10, 20, 30. number of time steps given to preparing users with the social model BEFORE exposing them to the ecological model.
num.iters.timesteps = 10
if ((scenario == "SuggestVonNeumann") | (scenario == "SuggestMoore") | (scenario == "SuggestVonNeumann2") | (scenario == "SuggestMoore2") | (scenario == "Null") | (scenario == "SuggestVonNeumann3") | (scenario == "SuggestMoore3") | (scenario == "SuggestVonNeumann4") | (scenario == "SuggestMoore4")) {
  num.iters.stochastic = 50
} else {
  num.iters.stochastic = 1 # Null and Enforce scenarios # EDIT: Added Null to repeated runs
}
num.iters.all = num.iters.stochastic * (num.iters.timesteps + num.iters.preparation)

num.users = 9        # number of User agents in java
rand.cut.prop = 0.01 # if forests are to be cut randomly, this proportion of them is cut in each year
pattern.episode.end = "ep_end_" # this string is added in filenames of java messages where an episode ended before final time step
# interpretting scenarios
w.m.3x3 = matrix(1,3,3) ; w.m.3x3[2,2] = 0 # Moore
w.vn.3x3 = matrix(c(2,1,2,
                    1,0,1,
                    2,1,2),3,3) # VonNeumann
w.m.5x5 = matrix(1,5,5) ; w.m.5x5[3,3] = 0 # Moore2
w.vn.5x5 = matrix(c(2,2,1,2,2,
                    2,1,1,1,2,
                    1,1,0,1,1,
                    2,1,1,1,2,
                    2,2,1,2,2),5,5) # VonNeumann2
w.m.7x7 = matrix(1,7,7) ; w.m.7x7[4,4] = 0 # Moore3
w.vn.7x7 = matrix(c(2,2,2,1,2,2,2,
                    2,2,1,1,1,2,2,
                    2,1,1,1,1,1,2,
                    1,1,1,0,1,1,1,
                    2,1,1,1,1,1,2,
                    2,2,1,1,1,2,2,
                    2,2,2,1,2,2,2),7,7) # VonNeumann3
w.m.9x9 = matrix(1,9,9) ; w.m.9x9[5,5] = 0 # Moore4
w.vn.9x9 = matrix(c(2,2,2,2,1,2,2,2,2,
                    2,2,2,1,1,1,2,2,2,
                    2,2,1,1,1,1,1,2,2,
                    2,1,1,1,1,1,1,1,2,
                    1,1,1,1,0,1,1,1,1,
                    2,1,1,1,1,1,1,1,2,
                    2,2,1,1,1,1,1,2,2,
                    2,2,2,1,1,1,2,2,2,
                    2,2,2,2,1,2,2,2,2),9,9)
w = matrix(NA, 3, 3) ; cut.directions = 0
if ((scenario == "SuggestVonNeumann") | (scenario == "EnforceVonNeumann") | (scenario == "Null")) {
  w = w.vn.3x3 ; cut.directions = 4 # Null scenario was added in this group just to have running parameters. Note Null will run only one episode. (What is the relevance of number of Null episodes? Let's run it 50 times)
} else if ((scenario == "SuggestMoore") | (scenario == "EnforceMoore")) {
  w = w.m.3x3 ; cut.directions = 8
} else if ((scenario == "SuggestVonNeumann2") | (scenario == "EnforceVonNeumann2")) {
  w = w.vn.5x5
} else if ((scenario == "SuggestMoore2") | (scenario == "EnforceMoore2")) {
  w = w.m.5x5
} else if ((scenario == "SuggestVonNeumann3") | (scenario == "EnforceVonNeumann3")) {
  w = w.vn.7x7
} else if ((scenario == "SuggestMoore3") | (scenario == "EnforceMoore3")) {
  w = w.m.7x7
} else if ((scenario == "SuggestVonNeumann4") | (scenario == "EnforceVonNeumann4")) {
  w = w.vn.9x9
} else if ((scenario == "SuggestMoore4") | (scenario == "EnforceMoore4")) {
  w = w.m.9x9
}
# a data frame with results of the runs will be saved at this address
results.df.filename <- paste0(folder.runs,"socioeco_",scenario,"_df.txt")
results.mat.filename <- paste0(folder.runs,"socioeco_",scenario,"_mat.txt")

w.halfsize <- switch(
  time.step,
  "1" = 50,
  "2" = 100,
  "3" = 150,
  "4" = 200
)

load(
  paste(
    "Regression_V9p1_-_",
    year.calculation,
    "_-_time_step_",
    time.step,
    "_-_w_halfsize_",
    w.halfsize,
    "_-_",
    years,
    sep = ""
  )
) # modified in V9p1

# Raster stack with predictor variables. Make lulc a separate raster.
r <- get.predictor.rasters()
lulc <- r[["lulc.mask"]]
r <- dropLayer(r, which("lulc.mask" == names(r)))

# DEBUGGING : UNCOMMENT THE LINE BELOW
mask.small <-
  raster(readGDAL("Infestation_data-increment/mask_kmlps_b3.ASC", silent = T)) # added in V9p2 # small mask updated to Kamloops TSA Block 3
# DEBUGGING
########################
# TO BE REMOVED AFTER! #
#mask.small <- raster(readGDAL("Infestation_data-increment/mask_kmlps.tif",silent=T)) # added in V9p2 
########################
#mask.small = lulc     #
# DEBUGGING            #
########################

mask.small <-
  mask.small & (lulc == 1) & calc(
    r,
    fun = function(x)
      all(!is.na(x))
  )
mask.small[is.na(mask.small)] = 0
total.size = cellStats(mask.small , sum)

user.zones = divide.selection(mask.small, nr = 3, nc = 3) # ATTENTION: 9 User agents assumed, hence 3x3!
user.zones.size = numeric(length = num.users)
for (i in 1:num.users) {
  user.zones.size[i] = cellStats(user.zones== i , sum)
}
cut.request = raster(mask.small) # here we only create cut.request to the size of mask.small ; values are assigned in the loop

# results data frame
results.df = data.frame(iter_sto=integer(num.iters.all),
                        iter_timestep=integer(num.iters.all),
                        proportion_healthy=double(num.iters.all),
                        proportion_infested=double(num.iters.all),
                        proportion_cut=double(num.iters.all),
                        coop_ratio=double(num.iters.all))
results.df.rowcount = 1

#########
# LOOPS #
#########

# STOCHASTICITY LOOP
for (iter.stochastic in 1:num.iters.stochastic) {
  if ((scenario == "SuggestVonNeumann") | (scenario == "SuggestMoore") | (scenario == "SuggestVonNeumann2") | (scenario == "SuggestMoore2") | (scenario == "SuggestVonNeumann3") | (scenario == "SuggestMoore3") | (scenario == "SuggestVonNeumann4") | (scenario == "SuggestMoore4")) {
    flag.episode.end = FALSE
  } else {
    flag.episode.end = TRUE # Null and Enforce scenarios
  }
  
  folder.out <-
    paste0(folder.runs, "sto", sprintf("%02d", iter.stochastic), "/")
  dir.create(folder.out)
  
  mask.dynamic <- mask.small     # here we reset mask.dynamic
  cut.prop.increment = rand.cut.prop
  
  ### TIMESTEPS LOOP
  for (i.start in year.calculation + ((1 - num.iters.preparation):num.iters.timesteps) * time.step) {
    final <- i.start + time.step
    
    # Here we send a message to java
    # with score of health of ecosystem and
    # with costs of suggested actions in each subdivision
    # these are based on the year i.start-2*time.step # verified: correct:
    # in 2005 G is able to see red attacks of 2003, i.e. the difference between 2003 and 2002
    
    # Update: Before exposing Users to the ecological situation, we prepare them by interactions 
    # in the social model only, hence the variable num.iters.preparation
    # During the preparation phase, only blank messages are sent to java,
    # and no ecological change occurs.
    # Therefore large parts of the code are enclosed in if-blocks.
    if (i.start <= year.calculation) {
      flag.preparation = TRUE
      i.prep = num.iters.preparation - year.calculation + i.start # 210811
    } else {
      flag.preparation = FALSE
    }
    
    if (flag.preparation) {
      # bypassing all the calculations of the message to java
      outbound.costs = rep(0, num.users)
      outbound.score = 1
    } else {
      # all the calculations of the message to java
      image.visible.cumulative = find.map(i.start - 2 * time.step , year.calculation , time.step , folder.out , years , str.algorithm , str.cutoff) &
        mask.dynamic
      if (time.step==1) {
        image.visible.cumulative1 = find.map(i.start - 2 * time.step - 1 , year.calculation , time.step , folder.out , years , str.algorithm , str.cutoff) &
          mask.dynamic
      } else {
        image.visible.cumulative1 = find.map(i.start - 2 * time.step - 1 , year.calculation - 1 , time.step , folder.out , years , str.algorithm , str.cutoff) &
          mask.dynamic
      }
      image.visible.increment = image.visible.cumulative - image.visible.cumulative1
      
      # score is based on cumulative image AND incremental image
      # it is ideal that the healthy proportion of the image be large
      # and the newly attacked proportion of the image be small
      cumulative.size = cellStats(image.visible.cumulative , sum)
      increment.size = cellStats(image.visible.increment , sum)
      healthy.forest.size = cellStats((mask.dynamic > image.visible.cumulative)*1 , sum)
      outbound.score = healthy.forest.size * (total.size - increment.size) / (total.size * total.size)
      
      # costs are based on incremental image
      # cut.request = focal(
      #   x = image.visible.increment ,
      #   w = w ,
      #   fun = max ,
      #   pad = TRUE ,
      #   padValue = 0
      # )
      values(cut.request) = 0
      index.red = which(values(image.visible.increment)==1)
      if (!(scenario %in% c("Null","NoHumans"))) { # if there is any harvesting
        index.cut.request = adjacent(x=image.visible.increment,
                                     cells=index.red,
                                     directions=w,#cut.directions,
                                     pairs=FALSE,include=TRUE)
        cut.request[index.cut.request] = 1
      }
      cut.zones = cut.request * user.zones * mask.dynamic
      
      unit.cost = 0.01 # 210810
      outbound.costs = numeric(length = num.users)
      for (i in 1:num.users) {
        ## cost is quantity*unit.cost . 
        # 210806 trying a new calculation:
        outbound.costs[i] = unit.cost * sum(values(cut.zones) == i) / user.zones.size[i]
      }
      # Debugging
      cat(paste("cut.prop.increment:",cut.prop.increment,", unit.cost:",unit.cost,"\n")) 
      #
    } # end if (flag.preparation) # the 'else' part
    # Alright, however the message to java was calculated, it will be sent from here...
    
    # do this only if flag.episode.end is FALSE
    # send message to java only if java is still running the current episode
    if (!flag.episode.end) {
      # next, the results are written in R's workshop
      # distinguishing outbound filenames for preparation vs. simulation 
      if (flag.preparation) {
        outbound.filename = paste0("sto",iter.stochastic,
                                   "prep",i.prep,".txt")
      } else {
        outbound.filename = paste0("sto",iter.stochastic,
                                   "year",i.start,".txt")
      }
      cat(outbound.costs,
          file=paste0(folder.workshop.r,
                      outbound.filename))
      cat("\n",
          file=paste0(folder.workshop.r,
                      outbound.filename),append=TRUE)
      cat(outbound.score,
          file=paste0(folder.workshop.r,
                      outbound.filename),append=TRUE)
      # cat(
      #   cat(outbound.costs),
      #   "\n",
      #   outbound.score,
      #   sep="",
      #   file = paste0(
      #     folder.inbox.java,
      #     "sto",
      #     iter.stochastic,
      #     "year",
      #     i.start,
      #     ".txt"
      #   )
      # )
      
      # next, the results are moved to java's inbox
      # use copy+remove instead of rename...
      file.copy(from=paste0(folder.workshop.r,
                            outbound.filename) ,
                to=paste0(folder.inbox.java,
                          outbound.filename))
      flag.move = TRUE    # we make sure that the workshop is emptied 
      while(flag.move) {  # before proceeding to the rest of the iteration
        file.remove(paste0(folder.workshop.r,
                           outbound.filename))
        flag.move = file.exists(paste0(folder.workshop.r,
                                       outbound.filename))
      }
    } # end if (!flag.episode.end)
    # message sending complete.
    
    
    
    # the next part, prediction, is not for the preparation phase, hence the if block
    if (!flag.preparation) {
      cat(paste("Prediction ", i.start + time.step, "...\n", sep = ""))
      
      #a <- read.cumkill(i.start,time.step,threshold.initial)
      #a <- dropLayer(a,3)
      # replacing function read.cumkill() with draft code:
      
      #x.obs <- "Infestation_data-cumulative/cumkill"
      #a <- f(raster(readGDAL(paste(x.obs,i.start - 1,".asc",sep=""),silent=TRUE)),th=threshold.initial)
      #a <- stack(a,f(raster(readGDAL(paste(x.obs,i.start,".asc",sep=""),silent=TRUE)),th=threshold.initial))
      
      # V9p2:
      if (time.step==1) {
        a = find.map(i.start - 1 , year.calculation , time.step , folder.out , years , str.algorithm , str.cutoff)
      } else {
        a = find.map(i.start - 1 , year.calculation - 1 , time.step , folder.out , years , str.algorithm , str.cutoff)
      }
      a = stack(a, find.map(i.start , year.calculation , time.step , folder.out , years , str.algorithm , str.cutoff))	# V9p2
      
      #a[is.na(mask.dynamic)] = NA
      a = a & mask.dynamic
      
      names(a) <- c("i-1", "i")
      # end of replacing function read.cumkill()
      if (one.year)
        a <- dropLayer(a, 1)
      proj4string(a) <- proj4string(r)
      
      # Now we compute neighborhood with "focal" function.
      cat(paste("Calculating neighborhood with focal function...\n"))
      #rr <- stack(r,focal.parallel(raster(a,1),w.halfsize,T))
      rr <- stack(r, focal.selection(raster(a, 1), mask.small))
      #if (!one.year) rr <- stack(rr,focal.parallel(raster(a,2),w.halfsize,T))
      if (!one.year)
        rr <- stack(rr, focal.selection(raster(a, 2), mask.small))
      
      # Building a mask that must fulfill four conditions:
      # 1.- only locations that are not infested in t=1 are valid.
      # 2.- only locations that are close to already infested locations are valid.
      # 3.- only locations with appropriate lulc classes can be infested.
      # 4.- only locations where other variables do not have NA are valid.
      cat("   Creating the mask...\n")
      # mask <- if (one.year)
      #   a == 0
      # else
      #   raster(a, 2) == 0
      if (one.year) {
        mask = (a==0)
      } else {
        mask = (raster(a, 2) == 0)
      }
      mask <- mask & rr[["identity.2"]] > 0
      #mask <- mask & lulc == 1
      #mask <- mask & calc(			# moved outside the loop to increase speed
      #  rr,
      #  fun = function(x)
      #    all(!is.na(x))
      #)
      
      mask = mask & mask.dynamic 	# V9p2
      
      # Create the dataset for the fit.
      cat(paste("   Selecting training and test datasets...\n"))
      dat <- as.data.frame(rr)
      dat <- dat[which(as.data.frame(mask) == 1), ]
      
      #for (j in 2:2) { # loop removed. instead, j is defined at the start of the code
      # 2021-05 We are only running RF # 2021-07 only interested in LR.e2
      # note: j has been defined at the start of the code
      file.name <- switch(
        j,
        "1" = "Binomial",
        "2" = "Binomial_parabollic_elevation",
        "3" = "Binomial_all_interactions",
        "4" = "Random_forest"
      )
      file.name <-
        paste(file.name,
              "_-_year_calculation_",
              year.calculation,
              sep = "")
      cat(paste("Predictions ", file.name, "...\n", sep = ""))
      reg <- switch(
        j,
        "1" = binomial,
        "2" = binomial.parabollic.elevation,
        "3" = binomial.interactions,
        "4" = rf
      )
      # pr <-
      #   if (j == 4)
      #     predict(reg$reg, data = dat)
      # else
      #   predict(reg$reg, newdata = dat, type = "response")
      if (j == 4) {
        pr <- predict(reg$reg, data = dat)
      } else {
        pr <- predict(reg$reg, newdata = dat, type = "response")
      }
      print("Checkpoint: predictions done")
      
      # b <- if (one.year)
      #   a
      # else
      #   raster(a, 2)
      if (one.year) {
        b <- a
      } else {
        b <- raster(a, 2)
      }
      print("Checkpoint: raster b initialized.")
      # b[Which(mask == 1)] <- if (j == 4)
      #   pr$predictions
      # else
      #   pr
      # if (j == 4) {
      #   b[Which(mask == 1)] <- pr$predictions
      # } else {
      #   b[Which(mask == 1)] <- pr
      # }
      if (j == 4) {
        b[mask == 1] <- pr$predictions
      } else {
        b[mask == 1] <- pr
      }
      print("Checkpoint: predictions inserted in raster b")
      file.name <-
        paste(
          folder.out,
          file.name,
          "__start_",
          i.start,
          "_-_",
          # V9p2 using folder.out
          "final_",
          i.start + time.step,
          "_-_",
          years,
          sep = ""
        )
      youden.cutoff <- reg$youden.cutoff
      ###################################
      # DEBUGGING - TO BE REMOVED AFTER #
      #youden.cutoff = 0.25             #
      ###################################
      kappa.cutoff <- reg$kappa.cutoff
      quantity.cutoff <- reg$quantity.cutoff  # added in V9p1
      #
      if (str.cutoff == "Youden") {
        cutoff = youden.cutoff
      } else if (str.cutoff == "Kappa") {
        cutoff = kappa.cutoff
      } else if (str.cutoff == "Quantity") {
        cutoff = quantity.cutoff
      } else {
        print("Error: str.cutoff should be 'Youden', 'Kappa' or 'Quantity'. ")
        # add code to stop execution and exit the program.
      }
      #
      b <- (b > cutoff) * 1    # modified in Integrator
      print("Checkpoint: cutoff image produced.")
    } # end if(!flag.preparation)
    # Next, the part where R receives and archives the message from java
    # should run reardless of flag.preparation
    # No if-blocks here
    
    # before writing the new image on disk,
    # places of cuts are identified and superimposed on mpb infestations.
    # the new image to write on disk is the result of superposition.
    # code waits for input to arrive in R's inbox.
    #inbox.list = list.files(folder.inbox.r , full.names = FALSE)
    #inbox.list.full = list.files(folder.inbox.r , full.names = TRUE)
    
    # if the episode is already ended in the social model 
    # then there is no java message to read. in that case 
    # the ecological model should continue running through the remaining timesteps of the current episode
    if (flag.episode.end) {
      if (scenario %in% c("Null","NoHumans")) {
        user.decisions = rep(0 , num.users)
      } else if (scenario %in% c("EnforceVonNeumann", "EnforceMoore", "EnforceVonNeumann2", "EnforceMoore2", "EnforceVonNeumann3", "EnforceMoore3", "EnforceVonNeumann4", "EnforceMoore4")) {
        user.decisions = rep(1 , num.users)
      }
    } else { # read message from java only if java is still running the current episode
      inbox.length = 0 #length(inbox.list) # if the inbox is cleared then this should be zero
      flag = TRUE
      while (flag) {
        check.list.full = list.files(folder.inbox.r , full.names = TRUE)
        check.length = length(check.list.full)
        if (check.length != inbox.length) {
          flag = FALSE
        }
        # debugging: this part gives file.remove() time, if needed, to clean up
        if (check.length > 1) {
          cat(paste("***\n","inbox not cleared.",Sys.time(),iter.stochastic,i.start,"\n"),
              file=paste0(folder.runs,"errorlog.txt"),
              append=TRUE)
          flag = TRUE
        }
        # debugging: this part makes sure the file exists
        if ((check.length == 1) & (!file.exists(check.list.full[1]))) {
          cat(paste("file not ready.",Sys.time(),iter.stochastic,i.start,"\n"),
              file=paste0(folder.runs,"errorlog.txt"),
              append=TRUE)
          flag = TRUE
        }
      }
      print("Checkpoint: flip-flop mechanism activated.")
      check.list = list.files(folder.inbox.r , full.names = FALSE)
      #in.filename      = setdiff(check.list      , inbox.list)
      #in.filename.full = setdiff(check.list.full , inbox.list.full)
      in.filename      = check.list[1]
      in.filename.full = check.list.full[1] # reminder: inbox is cleaned in each iteration
      print(cat("Checkpoint: in.filename.full found at ",in.filename.full))
      
      # that input is decisions of U's from java
      user.decisions = scan(file = in.filename.full)
      print(cat("Checkpoint: decisions read ",user.decisions))
      
      # cleaning and archiving # use copy+remove instead of rename...
      file.copy(from = in.filename.full ,
                to = paste0(folder.archive.r , in.filename))
      flag.move = TRUE    # we make sure that the inbox is emptied 
      while(flag.move) {  # before proceeding to the next iteration
        file.remove(in.filename.full)
        flag.move = file.exists(in.filename.full)
      }
      print("Checkpoint: received message moved to folder.archive.r")
      
      # check if the social model's episode ended here
      flag.episode.end = grepl(pattern.episode.end , in.filename) # return TRUE if the pattern is in the filename, FALSE otherwise
      # this information will be used in the next iteration
      
    } # end if (flag.episode.end)
    
    
    # now we select pixels to cut trees, and in so doing we update mask.dynamic
    # we do this for scenarios where there are humans to cut trees
    # this part is not done in the preparation phase, hence the first if-block
    if (!flag.preparation) {
      if (scenario != "NoHumans") {
        cut.size.increment = 0 # this variable is used in the next iteration for calculating unit cost and unit price
        for (i in 1:length(user.decisions)) {
          mask.zone = mask.dynamic * (user.zones == i)
          if (user.decisions[i] == 0) {
            # condition for random cuts
            zone.ind = which(values(mask.zone) == 1)
            cut.ind = sample(zone.ind , ceiling(rand.cut.prop * length(zone.ind)) , replace = FALSE)
          } else {
            # cutting based on scenarios
            cut.zone = cut.request * mask.zone
            cut.ind = which(values(cut.zone) == 1)
          }
          mask.dynamic[cut.ind] = 0 # removing pixels from forest
          cut.size.increment = cut.size.increment + length(cut.ind)
        }
        cut.prop.increment = cut.size.increment / total.size
        print("Checkpoint: mask.dynamic updated.")
      } # end if (scenario != "NoHumans")     
      #  writeRaster(b,
      #              filename=paste(file.name,"_-_probability_map_year_",".tif",sep=""),
      #              format="GTiff",overwrite=T)
      #  writeRaster((b>youden.cutoff)*1,
      #              filename=paste(file.name,"_-_TSS_cutoff_-_",".tif",sep=""),
      #              format="GTiff",overwrite=T)
      #  writeRaster((b>kappa.cutoff)*1,
      #              filename=paste(file.name,"_-_Kappa_cutoff_-_",".tif",sep=""),
      #              format="GTiff",overwrite=T)
      #writeRaster((b > quantity.cutoff) * 1,
      #            filename = paste(file.name, "_-_Quantity_cutoff_-_", ".tif", sep =
      #                               ""),
      #            format = "GTiff",
      #            overwrite = T
      #)	# this section edited in V9p1. Only Quantity_cutoff is recorded
      
      # superimposition and writing on disk
      writeRaster((b & mask.dynamic) * 1,
                  filename = paste(file.name, "_-_",str.cutoff,"_cutoff_-_", ".tif", sep =""),
                  format = "GTiff",
                  overwrite = T
      )
    } # end if (!flag.preparation)
    
    results.df[results.df.rowcount , "iter_sto"] = iter.stochastic
    if (flag.preparation) {
      results.df[results.df.rowcount , "iter_timestep"] = i.prep
    } else {
      results.df[results.df.rowcount , "iter_timestep"] = i.start
      results.df[results.df.rowcount , "proportion_healthy"] = 1.0 * cellStats((!b) & mask.dynamic , sum) / total.size
      results.df[results.df.rowcount , "proportion_infested"] = 1.0 * cellStats(b & mask.dynamic , sum) / total.size
      results.df[results.df.rowcount , "proportion_cut"] = 1.0 - (results.df[results.df.rowcount , "proportion_healthy"] + results.df[results.df.rowcount , "proportion_infested"])
    }
    results.df[results.df.rowcount , "coop_ratio"] = 1.0 * sum(user.decisions) / num.users
    
    results.df.rowcount = results.df.rowcount + 1
    #} # end for j # loop removed.
    # TEST&DEBUGGING
    if (!flag.preparation) {
      print(cellStats(a,sum))
    }
  } # end for i.start
  #} # end for iter.learning
} # end for iter.stochastic

# saving results data frame
write.table(results.df,file=results.df.filename,row.names=FALSE,sep=" ")

# checking
results.mat=matrix(NA , 
                   nrow=num.iters.stochastic , 
                   ncol=num.iters.timesteps)
for (temp.r in 1:nrow(results.mat)) {
  for (temp.c in 1:ncol(results.mat)) {
    results.mat[temp.r,temp.c] = 
      results.df[(10*(temp.r - 1) + temp.c),"coop_ratio"]
  }
}
# saving results matrix
write.table(results.mat,file=results.mat.filename,row.names=FALSE,col.names=FALSE,sep=" ")

if (!flag.preparation) {
  for (f in list.files(folder.out,full.names = TRUE)) {print(cellStats(raster(f),sum))}
}
# visually compare this results.mat with java output (rewards) file
# to check that in each episode, the time.step of the first reward is the same in the two tables
#quit(save = "no")
