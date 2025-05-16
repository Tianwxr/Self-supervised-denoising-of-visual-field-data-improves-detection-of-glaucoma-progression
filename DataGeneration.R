# Simulations need SOME type of random noise, otherwise just run one of each
# Random starting?
#
#
# 10 years
# 20 exams
# different rates
# different starting points
# Use ages in the 376
# range of defects
# - normal
# - diffuse depression
# - scatomas
#
# Calculate MD
# Calculate VFI
# - - PSD
#
###############
# Simulation  #
# - PoPLR     #
# - MD        #
# - GRI       #
# - VFI       #
###############
#
set.seed(2019)
path = "C:/Users/84522/Downloads/Test/"


# Add Noise to DB
addnoise <- function(DB) {
  lnsd = -0.081 * DB + 3.27
  s = exp(lnsd)
  return(rnorm(n = 1, mean = DB, sd = s))
}


# creates a 4 by 54 matrix of decay_Age_Normal , but decay_Fast at defect_Area
get_rates <- function(defect_Area, decay_rate) {
  rate_Loss <- rep(decay_Age_Normal, 54)
  names(rate_Loss) <- location_Names
  
  rates <- NULL
  for (r in 1:nrow(defect_Area)) {
    rates <- rbind(rates, rate_Loss)
    curr_defect <- defect_Area[r,]
    for (i in 1:length(curr_defect)) {
      rates[r,curr_defect[i]] <-  decay_rate
    }
  }
  
  return(rates)
}

defect_Small_Random <- function() {
  defect_Small_All <- NULL
  #####################
  # Small defect
  for (choice in seq(1:4)) {
    defect_Small <- NULL
    if (choice == 1) {
      defect_Small <- c("L_28", "L_29", "L_37")  
    } else if (choice == 2) {
      defect_Small <- c("L_40", "L_41", "L_42")
    } else if (choice == 3) {
      defect_Small <- c("L_11", "L_19", "L_20") 
    } else if (choice == 4) {
      defect_Small <- c("L_23", "L_24", "L_25")
    }
    
    defect_Small_All <- rbind(defect_Small_All, defect_Small)
  }
  return(defect_Small_All)
}

defect_Medium_Random <- function() {
  defect_Medium_All <- NULL
  
  
  #####################
  # Medium defect
  for (choice in seq(1:4)) {
    defect_Medium <- NULL
    if (choice == 1) {
      defect_Medium <- c("L_5", "L_6", "L_11", "L_12", "L_13", "L_19", "L_20", "L_21")
    } else if (choice == 2) {
      defect_Medium <- c("L_28", "L_29", "L_30", "L_37", "L_38", "L_39", "L_45", "L_46")
    } else if (choice == 3) {
      defect_Medium <- c("L_7", "L_8", "L_13", "L_14", "L_15", "L_16", "L_22", "L_25")
    } else if (choice == 4) {
      defect_Medium <- c("L_31", "L_34", "L_39", "L_40", "L_41", "L_42", "L_47", "L_48")
    }
    
    defect_Medium_All <- rbind(defect_Medium_All, defect_Medium)
  }
  
  
  return(defect_Medium_All)
}


defect_Large_Random <- function() {
  defect_Large_All <- NULL
  
  for (choice in seq(1:2)) {
    defect_Large <- NULL
    
    if (choice == 1) {
      defect_Large <- c("L_28", "L_29", "L_30", "L_31", "L_37", "L_38", "L_39", "L_40", "L_41", "L_42", "L_45", "L_46", "L_47", "L_48", "L_51", "L_52")
    } else if (choice == 2) {
      defect_Large <- c("L_5", "L_6", "L_7", "L_8", "L_11", "L_12", "L_13", "L_14", "L_15", "L_16", "L_19", "L_20", "L_21", "L_22", "L_23", "L_25")
    }
    
    defect_Large_All <- rbind(defect_Large_All, defect_Large)
  }
  
  
  return(defect_Large_All)
}

####################################################################################


####################################################################################
require("visualFields")

# Set up simulation
locations <- paste("L_", seq(1,54), sep="")
locations_VF <- paste("L", seq(1,54), sep="")
DB_MAX <- 36
DB_MIN <- 0

#####################
# FU Time and Fields
fields <- 20
time <- c(seq(0,(fields/2)-0.5,0.5)) 
#####################

#####################
# Age normal start


# Age 60
# initial_Locations_Age <- age_Normal
# names(initial_Locations_Age) <- locations

initial_Glaucomatous_Focal_Loss_1 <- c(28, 27, 27, 27,
                                       28, 29, 28, 28, 29, 27,
                                       25, 29, 28, 28, 30, 28, 31, 29,
                                       22, 26, 28, 27, 29, 31, 31,  0, 26,
                                       10, 14, 27, 29, 29, 31, 31,  0, 26,
                                       13, 28, 30, 28, 29, 30, 30, 29,
                                       26, 29, 30, 30, 31, 33,
                                       28, 27, 29, 28)
names(initial_Glaucomatous_Focal_Loss_1) <- locations

initial_Glaucomatous_Focal_Loss_2 <- c(15, 18, 18, 18,
                                       16, 21, 12, 10, 12, 24,
                                       20, 19, 22, 12,  0,  4, 15, 26,
                                       16, 20, 20, 16, 16, 28, 26,  0, 28,
                                       24, 27, 29, 30, 29, 31, 28,  0, 27,
                                       25, 26, 29, 28, 29, 28, 28, 29,
                                       27, 28, 28, 27, 30, 28,
                                       28, 29, 28, 29)
names(initial_Glaucomatous_Focal_Loss_2) <- locations
#####################

#####################
# Decay Rates (dB/year)
decay_Age_Normal <- -0.1
decay_No_Change <- -0.2
decay_Slow <- -0.5
decay_Moderate <- -1
decay_Fast <- -2
decay_Diffuse_Cataract <- -0.29
#####################


#####################
# Set up simulation
simulations <- seq(1,11,1)

simulation_Number <- 372


######################################
# VF parameters
require(lubridate)
# vf91016right
tperimetry <- "sap"
talgorithm <- "sitas"
tpattern <- "p24d2"
tdate <- as.Date("2000-01-01")
month(tdate) <- month(tdate) + (time*12)
day(tdate) <- days_in_month(tdate)
ttime <- head(vfpwgRetest24d2$time, length(time))
stype <- "PWG"
sage <- 60 + time
seye <- "OD"
sbsx <- 15
sbsy <- 1
sbsy <- -1
sfp <- 0
sfn <- 0
sfl <- 0
sduration <- head(vfpwgRetest24d2$duration, length(time))
#spause <- head(vfpwgRetest24d2$pause, length(time))
######################################
####################################################################################
####################################################################################

options(warn=2)

#################
# For plotting
location_Names <- paste("L_", seq(1,54,1), sep="")
plots_Included <- c(seq(16,19,1), 
                    seq(26,31,1), 
                    seq(36,43,1),
                    #seq(46,52,1),54,
                    #seq(57,63,1),65,
                    seq(46,54,1),
                    seq(57,65,1),
                    seq(69,76,1), 
                    seq(81,86,1), 
                    seq(93,96,1))
#################

#####VIRTUAL EYE SIMULATION======================= 
#####VIRTUAL EYE SIMULATION======================= 

to_Export <- NULL

################################################################
################################################################
#simulations_To_Run <- c(5,6,7,13,14,15,17,18,19,20,21,22)
#simulations_To_Run <- c(19, 23, 24, 25, 26, 27, 28)
#simulations_To_Run <- c(22)
################
# -one cycle (372 virtual eyes) each model from 29-34
# -4 cycles(2976 virtual eyes) model n°8
# -4 cycles(2976 virtual eyes) model n°16
# -8 cycles (5952 virtual eyes) model n°1
# -8 cycles (5952 virtual eyes) model n°9
#simulations_To_Run <- c(seq(29,34,1))

Group=c(rep("Training", 8), "Validation", "Test")

#simulations_To_Run <- simulations_To_Run
################
# There are 28 different simulations coded in here
Sim=function(simulations_To_Run, simulation_Number){
  
  fin=c()

  for (simulation in simulations_To_Run) {
    
    to_Export <- NULL
    to_Export_CS <- NULL
    to_Export_CS_TD <- NULL
    to_export_CS_TD_Prob <- NULL
    
    for (m in 1:simulation_Number) {
      
      to_Export <- NULL
      to_Export_CS <- NULL
      to_Export_CS_TD <- NULL
      to_export_CS_TD_Prob <- NULL
      
      # print(paste(simulation, " : ", m, " of ", simulation_Number, sep=""))
      
      for (seye in c("OD")) {
        #####################
        # 1.	SIMULATION 1:
        # a.	START: GLAUCOMATOUS FOCAL LOSS 1
        # b.	AGE-RELATED DECLINE
        defect_Area <- NULL
        if (simulation == 1) {
          initial_Locations <- initial_Glaucomatous_Focal_Loss_1
          rate_Loss <- rep(decay_Age_Normal, 54)
          names(rate_Loss) <- location_Names
        }
        
        #####################
        # 2.	SIMULATION 2:
        # a.	START: GLAUCOMATOUS FOCAL LOSS 1
        # b.	SLOW PROGRESSION
        # c.	GLOBAL DECLINE
        if (simulation == 2) {
          initial_Locations <- initial_Glaucomatous_Focal_Loss_1
          rate_Loss <- rep(decay_Slow, 54)
          names(rate_Loss) <- location_Names
        }
        
        #####################
        # 3.	SIMULATION 3:
        # a.	START: GLAUCOMATOUS FOCAL LOSS 1
        # b.	MEDIUM PROGRESSION
        # c.	GLOBAL DECLINE
        if (simulation == 3) {
          initial_Locations <- initial_Glaucomatous_Focal_Loss_1
          rate_Loss <- rep(decay_Moderate, 54)
          names(rate_Loss) <- location_Names
        }
        
        #####################
        # 4.	SIMULATION 4:
        # a.	START: GLAUCOMATOUS FOCAL LOSS 1
        # b.	FAST PROGRESSION
        # c.	GLOBAL DECLINE
        if (simulation == 4) {
          initial_Locations <- initial_Glaucomatous_Focal_Loss_1
          rate_Loss <- rep(decay_Fast, 54)
          names(rate_Loss) <- location_Names
        }
        
        #####################
        # 5.	SIMULATION 5:
        # a.	START: GLAUCOMATOUS FOCAL LOSS 1
        # b.	SLOW PROGRESSION
        # c.	MEDIUM INFERIOR or SUPERIOR DEFECT 
        if (simulation == 5) {
          initial_Locations <- initial_Glaucomatous_Focal_Loss_1
          
          #Defect area has a faster decay
          defect_Area <- defect_Medium_Random()
     
          rate_Loss <- get_rates(defect_Area, decay_Slow)
        }
        
        #####################
        # 6.	SIMULATION 6:
        # a.	START: GLAUCOMATOUS FOCAL LOSS 1
        # b.	MEDIUM PROGRESSION
        # c.	MEDIUM INFERIOR or SUPERIOR DEFECT
        if (simulation == 6) {
          initial_Locations <- initial_Glaucomatous_Focal_Loss_1
    
          #Defect area has a faster decay
          defect_Area <- defect_Medium_Random()
   
          rate_Loss <- get_rates(defect_Area, decay_Moderate)
        }
        
        #####################
        # 7.	SIMULATION 7:
        # a.	START: GLAUCOMATOUS FOCAL LOSS 1
        # b.	FAST PROGRESSION
        # c.	MEDIUM INFERIOR or SUPERIOR DEFECT
        if (simulation == 7) {
          initial_Locations <- initial_Glaucomatous_Focal_Loss_1
       
          
          #Defect area has a faster decay
          defect_Area <- defect_Medium_Random()
          
      
          
          rate_Loss <- get_rates(defect_Area, decay_Fast)
        }
        #####################
        
        #####################
        # 8.	SIMULATION 8:
        # a.	START: GLAUCOMATOUS FOCAL LOSS 1
        # b.	CATARACT RATE
        # c.	GLOBAL DECLINE
        if (simulation == 8) {
          defect_Area <- NULL
          initial_Locations <- initial_Glaucomatous_Focal_Loss_1
          rate_Loss <- rep(decay_Diffuse_Cataract, 54)
          names(rate_Loss) <- location_Names
        }
        #####################
        
        #####################
        # 9.	SIMULATION 9:
        # a.	START: GLAUCOMATOUS FOCAL LOSS 2
        # b.	AGE-RELATED DECLINE
        if (simulation == 9) {
          defect_Area <- NULL
          initial_Locations <- initial_Glaucomatous_Focal_Loss_2
          rate_Loss <- rep(decay_Age_Normal, 54)
          names(rate_Loss) <- location_Names
        }
        #####################
        
        #####################
        # 10.	SIMULATION 10:
        # a.	START: GLAUCOMATOUS FOCAL LOSS 2
        # b.	SLOW PROGRESSION
        # c.	GLOBAL DECLINE
        if (simulation == 10) {
          initial_Locations <- initial_Glaucomatous_Focal_Loss_2
          rate_Loss <- rep(decay_Slow, 54)
          names(rate_Loss) <- location_Names
        }
        #####################
        
        #####################
        # 11.	SIMULATION 11:
        # a.	START: GLAUCOMATOUS FOCAL LOSS 2
        # b.	MEDIUM PROGRESSION
        # c.	GLOBAL DECLINE
        if (simulation == 11) {
          initial_Locations <- initial_Glaucomatous_Focal_Loss_2
          rate_Loss <- rep(decay_Moderate, 54)
          names(rate_Loss) <- location_Names
        }
        #####################
        
        #####################
        # 12.	SIMULATION 12:
        # a.	START: GLAUCOMATOUS FOCAL LOSS 2
        # b.	FAST PROGRESSION
        # c.	GLOBAL DECLINE
        if (simulation == 12) {
          initial_Locations <- initial_Glaucomatous_Focal_Loss_2
          rate_Loss <- rep(decay_Fast, 54)
          names(rate_Loss) <- location_Names
        }
        #####################
        
        #####################
        # 13.	SIMULATION 13:
        # a.	START: GLAUCOMATOUS FOCAL LOSS 2
        # b.	SLOW PROGRESSION
        # c.	MEDIUM INFERIOR or SUPERIOR DEFECT 
        if (simulation == 13) {
          initial_Locations <- initial_Glaucomatous_Focal_Loss_2
        
          
          #Defect area has a faster decay
          defect_Area <- defect_Medium_Random()
          
      
          
          rate_Loss <- get_rates(defect_Area, decay_Slow)
        }
        #####################
        
        #####################
        # 14.	SIMULATION 14:
        # a.	START: GLAUCOMATOUS FOCAL LOSS 2
        # b.	MEDIUM PROGRESSION
        # c.	MEDIUM INFERIOR or SUPERIOR DEFECT
        if (simulation == 14) {
          initial_Locations <- initial_Glaucomatous_Focal_Loss_2
  
          
          #Defect area has a faster decay
          defect_Area <- defect_Medium_Random()
          
      
          
          rate_Loss <- get_rates(defect_Area, decay_Moderate)
        }
        #####################
        
        #####################
        # 15.	SIMULATION 15:
        # a.	START: GLAUCOMATOUS FOCAL LOSS 2
        # b.	FAST PROGRESSION
        # c.	MEDIUM INFERIOR or SUPERIOR DEFECT
        if (simulation == 15) {
          initial_Locations <- initial_Glaucomatous_Focal_Loss_2
   
          
          #Defect area has a faster decay
          defect_Area <- defect_Medium_Random()
          
       
          
          rate_Loss <- get_rates(defect_Area, decay_Fast)
        }
        #####################
        
        #####################
        # 16.	SIMULATION 16:
        # a.	START: GLAUCOMATOUS FOCAL LOSS 2
        # b.	CATARACT RATE
        # c.	GLOBAL DECLINE
        if (simulation == 16) {
          defect_Area <- NULL
          initial_Locations <- initial_Glaucomatous_Focal_Loss_2
          rate_Loss <- rep(decay_Diffuse_Cataract, 54)
          names(rate_Loss) <- location_Names
        }
        #####################
        
        
        #####################
        # 17.	SIMULATION 17:
        # a.	START: GLAUCOMATOUS FOCAL LOSS 1
        # b.	SLOW PROGRESSION
        # c.	LARGE INFERIOR or SUPERIOR DEFECT 
        if (simulation == 17) {
          initial_Locations <- initial_Glaucomatous_Focal_Loss_1
  
          
          #Defect area has a faster decay
          defect_Area <- defect_Large_Random()
          
      
          
          rate_Loss <- get_rates(defect_Area, decay_Slow)
        }
        #####################
        
        #####################
        # 18.	SIMULATION 18:
        # a.	START: GLAUCOMATOUS FOCAL LOSS 1
        # b.	MEDIUM PROGRESSION
        # c.	LARGE INFERIOR or SUPERIOR DEFECT
        if (simulation == 18) {
          initial_Locations <- initial_Glaucomatous_Focal_Loss_1
     
          
          #Defect area has a faster decay
          defect_Area <- defect_Large_Random()
          
     
          
          rate_Loss <- get_rates(defect_Area, decay_Moderate)
        }
        #####################
        
        #####################
        # 19.	SIMULATION 19:
        # a.	START: GLAUCOMATOUS FOCAL LOSS 1
        # b.	FAST PROGRESSION
        # c.	LARGE INFERIOR or SUPERIOR DEFECT
        if (simulation == 19) {
          initial_Locations <- initial_Glaucomatous_Focal_Loss_1
  
          #Defect area has a faster decay
          defect_Area <- defect_Large_Random()
          
      
          rate_Loss <- get_rates(defect_Area, decay_Fast)
        }
        #####################
        
        #####################
        # 20.	SIMULATION 20:
        # a.	START: GLAUCOMATOUS FOCAL LOSS 2
        # b.	SLOW PROGRESSION
        # c.	LARGE INFERIOR or SUPERIOR DEFECT 
        if (simulation == 20) {
          initial_Locations <- initial_Glaucomatous_Focal_Loss_2
  
          
          #Defect area has a faster decay
          defect_Area <- defect_Large_Random()
          
      
          
          rate_Loss <- get_rates(defect_Area, decay_Slow)
        }
        #####################
        
        #####################
        # 21.	SIMULATION 21:
        # a.	START: GLAUCOMATOUS FOCAL LOSS 2
        # b.	MEDIUM PROGRESSION
        # c.	LARGE INFERIOR or SUPERIOR DEFECT
        if (simulation == 21) {
          initial_Locations <- initial_Glaucomatous_Focal_Loss_2
  
          
          #Defect area has a faster decay
          defect_Area <- defect_Large_Random()
          
      
          
          rate_Loss <- get_rates(defect_Area, decay_Moderate)
        }
        #####################
        
        #####################
        # 22.	SIMULATION 22:
        # a.	START: GLAUCOMATOUS FOCAL LOSS 2
        # b.	FAST PROGRESSION
        # c.	LARGE INFERIOR or SUPERIOR DEFECT
        if (simulation == 22) {
          initial_Locations <- initial_Glaucomatous_Focal_Loss_2
  
          
          #Defect area has a faster decay
          defect_Area <- defect_Large_Random()
          
  
          
          rate_Loss <- get_rates(defect_Area, decay_Fast)
        }
        #####################
        
        
        #####################
        # 23.	SIMULATION 23:
        # a.	START: GLAUCOMATOUS FOCAL LOSS 1
        # b.	SLOW PROGRESSION
        # c.	SMALL INFERIOR or SUPERIOR DEFECT 
        if (simulation == 23) {
          initial_Locations <- initial_Glaucomatous_Focal_Loss_1
  
          
          #Defect area has a faster decay
          defect_Area <- defect_Small_Random()
          
  
          
          rate_Loss <- get_rates(defect_Area, decay_Slow)
        }
        #####################
        
        #####################
        # 24.	SIMULATION 24:
        # a.	START: GLAUCOMATOUS FOCAL LOSS 1
        # b.	MEDIUM PROGRESSION
        # c.	SMALL INFERIOR or SUPERIOR DEFECT
        if (simulation == 24) {
          initial_Locations <- initial_Glaucomatous_Focal_Loss_1
  
          
          #Defect area has a faster decay
          defect_Area <- defect_Small_Random()
       
          rate_Loss <- get_rates(defect_Area, decay_Moderate)
        }
        #####################
        
        #####################
        # 25.	SIMULATION 25:
        # a.	START: GLAUCOMATOUS FOCAL LOSS 1
        # b.	FAST PROGRESSION
        # c.	SMALL INFERIOR or SUPERIOR DEFECT
        if (simulation == 25) {
          initial_Locations <- initial_Glaucomatous_Focal_Loss_1
     
          #Defect area has a faster decay
          defect_Area <- defect_Small_Random()
          
          rate_Loss <- get_rates(defect_Area, decay_Fast)
        }
        #####################
        
        #####################
        # 26.	SIMULATION 26:
        # a.	START: GLAUCOMATOUS FOCAL LOSS 2
        # b.	SLOW PROGRESSION
        # c.	SMALL INFERIOR or SUPERIOR DEFECT 
        if (simulation == 26) {
          initial_Locations <- initial_Glaucomatous_Focal_Loss_2
       
          #Defect area has a faster decay
          defect_Area <- defect_Small_Random()
       
          
          rate_Loss <- get_rates(defect_Area, decay_Slow)
        }
        #####################
        
        #####################
        # 27.	SIMULATION 27:
        # a.	START: GLAUCOMATOUS FOCAL LOSS 2
        # b.	MEDIUM PROGRESSION
        # c.	SMALL INFERIOR or SUPERIOR DEFECT
        if (simulation == 27) {
          initial_Locations <- initial_Glaucomatous_Focal_Loss_2
          
          #Defect area has a faster decay
          defect_Area <- defect_Small_Random()
      
          rate_Loss <- get_rates(defect_Area, decay_Moderate)
        }
        #####################
        
        #####################
        # 28.	SIMULATION 28:
        # a.	START: GLAUCOMATOUS FOCAL LOSS 2
        # b.	FAST PROGRESSION
        # c.	SMALL INFERIOR or SUPERIOR DEFECT
        if (simulation == 28) {
          initial_Locations <- initial_Glaucomatous_Focal_Loss_2
          #Defect area has a faster decay
          defect_Area <- defect_Small_Random()
  
          rate_Loss <- get_rates(defect_Area, decay_Fast)
        }
        #####################
        
        if (!is.matrix(rate_Loss)) {
          rate_Loss <- t(as.matrix(rate_Loss))
        }
        
        #####################
        # Go through each location
        eyes <- NULL
        for (e in 1:nrow(rate_Loss)) {
          eye <- NULL
          for (l in 1:length(initial_Locations)) {
            curr_Locations <- NULL
            curr_Locations <- rate_Loss[e,l] * time +  initial_Locations[l]
            curr_Locations[curr_Locations<DB_MIN] <- DB_MIN # Nothing under dB Min
            curr_Locations[curr_Locations>DB_MAX] <- DB_MAX # Nothing over dB Max
            eye <- cbind(eye, curr_Locations)
          }
          eyes <- rbind(eyes, eye)
        }
        
        
        colnames(eyes) <- locations_VF
        #eye_TD <- tdval(eye)#t(apply(eye,1,'-',initial_Locations_Age))
        
        ###############################
        id <- simulation * 10000 + m
        print(id)
        id <- rep(id, each=fields)
        
        #eye_Abs <- data.frame(id, tperimetry, talgorithm, tpattern, tdate, ttime, stype, sage, seye, sbsx, sbsy, sfp, sfn, sfl, sduration, spause, eye)
        eye_Abs <- data.frame(id, seye, tdate, ttime, sage, stype, sfp, sfn, sfl, sduration, eyes)
        colnames(eye_Abs) <- c(colnames(vfpwgRetest24d2))
        #eye_TD <-  tdval(eye_Abs)#data.frame(id, tperimetry, talgorithm, tpattern, tdate, ttime, stype, sage, seye, sbsx, sbsy, sfp, sfn, sfl, sduration, spause, tdval(eye_Abs))
        ###############################
        
        # vf91016right
        # eye_Abs$L1 - tdval( eye_Abs )$L1
        ###############################
        # TD
        #eye_TD <- tdval(eye_Abs)
        ###############################
        
        ###############################
        # TD Map
        #eye_TDP <- tdpmap(eye_TD)
        ###############################
        
        ###############################
        # Global Variables
        globals <- getgl(eye_Abs)
        ###############################
        
        ###############################
        # VFI
        # mvfi : visual field indes (VFI); or the probability mapped value
        # svfi : standard deviation of the VFI at each location; or the probability mapped value
        vfi <- globals$vfi
        ###############################
        
        ###############################
        # MD
        # mtdev : mean deviation (mean value of the total-deviation values; or the probability mapped value)
        # stdev : standard deviation of the total-deviation values; or the probability mapped value
        # mpdev : mean value of the pattern-deviation values; or the probability mapped value)
        md <- globals$tmd
        ###############################
        
        to_Export <- rbind(to_Export, cbind(eye_Abs[,1:10], vfi, md, eye_Abs[,11:ncol(eye_Abs)], simtype=rep(simulation,length(id))))
        fin=rbind(fin,to_Export)
        
        #####################
        
      }
     
    }
    ################################
  }
  colnames(fin)[67]="simulation"
  fin_noise=fin
  print(nrow(fin_noise))
  for (i in 1:nrow(fin_noise)) {
    for (j in c(13:37,39:46,48:66)) {
      fin_noise[i,j]=max(min(addnoise(fin_noise[i,j]),DB_MAX),DB_MIN)
    }
  }
  print(2)
  return(list(fin,fin_noise))
}


# Add p-values
value_map = data.frame(cbind(pval=c(0.005,0.010,0.020,0.050,0.950,0.980,0.990,0.995,1.000), code=c(4,3,2,1,0,0,0,0,0)))
getp = function(td) {
  for (i in 1:nrow(td)) {
    for (j in c(11:64)[-c(26,35)]) {
      td[i,j]=value_map$code[value_map$pval==td[i,j]]
    }
  }
  return(td[,11:64])
}


################################################################
################################################################


WriteTable = function(SimName,SimNumber, n = 100) {
  simulations_To_Run <- SimNumber
  Result = Sim(simulations_To_Run,n)
  FileName = paste(path,SimName, sep="")

  True=Result[[1]]
  
  # This is the true visual field dataset without noise. It will be used as the reference when evaluating the denoised datasets.
  write.table(True, paste(FileName, "True", ".csv", sep=""), col.names=TRUE, row.names=FALSE, sep=",")
  
  Noise=Result[[2]]
  
  td=gettd(Noise[,-c(11,12,67)])
  p_value=gettdp(td)
  P=cbind(Noise,getp(p_value))
  # This is the visual field dataset with noised and calculated p-values. This dataset will be processed and denoised by the autoencoders.
  write.table(P, paste(FileName, "P", ".csv", sep=""), col.names=TRUE, row.names=FALSE, sep=",")
}


################################################################
################################################################


WriteTable("DAge",c(1,9), n = 100)
WriteTable("DSlow",c(2,10), n = 100)
WriteTable("DMedium",c(3,11), n = 100)
WriteTable("DFast",c(4,12), n = 100)

WriteTable("LSlow",c(17,20), n = 50)
WriteTable("LMedium",c(18,21), n = 50)
WriteTable("LFast",c(19,22), n = 50)

WriteTable("MSlow",c(5,13), n = 25)
WriteTable("MMedium",c(6,14), n = 25)
WriteTable("MFast",c(7,15), n = 25)

WriteTable("SSlow",c(23,26), n = 25)
WriteTable("SMedium",c(24,27), n = 25)
WriteTable("SFast",c(25,28), n = 25)


################################################################
################################################################