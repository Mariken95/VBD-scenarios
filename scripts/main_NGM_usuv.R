# CALCULATE R0 FROM NGM


# HPC ------------------------------------------------------------------
rm(list=ls())

# test 
print('is something happening?')

# load libraries -

.libPaths( c("/home/mdwit/R/library" , .libPaths() ) )

library(tidyverse,lib="/home/mdwit/R/library")
library(data.table,lib="/home/mdwit/R/library")
library(zoo,lib="/home/mdwit/R/library")
library(optparse,lib="/home/mdwit/R/library")

source("functions_NGM.R") 

# set-up ----------------------------------------------------------

# Define the command-line arguments
option_list <- list(
  make_option(c("-s", "--scenario"), type = "character", default = NULL,
              help = "scenario definition", metavar = "character"),
  make_option(c("-r", "--run"), type = "character", default = NULL,
              help = "index no of run", metavar = "character")
)

# Parse the command-line arguments
opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

# Check if scenario was provided
if (is.null(opt$scenario)) {
  stop("Scenario must be specified")
}

# Check if run index was provided
if (is.null(opt$run)) {
  stop("Run index must be specified")
}

print(opt$scenario)
print(opt$run)


# Create output folder
folder_path <- paste0("../Output/", opt$scenario, sep="")
# Check if the folder path exists
if (!dir.exists(folder_path)) {
  # Folder does not exist, create the folder
  if (dir.create(folder_path, recursive = TRUE)) {
   cat("Folder created successfully.\n")
  } else {
   cat("Error: Unable to create folder.\n")
  }
} else {
  cat("Folder already exists.\n")
}



# Draw sample from posterior ----------------------------------------------
load("../Data/model_input/posteriorD.Rda")

# sample from posterior
posteriorSamples <- as.data.frame(posterior.allchains[as.numeric(opt$run)*5,]) # specify samples based on run so that the same runs across scenarios use the same parameters

# or use mean estimate
posteriorSamples <- as.data.frame(t(colMeans(posterior.allchains[,c(3:9)])))

# Load input data ------------------------------------------------------------

load("../Data/model_input/fixedParamsUsutu.RData")

scenario <- opt$scenario

# scenarios A only have the future bird distributions
# scenarios B only have the future temperature and mosquito distributions

# scenario <- "ref"

# scenario <- "SSP1-A"
# scenario <- "SSP1-B"
# scenario <- "SSP1-full"

# scenario <- "SSP3-A"
scenario <- "SSP3-B"
# scenario <- "SSP3-full"

# scenario <- "SSP4-A"
# scenario <- "SSP4-B"
# scenario <- "SSP4-full"

# scenario <- "SSP5-A"
# scenario <- "SSP5-B"
# scenario <- "SSP5-full"
{ # runs full dataprep and NGM
  
if (scenario == "ref") {
  load("../Data/model_input/ref/mortality.RData")
  load("../Data/model_input/ref/EIP.RData")
  load("../Data/model_input/ref/bitingRate.RData")
  load("../Data/model_input/ref/diapause.RData")
  
  load("../Data/model_input/ref/blackbirdAdu_5km.RData")
  load("../Data/model_input/ref/blackbirdJuv_5km.RData")
  load("../Data/model_input/ref/culex.diapause_5km.RData")
  
  reservoirSc <- "ref"

# * ssp1 scenarios --------------------------------------------------------

} else if (scenario == "SSP1-A") {
  load("../Data/model_input/ref/mortality.RData")
  load("../Data/model_input/ref/EIP.RData")
  load("../Data/model_input/ref/bitingRate.RData")
  load("../Data/model_input/ref/diapause.RData")
  
  load("../Data/model_input/ssp1/blackbirdAdu_5km.RData")
  load("../Data/model_input/ssp1/blackbirdJuv_5km.RData")
  load("../Data/model_input/ref/culex.diapause_5km.RData")
  
  reservoirSc <- "ssp1"
  
} else if (scenario == "SSP1-B") {
  load("../Data/model_input/ssp1/mortality.RData")
  load("../Data/model_input/ssp1/EIP.RData")
  load("../Data/model_input/ssp1/bitingRate.RData")
  load("../Data/model_input/ssp1/diapause.RData")
  
  load("../Data/model_input/ref/blackbirdAdu_5km.RData")
  load("../Data/model_input/ref/blackbirdJuv_5km.RData")
  load("../Data/model_input/ssp1/culex.diapause_5km.RData")
  
  reservoirSc <- "ref"
  
} else if (scenario == "SSP1-full") {
  load("../Data/model_input/ssp1/mortality.RData")
  load("../Data/model_input/ssp1/EIP.RData")
  load("../Data/model_input/ssp1/bitingRate.RData")
  load("../Data/model_input/ssp1/diapause.RData")
  
  load("../Data/model_input/ssp1/blackbirdAdu_5km.RData")
  load("../Data/model_input/ssp1/blackbirdJuv_5km.RData")
  load("../Data/model_input/ssp1/culex.diapause_5km.RData")
  
  reservoirSc <- "ssp1"
  

# * ssp 3 scenarios ---------------------------------------------------------
} else if (scenario == "SSP3-A") {
  load("../Data/model_input/ref/mortality.RData")
  load("../Data/model_input/ref/EIP.RData")
  load("../Data/model_input/ref/bitingRate.RData")
  load("../Data/model_input/ref/diapause.RData")
  
  load("../Data/model_input/ssp3/blackbirdAdu_5km.RData")
  load("../Data/model_input/ssp3/blackbirdJuv_5km.RData")
  load("../Data/model_input/ref/culex.diapause_5km.RData")
  
  reservoirSc <- "ssp3"
  
} else if (scenario == "SSP3-B") {
  load("../Data/model_input/ssp3/mortality.RData")
  load("../Data/model_input/ssp3/EIP.RData")
  load("../Data/model_input/ssp3/bitingRate.RData")
  load("../Data/model_input/ssp3/diapause.RData")
  
  load("../Data/model_input/ref/blackbirdAdu_5km.RData")
  load("../Data/model_input/ref/blackbirdJuv_5km.RData")
  load("../Data/model_input/ssp3/culex.diapause_5km.RData")
  
  reservoirSc <- "ref"
  
} else if (scenario == "SSP3-full") {
  load("../Data/model_input/ssp3/mortality.RData")
  load("../Data/model_input/ssp3/EIP.RData")
  load("../Data/model_input/ssp3/bitingRate.RData")
  load("../Data/model_input/ssp3/diapause.RData")
  
  load("../Data/model_input/ssp3/blackbirdAdu_5km.RData")
  load("../Data/model_input/ssp3/blackbirdJuv_5km.RData")
  load("../Data/model_input/ssp3/culex.diapause_5km.RData")
  
  reservoirSc <- "ssp3"


# * ssp 4 scenarios --------------------------------------------------------

} else if (scenario == "SSP4-A") {
  load("../Data/model_input/ref/mortality.RData")
  load("../Data/model_input/ref/EIP.RData")
  load("../Data/model_input/ref/bitingRate.RData")
  load("../Data/model_input/ref/diapause.RData")
  
  load("../Data/model_input/ssp4/blackbirdAdu_5km.RData")
  load("../Data/model_input/ssp4/blackbirdJuv_5km.RData")
  load("../Data/model_input/ref/culex.diapause_5km.RData")
  
  reservoirSc <- "ssp4"
  
} else if (scenario == "SSP4-B") {
  load("../Data/model_input/ssp4/mortality.RData")
  load("../Data/model_input/ssp4/EIP.RData")
  load("../Data/model_input/ssp4/bitingRate.RData")
  load("../Data/model_input/ssp4/diapause.RData")
  
  load("../Data/model_input/ref/blackbirdAdu_5km.RData")
  load("../Data/model_input/ref/blackbirdJuv_5km.RData")
  load("../Data/model_input/ssp4/culex.diapause_5km.RData")
  
  reservoirSc <- "ref"
  
} else if (scenario == "SSP4-full") {
  load("../Data/model_input/ssp4/mortality.RData")
  load("../Data/model_input/ssp4/EIP.RData")
  load("../Data/model_input/ssp4/bitingRate.RData")
  load("../Data/model_input/ssp4/diapause.RData")
  
  load("../Data/model_input/ssp4/blackbirdAdu_5km.RData")
  load("../Data/model_input/ssp4/blackbirdJuv_5km.RData")
  load("../Data/model_input/ssp4/culex.diapause_5km.RData")
  
  reservoirSc <- "ssp4"
  
# * ssp 5 scenarios -------------------------------------------------------

} else if (scenario == "SSP5-A") {
  load("../Data/model_input/ref/mortality.RData")
  load("../Data/model_input/ref/EIP.RData")
  load("../Data/model_input/ref/bitingRate.RData")
  load("../Data/model_input/ref/diapause.RData")
  
  load("../Data/model_input/ssp5/blackbirdAdu_5km.RData")
  load("../Data/model_input/ssp5/blackbirdJuv_5km.RData")
  load("../Data/model_input/ref/culex.diapause_5km.RData")
  
  reservoirSc <- "ssp5"
  
} else if (scenario == "SSP5-B") {
  load("../Data/model_input/ssp5/mortality.RData")
  load("../Data/model_input/ssp5/EIP.RData")
  load("../Data/model_input/ssp5/bitingRate.RData")
  load("../Data/model_input/ssp5/diapause.RData")
  
  load("../Data/model_input/ref/blackbirdAdu_5km.RData")
  load("../Data/model_input/ref/blackbirdJuv_5km.RData")
  load("../Data/model_input/ssp5/culex.diapause_5km.RData")
  
  reservoirSc <- "ref"
  
} else if (scenario == "SSP5-full") {
  load("../Data/model_input/ssp5/mortality.RData")
  load("../Data/model_input/ssp5/EIP.RData")
  load("../Data/model_input/ssp5/bitingRate.RData")
  load("../Data/model_input/ssp5/diapause.RData")
  
  load("../Data/model_input/ssp5/blackbirdAdu_5km.RData")
  load("../Data/model_input/ssp5/blackbirdJuv_5km.RData")
  load("../Data/model_input/ssp5/culex.diapause_5km.RData")
  
  reservoirSc <- "ssp5"
  
} else {
  print("invalid scenario")
  stop()
}


# Create reservoir bird dataframes ----------------------------------------
# birth and adult mortality rates are dependent on the posterior sample so need to be calculated here

# * adult reservoir -------------------------------------------------------

load(paste0("../Data/model_input/", reservoirSc, "/Merel_5km.RData", sep=""))
load("../Data/model_input/ref/temperature5km.ref.RData")

# adult reservoir
resadu.abundance <- data.frame(matrix(NA, nrow = nrow(temperature), ncol = ncol(temperature)))
resadu.abundance[, 1:2] <- temperature[, 1:2]
resadu.abundance[93] <- birdscenario[3] # abundance starts on 1st of April

for (day in 3:ncol(resadu.abundance)) {
  print(day)
  if (day<93) {
    resadu.abundance[, day] <- 0
  } else if (day > 93) {
    resadu.abundance[,day] <- resadu.abundance[,(day-1)] - resadu.abundance[,(day-1)] * (1/posteriorSamples$lifespanR)
  }
}
colnames(resadu.abundance) <- colnames(temperature)

# * juvenile reservoir ----------------------------------------------------

## First: calculate number of births through Leslie matrix

# juvenile mortality rate same as blackbirds
juvenileSurvival.yearly <- 0.13
# adult mortality rate based on lifespanR
adultMortality.daily <- (1/posteriorSamples$lifespanR)
adultSurvival.yearly <- (1-adultMortality.daily)^365
# use optimise to find the birth rate that leads to an eigenvalue of 1
calculate_birthRes <- function(x) {
  matrix <- as.matrix(data.frame(c(0, juvenileSurvival.yearly), c(x, adultSurvival.yearly)))
  
  # Calculate the eigenvalues
  eigenvalues <- eigen(matrix)$values
  
  # Calculate the difference from 1
  difference <- eigenvalues - 1
  
  # Return the minimum absolute difference from 1
  return(min(abs(difference)))
}

# Use optimize to find the value of x that makes an eigenvalue 1
solution <- optimize(calculate_birthRes, c(0, 10))  
birthRate.res <- solution$minimum
print(paste0("birth rate res", birthRate.res, sep=":"))

## Second: create abundance dataframe
resjuv.abundance <- data.frame(matrix(NA, nrow = nrow(temperature), ncol = ncol(temperature)))
resjuv.abundance[, 1:2] <- temperature[, 1:2]

for (day in 3:ncol(resjuv.abundance)) {
  print(day)
  # resjuveniles only appear on day 112 (1 May)
  if (day < 123) {
    resjuv.abundance[, day] <- 0
    
    # day 112-167 births occur  
  } else if (day >= 123 & day <= 168) {
    print("birth day")
    resjuv.abundance[, day] <- resjuv.abundance[,(day-1)] + resadu.abundance[,(day-1)] * (birthRate.res/45) - resjuv.abundance[,(day-1)] * fixedParamsUsutu['mortality.juv']
    
    # after day 168 only deaths occur
  } else if (day > 168) {
    resjuv.abundance[, day] <- resjuv.abundance[,(day-1)] - resjuv.abundance[,(day-1)] * fixedParamsUsutu['mortality.juv']
    
  }
}
colnames(resjuv.abundance) <- colnames(temperature)

# Create prop.bites --------------------------------------------------------

# proportion of bites on competent hosts is calculated as for each day and location:
# for each day: nr.birds / max(nr.birds)
prop.bites <- data.frame(matrix(NA, nrow = nrow(temperature), ncol = ncol(temperature)))
prop.bites[, 1:2] <- temperature[, 1:2]

for (date in 3:ncol(prop.bites)) {
  if (date<93) {
    prop.bites[, date] <- 0
  } else if (date > 92) {
    prop.bites[,date] <- (resadu.abundance[,date] + resjuv.abundance[,date] + adu.abundance[,date] + juv.abundance[,date]) / 
                            (max(resadu.abundance[,date]) + max(resjuv.abundance[,date]) + max(adu.abundance[,date]) + max(juv.abundance[,date]))
  }
}
colnames(prop.bites) <- colnames(temperature)

# * set mean prop.bites to be equal to reference sc ---------------------

meanPropBites <- mean(colMeans(prop.bites[,c(94:367)])) # 0.457 for ref. 
correctionFactor <- meanPropBites - 0.457
prop.bites[,c(3:367)] <- prop.bites[,c(3:367)] - correctionFactor

# make sure it doesn't get below 0 anywhere
prop.bites[prop.bites < 0] <- 0

# Calculate R0 ------------------------------------------------------------

outcome <- data.frame()
for (day in 91:304) {

  print(day)
  column <- day +2

  relevant_points <- 1:nrow(mosquito) # all locations should be included in calculations
  relevant_positions <- mosquito[relevant_points,1:2] # initialise output dataframe holding all coordinates

  outcome <- rbind(outcome,cbind(relevant_positions,R0 = unlist(lapply(relevant_points,calculate.R0.usuv,
                                                                       day = column, mortality = mortality, EIP = EIP, bitingRate = bitingRate, diapause=diapause,
                                                                       Nadu = adu.abundance, Njuv = juv.abundance,
                                                                       Nresadu = resadu.abundance, Nresjuv = resjuv.abundance, Nmos = mosquito,
                                                                       prop.bites = prop.bites, params = fixedParamsUsutu, posterior= posteriorSamples,
                                                                       debug=TRUE)),day=day))
}

saveRDS(outcome, file=paste0("../Output/", scenario, "/usuv.R0.rds", sep="")) # when run locally
saveRDS(outcome, file=paste0(folder_path, "/usuv.run.", opt$run, "_R0.rds", sep="")) # when run on HPC


# # Calculate generation time -----------------------------------------------

gen.time.results <- data.frame()
for (day in 91:304) {

  column <- day +2

  relevant_points <- 1:nrow(mosquito) # all locations should be included in calculations
  relevant_positions <- mosquito[relevant_points,1:2] # initialise output dataframe holding all coordinates

  gen.time.results <- rbind(gen.time.results,cbind(relevant_positions,genTime = unlist(lapply(relevant_points,calculate.genTime,
                                                                       day = column, disease = 'usuv', mortality = mortality, EIP = EIP,
                                                                       params = fixedParamsUsutu, posterior= posteriorSamples,
                                                                       debug=TRUE)),day=day))
}
# toc()

saveRDS(gen.time.results, file=paste0("../Output/", scenario, "/usuv.genTime.rds", sep="")) # when run locally
saveRDS(gen.time.results, file=paste0(folder_path, "/usuv.run.", opt$run, "_genTime.rds", sep="")) # when run on HPC

}

