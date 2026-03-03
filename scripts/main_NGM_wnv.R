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

bitingPref <- 1

# Load input data ------------------------------------------------------------

load("../Data/model_input/fixedParamsWNV.RData")

scenario <- opt$scenario

# scenarios A only have the future bird distributions
# scenarios B only have the future temperature and mosquito distributions

# scenario <- "ref"
# scenario <- "ref-uniform"

# scenario <- "SSP1-A"
# scenario <- "SSP1-B"
# scenario <- "SSP1-full"
# scenario <- "SSP1-uniform"

# scenario <- "SSP3-A"
# scenario <- "SSP3-B"
# scenario <- "SSP3-full"
scenario <- "SSP3-uniform"

# scenario <- "SSP4-A"
# scenario <- "SSP4-B"
# scenario <- "SSP4-full"
# scenario <- "SSP4-uniform"

# scenario <- "SSP5-A"
# scenario <- "SSP5-B"
# scenario <- "SSP5-full"
# scenario <- "SSP5-uniform"

{ # runs full dataprep and NGM
  
if (scenario == "ref") {
  load("../Data/model_input/ref/mortality.RData")
  load("../Data/model_input/ref/EIP.RData")
  load("../Data/model_input/ref/bitingRate.RData")
  load("../Data/model_input/ref/diapause.RData")
  
  load("../Data/model_input/ref/mallardAdu_5km.RData")
  maladu.abundance <- adu.abundance
  load("../Data/model_input/ref/mallardJuv_5km.RData")
  maljuv.abundance <- juv.abundance
  load("../Data/model_input/ref/housesparrowAdu_5km.RData")
  hsadu.abundance <- adu.abundance
  load("../Data/model_input/ref/housesparrowJuv_5km.RData")
  hsjuv.abundance <- juv.abundance
  load("../Data/model_input/ref/culex.diapause_5km.RData")
  
  reservoirSc <- "ref"

} else if (scenario == "ref-uniform") {

  load("../Data/model_input/ref/mortality.RData")
  load("../Data/model_input/ref/EIP.RData")
  load("../Data/model_input/ref/bitingRate.RData")
  load("../Data/model_input/ref/diapause.RData")
  
  load("../Data/model_input/ref/uniform_mallardAdu_5km.RData")
  maladu.abundance <- adu.abundance
  load("../Data/model_input/ref/uniform_mallardJuv_5km.RData")
  maljuv.abundance <- juv.abundance
  load("../Data/model_input/ref/uniform_housesparrowAdu_5km.RData")
  hsadu.abundance <- adu.abundance
  load("../Data/model_input/ref/uniform_housesparrowJuv_5km.RData")
  hsjuv.abundance <- juv.abundance
  load("../Data/model_input/ref/culex.diapause_5km.RData")
  
  reservoirSc <- "ref"
  
# * ssp 1 scenarios --------------------------------------------------------

} else if (scenario == "SSP1-A") {
  load("../Data/model_input/ref/mortality.RData")
  load("../Data/model_input/ref/EIP.RData")
  load("../Data/model_input/ref/bitingRate.RData")
  load("../Data/model_input/ref/diapause.RData")
  
  load("../Data/model_input/ssp1/mallardAdu_5km.RData")
  maladu.abundance <- adu.abundance
  load("../Data/model_input/ssp1/mallardJuv_5km.RData")
  maljuv.abundance <- juv.abundance
  load("../Data/model_input/ssp1/housesparrowAdu_5km.RData")
  hsadu.abundance <- adu.abundance
  load("../Data/model_input/ssp1/housesparrowJuv_5km.RData")
  hsjuv.abundance <- juv.abundance
  load("../Data/model_input/ref/culex.diapause_5km.RData")
  
  reservoirSc <- "ssp1"
  
} else if (scenario == "SSP1-B") {
  load("../Data/model_input/ssp1/mortality.RData")
  load("../Data/model_input/ssp1/EIP.RData")
  load("../Data/model_input/ssp1/bitingRate.RData")
  load("../Data/model_input/ssp1/diapause.RData")
  
  load("../Data/model_input/ref/mallardAdu_5km.RData")
  maladu.abundance <- adu.abundance
  load("../Data/model_input/ref/mallardJuv_5km.RData")
  maljuv.abundance <- juv.abundance
  load("../Data/model_input/ref/housesparrowAdu_5km.RData")
  hsadu.abundance <- adu.abundance
  load("../Data/model_input/ref/housesparrowJuv_5km.RData")
  hsjuv.abundance <- juv.abundance
  load("../Data/model_input/ssp1/culex.diapause_5km.RData")
  
  reservoirSc <- "ref"
  
} else if (scenario == "SSP1-full") {
  load("../Data/model_input/ssp1/mortality.RData")
  load("../Data/model_input/ssp1/EIP.RData")
  load("../Data/model_input/ssp1/bitingRate.RData")
  load("../Data/model_input/ssp1/diapause.RData")
  
  load("../Data/model_input/ssp1/mallardAdu_5km.RData")
  maladu.abundance <- adu.abundance
  load("../Data/model_input/ssp1/mallardJuv_5km.RData")
  maljuv.abundance <- juv.abundance
  load("../Data/model_input/ssp1/housesparrowAdu_5km.RData")
  hsadu.abundance <- adu.abundance
  load("../Data/model_input/ssp1/housesparrowJuv_5km.RData")
  hsjuv.abundance <- juv.abundance
  load("../Data/model_input/ssp1/culex.diapause_5km.RData")
  
  reservoirSc <- "ssp1"
  
} else if (scenario == "SSP1-uniform") {
  load("../Data/model_input/ssp1/mortality.RData")
  load("../Data/model_input/ssp1/EIP.RData")
  load("../Data/model_input/ssp1/bitingRate.RData")
  load("../Data/model_input/ssp1/diapause.RData")
  
  load("../Data/model_input/ssp1/uniform_mallardAdu_5km.RData")
  maladu.abundance <- adu.abundance
  load("../Data/model_input/ssp1/uniform_mallardJuv_5km.RData")
  maljuv.abundance <- juv.abundance
  load("../Data/model_input/ssp1/uniform_housesparrowAdu_5km.RData")
  hsadu.abundance <- adu.abundance
  load("../Data/model_input/ssp1/uniform_housesparrowJuv_5km.RData")
  hsjuv.abundance <- juv.abundance
  load("../Data/model_input/ssp1/culex.diapause_5km.RData")
  
  reservoirSc <- "ssp1"
# * ssp 3 scenarios -------------------------------------------------------

} else if (scenario == "SSP3-A") {
  load("../Data/model_input/ref/mortality.RData")
  load("../Data/model_input/ref/EIP.RData")
  load("../Data/model_input/ref/bitingRate.RData")
  load("../Data/model_input/ref/diapause.RData")
  
  load("../Data/model_input/ssp3/mallardAdu_5km.RData")
  maladu.abundance <- adu.abundance
  load("../Data/model_input/ssp3/mallardJuv_5km.RData")
  maljuv.abundance <- juv.abundance
  load("../Data/model_input/ssp3/housesparrowAdu_5km.RData")
  hsadu.abundance <- adu.abundance
  load("../Data/model_input/ssp3/housesparrowJuv_5km.RData")
  hsjuv.abundance <- juv.abundance
  load("../Data/model_input/ref/culex.diapause_5km.RData")
  
  reservoirSc <- "ssp3"
  
} else if (scenario == "SSP3-B") {
  load("../Data/model_input/ssp3/mortality.RData")
  load("../Data/model_input/ssp3/EIP.RData")
  load("../Data/model_input/ssp3/bitingRate.RData")
  load("../Data/model_input/ssp3/diapause.RData")
  
  load("../Data/model_input/ref/mallardAdu_5km.RData")
  maladu.abundance <- adu.abundance
  load("../Data/model_input/ref/mallardJuv_5km.RData")
  maljuv.abundance <- juv.abundance
  load("../Data/model_input/ref/housesparrowAdu_5km.RData")
  hsadu.abundance <- adu.abundance
  load("../Data/model_input/ref/housesparrowJuv_5km.RData")
  hsjuv.abundance <- juv.abundance
  load("../Data/model_input/ssp3/culex.diapause_5km.RData")
  
  reservoirSc <- "ref"
  
} else if (scenario == "SSP3-full") {
  load("../Data/model_input/ssp3/mortality.RData")
  load("../Data/model_input/ssp3/EIP.RData")
  load("../Data/model_input/ssp3/bitingRate.RData")
  load("../Data/model_input/ssp3/diapause.RData")
  
  load("../Data/model_input/ssp3/mallardAdu_5km.RData")
  maladu.abundance <- adu.abundance
  load("../Data/model_input/ssp3/mallardJuv_5km.RData")
  maljuv.abundance <- juv.abundance
  load("../Data/model_input/ssp3/housesparrowAdu_5km.RData")
  hsadu.abundance <- adu.abundance
  load("../Data/model_input/ssp3/housesparrowJuv_5km.RData")
  hsjuv.abundance <- juv.abundance
  load("../Data/model_input/ssp3/culex.diapause_5km.RData")
  
  reservoirSc <- "ssp3"
  
} else if (scenario == "SSP3-uniform") {
  load("../Data/model_input/ssp3/mortality.RData")
  load("../Data/model_input/ssp3/EIP.RData")
  load("../Data/model_input/ssp3/bitingRate.RData")
  load("../Data/model_input/ssp3/diapause.RData")
  
  load("../Data/model_input/ssp3/uniform_mallardAdu_5km.RData")
  maladu.abundance <- adu.abundance
  load("../Data/model_input/ssp3/uniform_mallardJuv_5km.RData")
  maljuv.abundance <- juv.abundance
  load("../Data/model_input/ssp3/uniform_housesparrowAdu_5km.RData")
  hsadu.abundance <- adu.abundance
  load("../Data/model_input/ssp3/uniform_housesparrowJuv_5km.RData")
  hsjuv.abundance <- juv.abundance
  load("../Data/model_input/ssp3/culex.diapause_5km.RData")
  
  reservoirSc <- "ssp3"
  
# * ssp 4 scenarios --------------------------------------------------------
  
} else if (scenario == "SSP4-A") {
  load("../Data/model_input/ref/mortality.RData")
  load("../Data/model_input/ref/EIP.RData")
  load("../Data/model_input/ref/bitingRate.RData")
  load("../Data/model_input/ref/diapause.RData")
  
  load("../Data/model_input/ssp4/mallardAdu_5km.RData")
  maladu.abundance <- adu.abundance
  load("../Data/model_input/ssp4/mallardJuv_5km.RData")
  maljuv.abundance <- juv.abundance
  load("../Data/model_input/ssp4/housesparrowAdu_5km.RData")
  hsadu.abundance <- adu.abundance
  load("../Data/model_input/ssp4/housesparrowJuv_5km.RData")
  hsjuv.abundance <- juv.abundance
  load("../Data/model_input/ref/culex.diapause_5km.RData")
  
  reservoirSc <- "ssp4"
  
} else if (scenario == "SSP4-B") {
  load("../Data/model_input/ssp4/mortality.RData")
  load("../Data/model_input/ssp4/EIP.RData")
  load("../Data/model_input/ssp4/bitingRate.RData")
  load("../Data/model_input/ssp4/diapause.RData")
  
  load("../Data/model_input/ref/mallardAdu_5km.RData")
  maladu.abundance <- adu.abundance
  load("../Data/model_input/ref/mallardJuv_5km.RData")
  maljuv.abundance <- juv.abundance
  load("../Data/model_input/ref/housesparrowAdu_5km.RData")
  hsadu.abundance <- adu.abundance
  load("../Data/model_input/ref/housesparrowJuv_5km.RData")
  hsjuv.abundance <- juv.abundance
  load("../Data/model_input/ssp4/culex.diapause_5km.RData")
  
  reservoirSc <- "ref"
  
} else if (scenario == "SSP4-full") {
  load("../Data/model_input/ssp4/mortality.RData")
  load("../Data/model_input/ssp4/EIP.RData")
  load("../Data/model_input/ssp4/bitingRate.RData")
  load("../Data/model_input/ssp4/diapause.RData")
  
  load("../Data/model_input/ssp4/mallardAdu_5km.RData")
  maladu.abundance <- adu.abundance
  load("../Data/model_input/ssp4/mallardJuv_5km.RData")
  maljuv.abundance <- juv.abundance
  load("../Data/model_input/ssp4/housesparrowAdu_5km.RData")
  hsadu.abundance <- adu.abundance
  load("../Data/model_input/ssp4/housesparrowJuv_5km.RData")
  hsjuv.abundance <- juv.abundance
  load("../Data/model_input/ssp4/culex.diapause_5km.RData")
  
  reservoirSc <- "ssp4"
  
} else if (scenario == "SSP4-uniform") {
  load("../Data/model_input/ssp4/mortality.RData")
  load("../Data/model_input/ssp4/EIP.RData")
  load("../Data/model_input/ssp4/bitingRate.RData")
  load("../Data/model_input/ssp4/diapause.RData")
  
  load("../Data/model_input/ssp4/uniform_mallardAdu_5km.RData")
  maladu.abundance <- adu.abundance
  load("../Data/model_input/ssp4/uniform_mallardJuv_5km.RData")
  maljuv.abundance <- juv.abundance
  load("../Data/model_input/ssp4/uniform_housesparrowAdu_5km.RData")
  hsadu.abundance <- adu.abundance
  load("../Data/model_input/ssp4/uniform_housesparrowJuv_5km.RData")
  hsjuv.abundance <- juv.abundance
  load("../Data/model_input/ssp4/culex.diapause_5km.RData")
  
  reservoirSc <- "ssp4"
  
# * ssp 5 scenarios -------------------------------------------------------

} else if (scenario == "SSP5-A") {
  load("../Data/model_input/ref/mortality.RData")
  load("../Data/model_input/ref/EIP.RData")
  load("../Data/model_input/ref/bitingRate.RData")
  load("../Data/model_input/ref/diapause.RData")
  
  load("../Data/model_input/ssp5/mallardAdu_5km.RData")
  maladu.abundance <- adu.abundance
  load("../Data/model_input/ssp5/mallardJuv_5km.RData")
  maljuv.abundance <- juv.abundance
  load("../Data/model_input/ssp5/housesparrowAdu_5km.RData")
  hsadu.abundance <- adu.abundance
  load("../Data/model_input/ssp5/housesparrowJuv_5km.RData")
  hsjuv.abundance <- juv.abundance
  load("../Data/model_input/ref/culex.diapause_5km.RData")
  
  reservoirSc <- "ssp5"
  
} else if (scenario == "SSP5-B") {
  load("../Data/model_input/ssp5/mortality.RData")
  load("../Data/model_input/ssp5/EIP.RData")
  load("../Data/model_input/ssp5/bitingRate.RData")
  load("../Data/model_input/ssp5/diapause.RData")
  
  load("../Data/model_input/ref/mallardAdu_5km.RData")
  maladu.abundance <- adu.abundance
  load("../Data/model_input/ref/mallardJuv_5km.RData")
  maljuv.abundance <- juv.abundance
  load("../Data/model_input/ref/housesparrowAdu_5km.RData")
  hsadu.abundance <- adu.abundance
  load("../Data/model_input/ref/housesparrowJuv_5km.RData")
  hsjuv.abundance <- juv.abundance
  load("../Data/model_input/ssp5/culex.diapause_5km.RData")
  
  reservoirSc <- "ref"
  
} else if (scenario == "SSP5-full") {
  load("../Data/model_input/ssp5/mortality.RData")
  load("../Data/model_input/ssp5/EIP.RData")
  load("../Data/model_input/ssp5/bitingRate.RData")
  load("../Data/model_input/ssp5/diapause.RData")
  
  load("../Data/model_input/ssp5/mallardAdu_5km.RData")
  maladu.abundance <- adu.abundance
  load("../Data/model_input/ssp5/mallardJuv_5km.RData")
  maljuv.abundance <- juv.abundance
  load("../Data/model_input/ssp5/housesparrowAdu_5km.RData")
  hsadu.abundance <- adu.abundance
  load("../Data/model_input/ssp5/housesparrowJuv_5km.RData")
  hsjuv.abundance <- juv.abundance
  load("../Data/model_input/ssp5/culex.diapause_5km.RData")
  
  reservoirSc <- "ssp5"
  
} else if (scenario == "SSP5-uniform") {
  load("../Data/model_input/ssp5/mortality.RData")
  load("../Data/model_input/ssp5/EIP.RData")
  load("../Data/model_input/ssp5/bitingRate.RData")
  load("../Data/model_input/ssp5/diapause.RData")
  
  load("../Data/model_input/ssp5/uniform_mallardAdu_5km.RData")
  maladu.abundance <- adu.abundance
  load("../Data/model_input/ssp5/uniform_mallardJuv_5km.RData")
  maljuv.abundance <- juv.abundance
  load("../Data/model_input/ssp5/uniform_housesparrowAdu_5km.RData")
  hsadu.abundance <- adu.abundance
  load("../Data/model_input/ssp5/uniform_housesparrowJuv_5km.RData")
  hsjuv.abundance <- juv.abundance
  load("../Data/model_input/ssp5/culex.diapause_5km.RData")
  
  reservoirSc <- "ssp5"
  
} else {
  print("invalid scenario")
  stop()
}


# Create prop.bites --------------------------------------------------------

# proportion of bites on competent hosts is calculated as for each day and location:
# for each day: nr.birds / max(nr.birds)
prop.bites <- data.frame(matrix(NA, nrow = nrow(adu.abundance), ncol = ncol(adu.abundance)))
prop.bites[, 1:2] <- adu.abundance[, 1:2]

for (date in 3:ncol(prop.bites)) {
  if (date<93) {
    prop.bites[, date] <- 0
  } else if (date > 92) {
    prop.bites[,date] <- (maladu.abundance[,date] + maljuv.abundance[,date] + hsadu.abundance[,date] + hsjuv.abundance[,date]) / 
                            (max(maladu.abundance[,date]) + max(maljuv.abundance[,date]) + max(hsadu.abundance[,date]) + max(hsjuv.abundance[,date]))
  }
}
colnames(prop.bites) <- colnames(adu.abundance)

# * set mean prop.bites to be equal to reference sc ---------------------

meanPropBites <- mean(colMeans(prop.bites[,c(94:367)])) # 0.457 for ref for usuv. assume same bites distribution here.
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

  outcome <- rbind(outcome,cbind(relevant_positions,R0 = unlist(lapply(relevant_points,calculate.R0.wnv,
                                                                       day = column, mortality = mortality, EIP = EIP, bitingRate = bitingRate, diapause=diapause, bitingPref=bitingPref,
                                                                       Nmaladu = maladu.abundance, Nmaljuv = maljuv.abundance,
                                                                       Nhsadu = hsadu.abundance, Nhsjuv = hsjuv.abundance, Nmos = mosquito,
                                                                       prop.bites = prop.bites, params = fixedParamsWNV, posterior= posteriorSamples,
                                                                       debug=TRUE)),day=day))
}
# toc()

saveRDS(outcome, file=paste0("../Output/", scenario, "/wnv.R0.rds", sep="")) # when run locally
saveRDS(outcome, file=paste0(folder_path, "/wnv.run.", opt$run, "_R0.rds", sep="")) # when run on HPC


# Calculate generation time -----------------------------------------------

gen.time.results <- data.frame()
for (day in 91:304) {

  # print(day)
  column <- day +2

  relevant_points <- 1:nrow(mosquito) # all locations should be included in calculations
  relevant_positions <- mosquito[relevant_points,1:2] # initialise output dataframe holding all coordinates

  gen.time.results <- rbind(gen.time.results,cbind(relevant_positions,genTime = unlist(lapply(relevant_points,calculate.genTime,
                                                                       day = column, disease = 'wnv', mortality = mortality, EIP = EIP,
                                                                       params = fixedParamsWNV, posterior= posteriorSamples,
                                                                       debug=TRUE)),day=day))
}
# toc()

saveRDS(gen.time.results, file=paste0("../Output/", scenario, "/wnv.genTime.rds", sep="")) # when run locally
saveRDS(gen.time.results, file=paste0(folder_path, "/wnv.run.", opt$run, "_genTime.rds", sep="")) # when run on HPC

}

