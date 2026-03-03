
# CREATE INPUT FILES PER SCENARIO

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)

# Parameters --------------------------------------------------------------

# Fixed parameters
fixedParamsUsutu <- c(transmissionProbMH = 0.88,
                      IIR=0.67,
                      mortality.adu=0.0011,
                      mortality.juv=0.0059,
                      recovery = 0.25)
save(fixedParamsUsutu, file = paste0("../Data/model_input/fixedParamsUsutu.RData"))

fixedParamsWNV <- c(transmissionProbMH = 0.88,
                    transmissionProbMalMosq = 0.43,
                    transmissionProbHsMosq = 0.98,
                    IIR=1,
                    mortality.adu.mal=0.0013,
                    mortality.juv.mal=0.0018,
                    mortality.adu.hs=0.0015,
                    mortality.juv.hs=0.002,
                    recovery.mal = 0.25,
                    recovery.hs = 0.22,
                    dis.ind.mort.mal = 0,
                    dis.ind.mort.hs = 0.21)
save(fixedParamsWNV, file = paste0("../Data/model_input/fixedParamsWNV.RData"))


# Temperature-dependent parameters
tempCutOff.EIP <- 13
tempCutOff.mortality <- 32

calc.biting <- function(temperature) {
  bitingRate = 0.344/(1+1.231*exp(-0.184*(temperature-20)))
  return(bitingRate)
}

calc.EIP <- function(temperature) {
  EIRate <- 7.38*10^-5*temperature*(temperature-11.4)*sqrt(45.2-temperature)
  return(EIRate)
}

calc.mortality <- function(temperature) {
  mortality = 1/(-4.86*temperature+169.8)
  return(mortality)
}

calc.diapause <- function(day) {
  if (day > 245) {
    diapause = 0.05 
    } else {
      diapause = 0
    }
  return(diapause)
}

# Leslie matrix to calculate no of births with which the population size is stable (eigenvalue is 1)
calculate_birth <- function(x) {
  matrix <- as.matrix(data.frame(c(0, juvenileSurvival.yearly), c(x, adultSurvival.yearly)))
  
  # Calculate the eigenvalues
  eigenvalues <- eigen(matrix)$values
  
  # Calculate the difference from 1
  difference <- eigenvalues - 1
  
  # Return the minimum absolute difference from 1
  return(min(abs(difference)))
}


# Create dataframes for each scenario -------------------------------------

scenarios <- "ref" # c('ssp1', 'ssp3', 'ssp4', 'ssp5') 

# * temp ------------------------------------------------------------------

for (scenario in scenarios) {
  
  load(paste0("../Data/model_input/", scenario, "/temperature5km.", scenario, ".mean.RData", sep=""))
  
  bitingRate <- data.frame(matrix(NA, nrow = nrow(temperature), ncol = ncol(temperature)))
  EIP <- data.frame(matrix(NA, nrow = nrow(temperature), ncol = ncol(temperature)))
  mortality <- data.frame(matrix(NA, nrow = nrow(temperature), ncol = ncol(temperature)))
  diapause <- data.frame(matrix(NA, nrow = nrow(temperature), ncol = ncol(temperature)))
  
  # Copy the values from columns 1 and 2 of temperature to new df
  bitingRate[, 1:2] <- temperature[, 1:2]
  EIP[, 1:2] <- temperature[, 1:2]
  mortality[, 1:2] <- temperature[, 1:2]
  diapause[, 1:2] <- temperature[, 1:2]
  
  # Loop through columns 3 to 367 of temperature
  for (i in 3:367) {
    # Perform the specified calculation for each column
    bitingRate[, i] <- calc.biting(temperature = temperature[,i])
    
    EIP[, i] <- ifelse(temperature[,i] > tempCutOff.EIP,
                       calc.EIP(temperature = temperature[,i]),
                       0)
    
    mortality[, i] <- ifelse(temperature[,i] > tempCutOff.mortality,
                             0.17,
                             calc.mortality(temperature = temperature[,i]))
    diapause[,i] <- calc.diapause(day = i)
  }
  
  # Keep column names from temperature
  colnames(bitingRate) <- colnames(temperature)
  colnames(EIP) <- colnames(temperature)
  colnames(mortality) <- colnames(temperature)
  colnames(diapause) <- colnames(temperature)
  
  save(bitingRate, file = paste0("../Data/model_input/", scenario, "/bitingRate.RData", sep=""))
  save(EIP, file = paste0("../Data/model_input/", scenario, "/EIP.RData", sep=""))
  save(mortality, file = paste0("../Data/model_input/", scenario, "/mortality.RData", sep=""))
  save(diapause, file = paste0("../Data/model_input/", scenario, "/diapause.RData", sep=""))
}

# * blackbird abundance temporal ---------------------------------------------------
for (scenario in scenarios) {
  
  load(paste0("../Data/model_input/", scenario, "/Merel_5km.RData", sep=""))
  load(paste0("../Data/model_input/", scenario, "/temperature5km.", scenario, ".mean.RData", sep=""))
  
  # adult blackbirds
  adu.abundance <- data.frame(matrix(NA, nrow = nrow(temperature), ncol = ncol(temperature)))
  adu.abundance[, 1:2] <- temperature[, 1:2]
  adu.abundance[93] <- birdscenario[3] # abundance starts on 1st of April

  for (day in 3:ncol(adu.abundance)) {
    print(day)
    if (day<93) {
      adu.abundance[, day] <- 0
    } else if (day > 93) {
      adu.abundance[,day] <- adu.abundance[,(day-1)] - adu.abundance[,(day-1)] * fixedParamsUsutu['mortality.adu']
    }
  }
  colnames(adu.abundance) <- colnames(temperature)
  save(adu.abundance, file = paste0("../Data/model_input/", scenario, "/blackbirdAdu_5km.RData", sep=""))
  
  # juvenile blackbirds
  # calculate no of births from Leslie matrix
  juvenileSurvival.yearly <- 0.13
  adultSurvival.yearly <- 0.68
  solution <- optimize(calculate_birth, c(0, 10))  # Use optimize to find the value of x that makes an eigenvalue 1
  births.bb <- solution$minimum
  
  juv.abundance <- data.frame(matrix(NA, nrow = nrow(temperature), ncol = ncol(temperature)))
  juv.abundance[, 1:2] <- temperature[, 1:2]
  
  for (day in 3:ncol(juv.abundance)) {
    print(day)
    # juveniles only appear on day 112 (1 May)
    if (day < 123) {
      juv.abundance[, day] <- 0
      
      # day 112-167 births occur  
    } else if (day >= 123 & day <= 168) {
      print("birth day")
      juv.abundance[, day] <- juv.abundance[,(day-1)] + adu.abundance[,(day-1)] * (births.bb/45) - juv.abundance[,(day-1)] * fixedParamsUsutu['mortality.juv']
      
      # after day 168 only deaths occur
    } else if (day > 168) {
      juv.abundance[, day] <- juv.abundance[,(day-1)] - juv.abundance[,(day-1)] * fixedParamsUsutu['mortality.juv']
      
    }
  }
  colnames(juv.abundance) <- colnames(temperature)
  
  save(juv.abundance, file = paste0("../Data/model_input/", scenario, "/blackbirdJuv_5km.RData", sep=""))
}


# * mallard abundance temporal ---------------------------------------------------

uniform <- "yes"

for (scenario in scenarios) {
  
  load(paste0("../Data/model_input/", scenario, "/Wilde_Eend_5km.RData", sep=""))
  # load(paste0("../Data/model_input/", scenario, "/temperature5km.", scenario, ".mean.RData", sep=""))
  load(paste0("../Data/model_input/", scenario, "/temperature5km.", scenario, ".RData", sep=""))  # for ref
  
  # adult mallards
  adu.abundance <- data.frame(matrix(NA, nrow = nrow(temperature), ncol = ncol(temperature)))
  adu.abundance[, 1:2] <- temperature[, 1:2]
  adu.abundance[93] <- birdscenario[3] # abundance starts on 1st of April
  
  if (uniform == "yes") {
    mean.abundance <- colMeans(adu.abundance[93])
    adu.abundance[93] <- rep(mean.abundance, nrow(adu.abundance))
  } 
  
  for (day in 3:ncol(adu.abundance)) {
    print(day)
    if (day<93) {
      adu.abundance[, day] <- 0
    } else if (day > 93) {
      adu.abundance[,day] <- adu.abundance[,(day-1)] - adu.abundance[,(day-1)] * fixedParamsWNV['mortality.adu.mal']
    }
  }
  colnames(adu.abundance) <- colnames(temperature)
  
  # juvenile mallards
  # calculate no of births from Leslie matrix
  juvenileSurvival.yearly <- 0.52
  adultSurvival.yearly <- 0.63
  solution <- optimize(calculate_birth, c(0, 10))   # Use optimize to find the value of x that makes an eigenvalue 1
  births.mal <- solution$minimum
  
  juv.abundance <- data.frame(matrix(NA, nrow = nrow(temperature), ncol = ncol(temperature)))
  juv.abundance[, 1:2] <- temperature[, 1:2]
  
  for (day in 3:ncol(juv.abundance)) {
    print(day)
    # juveniles only appear on day 112 (1 May)
    if (day < 123) {
      juv.abundance[, day] <- 0
      
      # day 112-167 births occur  
    } else if (day >= 123 & day <= 168) {
      print("birth day")
      juv.abundance[, day] <- juv.abundance[,(day-1)] + adu.abundance[,(day-1)] * (births.mal/45) - juv.abundance[,(day-1)] * fixedParamsWNV['mortality.juv.mal']
      
      # after day 168 only deaths occur
    } else if (day > 168) {
      juv.abundance[, day] <- juv.abundance[,(day-1)] - juv.abundance[,(day-1)] * fixedParamsWNV['mortality.juv.mal']
      
    }
  }
  colnames(juv.abundance) <- colnames(temperature)
  
  # Set arbitrary abundance equal to blackbird to set the size of the competent population equal between wnv and usuv
  # Do this by setting the mean across all locations and the full year to be equal
  # mean adult blackbird abundance is 325, and mean 403 for juveniles, so 728 total. (mean(colMeans(adu.abundance[,c(3:367)]))) 
  # mean adult mallard abundance is 292, and mean 158 for juveniles, so 450 total. 
  adu.abundance[,c(3:367)] <- adu.abundance[,c(3:367)] * (728/450)
  juv.abundance[,c(3:367)] <- juv.abundance[,c(3:367)] * (728/450)
  
  if (uniform == "yes") {
    save(adu.abundance, file = paste0("../Data/model_input/", scenario, "/uniform_mallardAdu_5km.RData", sep=""))
    save(juv.abundance, file = paste0("../Data/model_input/", scenario, "/uniform_mallardJuv_5km.RData", sep=""))
  } else {
    save(adu.abundance, file = paste0("../Data/model_input/", scenario, "/mallardAdu_5km.RData", sep=""))
    save(juv.abundance, file = paste0("../Data/model_input/", scenario, "/mallardJuv_5km.RData", sep=""))
  }
  
}


# * house sparrow --------------------------------------------------------

for (scenario in scenarios) {
  
  load(paste0("../Data/model_input/", scenario, "/Huismus_5km.RData", sep=""))
  # load(paste0("../Data/model_input/", scenario, "/temperature5km.", scenario, ".mean.RData", sep=""))
  load(paste0("../Data/model_input/", scenario, "/temperature5km.", scenario, ".RData", sep="")) # for ref
  
  # adult house sparrows
  adu.abundance <- data.frame(matrix(NA, nrow = nrow(temperature), ncol = ncol(temperature)))
  adu.abundance[, 1:2] <- temperature[, 1:2]
  adu.abundance[93] <- birdscenario[3] # abundance starts on 1st of April
  
  if (uniform == "yes") {
    mean.abundance <- colMeans(adu.abundance[93])
    adu.abundance[93] <- rep(mean.abundance, nrow(adu.abundance))
  } 
  
  for (day in 3:ncol(adu.abundance)) {
    print(day)
    if (day<93) {
      adu.abundance[, day] <- 0
    } else if (day > 93) {
      adu.abundance[,day] <- adu.abundance[,(day-1)] - adu.abundance[,(day-1)] * fixedParamsWNV['mortality.adu.hs']
    }
  }
  colnames(adu.abundance) <- colnames(temperature)
  
  # juvenile house sparrows
  # calculate no of births from Leslie matrix
  juvenileSurvival.yearly <- 0.49
  adultSurvival.yearly <- 0.57
  solution <- optimize(calculate_birth, c(0, 10))   # Use optimize to find the value of x that makes an eigenvalue 1
  births.hs <- solution$minimum
  
  juv.abundance <- data.frame(matrix(NA, nrow = nrow(temperature), ncol = ncol(temperature)))
  juv.abundance[, 1:2] <- temperature[, 1:2]
  
  for (day in 3:ncol(juv.abundance)) {
    print(day)
    # juveniles only appear on day 112 (1 May)
    if (day < 123) {
      juv.abundance[, day] <- 0
      
      # day 112-167 births occur  
    } else if (day >= 123 & day <= 168) {
      print("birth day")
      juv.abundance[, day] <- juv.abundance[,(day-1)] + adu.abundance[,(day-1)] * (births.hs/45) - juv.abundance[,(day-1)] * fixedParamsWNV['mortality.juv.hs']
      
      # after day 168 only deaths occur
    } else if (day > 168) {
      juv.abundance[, day] <- juv.abundance[,(day-1)] - juv.abundance[,(day-1)] * fixedParamsWNV['mortality.juv.hs']
      
    }
  }
  colnames(juv.abundance) <- colnames(temperature)
  
  # Set arbitrary abundance equal to blackbird to set the size of the competent population equal between wnv and usuv
  # Do this by setting the mean across all locations and the full year to be equal
  # mean adult blackbird abundance is 325, and mean 403 for juveniles, so 728 total. (mean(colMeans(adu.abundance[,c(3:367)]))) 
  # mean adult house sparrow abundance is 401, and mean 266 for juveniles, so 667 total. 
  adu.abundance[,c(3:367)] <- adu.abundance[,c(3:367)] * (728/667)
  juv.abundance[,c(3:367)] <- juv.abundance[,c(3:367)] * (728/667)
  
  if (uniform == "yes") {
    save(adu.abundance, file = paste0("../Data/model_input/", scenario, "/uniform_housesparrowAdu_5km.RData", sep=""))
    save(juv.abundance, file = paste0("../Data/model_input/", scenario, "/uniform_housesparrowJuv_5km.RData", sep=""))
  } else {
    save(adu.abundance, file = paste0("../Data/model_input/", scenario, "/housesparrowAdu_5km.RData", sep=""))
    save(juv.abundance, file = paste0("../Data/model_input/", scenario, "/housesparrowJuv_5km.RData", sep=""))
  }
}


# * mosquito abundance ---------------------------------------------------
## add diapause to make population abundance go down towards end of the year

for (scenario in scenarios) {
  
  load(paste0("../Data/model_input/", scenario, "/culex_5km.RData", sep=""))
  load(paste0("../Data/model_input/", scenario, "/diapause.RData", sep=""))
  

for (day in 3:ncol(mosquito)) {
  print(day)
  if (day > 245) {
    mosquito[, day] <- mosquito[,(day-1)] - mosquito[,(day-1)] * diapause[, day]
  }
}

save(mosquito, file = paste0("../Data/model_input/", scenario, "/culex.diapause_5km.RData", sep=""))
}

# create plot
mosquito.plot <- mosquito %>% dplyr::select(-c(x, y))
mosquito.plot <- data.frame(t(mosquito.plot))
mosquito.plot <- mosquito.plot %>%
  rowwise(.) %>%
  mutate(daily.mean.ab = mean(c_across(1:1398))) 

mosquito.plot$date <- seq(from = as.Date("2022-01-01"), to = as.Date("2022-12-31"), by = 'day')

mosq.abundance.plot <- 
  ggplot(data=mosquito.plot) +
  geom_line(aes(x=date, y=daily.mean.ab), linewidth=1) +
  ggtitle("Mean daily mosquito abundance") +
  ylab("Abundance (arbitrary unit)") + 
  theme_bw() +
  # scale_colour_manual(values = brewer.pal(9, "YlOrRd")[3:9]) +
  theme(axis.text=element_text(size=16), axis.title = element_text(size=16), legend.text = element_text(size=16),
        legend.title = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggsave(mosq.abundance.plot, file=paste0("../Output/Plots/InputData/mosquito/", scenario, "_abundance_diapause.png", sep=""), width=7, height=5)

