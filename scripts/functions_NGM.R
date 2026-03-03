

# General -----------------------------------------------------------------

combine_rds_files <- function(folder_paths, file_name) {
  combined_data <- list()
      browser()

  for (folder in folder_paths) {
    file_path <- file.path(folder, file_name)
    
    data <- readRDS(file_path)
    
    # Assuming the loaded data is a data frame and is named `data`
    if (is.null(combined_data)) {
      combined_data <- data
    } else {
      combined_data <- c(combined_data, data)
    }
  } 
  
  return(combined_data)
}


# R0 ----------------------------------------------------------------------

calculate.R0.usuv <- function(pos, day, 
                              mortality, EIP, bitingRate, diapause,
                              Nadu, Njuv, Nresadu, Nresjuv, Nmos, 
                              prop.bites, params, posterior,
                              debug=FALSE) {
  if (debug == TRUE) {
    
    # Only print day when it changed. Check if the current day is different from the last printed day
    last_day <- NULL
    if (!is.null(last_day) && day != last_day) {
      print(paste("Day changed to:", day))
    }
    
    # Update the last_day variable
    last_day <- day
    } 


  R0.mosq.juv <- ifelse(Njuv[pos,day]>0,
                        (EIP[pos,day] * bitingRate[pos,day] * prop.bites[pos,day] * params['transmissionProbMH'] * Njuv[pos,day]) /
                          ((EIP[pos,day] + mortality[pos,day] + diapause[pos,day]) * (mortality[pos,day] + diapause[pos,day]) * ((Nresadu[pos,day] + Nresjuv[pos,day])* posterior$bitingPref + Njuv[pos,day] + Nadu[pos,day])),
                        0)

  R0.mosq.adu <- (EIP[pos,day] * bitingRate[pos,day] * prop.bites[pos,day] * params['transmissionProbMH'] * Nadu[pos,day]) / 
    ((EIP[pos,day] + mortality[pos,day] + diapause[pos,day]) * (mortality[pos,day] + diapause[pos,day]) * ((Nresadu[pos,day] + Nresjuv[pos,day])* posterior$bitingPref + Njuv[pos,day] + Nadu[pos,day]))
  

  R0.mosq.resjuv <- ifelse(Nresjuv[pos,day]>0,
                           (EIP[pos,day] * bitingRate[pos,day] * prop.bites[pos,day] * params['transmissionProbMH'] *  (Nresjuv[pos,day] * posterior$bitingPref)) / 
                             ((EIP[pos,day] + mortality[pos,day] + diapause[pos,day]) * (mortality[pos,day] + diapause[pos,day]) * ((Nresadu[pos,day] + Nresjuv[pos,day])* posterior$bitingPref + Njuv[pos,day] + Nadu[pos,day])),
                           0)
  
  R0.mosq.resadu <- (EIP[pos,day] * bitingRate[pos,day] * prop.bites[pos,day] * params['transmissionProbMH'] *  (Nresadu[pos,day] * posterior$bitingPref)) / 
    ((EIP[pos,day] + mortality[pos,day] + diapause[pos,day]) * (mortality[pos,day] + diapause[pos,day]) * ((Nresadu[pos,day] + Nresjuv[pos,day])* posterior$bitingPref + Njuv[pos,day] + Nadu[pos,day]))
  
  
  R0.juv.mosq <- ifelse(Njuv[pos,day]>0, 
                        (params['IIR'] * bitingRate[pos,day] * prop.bites[pos,day] * posterior$transmissionProbHM * Njuv[pos,day] * Nmos[pos,day]) /
                          ((params['IIR'] + params['mortality.juv']) * (params['mortality.juv'] + params['recovery'] + posterior$disIndMortality) * ((Nresadu[pos,day] + Nresjuv[pos,day])* posterior$bitingPref + Njuv[pos,day] + Nadu[pos,day]) * (Njuv[pos,day]*posterior$scalingParameter)),
                        0)
  
  R0.adu.mosq <- (params['IIR'] * bitingRate[pos,day] * prop.bites[pos,day] * posterior$transmissionProbHM * Nadu[pos,day] * Nmos[pos,day]) /
    ((params['IIR'] + params['mortality.adu']) * (params['mortality.adu'] + params['recovery'] + posterior$disIndMortality) * ((Nresadu[pos,day] + Nresjuv[pos,day])* posterior$bitingPref + Njuv[pos,day] + Nadu[pos,day]) * (Nadu[pos,day]*posterior$scalingParameter))
  

  R0.resjuv.mosq <- ifelse(Nresjuv[pos,day]>0,
                           (params['IIR'] * bitingRate[pos,day] * prop.bites[pos,day] * posterior$transmissionProbHM * (Nresjuv[pos,day] * posterior$bitingPref) * Nmos[pos,day]) /
                             ((params['IIR'] + params['mortality.juv']) * (params['mortality.juv'] + params['recovery']) * ((Nresadu[pos,day] + Nresjuv[pos,day])* posterior$bitingPref+ Njuv[pos,day] + Nadu[pos,day]) * (Nresjuv[pos,day]*posterior$scalingParameter)),
                           0)

  R0.resadu.mosq <- (params['IIR'] * bitingRate[pos,day] * prop.bites[pos,day] * posterior$transmissionProbHM * (Nresadu[pos,day] * posterior$bitingPref) * Nmos[pos,day]) /
    ((params['IIR'] + (1/posterior$lifespanR)) * ((1/posterior$lifespanR) + params['recovery']) * ((Nresadu[pos,day] + Nresjuv[pos,day])* posterior$bitingPref+ Njuv[pos,day] + Nadu[pos,day]) * (Nresadu[pos,day]*posterior$scalingParameter))

  
  NGM <- matrix(data=c(0, R0.juv.mosq, R0.adu.mosq, R0.resjuv.mosq, R0.resadu.mosq,
                       R0.mosq.juv, 0, 0, 0, 0,
                       R0.mosq.adu, 0, 0, 0, 0,
                       R0.mosq.resjuv, 0, 0, 0, 0,
                       R0.mosq.resadu, 0, 0, 0, 0),
                nrow=5, ncol=5)
  
  eigenvalues <- eigen(NGM)
  R0 <- max(abs(eigenvalues$values))
  
  # Only print every 7 days. Check if the current iteration is a multiple of 7
  if (day %% 7 == 0) {
    print(paste("R0 from NGM:", R0))
  }

  return(R0)
  
}


calculate.R0.wnv <- function(pos, day, 
                             mortality, EIP, bitingRate, diapause, bitingPref,
                             Nmaladu, Nmaljuv, Nhsadu, Nhsjuv, Nmos, 
                             prop.bites, params, posterior,
                             debug=FALSE) {
  if (debug == TRUE) {
    
    # Only print day when it changed. Check if the current day is different from the last printed day
    last_day <- NULL
    if (!is.null(last_day) && day != last_day) {
      print(paste("Day changed to:", day))
    }
    
    # Update the last_day variable
    last_day <- day
  } 
  
  # browser()
  
  R0.mosq.maljuv <- ifelse(Nmaljuv[pos,day]>0,
                        (EIP[pos,day] * bitingRate[pos,day] * prop.bites[pos,day] * params['transmissionProbMH'] * Nmaljuv[pos,day]) /
                          ((EIP[pos,day] + mortality[pos,day] + diapause[pos,day]) * (mortality[pos,day] + diapause[pos,day]) * ((Nhsadu[pos,day] + Nhsjuv[pos,day])* bitingPref + Nmaljuv[pos,day] + Nmaladu[pos,day])),
                        0)

  R0.mosq.maladu <- (EIP[pos,day] * bitingRate[pos,day] * prop.bites[pos,day] * params['transmissionProbMH'] * Nmaladu[pos,day]) / 
    ((EIP[pos,day] + mortality[pos,day] + diapause[pos,day]) * (mortality[pos,day] + diapause[pos,day]) * ((Nhsadu[pos,day] + Nhsjuv[pos,day])* bitingPref + Nmaljuv[pos,day] + Nmaladu[pos,day]))
  

  R0.mosq.hsjuv <- ifelse(Nhsjuv[pos,day]>0,
                           (EIP[pos,day] * bitingRate[pos,day] * prop.bites[pos,day] * params['transmissionProbMH'] *  (Nhsjuv[pos,day] * bitingPref)) / 
                             ((EIP[pos,day] + mortality[pos,day] + diapause[pos,day]) * (mortality[pos,day] + diapause[pos,day]) * ((Nhsadu[pos,day] + Nhsjuv[pos,day])* bitingPref + Nmaljuv[pos,day] + Nmaladu[pos,day])),
                           0)
  
  R0.mosq.hsadu <- (EIP[pos,day] * bitingRate[pos,day] * prop.bites[pos,day] * params['transmissionProbMH'] *  (Nhsadu[pos,day] * bitingPref)) / 
    ((EIP[pos,day] + mortality[pos,day] + diapause[pos,day]) * (mortality[pos,day] + diapause[pos,day]) * ((Nhsadu[pos,day] + Nhsjuv[pos,day])* bitingPref + Nmaljuv[pos,day] + Nmaladu[pos,day]))
  
  
  R0.maljuv.mosq <- ifelse(Nmaljuv[pos,day]>0, 
                        (params['IIR'] * bitingRate[pos,day] * prop.bites[pos,day] * params['transmissionProbMalMosq'] * Nmaljuv[pos,day] * Nmos[pos,day]) /
                          ((params['IIR'] + params['mortality.juv.mal']) * (params['mortality.juv.mal'] + params['recovery.mal'] + params['dis.ind.mort.mal']) * ((Nhsadu[pos,day] + Nhsjuv[pos,day])* bitingPref + Nmaljuv[pos,day] + Nmaladu[pos,day]) * (Nmaljuv[pos,day]*posterior$scalingParameter)),
                        0)
  
  R0.maladu.mosq <- (params['IIR'] * bitingRate[pos,day] * prop.bites[pos,day] * params['transmissionProbMalMosq'] * Nmaladu[pos,day] * Nmos[pos,day]) /
    ((params['IIR'] + params['mortality.adu.mal']) * (params['mortality.adu.mal'] + params['recovery.mal'] + params['dis.ind.mort.mal']) * ((Nhsadu[pos,day] + Nhsjuv[pos,day])* bitingPref + Nmaljuv[pos,day] + Nmaladu[pos,day]) * (Nmaladu[pos,day]*posterior$scalingParameter))
  

  R0.hsjuv.mosq <- ifelse(Nhsjuv[pos,day]>0,
                           (params['IIR'] * bitingRate[pos,day] * prop.bites[pos,day] * params['transmissionProbHsMosq'] * (Nhsjuv[pos,day] * bitingPref) * Nmos[pos,day]) /
                             ((params['IIR'] + params['mortality.juv.hs']) * (params['mortality.juv.hs'] + params['recovery.hs'] + params['dis.ind.mort.hs']) * ((Nhsadu[pos,day] + Nhsjuv[pos,day])* bitingPref + Nmaljuv[pos,day] + Nmaladu[pos,day]) * (Nhsjuv[pos,day]*posterior$scalingParameter)),
                           0)

  R0.hsadu.mosq <- (params['IIR'] * bitingRate[pos,day] * prop.bites[pos,day] * params['transmissionProbHsMosq'] * (Nhsadu[pos,day] * bitingPref) * Nmos[pos,day]) /
    ((params['IIR'] + params['mortality.adu.hs']) * (params['mortality.adu.hs'] + params['recovery.hs'] + params['dis.ind.mort.hs']) * ((Nhsadu[pos,day] + Nhsjuv[pos,day])* bitingPref + Nmaljuv[pos,day] + Nmaladu[pos,day]) * (Nhsadu[pos,day]*posterior$scalingParameter))

  
  NGM <- matrix(data=c(0, R0.maljuv.mosq, R0.maladu.mosq, R0.hsjuv.mosq, R0.hsadu.mosq,
                       R0.mosq.maljuv, 0, 0, 0, 0,
                       R0.mosq.maladu, 0, 0, 0, 0,
                       R0.mosq.hsjuv, 0, 0, 0, 0,
                       R0.mosq.hsadu, 0, 0, 0, 0),
                nrow=5, ncol=5)
  
  eigenvalues <- eigen(NGM)
  R0 <- max(abs(eigenvalues$values))
  
  # Only print every 7 days. Check if the current iteration is a multiple of 7
  if (day %% 7 == 0) {
    print(paste("R0 from NGM:", R0))
  }

  return(R0)
  
}


# Generation time ---------------------------------------------------------

calculate.genTime <- function(pos, day, disease,
                         mortality, EIP, 
                         params, posterior,
                         debug=FALSE) {
  if (debug == TRUE) {
    
    # Only print day when it changed. Check if the current day is different from the last printed day
    last_day <- NULL
    if (!is.null(last_day) && day != last_day) {
      print(paste("Day changed to:", day))
    }
    
    # Update the last_day variable
    last_day <- day
  } 
  
  # browser()
  if (disease == "usuv") {
    total.clearance.rate <- params['recovery'] + # recovery rate (same for both)
      posterior$disIndMortality / 2 + # dis-ind mortality (take mean for blackbird and reservoir)
      params['mortality.adu']*(1/4) + params['mortality.juv']*(1/4) + 
       (1/posterior$lifespanR)*(1/4) + params['mortality.juv']*(1/4) # natural mortality (take mean) 
    
    generationTime <- ifelse(EIP[pos,day]>0, (1/EIP[pos,day]) + (1/params['IIR']) + (1/mortality[pos,day])/2 + (1/total.clearance.rate)/2, NA)
    
  } else if (disease == "wnv") {
    
    total.clearance.rate <- (params['recovery.mal'] + params['recovery.hs']) / 2 +  # recovery rate (take mean)
      (params['dis.ind.mort.mal'] + params['dis.ind.mort.hs']) / 2 + # dis-ind mortality (take mean)
      params['mortality.adu.mal']*(1/4) + params['mortality.juv.mal']*(1/4) + 
      params['mortality.adu.hs']*(1/4) + params['mortality.juv.hs']*(1/4)  # natural mortality (take mean) 
    
     generationTime <- ifelse(EIP[pos,day]>0, (1/EIP[pos,day]) + (1/params['IIR']) + (1/mortality[pos,day])/2 + (1/total.clearance.rate)/2, NA)
    
  }
 
  return(generationTime)
}
  
