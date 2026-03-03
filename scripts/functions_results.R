

combine_rds_files <- function(file_paths) {
  combined_data <- list()

  for (file in file_paths) {
    data <- readRDS(file)
    
    if (is.null(combined_data)) {
      combined_data <- data
    } else {
      combined_data <- c(combined_data, list(data))
    }
  } 
  
  return(combined_data)
}


# Overall -----------------------------------------------------------------

calc.noGens <- function(disease, scenario, runtype, simOutput.r0, simOutput.gen) {

    if (runtype=="single"){
    output <- readRDS(file=paste0("../Output/", scenario, "/", disease, ".genTime.rds", sep=""))
    output.r0 <- readRDS(file=paste0("../Output/", scenario, "/", disease, ".R0.rds", sep=""))
    output$R0 <- output.r0$R0 # add generation time and R0 into same dataframe
    
    output <- output %>%
      mutate(above = ifelse(R0>1, 1, 0)) %>%
      group_by(x,y) %>%
      summarise(seasonLength = max(rle(above)$lengths[rle(above)$values == 1]), #longest sequence of R>1
                meanGenTime = mean(genTime, na.rm=T)) %>% 
      mutate(noGens = seasonLength / meanGenTime)
    
    
  } else if (runtype=="multi") {

    simoutput.r0.scenario <- simOutput.r0[[scenario]]
    simoutput.genTime.scenario <- simOutput.genTime[[scenario]]
    
    # add gen time and r0 into same dataframe
    for (i in 1:length(simoutput.r0.scenario)) {
      simoutput.r0.scenario[[i]]$genTime <- simoutput.genTime.scenario[[i]]$genTime
    }
    
    output <- data.frame(matrix(nrow=1, ncol=0))
    
    for (ii in 1:length(simoutput.r0.scenario)) {
      # first calculate national average R0 value
      output.list_i <- simoutput.r0.scenario[[ii]] %>%
        mutate(above = ifelse(R0>1, 1, 0)) %>%
        group_by(x,y) %>%
        summarise(seasonLength = max(rle(above)$lengths[rle(above)$values == 1]), #longest sequence of R>1
                  meanGenTime = mean(genTime, na.rm=T)) %>% 
        mutate(noGens = seasonLength / meanGenTime) %>%
        ungroup() %>%
        summarise(seasonLength = mean(seasonLength),
                  meanGenTime = mean(meanGenTime),
                  noGens = mean(noGens))
      
      output <- cbind(output, output.list_i$seasonLength, output.list_i$meanGenTime, output.list_i$noGens)
    }
    
    colnames(output) <- c(paste(c("seasonLength", "meanGenTime", "noGens"), rep(1:length(simoutput.r0.scenario), each = 3), sep="_"))
    
    output <- output %>%
      mutate(mean.seasonLength = mean(c_across(c(all_of(grep("seasonLength", names(.), fixed=TRUE)))), na.rm=T),
                lb.seasonLength = quantile(c_across(c(all_of(grep("seasonLength", names(.), fixed=TRUE)))), c(0.025, 0.975), na.rm=T)[1],
                ub.seasonLength = quantile(c_across(c(all_of(grep("seasonLength", names(.), fixed=TRUE)))), c(0.025, 0.975), na.rm=T)[2],
                
                mean.genTime = mean(c_across(c(all_of(grep("meanGenTime", names(.), fixed=TRUE)))), na.rm=T),
                lb.genTime = quantile(c_across(c(all_of(grep("meanGenTime", names(.), fixed=TRUE)))), c(0.025, 0.975), na.rm=T)[1],
                ub.genTime = quantile(c_across(c(all_of(grep("meanGenTime", names(.), fixed=TRUE)))), c(0.025, 0.975), na.rm=T)[2],
                
                mean.noGens = mean(c_across(c(all_of(grep("noGens", names(.), fixed=TRUE)))), na.rm=T),
                lb.noGens = quantile(c_across(c(all_of(grep("noGens", names(.), fixed=TRUE)))), c(0.025, 0.975), na.rm=T)[1],
                ub.noGens = quantile(c_across(c(all_of(grep("noGens", names(.), fixed=TRUE)))), c(0.025, 0.975), na.rm=T)[2])
    
  } 
  return(output)
}

# Over time ---------------------------------------------------------------

calc.r0.day <- function(disease, scenario, runtype, simOutput) {
  
  if (runtype=="single"){
    output <- readRDS(file=paste0("../Output/", scenario, "/", disease, ".R0.rds", sep=""))

    output <- output %>%
      group_by(day) %>%
      summarise(mean = mean(R0),
                lb = quantile(R0, c(0.025, 0.975))[1],
                ub = quantile(R0, c(0.025, 0.975))[2]) 
    
    output <- transform(output, avg_mean = rollmeanr(mean, 7, fill=NA))
    output <- transform(output, avg_lb = rollmeanr(lb, 7, fill=NA))
    output <- transform(output, avg_ub = rollmeanr(ub, 7, fill=NA))
    
  } else if (runtype=="multi") {

    simoutput.scenario <- simOutput[[scenario]]
    output <- data.frame(matrix(nrow=length(unique(simoutput.scenario[[1]]$day)), ncol=0))
    
    for (ii in 1:length(simoutput.scenario)) {
      # first calculate national average R0 value
      output.list_i <- simoutput.scenario[[ii]] %>%
        group_by(day) %>%
        summarise(mean = mean(R0)) 
      
      output <- cbind(output, output.list_i$mean)
    }
    
    # add date and coordinates column
    output <- cbind(output, output.list_i$day)
    
    colnames(output) <- c(paste(c("R0"), rep(1:length(simoutput.scenario), each = 1), sep="_"), "day")
    
    output <- output %>%
      group_by(day) %>%
      mutate(mean = mean(c_across(c(all_of(grep("R0", names(.), fixed=TRUE)))), na.rm=T),
                lb = quantile(c_across(c(all_of(grep("R0", names(.), fixed=TRUE)))), c(0.025, 0.975), na.rm=T)[1],
                ub = quantile(c_across(c(all_of(grep("R0", names(.), fixed=TRUE)))), c(0.025, 0.975), na.rm=T)[2]) 
    
    output <- transform(output, avg_mean = rollmeanr(mean, 7, fill=NA))
    output <- transform(output, avg_lb = rollmeanr(lb, 7, fill=NA))
    output <- transform(output, avg_ub = rollmeanr(ub, 7, fill=NA))
    
  } 
  return(output)
}


calc.prop.above <- function(disease, scenario, runtype, simOutput) {
  
  if (runtype=="single"){
    output <- readRDS(file=paste0("../Output/", scenario, "/", disease, ".R0.rds", sep=""))

    output <- output %>%
      mutate(above = ifelse(R0>1, 1, 0)) %>%
      group_by(day) %>%
      summarise(totalAbove = sum(above),
                propAbove = totalAbove/1398) 
    
    output <- transform(output, propAbove_7d = rollmeanr(propAbove, 7, fill=NA))

    
  } else if (runtype=="multi") {

    simoutput.scenario <- simOutput[[scenario]]
    output <- data.frame(matrix(nrow=length(unique(simoutput.scenario[[1]]$day)), ncol=0))
    
    for (ii in 1:length(simoutput.scenario)) {
      output.list_i <- simoutput.scenario[[ii]] %>%
        mutate(above = ifelse(R0>1, 1, 0)) %>%
        group_by(day) %>%
        summarise(totalAbove = sum(above),
                  propAbove = totalAbove/1398) 
      
      output <- cbind(output, output.list_i$propAbove)
    }
    
    # add date and coordinates column
    output <- cbind(output, output.list_i$day)
    
    colnames(output) <- c(paste(c("propAbove"), rep(1:length(simoutput.scenario), each = 1), sep="_"), "day")
    
    output <- output %>%
      group_by(day) %>%
      mutate(mean = mean(c_across(c(all_of(grep("propAbove", names(.), fixed=TRUE)))), na.rm=T),
             lb = quantile(c_across(c(all_of(grep("propAbove", names(.), fixed=TRUE)))), c(0.025, 0.975), na.rm=T)[1],
             ub = quantile(c_across(c(all_of(grep("propAbove", names(.), fixed=TRUE)))), c(0.025, 0.975), na.rm=T)[2]) 
    
    output <- transform(output, avg_mean = rollmeanr(mean, 7, fill=NA))
    output <- transform(output, avg_lb = rollmeanr(lb, 7, fill=NA))
    output <- transform(output, avg_ub = rollmeanr(ub, 7, fill=NA))
    
  } 
  return(output)
}

# growth rate is ln(R)/T
# doubling time = ln(2)/growth rate

calc.genTime.day <- function(disease, scenario, runtype, simOutput.r0, simOutput.genTime) {

    if (runtype=="single"){
    output <- readRDS(file=paste0("../Output/", scenario, "/", disease, ".genTime.rds", sep=""))
    output.r0 <- readRDS(file=paste0("../Output/", scenario, "/", disease, ".R0.rds", sep=""))
    output$R0 <- output.r0$R0 # add generation time and R0 into same dataframe
    
    output <- output %>%
      group_by(day) %>%
      mutate(meanR0 = mean(R0)) %>%
      mutate(meanGenTime = mean(genTime)) %>%
      mutate(growthRate = if_else(meanR0>1, log(meanR0) / meanGenTime, NA)) %>%
      mutate(dTime = if_else(growthRate>0, log(2)/growthRate, NA)) %>%     # force doubling time to NA when growth rate is negative

      summarise(genTime.median = median(genTime, na.rm=T),
                genTime.lb = quantile(genTime, c(0.025, 0.975), na.rm=T)[1],
                genTime.ub = quantile(genTime, c(0.025, 0.975), na.rm=T)[2],

                growthRate.median = median(growthRate, na.rm=T),
                growthRate.lb = quantile(growthRate, c(0.025, 0.975), na.rm=T)[1],
                growthRate.ub = quantile(growthRate, c(0.025, 0.975), na.rm=T)[2],

                dTime.median = median(dTime, na.rm=T),
                dTime.lb = quantile(dTime, c(0.025, 0.975), na.rm=T)[1],
                dTime.ub = quantile(dTime, c(0.025, 0.975), na.rm=T)[2])


    output <- transform(output, avg_median.genTime = rollmeanr(genTime.median, 7, fill=NA))
    output <- transform(output, avg_lb.genTime = rollmeanr(genTime.lb, 7, fill=NA))
    output <- transform(output, avg_ub.genTime = rollmeanr(genTime.ub, 7, fill=NA))

    output <- transform(output, avg_median.growthRate = rollmeanr(growthRate.median, 7, fill=NA))
    output <- transform(output, avg_lb.growthRate = rollmeanr(growthRate.lb, 7, fill=NA))
    output <- transform(output, avg_ub.growthRate = rollmeanr(growthRate.ub, 7, fill=NA))

    output <- transform(output, avg_median.dTime = rollmeanr(dTime.median, 7, fill=NA))
    output <- transform(output, avg_lb.dTime = rollmeanr(dTime.lb, 7, fill=NA))
    output <- transform(output, avg_ub.dTime = rollmeanr(dTime.ub, 7, fill=NA))
    
  } else if (runtype=="multi") {

    simoutput.r0.scenario <- simOutput.r0[[scenario]]
    simoutput.genTime.scenario <- simOutput.genTime[[scenario]]
    
    # add gen time and r0 into same dataframe
    for (i in 1:length(simoutput.r0.scenario)) {
    simoutput.r0.scenario[[i]]$genTime <- simoutput.genTime.scenario[[i]]$genTime
    }
    
    output <- data.frame(matrix(nrow=length(unique(simoutput.r0.scenario[[1]]$day)), ncol=0))

    for (ii in 1:length(simoutput.r0.scenario)) {
      # first calculate national average R0 value
      output.list_i <- simoutput.r0.scenario[[ii]] %>%
        group_by(day) %>%
        mutate(meanR0 = mean(R0)) %>%
        mutate(meanGenTime = mean(genTime)) %>%
        mutate(growthRate = if_else(meanR0>1, log(meanR0) / meanGenTime, NA)) %>%
        mutate(dTime = if_else(growthRate>0, log(2)/growthRate, NA))     # force doubling time to NA when growth rate is negative
        
      output <- cbind(output, output.list_i$meanGenTime, output.list_i$growthRate, output.list_i$dTime)
    }
    
    # add date and coordinates column
    output <- cbind(output, output.list_i$day)
    
    # browser()
    colnames(output) <- c(paste(c("genTime", "growthRate", "dTime"), rep(1:length(simoutput.r0.scenario), each = 3), sep="_"), "day")
    
    output <- output %>%
      group_by(day) %>%
      summarise(genTime.median = median(c_across(c(all_of(grep("genTime", names(.), fixed=TRUE)))), na.rm=T),
                genTime.lb = quantile(c_across(c(all_of(grep("genTime", names(.), fixed=TRUE)))), c(0.025, 0.975), na.rm=T)[1],
                genTime.ub = quantile(c_across(c(all_of(grep("genTime", names(.), fixed=TRUE)))), c(0.025, 0.975), na.rm=T)[2],
                
                growthRate.median = median(c_across(c(all_of(grep("growthRate", names(.), fixed=TRUE)))), na.rm=T),
                growthRate.lb = quantile(c_across(c(all_of(grep("growthRate", names(.), fixed=TRUE)))), c(0.025, 0.975), na.rm=T)[1],
                growthRate.ub = quantile(c_across(c(all_of(grep("growthRate", names(.), fixed=TRUE)))), c(0.025, 0.975), na.rm=T)[2], 
                
                dTime.median = median(c_across(c(all_of(grep("dTime", names(.), fixed=TRUE)))), na.rm=T),
                dTime.lb = quantile(c_across(c(all_of(grep("dTime", names(.), fixed=TRUE)))), c(0.025, 0.975), na.rm=T)[1],
                dTime.ub = quantile(c_across(c(all_of(grep("dTime", names(.), fixed=TRUE)))), c(0.025, 0.975), na.rm=T)[2]) 
    
    output <- transform(output, avg_median.genTime = rollmeanr(genTime.median, 7, fill=NA))
    output <- transform(output, avg_lb.genTime = rollmeanr(genTime.lb, 7, fill=NA))
    output <- transform(output, avg_ub.genTime = rollmeanr(genTime.ub, 7, fill=NA))
    
    output <- transform(output, avg_median.growthRate = rollmeanr(growthRate.median, 7, fill=NA))
    output <- transform(output, avg_lb.growthRate = rollmeanr(growthRate.lb, 7, fill=NA))
    output <- transform(output, avg_ub.growthRate = rollmeanr(growthRate.ub, 7, fill=NA))
    
    output <- transform(output, avg_median.dTime = rollmeanr(dTime.median, 7, fill=NA))
    output <- transform(output, avg_lb.dTime = rollmeanr(dTime.lb, 7, fill=NA))
    output <- transform(output, avg_ub.dTime = rollmeanr(dTime.ub, 7, fill=NA))
    
  } 
  return(output)
}




# Over space --------------------------------------------------------------


# * Single simulation ----------------------------------------------------

# ** Annual mean maps ----------------------------------------------------

calc.r0.loc <- function(disease, scenario) {
  
  output <- readRDS(file=paste0("../Output/", scenario, "/", disease, ".R0.rds", sep=""))
  
  output$date <- as.Date(output$day, origin = "2021-12-31")
  output$month <- month(output$date)
  
  output <- output %>%
    filter(month<11) %>% # remove accidental november days
    group_by(x,y) %>%
    summarise(mean = mean(R0),
              lb = quantile(R0, c(0.025, 0.975))[1],
              ub = quantile(R0, c(0.025, 0.975))[2]) 
  
  return(output)
}



# ** Monthly mean maps ----------------------------------------------------

calc.r0.loc.month <- function(disease, scenario) {
  
  output <- readRDS(file=paste0("../Output/", scenario, "/", disease, ".R0.rds", sep=""))
  output$date <- as.Date(output$day, origin = "2021-12-31")
  output$month <- month(output$date)
  
  output <- output %>%
    filter(month<11) %>%
    # filter(month>3) %>%
    group_by(x,y, month) %>%
    summarise(mean = mean(R0)) 
  
  return(output)
}


# ** Doubling time --------------------------------------------------------

calc.doubling.loc <- function(disease, scenario) {
  # browser()
  
  output <- readRDS(file=paste0("../Output/", scenario, "/", disease, ".genTime.rds", sep=""))
  output.r0 <- readRDS(file=paste0("../Output/", scenario, "/", disease, ".R0.rds", sep=""))
  output$R0 <- output.r0$R0 # add generation time and R0 into same dataframe
  
  output$date <- as.Date(output$day, origin = "2021-12-31")
  output$month <- month(output$date)
  
  output <- output %>%
    filter(month<11) %>% # remove accidental november days
    mutate(growthRate = if_else(R0>1, log(R0) / genTime, NA)) %>%
    mutate(dTime = if_else(growthRate>0, log(2)/growthRate, NA)) %>%     # force doubling time to NA when growth rate is negative
    
    group_by(x,y) %>%

    summarise(R0 = mean(R0),
              genTime = mean(genTime, na.rm=T),
              dTime.median = median(dTime, na.rm=T),
              dTime.lb = quantile(dTime, c(0.025, 0.975), na.rm=T)[1],
              dTime.ub = quantile(dTime, c(0.025, 0.975), na.rm=T)[2])
  
  return(output)
}



# Province and land use analyses ------------------------------------------

# Function to extract values per province
add.province <- function(r0_file, shapefile_provinces) {

    # Load raster
  raster_data <- rast(r0_file)
  
  # Load shapefile (provinces)
  provinces <- vect(shapefile_provinces)
  
  # Ensure CRS (Coordinate Reference System) matches between raster and shapefile
  crs(raster_data) <- "EPSG:28992"
  if (!identical(crs(raster_data), crs(provinces))) {
    provinces <- project(provinces, crs(raster_data))
  }
  
  # Rasterize the shapefile to match the raster resolution and extent
  province_raster <- rasterize(provinces, raster_data, field = "statnaam")  
  
  # Combine the original raster with the new province raster (as a new layer)
  combined_raster <- c(raster_data, province_raster)
  
  # Convert the combined raster to a dataframe
  combined_df <- terra::as.data.frame(combined_raster, xy = TRUE)
  
  return(combined_df)
}

add.landuse <- function(scenario, modelraster, r0.file) {
  # load land use file
  landuseMap <- rast(paste0("../Data/land_use/", scenario, ".asc"))
  
  # Check CRS of both rasters
  crs(landuseMap) <- "EPSG:28992" 

  # project land use raster to model raster using nearest neighbour method
  lu.resampled <- crop(landuseMap, modelraster)
  lu.resampled <- project(lu.resampled,modelraster,method='near') 

  lu.resampled.DF <- terra::as.data.frame(lu.resampled, xy=TRUE, cells=FALSE, na.rm=FALSE)
  mergedDF <- merge(r0.file, lu.resampled.DF, by = c("x", "y"))
  
  colnames(mergedDF) <- c("x", "y", "mean", "lb", "ub", "landuse")
  
  mergedDF <- mergedDF %>%
    mutate(landuse.string = case_when(landuse == 0 ~ "Urban",
                                      landuse == 1 ~ "Pasture",
                                      landuse == 2 ~ "Crops",
                                      landuse == 3 ~ "Forest",
                                      landuse == 4 ~ "Non-forest nature"))
  
  return(mergedDF)
}

