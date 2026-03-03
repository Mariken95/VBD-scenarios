

# Blackbird abundance from Random Forest model ----------------------------

create.birdscenario <- function(scenario, species, columnName, modelraster, outputfile) {
  
  filename.bird <- paste(scenario, "_", species, ".asc", sep="")
  files.bird <- dir("C:/Users/wit109/OneDrive - Wageningen University & Research/Data/ChangeScenarios/bird predictions")
  files.bird <- files.bird[grep(filename.bird, files.bird, TRUE)]
  
  setwd("C:/Users/wit109/OneDrive - Wageningen University & Research/Data/ChangeScenarios/bird predictions")
  
  birdabundance <- create.birdabundance(abundancemaps = files.bird, columnName = columnName, modelraster = modelraster, 
                                        outputfile = outputfile, verbose=0)
  
  return(birdabundance)
  
}

create.birdabundance <- function(abundancemaps, columnName, modelraster, outputfile, verbose) {
  if (verbose==1) { 
    browser()
  }
  
  for (i in 1:length(abundancemaps)) { #length(abundancemaps)
    # browser()
    print(i)
    
    # 1. Load data
    # raster object contained relative bird abundance from Martha's model already loaded
    abundancemap <- rast(abundancemaps[i])

    
    # 2. Turn raw bird unit raster into spatial dataframe to allow sum and mean to be calculated
    rasterBirdAb.DF <- as.data.frame(abundancemap, xy=TRUE) 

    # 3. Extract/transform bird units to fit raster used in model
    # crop bird units raster to be the same size as model raster
    rasterBirdAbCropped <- crop(abundancemap, modelraster)
    # resample to same grid as model raster
    rasterBirdAbresampled=resample(rasterBirdAbCropped,modelraster,method='bilinear') # two cells get NaN

    # turn bird unit raster into spatial dataframe 
    croppedBirdAbraster.DF <- as.data.frame(rasterBirdAbresampled, xy=TRUE) 
    
    # calculate total number of bird units (sum of all raster values)
    print(sum(croppedBirdAbraster.DF[3], na.rm=TRUE)) # For 5x5km: sum is around 2200 for old version, 2400 for new version
    
    # For 5x5:
    # 2879 bird units in total 
    # corresponds to 500,000-900,000 blackbirds https://www.vogelbescherming.nl/ontdek-vogels/kennis-over-vogels/vogelgids/vogel/merel
    # so a value of 1 unit in raster equals 700,000/2900 = 243 blackbirds 
    
    # 4. Turn bird unit values into real number of birds. see calculation above
    croppedBirdAbraster.DF[3] <- croppedBirdAbraster.DF[3]*243
    # check if sum of birds adds up to 700,000
    print(sum(croppedBirdAbraster.DF[3], na.rm=TRUE)) # sum is close to 700,000 so ok
    
    # 5. Add bird abundance to skeleton raster
    outputfile <- left_join(outputfile, croppedBirdAbraster.DF, by=c("x", "y")) 
    # if NA (bc bird raster misses some values on the border, slightly different shapefile),
    # take value from cell below
    birdabundance <- fill(outputfile, grep(paste(columnName), names(outputfile)), .direction="up") 
    
  }
  
  return(birdabundance)
}

# Mosquito abundance from Random Forest model ----------------------------


create.mosquitoabundance <- function(abundancemaps, modelraster, outputfile, verbose=0) {
  if (verbose==1) { 
    browser()
  }
  
  for (i in 1:length(abundancemaps)) { #length(abundancemaps)
    
    print(i)
    
    # 1. Load data
    abundancemap <- rast(abundancemaps[i])
    # 2. Extract/transform to fit raster used in model
    # crop raster to be the same size as model raster
    rasterMosqAbCropped <- crop(abundancemap, modelraster)
    # resample to same grid as model raster
    rasterMosqAbresampled=resample(rasterMosqAbCropped,modelraster,method='bilinear') 


    # 3. Multiply raw numbers for more realistic mosquito numbers
    # turn into spatial DF to allow joining
    croppedMosqAbraster.DF <- as.data.frame(rasterMosqAbresampled, xy=TRUE) 
    croppedMosqAbraster.DF[,-c(1:2)] <- croppedMosqAbraster.DF[,-c(1:2)] * 1000
    
    # 4. Add mosquito abundance to skeleton raster
    outputfile <- left_join(outputfile, croppedMosqAbraster.DF, by=c("x", "y")) 
    # if NA (bc mosquito raster misses some values on the border, slightly different shapefile),
    # take value from cell below
    mosquitoabundance <- fill(outputfile, everything(), .direction="up")
    
  }
  
  return(mosquitoabundance)
}


# Temperature -------------------------------------------------------------

## Step 1
# - reads KNMI scenario file
# - averages the data across the 8 ensemble models
# - takes a defined list of days (e.g. 'June 4th')
# - calculates the mean temperature for that day, averaged over all years in the KNMI file
# - outputs a raster file for each day

#load data and calculate mean across ensembles
getEnsMean<-function(fpath){
  tnc <- tidync::tidync(fpath)
  data <- hyper_tibble(tnc)
  ensList<-list()
  for(i in 1:8){
    ensList[[i]]<-data[data$ens==i,]
  }
  rm(data)
  data2<-join_all(ensList, by=c('lon','lat','time'), type='left')
  rm(ensList)
  while('ens'%in%colnames(data2)){
    data2$ens<-NULL
  }
  data2<-data2[,c(4,2,3,1,5:11)]
  data2$values<-rowMeans(data2[,4:11])
  data2$time<-floor(data2$time)
  baseDate <- ymd("2036-01-01")
  data2$date<-baseDate+days(data2$time)
  return(data2[,c(1:3,12,13)])
}

#Make temperature raster
makeTempRaster<-function(predictionDate,data,scenario){
  predictionDate<-as.Date(predictionDate,origin = "1970-01-01")
  data$day<-format(data$date,"%m%d")
  predictionDateF<-format(predictionDate,"%m%d")
  dayData<-data[data$day==predictionDateF,]
  averaged<-aggregate(dayData$values, list(dayData$lon,dayData$lat), FUN=mean)
  colnames(averaged)<-c('lon','lat','values')
  sp_av<-st_as_sf(averaged,coords = c("lon", "lat"), crs = 4326)
  Rfile<-rasterize(sp_av, example_raster, field='values', fun='last', background=NA, na.rm=TRUE,)
  reprojR<-projectRaster(Rfile,crs=28992)
  resampR<-resample(reprojR,rasterTemplate,method='bilinear')
  finalR<-fillMissing(resampR)
  dateFormat<-paste0(format(predictionDate,"%d"),format(predictionDate,"%m"))
  writeRaster(finalR,paste0("../Data/temperature/", scenario,"_",dateFormat,".asc"),format="ascii",overwrite=TRUE)
}

#function to fill in missing values (because scaling from 12km grid to 1km grid - some cells will be missing)
fillMissing<-function(R){
  values(R)[naMask]<-NA
  #identify missing values which are not missing in land use rasters
  missingVals <- setdiff(which(is.na(values(R))),which(is.na(values(sampleR))))
  #interpolate - may need to do this multiple times
  na_count <- terra::freq(R, value = NA)
  while(na_count > terra::freq(sampleR, value = NA)){
    R <- focal(R, w=matrix(1,3,3), fun=mean,na.rm=TRUE,NAonly=TRUE)
    rMasked<-R
    values(rMasked)[naMask]<-NA
    na_count <- terra::freq(rMasked, value = NA)
  }
  rMasked<-R
  values(rMasked)[naMask]<-NA
  return(rMasked)
}


## Step 2
# - loop through all daily raster files
# - resample to model grid
# - add into one file, ordered by date

create.temperature <- function(temperaturemaps, modelraster, outputfile.full, outputfile.nonNA) {
  
  raster_list <- list()
  # browser()
  
  for(i in temperaturemaps){ 
    print(i)
    
    temp_raster <- rast(i)
    
    # crop temperature raster to be the same size as model raster
    rasterTempCropped <- crop(temp_raster, modelraster)
    
        # Resample the temperature raster to match the resolution and extent of the model raster
    temp_raster_resampled <- resample(rasterTempCropped, modelraster, method = "bilinear")
    
    # Extract the values and add them to the list
    raster_values <- values(temp_raster_resampled)
    raster_list[[i]] <- raster_values
  }  
  
  browser()
  # Combine the raster values into a single dataframe
  combined_df <- data.frame(do.call(cbind, raster_list))
  
  # Rename the columns with the dates
  colnames(combined_df) <- gsub("^.{3}", "2050", files.temp) # remove first three characters that refer to the scenario (or first four for ref)
  colnames(combined_df) <- gsub(".asc", "", colnames(combined_df))
  
  # Remove 29 february
  combined_df <- combined_df[, !colnames(combined_df) == "20502902"] 
  
  # Convert column names to Date objects
  date_columns <- as.Date(colnames(combined_df), format = "%Y%d%m")
  
  # Sort the column names chronologically
  sorted_columns <- colnames(combined_df)[order(date_columns)]
  
  # Reorder the dataframe columns
  combined_df <- combined_df[, sorted_columns]
  
  # Extract column names 
  column_names <- colnames(combined_df)
  
  # Mutate the column names
  new_column_names <- as.Date(column_names, format = "%Y%d%m")
  
  # Set the mutated column names
  colnames(combined_df)<- new_column_names
  
  # Extract the first two columns of the model raster to add coordinates
  coordinates <- outputfile.full[, 1:2]
  
  # Combine the dataframes
  temperature_file <- cbind(coordinates, combined_df)
  
  # Add values to skeleton raster to ensure the same squares are used
  temperature_file <- left_join(outputfile.nonNA, temperature_file, by=c("x", "y")) 
  # if NA (bc misses some values on the border, slightly different shapefile), take value from cell below
  temperature_file <- fill(temperature_file, everything(), .direction = "up")
  
  return(temperature_file)
}


