
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
{
  # for bird abundance
  library(raster) 
  library(tidyverse)
  library(tidyr)
  
  # for temperature
  library(tidync) # package to deal with netcdf files
  library(sf)     # spatial functions
  library(tidyverse)
  library(raster)
  library(plyr)
  library(terra)
  library(stringr)

  # for mosquito
  library(zoo) # to take weekly average
  
  # find day number
  library(lubridate)
  
  # for scales in maps
  library(scales)
}

source("functions_inputData.R")

modelraster <- rast("../Data/model_input/spatialDF5kmNoWater.tif")
outputfile.full <- subset(terra::as.data.frame(modelraster, xy=TRUE, 
                                          cells=FALSE, na.rm=FALSE), select=c("x", "y"))
outputfile.nonNA <- subset(terra::as.data.frame(modelraster, xy=TRUE, 
                                               cells=FALSE, na.rm=TRUE), select=c("x", "y"))

# Temperature -------------------------------------------------------------

# * 1. create daily raster file ----------------------------------------------

## Step 1 (from Martha)
# - reads KNMI scenario file
# - averages the data across the 8 ensemble models
# - takes a defined list of days (e.g. 'June 4th')
# - calculates the mean temperature for that day, averaged over all years in the KNMI file
# - outputs a raster file for each day

#load raster template (this 1km grid was used for all mosquito and bird modelling so I transform KNMI data to this grid)
rasterTemplate<-raster("../Data/rasterTemplateTemperature.asc")
crs(rasterTemplate)<-"EPSG:28992"

#define template for KNMI23 files (12km grid)
example_raster <- raster(crs = 4326, vals = NA, nrows=27,ncols=30, ext = extent(c(3.4625, 7.2125, 50.75, 53.45)))

#define NA cells (i.e. sea or different country)
#(this could be anything, doesn't have to be current land use. The point is to use something which tells you which cells have value 'NA')
sampleR<-raster("../Data/current land use.asc")
naMask<-which(is.na(values(sampleR)))

#average temperature data across ensembles
tempFile<-'C:/Users/wit109/OneDrive - Wageningen University & Research/Data/ChangeScenarios/temperature/tas_Md_2050_interp.nc' # specify scenario here
tempScenData<-getEnsMean(tempFile)

#define dates of interest (year does not matter - this code averages over all years in the dataset)
startDate<-as.Date("2036/01/01",format="%Y/%m/%d") 
endDate<-as.Date("2036/12/31",format="%Y/%m/%d")
allDates<-seq(startDate,endDate,by=1)

#calculate mean temperature for each day
#saves output raster to working directory 
for (i in 1:length(allDates)) {
  print(allDates[i])
  makeTempRaster(predictionDate=allDates[i], data=tempScenData, scenario="Md") # specify scenario here
}

#check output
plot(raster("ref_0701.asc"))


# * 2. combine to one file ---------------------------------------------------
plot(rast("Md_1207.asc"))
terra::values(rast("Ld_1207.asc"))[1:10]



## Step 2
# - loop through all daily raster files
# - resample to model grid
# - add into one file, ordered by date

filename.temp <- "Mn"
files.temp <- dir("../Data/temperature/")
files.temp <- files.temp[grep(filename.temp, files.temp, TRUE)]

setwd("../Data/temperature/")

temperature <- create.temperature(temperaturemaps = files.temp, modelraster = modelraster, 
                                  outputfile.full = outputfile.full, outputfile.nonNA = outputfile.nonNA)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
save(temperature,file="../Data/model_input/ssp4/temperature5km.ssp4.wet.RData")


# * 3. combine nat and droog ---------------------------------------------------

## Step 3
# - average between nat and droog scenarios
# - create dataframe with temperature values (row=raster location, column=date)

load("../Data/model_input/ssp4/temperature5km.ssp4.dry.RData")
dry <- temperature
load("../Data/model_input/ssp4/temperature5km.ssp4.wet.RData")
wet <- temperature

# Initialize an empty dataframe to store the result
result_df <- data.frame(x = dry$x, y = dry$y)

# Loop through each date column and calculate the mean for each cell
for (i in 3:(ncol(dry))) {
  result_df[, i] <- (dry[, i] + wet[, i]) / 2
}

colnames(result_df) <- colnames(dry)
temperature <- result_df
save(temperature, file= "../Data/model_input/ssp4/temperature5km.ssp4.mean.RData")

# SSP 3 and 5 use the same temperature data (scenario H), so the same file (temperature5km.ssp5.mean.RData) is saved in both folders with a different ssp

# * plot over time ------------------------------------------------------------------
load("C:/Repos/change-scenarios/Data/model_input/ref/temperature5km.ref.RData")
ref <- temperature[,3:367]
ref <- as.data.frame(colMeans(ref))
load("C:/Repos/change-scenarios/Data/model_input/ssp1/temperature5km.ssp1.mean.RData")
ssp1 <- temperature[,3:367]
ssp1 <- as.data.frame(colMeans(ssp1))
load("C:/Repos/change-scenarios/Data/model_input/ssp3/temperature5km.ssp3.mean.RData")
ssp3 <- temperature[,3:367]
ssp3 <- as.data.frame(colMeans(ssp3))
load("C:/Repos/change-scenarios/Data/model_input/ssp4/temperature5km.ssp4.mean.RData")
ssp4 <- temperature[,3:367]
ssp4 <- as.data.frame(colMeans(ssp4))

total <- cbind(ref, ssp1, ssp3, ssp4)
colnames(total) <- c("Reference", "SSP 1", "SSP 3/5", "SSP 4")
total$day <- seq(from = 1, to = 365)
total <- total[92:306,]

total <- total %>%   
    pivot_longer(cols=c(1:4), names_to = "Scenario", values_to = "Temperature")
  
scenario <- c("Reference" = "black",  
              "SSP 1" = "chartreuse4",
              "SSP 3/5" = "orangered2",
              "SSP 4" = "plum3")

ggplot(data=total) +
  geom_line(aes(x=day, y=Temperature, group=Scenario, colour=Scenario), linewidth=1) +
  ylab("Mean daily temperature") + xlab("") +
  scale_x_continuous(breaks = c(92,153,214,275), 
                     labels = c("April","June","August","October")) +  
  theme_bw() +
  scale_colour_manual(values=scenario) +
  theme(axis.text=element_text(size=16), axis.title = element_text(size=16), legend.text = element_text(size=16),
        legend.title = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())


ggsave(file="../Output/Plots/InputData/temperature.png", width=7, height=5)

# * plot over space ------------------------------------------------------------------
load("C:/Repos/change-scenarios/Data/model_input/ref/temperature5km.ref.RData")
ref <- temperature[,93:306] # April to Nov
ref <- as.data.frame(rowMeans(ref))
load("C:/Repos/change-scenarios/Data/model_input/ssp1/temperature5km.ssp1.mean.RData")
ssp1 <- temperature[,93:306]
ssp1 <- as.data.frame(rowMeans(ssp1))
load("C:/Repos/change-scenarios/Data/model_input/ssp3/temperature5km.ssp3.mean.RData")
ssp3 <- temperature[,93:306]
ssp3 <- as.data.frame(rowMeans(ssp3))
load("C:/Repos/change-scenarios/Data/model_input/ssp4/temperature5km.ssp4.mean.RData")
ssp4 <- temperature[,93:306]
ssp4 <- as.data.frame(rowMeans(ssp4))

total <- cbind(ref, ssp1, ssp3, ssp4)
colnames(total) <- c("Reference", "SSP 1", "SSP 3/5", "SSP 4")
total$x <- temperature$x
total$y <- temperature$y
total$`SSP1 relative` <- (total$`SSP 1` - total$Reference) / total$Reference *100
total$`SSP3/5 relative` <- (total$`SSP 3/5` - total$Reference) / total$Reference *100
total$`SSP4 relative` <- (total$`SSP 4` - total$Reference) / total$Reference *100

total.abs <- total[,1:6]
total.abs <- total.abs %>%   
  pivot_longer(cols=c(1:4), names_to = "Scenario", values_to = "Temperature")


ggplot(total.abs,aes(x,y, fill=Temperature)) +
  facet_wrap(~Scenario, ncol=4) +
  geom_tile() +
  scale_fill_gradientn(
    "Temperature",
    colours = c("lightblue", "red", "darkred"),
    values = rescale(c(13.7, 16, 17)), 
    limits = c(13.7, 17)
  ) +
  ylab("") + xlab("") +
  theme_bw() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
        strip.text.x=element_text(size=16), 
        panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggsave(filename="../Output/Plots/inputData/temperature.map.png",
       width=12, height=4, bg="white")


total.rel <- total[,5:9]
total.rel <- total.rel %>%   
  pivot_longer(cols=c(3:5), names_to = "Scenario", values_to = "Temperature")

ggplot(total.rel,aes(x,y, fill=Temperature)) +
  facet_wrap(~Scenario, ncol=4) +
  geom_tile() +
  scale_fill_gradientn(
    "% Change in \temperature",
    colours = c("lightblue", "white", "red"),
    values = rescale(c(-10, 0, 13)), 
    limits = c(-10, 13)
  ) +
  ylab("") + xlab("") +
  theme_bw() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
        strip.text.x=element_text(size=16), 
        panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggsave(filename="../Output/Plots/inputData/temperature.map.rel.png",
       width=9, height=4, bg="white")


# Bird abundance ----------------------------------------------------------

# * create map ------------------------------------------------------------

scenario <- "SSP4" # ref, SSP1, SSP3, SSP4, SSP5
species <- "Wilde_Eend" # Merel, Huismus, Wilde_Eend
columnName <- paste0(scenario, "_", species)

birdscenario <- create.birdscenario(scenario = scenario, species = species, columnName = columnName,
                                    modelraster = modelraster, outputfile = outputfile.nonNA)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
save(birdscenario, file=paste0("../Data/model_input/", scenario, "/", species, "_5km.RData"))

# compare bb maps
old <- rast("C:/Repos/vbd-siminf/Data/bird_abundance/newModelPrediction.asc")
new <- rast("C:/Users/wit109/OneDrive - Wageningen University & Research/Data/ChangeScenarios/bird predictions/ref_Merel.asc")

# * plot ------------------------------------------------------------------

species <- "Huismus" # Merel, Huismus, Wilde_Eend

load(paste0("../Data/model_input/ref/", species, "_5km.RData", sep=""))
ref <- as.data.frame(birdscenario[,3])
load(paste0("../Data/model_input/SSP1/", species, "_5km.RData", sep=""))
ssp1 <- as.data.frame(birdscenario[,3])
load(paste0("../Data/model_input/SSP3/", species, "_5km.RData", sep=""))
ssp3 <- as.data.frame(birdscenario[,3])
load(paste0("../Data/model_input/SSP4/", species, "_5km.RData", sep=""))
ssp4 <- as.data.frame(birdscenario[,3])
load(paste0("../Data/model_input/SSP5/", species, "_5km.RData", sep=""))
ssp5 <- as.data.frame(birdscenario[,3])

total <- cbind(ref, ssp1, ssp3, ssp4, ssp5)
colnames(total) <- c("Reference", "SSP 1", "SSP 3", "SSP 4", "SSP 5")
total$x <- birdscenario$x
total$y <- birdscenario$y
total$`SSP1 relative` <- (total$`SSP 1` - total$Reference) / total$Reference *100
total$`SSP3 relative` <- (total$`SSP 3` - total$Reference) / total$Reference *100
total$`SSP4 relative` <- (total$`SSP 4` - total$Reference) / total$Reference *100
total$`SSP5 relative` <- (total$`SSP 5` - total$Reference) / total$Reference *100

total.abs <- total[,1:7]
total.abs <- total.abs %>%   
  pivot_longer(cols=c(1:5), names_to = "Scenario", values_to = "Abundance")
total.abs$norm <- total.abs$Abundance / max(total.abs$Abundance)

ggplot(total.abs,aes(x,y, fill=norm)) +
  facet_wrap(~Scenario, ncol=5) +
  geom_tile() +
  scale_fill_gradientn(
    "Abundance \n(normalised)",
    colours = c("white", "lightblue", "darkolivegreen3", "red", "darkred"),
    values = rescale(c(0, 0.25, 0.5, 0.75, 1)), # normalised
    # colours = c("lightgreen", "darkolivegreen3", "darkgreen"),
    # values = rescale(c(0, 0.5, 1)), # normalised
    limits = c(0, 1)
    # values = rescale(c(1, 300, 400, 700, 1100)), # merel
    # limits = c(1, 1100)
    # values = rescale(c(1, 300, 400, 600, 850)), # wilde eend
    # limits = c(1, 850)
    # values = rescale(c(1, 600, 800, 1100, 1500)), # huismus
    # limits = c(1, 1500)
  ) +
  ylab("") + xlab("") +
  theme_bw() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
        # strip.text.x=element_text(size=16), 
        strip.text.x=element_blank(),
        # strip.background = element_rect(color="white", fill="white"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggsave(filename=paste0("../Output/Plots/inputData/bird/abundance", species, ".png", sep=""),
       width=12, height=3, bg="white")


total.rel <- total[,6:11]
total.rel <- total.rel %>%   
  pivot_longer(cols=c(3:6), names_to = "Scenario", values_to = "Abundance")

ggplot(total.rel,aes(x,y, fill=Abundance)) +
  facet_wrap(~Scenario, ncol=5) +
  geom_tile() +
  scale_fill_gradientn(
    "% Change in \nabundance",
    colours = c("lightblue", "white", "red"),
    values = rescale(c(-30, 0, 166)), 
    limits = c(-30, 166)
  ) +
  ylab("") + xlab("") +
  theme_bw() +
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), 
        strip.text.x=element_text(size=16),
        # strip.text.x=element_blank(),
        # strip.background = element_rect(color="white", fill="white"),
        panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggsave(filename=paste0("../Output/Plots/inputData/bird/abundance", species, "rel.png", sep=""),
       width=12, height=4, bg="white")


# Mosquito abundance ----------------------------------------------------------

# * 1. take mean across years -----------------------------------------------
# - takes a defined list of days (e.g. 'June 4th')
# - calculates the mean abundance for that day, averaged over all years in the folder
# - outputs a raster file for each day

scenario <- "SSP3"
dataloc<-paste0("C:/Users/wit109/OneDrive - Wageningen University & Research/Data/ChangeScenarios/", scenario, "/", scenario, sep="")
# dataloc<-paste0("C:/Users/wit109/OneDrive - Wageningen University & Research/Data/ChangeScenarios/", scenario, sep="") for 'ref'
files<-data.frame(fname=dir(dataloc))
files$date<-str_sub(files$fname,7,14)
# files$date<-str_sub(files$fname,6,13) # for 'ref'
files$date<-as_date(files$date,format="%d%m%Y")
files$day<-format(files$date,"%m%d")

startDate<-as_date("2036-04-01")
endDate<-as_date("2036-10-31")
allDays<-seq(startDate,endDate,by=1)
allDays<-format(allDays,"%m%d")

for(i in allDays){
  print(i)
  dayFiles<-files[files$day==i,'fname']
  rstack<-stack(paste(dataloc,dayFiles,sep='/'))
  meanRaster<-calc(rstack,mean)
  writeRaster(meanRaster,paste0("../Data/mosquito_abundance/", scenario, "/mean",i,".asc"),format="ascii",overwrite=TRUE)
}



# * 2. combine to one file ------------------------------------------------

## Step 2
# - loop through all daily raster files
# - resample to model grid
# - add into one file, ordered by date

filename.mosq <- "mean"
files.mosq <- dir(paste("../Data/mosquito_abundance/", scenario, sep=""))
files.mosq <- files.mosq[grep(filename.mosq, files.mosq, TRUE)]

# Make sure to sort the files in the folder as they are not in correct list 
# Remove ".asc" from filenames
date_strings <- gsub(".asc", "", substr(files.mosq, 5, nchar(files.mosq)))

# Convert date strings to Date objects
dates <- as.Date(date_strings, format = "%m%d")

# Sort files based on dates
files_sorted <- files.mosq[order(dates)]

setwd(paste("../Data/mosquito_abundance/", scenario, sep=""))

mosquito <- create.mosquitoabundance(abundancemaps = files_sorted, modelraster = modelraster, outputfile=outputfile.nonNA)

# Insert columns with zeroes for jan-apr and nov-dec
# Create a data frame with 60 columns filled with zeroes
zero_columns_jan_apr <- data.frame(matrix(0, nrow = nrow(mosquito), ncol = 90))
zero_columns_nov_dec <- data.frame(matrix(0, nrow = nrow(mosquito), ncol = 61))

# Combine the original data frame and the zero columns
mosquito <- cbind(mosquito[, 1:2],
                    zero_columns_jan_apr, 
                    mosquito[, 3:ncol(mosquito)],
                    zero_columns_nov_dec)

# Rename the columns to align with other dataframes
# Create a vector of date strings
date_strings <- seq(as.Date("2022-01-01"), by = "1 day", length.out = 365) 
colnames(mosquito)[3:ncol(mosquito)] <- as.character(date_strings)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
save(mosquito,file=paste0("../Data/model_input/", scenario, "/culex_5km.RData", sep=""))


# * 3. Align abundance value to input from abc paper --------------------------------------------

load("C:/Repos/change-scenarios/Data/model_input/ref/culex_5km.RData")
ref <- as.data.frame(colMeans(mosquito[,93:260]))                        
                          
load("C:/Repos/vbd-siminf/Data/model_input/mosquitoabundance_14d_5kmNoWater_2016.RData")
ab2016 <- as.data.frame(colMeans(mosquitoabundance_14d[["mean"]]))
load("C:/Repos/vbd-siminf/Data/model_input/mosquitoabundance_14d_5kmNoWater_2017.RData")
ab2017 <- as.data.frame(colMeans(mosquitoabundance_14d[["mean"]]))
load("C:/Repos/vbd-siminf/Data/model_input/mosquitoabundance_14d_5kmNoWater_2018.RData")
ab2018 <- as.data.frame(colMeans(mosquitoabundance_14d[["mean"]]))
load("C:/Repos/vbd-siminf/Data/model_input/mosquitoabundance_14d_5kmNoWater_2019.RData")
ab2019 <- as.data.frame(colMeans(mosquitoabundance_14d[["mean"]]))
load("C:/Repos/vbd-siminf/Data/model_input/mosquitoabundance_14d_5kmNoWater_2020.RData")
ab2020 <- as.data.frame(colMeans(mosquitoabundance_14d[["mean"]]))
load("C:/Repos/vbd-siminf/Data/model_input/mosquitoabundance_14d_5kmNoWater_2021.RData")
ab2021 <- as.data.frame(colMeans(mosquitoabundance_14d[["mean"]]))
load("C:/Repos/vbd-siminf/Data/model_input/mosquitoabundance_14d_5kmNoWater_2022.RData")
ab2022 <- as.data.frame(colMeans(mosquitoabundance_14d[["mean"]]))

all <- cbind(ab2016, ab2017, ab2018, ab2019, ab2020, ab2021, ab2022, ref)
colnames(all) <- c("ab2016", "ab2017", "ab2018", "ab2019", "ab2020", "ab2021", "ab2022", "ref")
all$day <- seq(1:168)

# Calculate correction factor
# mean across all days and years 
mean(colMeans(all[,1:7])) # Mean mosquito abundance 2016-2022 is 11754
mean(all[,8]) # Mean mosquito abundance in REF scenario is 3568
# So the correction factor to transform REF scenario to the same scale as ABC paper = 11754/3568 = 3.3

ggplot(data=all) +
  geom_line(aes(x=day, y=ab2016, colour="ab2016"), linewidth=1) +
  geom_line(aes(x=day, y=ab2017, colour="ab2017"), linewidth=1) +
  geom_line(aes(x=day, y=ab2018, colour="ab2018"), linewidth=1) +
  geom_line(aes(x=day, y=ab2019, colour="ab2019"), linewidth=1) +
  geom_line(aes(x=day, y=ab2020, colour="ab2020"), linewidth=1) +
  geom_line(aes(x=day, y=ab2021, colour="ab2021"), linewidth=1) +
  geom_line(aes(x=day, y=ab2022, colour="ab2022"), linewidth=1) +
  geom_line(aes(x=day, y=ref*3.3, colour="REF"), linewidth=2) +
  ggtitle("Daily mosquito abundance") +
  ylab("Abundance (arbitrary unit)") + xlab("Day") +
  theme_bw() +
  theme(axis.text=element_text(size=16), axis.title = element_text(size=16), legend.text = element_text(size=16),
        legend.title = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank())


load("C:/Repos/change-scenarios/Data/model_input/ref/culex_5km.RData")
mosquito[,c(3:367)] <- mosquito[,c(3:367)]*3.3
save(mosquito,file=paste0("../Data/model_input/ref/culex_5km.RData", sep=""))

load("C:/Repos/change-scenarios/Data/model_input/ssp1/culex_5km.RData")
mosquito[,c(3:367)] <- mosquito[,c(3:367)]*3.3
save(mosquito,file=paste0("../Data/model_input/ssp1/culex_5km.RData", sep=""))

load("C:/Repos/change-scenarios/Data/model_input/ssp3/culex_5km.RData")
mosquito[,c(3:367)] <- mosquito[,c(3:367)]*3.3
save(mosquito,file=paste0("../Data/model_input/ssp3/culex_5km.RData", sep=""))

load("C:/Repos/change-scenarios/Data/model_input/ssp4/culex_5km.RData")
mosquito[,c(3:367)] <- mosquito[,c(3:367)]*3.3
save(mosquito,file=paste0("../Data/model_input/ssp4/culex_5km.RData", sep=""))

load("C:/Repos/change-scenarios/Data/model_input/ssp5/culex_5km.RData")
mosquito[,c(3:367)] <- mosquito[,c(3:367)]*3.3
save(mosquito,file=paste0("../Data/model_input/ssp5/culex_5km.RData", sep=""))

