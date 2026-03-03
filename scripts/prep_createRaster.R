
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tmap)       # provides NLD_muni and NLD_prov maps (shapefiles). does not contain water-areas
library(tidyverse) 
library(Rcpp)
library(sf)
library(raster)
library(rgdal)
library(viridis)    # fill
library(cowplot)    # plot grids
library(mapview)



test = rasterize(NLD_prov, r, getCover=TRUE)

# Create spatial DF without water regions --------------------------------------------------------


# define size of raster
km=5

create.raster.nowater <- function(gridsize.km, verbose=0) {
  if (verbose==1) { 
    browser()
  }
  
  # create empty raster
  r=raster()
  
  # original raster from: https://epsg.io/28992 Amersfoort/RD new
  # 646 308975
  # 276050 636456
  # make size a little bit smaller, so that it only just covers the country
  # r: ncols = 56. nrows = 68. ncells = 3808.
  r <- raster(nrows=68, ncols=56, xmn= 646, ymn= 296050, xmx = 278975, ymx = 636456, resolution = gridsize.km*1000)
  
  # set crs
  crs(r)<- CRS("+init=EPSG:28992")
  r <- setValues(r, seq(from=1, to=ncell(r), by=1)) 
  
  mapview(r)
  plot(r)
  
  # get NL shape and only keep these cells 
  data("NLD_prov")
  
  # turn country map into raster. values outside country are NA
  nl_r = rasterize(NLD_prov, r, getCover=TRUE)
  nl_r[nl_r<0.5] <- NA # only keep cells that are for more than x% (used 50%) within the country
  
  res(nl_r) #5000 m x 5000 m raster (5km x 5km)
  mapview(nl_r) # raster version of regions_map_org
  
  # for original raster, hide the squares that are outside the country (=regions_map_org)
  r_masked=mask(r, nl_r)
  
  mapview(nl_r)             # raster version of regions_map_org
  mapview(r_masked)         # raster only for area falling within NL
  mapview(r)                # square raster over NL
  
  # crop r_masked according to NL boundaries
  croppedNLraster <- crop(x=r_masked, y=extent(NLD_prov))
  mapview(croppedNLraster)
  
  # save raster as spatial data frame
  croppedNLraster.DF <- as.data.frame(croppedNLraster, xy=TRUE) %>% drop_na(layer)
  # assign index to each grid cell 
  croppedNLraster.DF <- croppedNLraster.DF %>% mutate(index=seq(from=1, to=nrow(croppedNLraster.DF)))
  
  # dataframe with grid index number of cells that are outside NL
  cells.outsideNL <- seq(from=1, to=ncell(r), by=1) 
  cells.outsideNL <- cells.outsideNL[-c(croppedNLraster.DF$layer)]
  
  ggplot(croppedNLraster.DF, aes(x, y)) +
    geom_tile()
  
  # return(croppedNLraster)
  return(croppedNLraster.DF)
} 

croppedNLraster <- create.raster.nowater(gridsize.km = 5)
writeRaster(croppedNLraster, "../Data/model_input/spatialDF5kmNoWater", format="GTiff", overwrite=TRUE)

croppedNLraster.DF <- create.raster.nowater(gridsize.km = 5)
write.csv(croppedNLraster.DF,"../Data/model_input/spatialDF5kmNoWater.csv", row.names = FALSE)


