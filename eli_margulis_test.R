## eli margulis swe rough plot
# november 15th, 2022
# jack tarricone

library(rhdf5) # hdf5 reader from bioc, which is the format the margulis data is in
library(terra) # foundational package of spatial data analysis with d
library(ggplot2) # plotting package
library(sf)
library(ggmap)

#set working directory
setwd("/Users/jacktarricone/ch1_margulis/")

# list hdf swe files
swe_list <-list.files("./swe/hdf", pattern = ".h5", full.names = TRUE)
print(swe_list) # static and wy2015 SCA

# list static rasters
static_list <-list.files("./static/rasters", pattern = ".tif", full.names = TRUE)
print(static_list) # static and wy2015 SCA

# set paths for wy1993
swe_wy16_path <-swe_list[32]

### list attributes for swe file
h5ls(swe_wy16_path) # contains 3 groups: lat, long, and SCA
h5readAttributes(swe_wy16_path, name = "SWE") # SWE units = mm

# set parameters for read in
nrow <-6601
ncol <-5701
dowy <-183 # april 1st (it's a leap year!)

# read in dowy 185 for wy98 as a matrix
swe16_183_raw <- h5read(swe_wy16_path, 
                        name = "/SWE",
                        index = list(1:nrow,1:ncol,dowy))

## bring in static rasters for georeferencing
print(static_list)
dem_path <-static_list[3] # select dem
dem <-rast(dem_path) # load in raster
dem # inspect
plot(dem)

# define function for converting from array to raster with proper crs and extent from DEM
# using WSG84 and lat/lon but not EPSG:4326.. need to look into that

array_to_snrs_rast <-function(x){
  
  r <-rast(x) # convert from matrix to raster
  values(r)[values(r) == -32768] <- NA # change the value using raster function values
  ext(r) <-c(-123.3,-117.6,35.4,42)
  crs(r) <-crs(dem)
  return(r)
  
}

# convert
swe_rast <-array_to_snrs_rast(swe16_183_raw)
swe_rast # inspect

# test plot
plot(swe_rast)

# bring in yuba bear shp file
yuba_shp <-st_read("/Users/jacktarricone/dhvsm_swe/shp_files/Yuba-Bear_Watershed/Yuba-Bear_Watershed.shp")
yuba_bound <-st_geometry(yuba_shp)
plot(yuba_bound, axes = TRUE)

# full plot test
plot(swe_rast)
plot(yuba_bound, add = TRUE)

# crop, mask, and plot again
swe_crop <-crop(swe_rast, ext(yuba_shp))
swe_mc <-mask(swe_crop, yuba_shp)
plot(swe_mc)
plot(yuba_bound, add = TRUE)

# convert swe to df for plotting
swe_df <-as.data.frame(swe_mc, xy = TRUE)
names(swe_df)[3] <-"swe_mm"
head(swe_df)

# set map extent
loc <-c(-121.5,38.9,-120.28,39.82) # set map bounds manually
map <-get_map(loc, maptype = 'terrain-background', source = 'stamen')

# set scale
blues_scale <-RColorBrewer::brewer.pal(9, 'Blues')

# plot swe date with terrian basemap
ggmap(map) +
  geom_raster(swe_df, mapping = aes(x,y, fill = swe_mm)) +
  geom_sf(data = yuba_bound, fill = NA, color = "black", inherit.aes = FALSE) + # inherit.aes makes this work
  scale_fill_gradientn(colors = blues_scale, limits = c(0,1500), na.value="transparent") +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(title = "Yuba/Bear Margulis SWE: 2016-04-01", fill = "SWE (mm)",
       x="Lon (deg)", y="Lat (deg)")

setwd("/Users/jacktarricone/dhvsm_swe/plots/")
ggsave(file = "yuba_swe_2016_04_01.png",
       width = 7, 
       height = 7,
       dpi = 300)
