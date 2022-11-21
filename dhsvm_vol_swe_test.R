# testing dhvsm vol swe code
# novemeber 20th, 2022
# jack tarricone

library(terra)
library(rhdf5)

# setwd
setwd('/Users/jacktarricone/dhsvm_swe/')

# bring static layers
pixel_area <-rast("/Users/jacktarricone/ch1_margulis/static/rasters/SNSR_pixel_area.tif")
x_cell <-rast("/Users/jacktarricone/ch1_margulis/static/rasters/x_cell_num.tif")
y_cell <-rast("/Users/jacktarricone/ch1_margulis/static/rasters/y_cell_num.tif")
dem <-rast("/Users/jacktarricone/ch1_margulis/static/rasters/SNSR_DEM.tif")

# stack
static <-c(pixel_area, dem, x_cell, y_cell)

### bring in domain masks for the three different basins, convert 0 to NaNs
# yuba
yuba_mask <-rast('./rasters/domain_maks/TCSImask_YubaDomain.tif')
values(yuba_mask)[values(yuba_mask) == 0] = NA # 0 to NaN
yuba_shp <-vect('./shp_files/YubaModelingDomain.shp')

# american
american_mask <-rast('./rasters/domain_maks/TCSImask_AmericanDomain.tif')
values(american_mask)[values(american_mask) == 0] = NA
american_shp <-vect("./shp_files/AmericanModelingDomain.shp")

# truckee
truckee_mask <-rast('./rasters/domain_maks/TCSImask_TruckeeDomain.tif')
values(truckee_mask)[values(truckee_mask) == 0] = NA
truckee_shp <-vect("./shp_files/TruckeeModelingDomain.shp")

### reproj and test plot
static_v2 <-project(static, crs(yuba_mask))

# crop down and na transform, plot
static_crop <-crop(static_v2, yuba_mask)

# plot
plot(static_crop[[2]])
plot(yuba_shp, add = TRUE)
plot(american_shp, add = TRUE)
plot(truckee_shp, add = TRUE)

# find crop ext by extracting bounds from cell number rasts
max_x <-as.numeric(global(static_crop[[3]], max, na.rm = TRUE))
max_x <-round(max_x, digit = 0)

min_x <-as.numeric(global(static_crop[[3]], min, na.rm = TRUE))
min_x <-round(min_x, digit = 0)

max_y <-as.numeric(global(static_crop[[4]], max, na.rm = TRUE))
max_y <-round(max_y, digit = 0)

min_y <-as.numeric(global(static_crop[[4]], min, na.rm = TRUE))
min_y <-round(min_y, digit = 0)

######################
## read in SWE data ##
######################

#set working directory
setwd("/Users/jacktarricone/ch1_margulis/")

# list hdf swe files
swe_list <-list.files("./swe/hdf", pattern = ".h5", full.names = TRUE)
print(swe_list) # static and wy2015 SCA

# set paths for wy1993
swe_wy16_path <-swe_list[32]

### list attributes for swe file
h5ls(swe_wy16_path) # contains 3 groups: lat, long, and SCA
h5readAttributes(swe_wy16_path, name = "SWE") # SWE units = mm

# pull out number of days in given year
test <-h5ls(swe_wy16_path) # contains 3 groups: lat, long, and SCA
dims <-test$dim[1]
nday <-as.integer(sub("6601 x 5701 x ","",dims))

# read in dowy 185 for wy98 as a matrix
swe16_raw <-h5read(swe_wy16_path, 
                   name = "/SWE",
                   index = list(min_y:max_y, min_x:max_x,1:nday))

# function for converting to array to rasters stack with the proper geolocation
array_to_dhsvm_rast <-function(x){
  
  r <-rast(x) # convert from matrix to raster
  values(r)[values(r) == -32768] <- NA # change the value using raster function values
  ext(r) <-ext(static_crop)
  crs(r) <-crs(static_crop)
  return(r)
  
}

# testing to see if function works
swe16 <-array_to_dhsvm_rast(swe16_raw)
swe16
plot(swe16[[230]])

# mask with shp
test <-mask(swe16, yuba_shp)
plot(test[[180]])

pix_vol_swe <-function(a,b){return((a*1e-6)*(b*1e-6))}

x <-test[[180]]
vol_swe <-function(x){
  swe_by_pixel <-lapp(x, static_crop[[1]], fun = pix_vol_swe)  
  vol_swe <-global(swe_by_pixel,'sum', digits=9, na.rm=TRUE)
  print(vol_swe)
}

vol_swe(test[[180]])
