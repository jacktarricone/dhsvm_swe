# testing dhvsm vol swe code
# novemeber 20th, 2022
# jack tarricone

library(terra)

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
pixel_area_v2 <-project(pixel_area, crs(yuba_mask))

# crop down and na transform, plot
pa_crop <-crop(pixel_area_v2, yuba_mask)

# plot
plot(pa_crop)
plot(yuba_shp, add = TRUE)
plot(american_shp, add = TRUE)
plot(truckee_shp, add = TRUE)


pix_vol_swe <-function(a,b){return((a*1e-6)*(b*1e-6))}

vol_swe<-function(x){
  swe_by_pixel <- overlay(x, pixel_area, fun = pix_vol_swe )  
  vol_swe <-cellStats(swe_by_pixel,'sum', digits=9, na.rm=TRUE)
  print(vol_swe)
}