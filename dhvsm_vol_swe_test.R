# testing dhvsm vol swe code
# novemeber 20th, 2022
# jack tarricone


library(terra)

# setwd
setwd('/Users/jacktarricone/dhsvm_swe/')

# bring in pixel area
pixel_area <-rast("/Users/jacktarricone/ch1_margulis/static/rasters/SNSR_pixel_area.tif")

### bring in domain masks for the three different basins, convert 0 to NaNs
# yuba
yuba_mask <-rast('./dhsvm_swe/rasters/domain_maks/TCSImask_YubaDomain.tif')
values(yuba_mask)[values(yuba_mask) == 0] = NA
yuba_shp <-

# american
american_mask <-rast('./dhsvm_swe/rasters/domain_maks/TCSImask_AmericanDomain.tif')
values(american_mask)[values(american_mask) == 0] = NA
american_shp <-vect("./dhsvm_swe/shp_files/AmericanModelingDomain/AmericanModelingDomain.shp")

truckee_mask <-rast('./dhsvm_swe/rasters/domain_maks/TCSImask_TruckeeDomain.tif')
values(truckee_mask)[values(truckee_mask) == 0] = NA

# reproj and test plot
pixel_area_v2 <-project(pixel_area, crs(yuba_mask))

plot(pixel_area_v2)
plot(yuba_mask, add = TRUE)
plot(american_mask, add = TRUE)
plot(truckee_mask, add = TRUE)

# crop down and na transform, plot
pa_crop <-crop(pixel_area_v2, yuba_mask)

plot(pa_crop)
plot(yuba_mask, col = 'red', add = TRUE)
plot(american_mask, col = 'black', add = TRUE)
plot(truckee_mask, add = TRUE)


pix_vol_swe <-function(a,b){return((a*1e-6)*(b*1e-6))}

vol_swe<-function(x){
  swe_by_pixel <- overlay(x, pixel_area, fun = pix_vol_swe )  
  vol_swe <-cellStats(swe_by_pixel,'sum', digits=9, na.rm=TRUE)
  print(vol_swe)
}