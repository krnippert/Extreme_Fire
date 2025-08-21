### Lidar canopy data from KPBS for two watersheds from 2020 - 2024 ###
### Authors: Kalea R. Nippert-Churchman and Zak Ratajczak ###
### Last updated: 08/21/2025 ###
### code used to determine effects of wildfire and prescribed fire on woody plant cover and density ###


setwd("~/Ratajczak_lab/can_ht_shrub_code")
library(raster)
library(rpart)
library(sf)
library(ggplot2)
library(tibble)

#load in raster files
can_ht_2022 = raster("neon_2022_lidar_canht.tif")
plot(can_ht_2022)
can_ht_2023 = raster("neon_2023_lidar_canht.tif")
plot(can_ht_2023)
can_ht_2020 = raster("can_ht_2020_new.tif")
plot(can_ht_2020)
can_ht_2024 = raster("neon_2024_lidar_canht.tif")
plot(can_ht_2024)

##checking coordinate reference system and reprojecting 2020######
crs(can_ht_2022k4a) == crs(can_ht_2020k4a)


crs(can_ht_2022)
crs(can_ht_2020)
crs(can_ht_2023)
crs(can_ht_2024)

#can_ht_2020 = projectRaster(can_ht_2020, can_ht_2022, method="bilinear")
#writeRaster(can_ht_2020, "can_ht_2020_new.tif", overwrite = T)

##Watershed specific (raster masked to extent of watershed)####
can_ht_20234b <- raster("can_ht_2023_4b.tif")
plot(can_ht_20234b)
can_ht_20224b <- raster("can_ht_2022_4b.tif")
plot(can_ht_20224b)
can_ht_20204b <- raster("can_ht_2020_4b.tif")
plot(can_ht_20204b)
can_ht_20244b <- raster("can_ht_2024_4b.tif")
plot(can_ht_20244b)


can_ht_2023k4a = raster("can_ht_2023_k4a.tif")
plot(can_ht_2023k4a)
can_ht_2022k4a = raster("can_ht_2022_k4a.tif")
plot(can_ht_2022k4a)
can_ht_2020k4a <- raster("can_ht_2020_k4a.tif")
plot(can_ht_2020k4a)
can_ht_2024k4a <- raster("can_ht_2024_k4a.tif")
plot(can_ht_2024k4a)


## masking 2024 raster to 4b and k4a

#do some raster math to see how canopy heights change year to year
#delta_2022_2023 = can_ht_2023-can_ht_2022
#plot(delta_2022_2023)

#delta_new2020_2022 = can_ht_2022-can_ht_2020
#plot(delta_2020_2022)

#export the file for making maps elsewhere
writeRaster(delta_2022_2023, "delta_2023_2023.tif")
writeRaster(delta_2022_20234b, "delta_2022_20234b.tif")
writeRaster(delta_2022_2023k4a, "delta_2022_2023k4a.tif")
writeRaster(delta_2020_20224b, "delta_2020_20224b.tif")
writeRaster(delta_2020_2022k4a, "delta_2020_2022k4a.tif")
writeRaster(new_2020_can_ht, "can_ht_2020_new.tif")


##bringing in the stream and elevation files and reprojecting to match extents
stream_buffer_4b = st_read("4b_streams.shp")
crs(stream_buffer_4b)
stream_buffer_k4a = st_read("k4a_streams.shp")
elevation_4b = raster("4b_elevation.tif")
elevation_k4a = raster("k4a_elevation.tif")
crs(elevation_4b)
crs(elevation_k4a)

#elevation_4b <- projectRaster(elevation_4b, can_ht_2023, method="bilinear")
#elevation_4b[elevation_4b == 0] <- NA

#elevation_k4a <- projectRaster(elevation_k4a, can_ht_2023, method="bilinear")
#elevation_k4a[elevation_k4a == 0] <- NA

#plot(elevation_4b)
#plot(elevation_k4a)

##writeRaster(elevation_k4a, "k4a_elevation.tif", overwrite = T)
#writeRaster(elevation_4b, "4b_elevation.tif", overwrite = T)

### Masking lidar to buffer out streams and mask to the extent of the desired watershed
###2020#########
can_ht_20204b_masked <- can_ht_20204b
can_ht_20204b_masked = mask(can_ht_20204b_masked, stream_buffer_4b, inverse=T)
plot(can_ht_20204b_masked)
can_ht_20204b_masked = mask(can_ht_20204b_masked, elevation_4b, inverse=F)
plot(can_ht_20204b_masked, xlim = c(707000, 708500), ylim = c(4327500, 4328500))
writeRaster(can_ht_20204b_masked, "can_ht_20204b_mask.tif", overwrite = T)
can_ht_20204b_masked <- raster("can_ht_20204b_mask.tif")


can_ht_2020k4a_masked <- can_ht_2020k4a
can_ht_2020k4a_masked = mask(can_ht_2020k4a_masked, stream_buffer_k4a, inverse=T)
can_ht_2020k4a_masked = mask(can_ht_2020k4a_masked, elevation_k4a, inverse=F)
plot(can_ht_2020k4a_masked)
writeRaster(can_ht_2020k4a_masked, "can_ht_2020k4a_mask.tif")
can_ht_2020_k4a_masked <- raster("can_ht_2020k4a_mask.tif")


###2022########
can_ht_20224b_masked <- can_ht_20224b
can_ht_20224b_masked = mask(can_ht_20224b_masked, stream_buffer_4b, inverse=T)
can_ht_20224b_masked = mask(can_ht_20224b_masked, elevation_4b, inverse=F)
plot(can_ht_20224b_masked)
writeRaster(can_ht_20224b_masked, "can_ht_20224b_mask.tif")
can_ht_20224b_masked <- raster("can_ht_20224b_mask.tif")

can_ht_2022k4a_masked <- can_ht_2022k4a
can_ht_2022k4a_masked = mask(can_ht_2022k4a_masked, stream_buffer_k4a, inverse=T)
can_ht_2022k4a_masked = mask(can_ht_2022k4a_masked, new_elevation_k4a, inverse=F)
plot(can_ht_2022k4a_masked)
writeRaster(can_ht_2022k4a_masked, "can_ht_2022k4a_mask.tif")
can_ht_2022k4a_masked <- raster("can_ht_2022k4a_mask.tif")

###2023######
can_ht_20234b_masked <- can_ht_20234b
can_ht_20234b_masked = mask(can_ht_20234b_masked, stream_buffer_4b, inverse=T)
can_ht_20234b_masked = mask(can_ht_20234b_masked, new_elevation_4b, inverse=F)
plot(can_ht_20234b_masked)
writeRaster(can_ht_20234b_masked, "can_ht_20234b_mask.tif")
can_ht_20234b_masked <- raster("can_ht_2022k4a_mask.tif")

can_ht_2023k4a_masked <- can_ht_2023k4a
can_ht_2023k4a_masked = mask(can_ht_2023k4a_masked, stream_buffer_k4a, inverse=T)
can_ht_2023k4a_masked = mask(can_ht_2023k4a_masked, new_elevation_k4a, inverse=F)
plot(can_ht_2023k4a_masked)
writeRaster(can_ht_2023k4a_masked, "can_ht_2023k4a_mask.tif")
can_ht_2023k4a_masked <- raster("can_ht_2023k4a_mask.tif")

###2024###
can_ht_20244b_masked <- can_ht_20244b
can_ht_20244b_masked = mask(can_ht_20244b_masked, stream_buffer_4b, inverse=T)
can_ht_20244b_masked = mask(can_ht_20244b_masked, new_elevation_4b, inverse=F)
plot(can_ht_20244b_masked)
writeRaster(can_ht_20244b_masked, "can_ht_20244b_mask.tif")
can_ht_20244b_masked <- raster("can_ht_20244b_mask.tif")

can_ht_2024k4a_masked <- can_ht_2024k4a
can_ht_2024k4a_masked = mask(can_ht_2024k4a_masked, stream_buffer_k4a, inverse=T)
can_ht_2024k4a_masked = mask(can_ht_2024k4a_masked, new_elevation_k4a, inverse=F)
plot(can_ht_2024k4a_masked)
writeRaster(can_ht_2024k4a_masked, "can_ht_2024k4a_mask.tif")
can_ht_2024k4a_masked <- raster("can_ht_2024k4a_mask.tif")



##watershed specific raster math with streams and watershed buffered out####
delta_2022_20234b = can_ht_20234b_masked-can_ht_20224b_masked
plot(delta_2022_20234b)
hist(delta_2022_20234b)

delta_2020_20224b = can_ht_20224b_masked-can_ht_20204b_masked
plot(delta_2020_20224b)
hist(delta_2020_20224b)

delta_2022_2023k4a = can_ht_2023k4a_masked-can_ht_2022k4a_masked
plot(delta_2022_2023k4a)
hist(delta_2022_2023k4a)

delta_2020_2022k4a = can_ht_2022k4a_masked-can_ht_2020k4a_masked
plot(delta_2020_2022k4a)
hist(delta_2020_2022k4a)

######## gettin values of delta plots (canopy change between years) ###########
#2020-2022
d_2020_20224b <- getValues(delta_2020_20224b)
delta_values_4b_20_22 = as.data.frame(d_2020_20224b)
d_2020_2022k4a <- getValues(delta_2020_2022k4a)
delta_values_k4a_20_22 = as.data.frame(d_2020_2022k4a)

#2022-2023
d_2022_20234b <- getValues(delta_2022_20234b)
delta_values_4b_22_23 = as.data.frame(d_2022_20234b)
d_2022_2023k4a <- getValues(delta_2022_2023k4a)
delta_values_k4a_22_23 = as.data.frame(d_2022_2023k4a)

##making histogram of values
ggplot(delta_values_4b_20_22, aes(x=d_2020_20224b))+
  geom_histogram(binwidth = 1)+
  scale_x_continuous(limits = c(-3,3))

ggplot(delta_values_k4a_20_22, aes(x=d_2020_2022k4a))+
  geom_histogram(binwidth = 1)+
  scale_x_continuous(limits = c(-3,3))

ggplot(delta_values_4b_22_23, aes(x=d_2022_20234b))+
  geom_histogram(binwidth = 1)+
  scale_x_continuous(limits = c(-3,3))

ggplot(delta_values_k4a_22_23, aes(x=d_2022_2023k4a))+
  geom_histogram(binwidth = 1)+
  scale_x_continuous(limits = c(-3,3))

######changing names to identify stacked layers #########
names(can_ht_20204b_masked) = "Initial Height 4b 2020"
names(can_ht_2020k4a_masked) = "Initial Height k4a 2020"
names(can_ht_20224b_masked) = "Initial Height 4b 2022"
names(can_ht_2022k4a_masked) = "Initial Height k4a 2022"
names(can_ht_20234b_masked) = "Initial Height 4b 2023"
names(can_ht_2023k4a_masked) = "Initial Height k4a 2023"
names(delta_2020_20224b) = "delta height 2022-2020 4b"
names(delta_2020_2022k4a) = "delta height 2022-2020 k4a"
names(delta_2022_20234b) = "delta height 2023-2022 4b"
names(delta_2022_2023k4a) = "delta height 2023-2022 k4a"

####### Identifying woody vs herbaceous ########
pal = colorRampPalette(c ("black", "red"))
#changing name
can_ht_20204b_wood = can_ht_20204b_masked
can_ht_2020k4a_wood = can_ht_2020k4a_masked
can_ht_20224b_wood = can_ht_20224b_masked
can_ht_2022k4a_wood = can_ht_2022k4a_masked
can_ht_20234b_wood = can_ht_20234b_masked
can_ht_2023k4a_wood = can_ht_2023k4a_masked
can_ht_20244b_wood = can_ht_20244b_masked
can_ht_2024k4a_wood = can_ht_2024k4a_masked

#using a 0.95m cutoff
#identifying the woody areas in 4b and k4a in 2020, 2022, 2023, and 2024
#2020
can_ht_20204b_wood[can_ht_20204b_wood < 0.95] = 0
can_ht_20204b_wood[can_ht_20204b_wood > 0.95] = 1
can_ht_2020k4a_wood[can_ht_2020k4a_wood < 0.95] = 0
can_ht_2020k4a_wood[can_ht_2020k4a_wood > 0.95] = 1
plot(can_ht_20204b_wood)
plot(can_ht_2020k4a_wood)

#2022
can_ht_20224b_wood[can_ht_20224b_wood < 0.95] = 0
can_ht_20224b_wood[can_ht_20224b_wood > 0.95] = 1
can_ht_2022k4a_wood[can_ht_2022k4a_wood < 0.95] = 0
can_ht_2022k4a_wood[can_ht_2022k4a_wood > 0.95] = 1
plot(can_ht_20224b_wood)
plot(can_ht_2022k4a_wood)

#2023
can_ht_20234b_wood[can_ht_20234b_wood < 0.95] = 0
can_ht_20234b_wood[can_ht_20234b_wood > 0.95] = 1
can_ht_2023k4a_wood[can_ht_2023k4a_wood < 0.95] = 0
can_ht_2023k4a_wood[can_ht_2023k4a_wood > 0.95] = 1
plot(can_ht_20234b_wood)
plot(can_ht_2023k4a_wood)

#2024
can_ht_20244b_wood[can_ht_20244b_wood < 0.95] = 0
can_ht_20244b_wood[can_ht_20244b_wood > 0.95] = 1
can_ht_2024k4a_wood[can_ht_2024k4a_wood < 0.95] = 0
can_ht_2024k4a_wood[can_ht_2024k4a_wood > 0.95] = 1
plot(can_ht_20244b_wood)
plot(can_ht_2024k4a_wood)

# Identifying grassy areas in 4b and k4a in 2020, 2022, 2023, and 2024
#changing names
can_ht_20204b_grass = can_ht_20204b_masked
can_ht_2020k4a_grass = can_ht_2020k4a_masked
can_ht_20224b_grass = can_ht_20224b_masked
can_ht_2022k4a_grass = can_ht_2022k4a_masked
can_ht_20234b_grass = can_ht_20234b_masked
can_ht_2023k4a_grass = can_ht_2023k4a_masked
can_ht_20244b_grass = can_ht_20244b_masked
can_ht_2024k4a_grass = can_ht_2024k4a_masked

#Identifying grassy areas (setting parameters)
#2020
can_ht_20204b_grass[can_ht_20204b_grass < 0.95] = -1
can_ht_20204b_grass[can_ht_20204b_grass > 0.95] = 0
can_ht_20204b_grass[can_ht_20204b_grass < -.5] = 1
plot(can_ht_20204b_grass)
can_ht_2020k4a_grass[can_ht_2020k4a_grass < 0.95] = -1
can_ht_2020k4a_grass[can_ht_2020k4a_grass > 0.95] = 0
can_ht_2020k4a_grass[can_ht_2020k4a_grass < -.5] = 1
plot(can_ht_2020k4a_grass)

#2022
can_ht_20224b_grass[can_ht_20224b_grass < 0.95] = -1
can_ht_20224b_grass[can_ht_20224b_grass > 0.95] = 0
can_ht_20224b_grass[can_ht_20224b_grass < -.5] = 1
plot(can_ht_20224b_grass)
can_ht_2022k4a_grass[can_ht_2022k4a_grass < 0.95] = -1
can_ht_2022k4a_grass[can_ht_2022k4a_grass > 0.95] = 0
can_ht_2022k4a_grass[can_ht_2022k4a_grass < -.5] = 1
plot(can_ht_2022k4a_grass)

#2023
can_ht_20234b_grass[can_ht_20234b_grass < 0.95] = -1
can_ht_20234b_grass[can_ht_20234b_grass > 0.95] = 0
can_ht_20234b_grass[can_ht_20234b_grass < -.5] = 1
plot(can_ht_20234b_grass)
can_ht_2023k4a_grass[can_ht_2023k4a_grass < 0.95] = -1
can_ht_2023k4a_grass[can_ht_2023k4a_grass > 0.95] = 0
can_ht_2023k4a_grass[can_ht_2023k4a_grass < -.5] = 1
plot(can_ht_2023k4a_grass)

#2024
can_ht_20244b_grass[can_ht_20244b_grass < 0.95] = -1
can_ht_20244b_grass[can_ht_20244b_grass > 0.95] = 0
can_ht_20244b_grass[can_ht_20244b_grass < -.5] = 1
plot(can_ht_20244b_grass)
can_ht_2024k4a_grass[can_ht_2024k4a_grass < 0.95] = -1
can_ht_2024k4a_grass[can_ht_2024k4a_grass > 0.95] = 0
can_ht_2024k4a_grass[can_ht_2024k4a_grass < -.5] = 1
plot(can_ht_2024k4a_grass)

####### changing names again for woody and grass layers #######
names(can_ht_20204b_wood) = "woody locations 4b 2020"
names(can_ht_20204b_grass) = "grass locations 4b 2020"
names(can_ht_2020k4a_wood) = "woody locations k4a 2020"
names(can_ht_2020k4a_grass) = "grass locations k4a 2020"

names(can_ht_20224b_wood) = "woody locations 4b 2022"
names(can_ht_20224b_grass) = "grass locations 4b 2022"
names(can_ht_2022k4a_wood) = "woody locations k4a 2022"
names(can_ht_2022k4a_grass) = "grass locations k4a 2022"

names(can_ht_20234b_wood) = "woody locations 4b 2023"
names(can_ht_20234b_grass) = "grass locations 4b 2023"
names(can_ht_2023k4a_wood) = "woody locations k4a 2023"
names(can_ht_2023k4a_grass) = "grass locations k4a 2023"

names(can_ht_20244b_wood) = "woody locations 4b 2024"
names(can_ht_20244b_grass) = "grass locations 4b 2024"
names(can_ht_2024k4a_wood) = "woody locations k4a 2024"
names(can_ht_2024k4a_grass) = "grass locations k4a 2024"

#### stacking plots to view at the same time ###
stacked_2020_4B = stack(can_ht_20204b_grass, can_ht_20204b_wood)
plot(stacked_2020_4B)
stacked_2020_k4a = stack(can_ht_2020k4a_grass, can_ht_2020k4a_wood)
plot(stacked_2020_k4a)
stacked_2022_4b = stack(can_ht_20224b_grass, can_ht_20224b_wood)
plot(stacked_2022_4b)
stacked_2022_k4a = stack(can_ht_2022k4a_grass, can_ht_2022k4a_wood)
plot(stacked_2022_k4a)
stacked_2023_4b = stack(can_ht_20234b_grass, can_ht_20234b_wood)
plot(stacked_2023_4b)
stacked_2023_k4a = stack(can_ht_2023k4a_grass, can_ht_2023k4a_wood)
plot(stacked_2023_k4a)
stacked_2024_4b = stack(can_ht_20244b_grass, can_ht_20244b_wood)
plot(stacked_2024_4b)
stacked_2024_k4a = stack(can_ht_2024k4a_grass, can_ht_2024k4a_wood)
plot(stacked_2024_k4a)

stacked_4b = stack(can_ht_20204b_wood, can_ht_20224b_wood, can_ht_20234b_wood, can_ht_20244b_wood)
plot(stacked_4b)

#renaming stack#
woody_layer_20204b = stacked_2020_4B$woody.locations.4b.2020
woody_layer_2020k4a = stacked_2020_k4a$woody.locations.k4a.2020
woody_layer_20224b = stacked_2022_4b$woody.locations.4b.2022
woody_layer_2022k4a = stacked_2022_k4a$woody.locations.k4a.2022
woody_layer_20234b = stacked_2023_4b$woody.locations.4b.2023
woody_layer_2023k4a = stacked_2023_k4a$woody.locations.k4a.2023
woody_layer_20244b = stacked_2024_4b$woody.locations.4b.2024
woody_layer_2024k4a = stacked_2024_k4a$woody.locations.k4a.2024


##### calculating grass percent around each pixel #########
buffer_size = 5

##2020 4b
cell_size_20204b = res(woody_layer_20204b)[1]
buffer_cells_20204b = ceiling(buffer_size/cell_size_20204b)
buffered_grass_20204b = focal(woody_layer_20204b, w = matrix(1, nrow = buffer_cells_20204b
                      *2 +1, ncol = buffer_cells_20204b*2+1),
                      fun =function(x) {1-mean (x)})
percentage_grass_20204b = buffered_grass_20204b * 100
plot(percentage_grass_20204b)
hist(percentage_grass_20204b)

##2020 k4a
cell_size_2020k4a = res(woody_layer_2020k4a)[1]
buffer_cells_2020k4a = ceiling(buffer_size/cell_size_2020k4a)
buffered_grass_2020k4a = focal(woody_layer_2020k4a, w = matrix(1, nrow = buffer_cells_2020k4a
                        *2 +1, ncol = buffer_cells_2020k4a*2+1),
                        fun =function(x) {1-mean (x)})
percentage_grass_2020k4a = buffered_grass_2020k4a * 100
plot(percentage_grass_2020k4a)
hist(percentage_grass_2020k4a)

##2022 4b
cell_size_20224b = res(woody_layer_20224b)[1]
buffer_cells_20224b = ceiling(buffer_size/cell_size_20224b)
buffered_grass_20224b = focal(woody_layer_20224b, w = matrix(1, nrow = buffer_cells_20224b
                        *2 +1, ncol = buffer_cells_20224b*2+1),
                        fun =function(x) {1-mean (x)})
percentage_grass_20224b = buffered_grass_20224b * 100
plot(percentage_grass_20224b)
hist(percentage_grass_20224b)

##2022 K4a
cell_size_2022k4a = res(woody_layer_2022k4a)[1]
buffer_cells_2022k4a = ceiling(buffer_size/cell_size_2022k4a)
buffered_grass_2022k4a = focal(woody_layer_2022k4a, w = matrix(1, nrow = buffer_cells_2022k4a
                        *2 +1, ncol = buffer_cells_2022k4a*2+1),
                        fun =function(x) {1-mean (x)})
percentage_grass_2022k4a = buffered_grass_2022k4a * 100
plot(percentage_grass_2022k4a)
hist(percentage_grass_2022k4a)

##2023 4b
cell_size_20234b = res(woody_layer_20234b)[1]
buffer_cells_20234b = ceiling(buffer_size/cell_size_20234b)
buffered_grass_20234b = focal(woody_layer_20234b, w = matrix(1, nrow = buffer_cells_20234b
                        *2 +1, ncol = buffer_cells_20234b*2+1),
                        fun =function(x) {1-mean (x)})
percentage_grass_20234b = buffered_grass_20234b * 100
plot(percentage_grass_20234b)
hist(percentage_grass_20234b)

#2023 K4a
cell_size_2023k4a = res(woody_layer_2023k4a)[1]
buffer_cells_2023k4a = ceiling(buffer_size/cell_size_2023k4a)
buffered_grass_2023k4a = focal(woody_layer_2023k4a, w = matrix(1, nrow = buffer_cells_2023k4a
                          *2 +1, ncol = buffer_cells_2023k4a*2+1),
                          fun =function(x) {1-mean (x)})
percentage_grass_2023k4a = buffered_grass_2023k4a * 100
plot(percentage_grass_2023k4a)
hist(percentage_grass_2023k4a)


#2024 4b
cell_size_20244b = res(woody_layer_20244b)[1]
buffer_cells_20244b = ceiling(buffer_size/cell_size_20244b)
buffered_grass_20244b = focal(woody_layer_20244b, w = matrix(1, nrow = buffer_cells_20244b
                                                             *2 +1, ncol = buffer_cells_20244b*2+1),
                              fun =function(x) {1-mean (x)})
percentage_grass_20244b = buffered_grass_20244b * 100
plot(percentage_grass_20244b)
hist(percentage_grass_20244b)

#2024 K4a
cell_size_2024k4a = res(woody_layer_2024k4a)[1]
buffer_cells_2024k4a = ceiling(buffer_size/cell_size_2024k4a)
buffered_grass_2024k4a = focal(woody_layer_2024k4a, w = matrix(1, nrow = buffer_cells_2024k4a
                                                               *2 +1, ncol = buffer_cells_2024k4a*2+1),
                               fun =function(x) {1-mean (x)})
percentage_grass_2024k4a = buffered_grass_2024k4a * 100
plot(percentage_grass_2024k4a)
hist(percentage_grass_2024k4a)

####renaming again##########
names(percentage_grass_2023k4a) <- "percentage grass"
names(percentage_grass_20234b) <- "percentage grass"
names(percentage_grass_20204b) <- "percentage grass"
names(percentage_grass_2020k4a) <- "percentage grass"
names(percentage_grass_20224b) <- "percentage grass"
names(percentage_grass_2022k4a) <- "percentage grass"
names(percentage_grass_20244b) <- "percentage grass"
names(percentage_grass_2024k4a) <- "percentage grass"

####Stacking all together finally ######
#2020
stacked_20204b_final = stack(stacked_2020_4B, percentage_grass_20204b)
plot(stacked_20204b_final)
stacked_2020k4a_final = stack(stacked_2020_k4a, percentage_grass_2020k4a)
plot(stacked_2020k4a_final)
#2022
stacked_20224b_final = stack(stacked_2022_4b, percentage_grass_20224b)
plot(stacked_20224b_final)
stacked_2022k4a_final = stack(stacked_2022_k4a, percentage_grass_2022k4a)
plot(stacked_2022k4a_final)
#2023
stacked_20234b_final = stack(stacked_2023_4b, percentage_grass_20234b)
plot(stacked_20234b_final)
stacked_2023k4a_final = stack(stacked_2023_k4a, percentage_grass_2023k4a)
plot(stacked_2023k4a_final)
#2024
stacked_20244b_final = stack(stacked_2024_4b, percentage_grass_20244b)
plot(stacked_20244b_final)
stacked_2024k4a_final = stack(stacked_2024_k4a, percentage_grass_2024k4a)
plot(stacked_2024k4a_final)

####ok after all that trouble we are finally going to calculate some cover values

#2020
cells0.95_4b_2020 <- sum(values(can_ht_20204b_masked) >= 0.95, na.rm=TRUE)
total_cells_4b_2020 <- sum(values(can_ht_20204b_masked) >= -1, na.rm=TRUE)
percentage_gt_0.95_20204b <- (cells0.95_4b_2020/total_cells_4b_2020) *100

cells0.95_k4a_2020 <- sum(values(can_ht_2020k4a_masked) >= 0.95, na.rm=TRUE)
total_cells_k4a_2020 <- sum(values(can_ht_2020k4a_masked) >= -1, na.rm=TRUE)
percentage_gt_0.95_2020k4a <- (cells0.95_k4a_2020/total_cells_k4a_2020) *100

#2022
cells0.95_4b_2022 <- sum(values(can_ht_20224b_masked) >= 0.95, na.rm=TRUE)
total_cells_4b_2022 <- sum(values(can_ht_20224b_masked) >= -1, na.rm=TRUE)
percentage_gt_0.95_20224b <- (cells0.95_4b_2022/total_cells_4b_2022) *100

cells0.95_k4a_2022 <- sum(values(can_ht_2022k4a_masked) >= 0.95, na.rm=TRUE)
total_cells_k4a_2022 <- sum(values(can_ht_2022k4a_masked) >= -1, na.rm=TRUE)
percentage_gt_0.95_2022k4a <- (cells0.95_k4a_2022/total_cells_k4a_2022) *100

#2023
cells0.95_4b_2023 <- sum(values(can_ht_20234b_masked) >= 0.95, na.rm=TRUE)
total_cells_4b_2023 <- sum(values(can_ht_20234b_masked) >= -1, na.rm=TRUE)
percentage_gt_0.95_20234b <- (cells0.95_4b_2023/total_cells_4b_2023) *100

cells0.95_k4a_2023 <- sum(values(can_ht_2023k4a_masked) >= 0.95, na.rm=TRUE)
total_cells_k4a_2023 <- sum(values(can_ht_2023k4a_masked) >= -1, na.rm=TRUE)
percentage_gt_0.95_2023k4a <- (cells0.95_k4a_2023/total_cells_k4a_2023) *100

#2024
cells0.95_4b_2024 <- sum(values(can_ht_20244b_masked) >= 0.95, na.rm=TRUE)
total_cells_4b_2024 <- sum(values(can_ht_20244b_masked) >= -1, na.rm=TRUE)
percentage_gt_0.95_20244b <- (cells0.95_4b_2024/total_cells_4b_2024) *100

cells0.95_k4a_2024 <- sum(values(can_ht_2024k4a_masked) >= 0.95, na.rm=TRUE)
total_cells_k4a_2024 <- sum(values(can_ht_2024k4a_masked) >= -1, na.rm=TRUE)
percentage_gt_0.95_2024k4a <- (cells0.95_k4a_2024/total_cells_k4a_2024) *100


####printing percentage of change in heights ##########
###4B###
print(percentage_gt_0.95_20204b)
print(percentage_gt_0.95_20224b)
print(percentage_gt_0.95_20234b)
print(percentage_gt_0.95_20244b)

####K4A#####
print(percentage_gt_0.95_2020k4a)
print(percentage_gt_0.95_2022k4a)
print(percentage_gt_0.95_2023k4a)
print(percentage_gt_0.95_2024k4a)


########### changing woody height classification values to see how it affects cover ######
####### Identifying woody vs herbaceous ########
pal = colorRampPalette(c ("black", "red"))
#changing name
can_ht_20204b_wood_1.5 = can_ht_20204b_masked
can_ht_2020k4a_wood_1.5 = can_ht_2020k4a_masked
can_ht_20224b_wood_1.5 = can_ht_20224b_masked
can_ht_2022k4a_wood_1.5 = can_ht_2022k4a_masked
can_ht_20234b_wood_1.5 = can_ht_20234b_masked
can_ht_2023k4a_wood_1.5 = can_ht_2023k4a_masked
can_ht_20244b_wood_1.5 = can_ht_20244b_masked
can_ht_2024k4a_wood_1.5 = can_ht_2024k4a_masked

#using a 1.5m cutoff
#identifying the woody areas in 4b and k4a in 2020, 2022, and 2023
#2020
can_ht_20204b_wood_1.5[can_ht_20204b_wood_1.5 < 1.5] = 0
can_ht_20204b_wood_1.5[can_ht_20204b_wood_1.5 > 1.5] = 1
can_ht_2020k4a_wood_1.5[can_ht_2020k4a_wood_1.5 < 1.5] = 0
can_ht_2020k4a_wood_1.5[can_ht_2020k4a_wood_1.5 > 1.5] = 1
plot(can_ht_20204b_wood_1.5)
plot(can_ht_2020k4a_wood_1.5)

#2022
can_ht_20224b_wood_1.5[can_ht_20224b_wood_1.5 < 1.5] = 0
can_ht_20224b_wood_1.5[can_ht_20224b_wood_1.5 > 1.5] = 1
can_ht_2022k4a_wood_1.5[can_ht_2022k4a_wood_1.5 < 1.5] = 0
can_ht_2022k4a_wood_1.5[can_ht_2022k4a_wood_1.5 > 1.5] = 1
plot(can_ht_20224b_wood_1.5)
plot(can_ht_2022k4a_wood_1.5)

#2023
can_ht_20234b_wood_1.5[can_ht_20234b_wood_1.5 < 1.5] = 0
can_ht_20234b_wood_1.5[can_ht_20234b_wood_1.5 > 1.5] = 1
can_ht_2023k4a_wood_1.5[can_ht_2023k4a_wood_1.5 < 1.5] = 0
can_ht_2023k4a_wood_1.5[can_ht_2023k4a_wood_1.5 > 1.5] = 1
plot(can_ht_20234b_wood_1.5)
plot(can_ht_2023k4a_wood_1.5)

#2024
can_ht_20244b_wood_1.5[can_ht_20244b_wood_1.5 < 1.5] = 0
can_ht_20244b_wood_1.5[can_ht_20244b_wood_1.5 > 1.5] = 1
can_ht_2024k4a_wood_1.5[can_ht_2024k4a_wood_1.5 < 1.5] = 0
can_ht_2024k4a_wood_1.5[can_ht_2024k4a_wood_1.5 > 1.5] = 1
plot(can_ht_20244b_wood_1.5)
plot(can_ht_2024k4a_wood_1.5)

# Identifying grassy areas in 4b and k4a in 2020, 2022, and 2023
#changing names
can_ht_20204b_grass_1.5 = can_ht_20204b_masked
can_ht_2020k4a_grass_1.5 = can_ht_2020k4a_masked
can_ht_20224b_grass_1.5 = can_ht_20224b_masked
can_ht_2022k4a_grass_1.5 = can_ht_2022k4a_masked
can_ht_20234b_grass_1.5 = can_ht_20234b_masked
can_ht_2023k4a_grass_1.5 = can_ht_2023k4a_masked
can_ht_20244b_grass_1.5 = can_ht_20244b_masked
can_ht_2024k4a_grass_1.5 = can_ht_2024k4a_masked
#Identifying grassy areas (setting parameters)
#2020
can_ht_20204b_grass_1.5[can_ht_20204b_grass_1.5 < 1.5] = -1
can_ht_20204b_grass_1.5[can_ht_20204b_grass_1.5 > 1.5] = 0
can_ht_20204b_grass_1.5[can_ht_20204b_grass_1.5 < -.5] = 1
plot(can_ht_20204b_grass_1.5)
can_ht_2020k4a_grass_1.5[can_ht_2020k4a_grass_1.5 < 1.5] = -1
can_ht_2020k4a_grass_1.5[can_ht_2020k4a_grass_1.5 > 1.5] = 0
can_ht_2020k4a_grass_1.5[can_ht_2020k4a_grass_1.5 < -.5] = 1
plot(can_ht_2020k4a_grass_1.5)

#2022
can_ht_20224b_grass_1.5[can_ht_20224b_grass_1.5 < 1.5] = -1
can_ht_20224b_grass_1.5[can_ht_20224b_grass_1.5 > 1.5] = 0
can_ht_20224b_grass_1.5[can_ht_20224b_grass_1.5 < -.5] = 1
plot(can_ht_20224b_grass_1.5)
can_ht_2022k4a_grass_1.5[can_ht_2022k4a_grass_1.5 < 1.5] = -1
can_ht_2022k4a_grass_1.5[can_ht_2022k4a_grass_1.5 > 1.5] = 0
can_ht_2022k4a_grass_1.5[can_ht_2022k4a_grass_1.5 < -.5] = 1
plot(can_ht_2022k4a_grass_1.5)

#2023
can_ht_20234b_grass_1.5[can_ht_20234b_grass_1.5 < 1.5] = -1
can_ht_20234b_grass_1.5[can_ht_20234b_grass_1.5 > 1.5] = 0
can_ht_20234b_grass_1.5[can_ht_20234b_grass_1.5 < -.5] = 1
plot(can_ht_20234b_grass_1.5)
can_ht_2023k4a_grass_1.5[can_ht_2023k4a_grass_1.5 < 1.5] = -1
can_ht_2023k4a_grass_1.5[can_ht_2023k4a_grass_1.5 > 1.5] = 0
can_ht_2023k4a_grass_1.5[can_ht_2023k4a_grass_1.5 < -.5] = 1
plot(can_ht_2023k4a_grass_1.5)

#2024
can_ht_20244b_grass_1.5[can_ht_20244b_grass_1.5 < 1.5] = -1
can_ht_20244b_grass_1.5[can_ht_20244b_grass_1.5 > 1.5] = 0
can_ht_20244b_grass_1.5[can_ht_20244b_grass_1.5 < -.5] = 1
plot(can_ht_20244b_grass_1.5)
can_ht_2024k4a_grass_1.5[can_ht_2024k4a_grass_1.5 < 1.5] = -1
can_ht_2024k4a_grass_1.5[can_ht_2024k4a_grass_1.5 > 1.5] = 0
can_ht_2024k4a_grass_1.5[can_ht_2024k4a_grass_1.5 < -.5] = 1
plot(can_ht_2024k4a_grass_1.5)

####### changing names again for woody and grass layers #######
names(can_ht_20204b_wood_1.5) = "woody locations 4b 2020"
names(can_ht_20204b_grass_1.5) = "grass locations 4b 2020"
names(can_ht_2020k4a_wood_1.5) = "woody locations k4a 2020"
names(can_ht_2020k4a_grass_1.5) = "grass locations k4a 2020"

names(can_ht_20224b_wood_1.5) = "woody locations 4b 2022"
names(can_ht_20224b_grass_1.5) = "grass locations 4b 2022"
names(can_ht_2022k4a_wood_1.5) = "woody locations k4a 2022"
names(can_ht_2022k4a_grass_1.5) = "grass locations k4a 2022"

names(can_ht_20234b_wood_1.5) = "woody locations 4b 2023"
names(can_ht_20234b_grass_1.5) = "grass locations 4b 2023"
names(can_ht_2023k4a_wood_1.5) = "woody locations k4a 2023"
names(can_ht_2023k4a_grass_1.5) = "grass locations k4a 2023"

names(can_ht_20244b_wood_1.5) = "woody locations 4b 2024"
names(can_ht_20244b_grass_1.5) = "grass locations 4b 2024"
names(can_ht_2024k4a_wood_1.5) = "woody locations k4a 2024"
names(can_ht_2024k4a_grass_1.5) = "grass locations k4a 2024"

stacked_4b_final_1.5 = stack(can_ht_20204b_wood_1.5,can_ht_20224b_wood_1.5, can_ht_20234b_wood_1.5, can_ht_20244b_wood_1.5)
plot(stacked_4b_final_1.5)
stacked_k4a_final_1.5 = stack(can_ht_2020k4a_wood_1.5, can_ht_2022k4a_wood_1.5, can_ht_2023k4a_wood_1.5, can_ht_2024k4a_wood_1.5)
plot(stacked_k4a_final_1.5)

stacked_4b_final_0.95 = stack(can_ht_20204b_wood, can_ht_20224b_wood, can_ht_20234b_wood, can_ht_20244b_wood)
plot(stacked_4b_final_0.95)
stacked_k4a_final_0.95 = stack(can_ht_2020k4a_wood, can_ht_2022k4a_wood, can_ht_2023k4a_wood, can_ht_2024k4a_wood)
plot(stacked_k4a_final_0.95)

######### percent cover values ##############
#2020
cells1.5_4b_2020 <- sum(values(can_ht_20204b_masked) >= 1.5, na.rm=TRUE)
total_cells_4b_2020 <- sum(values(can_ht_20204b_masked) >= -1, na.rm=TRUE)
percentage_gt_1.5_20204b <- (cells1.5_4b_2020/total_cells_4b_2020) *100

cells1.5_k4a_2020 <- sum(values(can_ht_2020k4a_masked) >= 1.5, na.rm=TRUE)
total_cells_k4a_2020 <- sum(values(can_ht_2020k4a_masked) >= -1, na.rm=TRUE)
percentage_gt_1.5_2020k4a <- (cells1.5_k4a_2020/total_cells_k4a_2020) *100

#2022
cells1.5_4b_2022 <- sum(values(can_ht_20224b_masked) >= 1.5, na.rm=TRUE)
total_cells_4b_2022 <- sum(values(can_ht_20224b_masked) >= -1, na.rm=TRUE)
percentage_gt_1.5_20224b <- (cells1.5_4b_2022/total_cells_4b_2022) *100

cells1.5_k4a_2022 <- sum(values(can_ht_2022k4a_masked) >= 1.5, na.rm=TRUE)
total_cells_k4a_2022 <- sum(values(can_ht_2022k4a_masked) >= -1, na.rm=TRUE)
percentage_gt_1.5_2022k4a <- (cells1.5_k4a_2022/total_cells_k4a_2022) *100

#2023
cells1.5_4b_2023 <- sum(values(can_ht_20234b_masked) >= 1.5, na.rm=TRUE)
total_cells_4b_2023 <- sum(values(can_ht_20234b_masked) >= -1, na.rm=TRUE)
percentage_gt_1.5_20234b <- (cells1.5_4b_2023/total_cells_4b_2023) *100

cells1.5_k4a_2023 <- sum(values(can_ht_2023k4a_masked) >= 1.5, na.rm=TRUE)
total_cells_k4a_2023 <- sum(values(can_ht_2023k4a_masked) >= -1, na.rm=TRUE)
percentage_gt_1.5_2023k4a <- (cells1.5_k4a_2023/total_cells_k4a_2023) *100

#2024
cells1.5_4b_2024 <- sum(values(can_ht_20244b_masked) >= 1.5, na.rm=TRUE)
total_cells_4b_2024 <- sum(values(can_ht_20244b_masked) >= -1, na.rm=TRUE)
percentage_gt_1.5_20244b <- (cells1.5_4b_2024/total_cells_4b_2024) *100

cells1.5_k4a_2024 <- sum(values(can_ht_2024k4a_masked) >= 1.5, na.rm=TRUE)
total_cells_k4a_2024 <- sum(values(can_ht_2024k4a_masked) >= -1, na.rm=TRUE)
percentage_gt_1.5_2024k4a <- (cells1.5_k4a_2024/total_cells_k4a_2024) *100
####printing percentage of change in heights ##########
###4B###
print(percentage_gt_1.5_20204b)
print(percentage_gt_1.5_20224b)
print(percentage_gt_1.5_20234b)
print(percentage_gt_1.5_20244b)

####K4A#####
print(percentage_gt_1.5_2020k4a)
print(percentage_gt_1.5_2022k4a)
print(percentage_gt_1.5_2023k4a)
print(percentage_gt_1.5_2024k4a)


# 1.25 m cutoff -----------------------------------------------------------
#changing name
can_ht_20204b_wood_1.25 = can_ht_20204b_masked
can_ht_2020k4a_wood_1.25 = can_ht_2020k4a_masked
can_ht_20224b_wood_1.25 = can_ht_20224b_masked
can_ht_2022k4a_wood_1.25 = can_ht_2022k4a_masked
can_ht_20234b_wood_1.25 = can_ht_20234b_masked
can_ht_2023k4a_wood_1.25 = can_ht_2023k4a_masked
can_ht_20244b_wood_1.25 = can_ht_20244b_masked
can_ht_2024k4a_wood_1.25 = can_ht_2024k4a_masked


#using a 1.25m cutoff
#identifying the woody areas in 4b and k4a in 2020-2024
#2020
can_ht_20204b_wood_1.25[can_ht_20204b_wood_1.25 < 1.25] = 0
can_ht_20204b_wood_1.25[can_ht_20204b_wood_1.25 > 1.25] = 1
can_ht_2020k4a_wood_1.25[can_ht_2020k4a_wood_1.25 < 1.25] = 0
can_ht_2020k4a_wood_1.25[can_ht_2020k4a_wood_1.25 > 1.25] = 1
plot(can_ht_20204b_wood_1.25)
plot(can_ht_2020k4a_wood_1.25)

#2022
can_ht_20224b_wood_1.25[can_ht_20224b_wood_1.25 < 1.25] = 0
can_ht_20224b_wood_1.25[can_ht_20224b_wood_1.25 > 1.25] = 1
can_ht_2022k4a_wood_1.25[can_ht_2022k4a_wood_1.25 < 1.25] = 0
can_ht_2022k4a_wood_1.25[can_ht_2022k4a_wood_1.25 > 1.25] = 1
plot(can_ht_20224b_wood_1.25)
plot(can_ht_2022k4a_wood_1.25)

#2023
can_ht_20234b_wood_1.25[can_ht_20234b_wood_1.25 < 1.25] = 0
can_ht_20234b_wood_1.25[can_ht_20234b_wood_1.25 > 1.25] = 1
can_ht_2023k4a_wood_1.25[can_ht_2023k4a_wood_1.25 < 1.25] = 0
can_ht_2023k4a_wood_1.25[can_ht_2023k4a_wood_1.25 > 1.25] = 1
plot(can_ht_20234b_wood_1.25)
plot(can_ht_2023k4a_wood_1.25)

#2024
can_ht_20244b_wood_1.25[can_ht_20244b_wood_1.25 < 1.25] = 0
can_ht_20244b_wood_1.25[can_ht_20244b_wood_1.25 > 1.25] = 1
can_ht_2024k4a_wood_1.25[can_ht_2024k4a_wood_1.25 < 1.25] = 0
can_ht_2024k4a_wood_1.25[can_ht_2024k4a_wood_1.25 > 1.25] = 1
plot(can_ht_20244b_wood_1.25)
plot(can_ht_2024k4a_wood_1.25)

# Identifying grassy areas in 4b and k4a in 2020-2024
#changing names
can_ht_20204b_grass_1.25 = can_ht_20204b_masked
can_ht_2020k4a_grass_1.25 = can_ht_2020k4a_masked
can_ht_20224b_grass_1.25 = can_ht_20224b_masked
can_ht_2022k4a_grass_1.25 = can_ht_2022k4a_masked
can_ht_20234b_grass_1.25 = can_ht_20234b_masked
can_ht_2023k4a_grass_1.25 = can_ht_2023k4a_masked
can_ht_20244b_grass_1.25 = can_ht_20244b_masked
can_ht_2024k4a_grass_1.25 = can_ht_2024k4a_masked
#Identifying grassy areas (setting parameters)
#2020
can_ht_20204b_grass_1.25[can_ht_20204b_grass_1.25 < 1.25] = -1
can_ht_20204b_grass_1.25[can_ht_20204b_grass_1.25 > 1.25] = 0
can_ht_20204b_grass_1.25[can_ht_20204b_grass_1.25 < -.5] = 1
plot(can_ht_20204b_grass_1.25)
can_ht_2020k4a_grass_1.25[can_ht_2020k4a_grass_1.25 < 1.25] = -1
can_ht_2020k4a_grass_1.25[can_ht_2020k4a_grass_1.25 > 1.25] = 0
can_ht_2020k4a_grass_1.25[can_ht_2020k4a_grass_1.25 < -.5] = 1
plot(can_ht_2020k4a_grass_1.25)

#2022
can_ht_20224b_grass_1.25[can_ht_20224b_grass_1.25 < 1.25] = -1
can_ht_20224b_grass_1.25[can_ht_20224b_grass_1.25 > 1.25] = 0
can_ht_20224b_grass_1.25[can_ht_20224b_grass_1.25 < -.5] = 1
plot(can_ht_20224b_grass_1.25)
can_ht_2022k4a_grass_1.25[can_ht_2022k4a_grass_1.25 < 1.25] = -1
can_ht_2022k4a_grass_1.25[can_ht_2022k4a_grass_1.25 > 1.25] = 0
can_ht_2022k4a_grass_1.25[can_ht_2022k4a_grass_1.25 < -.5] = 1
plot(can_ht_2022k4a_grass_1.25)

#2023
can_ht_20234b_grass_1.25[can_ht_20234b_grass_1.25 < 1.25] = -1
can_ht_20234b_grass_1.25[can_ht_20234b_grass_1.25 > 1.25] = 0
can_ht_20234b_grass_1.25[can_ht_20234b_grass_1.25 < -.5] = 1
plot(can_ht_20234b_grass_1.25)
can_ht_2023k4a_grass_1.25[can_ht_2023k4a_grass_1.25 < 1.25] = -1
can_ht_2023k4a_grass_1.25[can_ht_2023k4a_grass_1.25 > 1.25] = 0
can_ht_2023k4a_grass_1.25[can_ht_2023k4a_grass_1.25 < -.5] = 1
plot(can_ht_2023k4a_grass_1.25)

#2024
can_ht_20244b_grass_1.25[can_ht_20244b_grass_1.25 < 1.25] = -1
can_ht_20244b_grass_1.25[can_ht_20244b_grass_1.25 > 1.25] = 0
can_ht_20244b_grass_1.25[can_ht_20244b_grass_1.25 < -.5] = 1
plot(can_ht_20244b_grass_1.25)
can_ht_2024k4a_grass_1.25[can_ht_2024k4a_grass_1.25 < 1.25] = -1
can_ht_2024k4a_grass_1.25[can_ht_2024k4a_grass_1.25 > 1.25] = 0
can_ht_2024k4a_grass_1.25[can_ht_2024k4a_grass_1.25 < -.5] = 1
plot(can_ht_2024k4a_grass_1.25)

####### changing names again for woody and grass layers #######
names(can_ht_20204b_wood_1.25) = "woody locations 4b 2020"
names(can_ht_20204b_grass_1.25) = "grass locations 4b 2020"
names(can_ht_2020k4a_wood_1.25) = "woody locations k4a 2020"
names(can_ht_2020k4a_grass_1.25) = "grass locations k4a 2020"

names(can_ht_20224b_wood_1.25) = "woody locations 4b 2022"
names(can_ht_20224b_grass_1.25) = "grass locations 4b 2022"
names(can_ht_2022k4a_wood_1.25) = "woody locations k4a 2022"
names(can_ht_2022k4a_grass_1.25) = "grass locations k4a 2022"

names(can_ht_20234b_wood_1.25) = "woody locations 4b 2023"
names(can_ht_20234b_grass_1.25) = "grass locations 4b 2023"
names(can_ht_2023k4a_wood_1.25) = "woody locations k4a 2023"
names(can_ht_2023k4a_grass_1.25) = "grass locations k4a 2023"

names(can_ht_20244b_wood_1.25) = "woody locations 4b 2024"
names(can_ht_20244b_grass_1.25) = "grass locations 4b 2024"
names(can_ht_2024k4a_wood_1.25) = "woody locations k4a 2024"
names(can_ht_2024k4a_grass_1.25) = "grass locations k4a 2024"

stacked_4b_final_1.25 = stack(can_ht_20204b_wood_1.25,can_ht_20224b_wood_1.25, can_ht_20234b_wood_1.25, can_ht_20244b_wood_1.25)
plot(stacked_4b_final_1.25)
stacked_k4a_final_1.25 = stack(can_ht_2020k4a_wood_1.25, can_ht_2022k4a_wood_1.25, can_ht_2023k4a_wood_1.25, can_ht_2024k4a_wood_1.25)
plot(stacked_k4a_final_1.25)


######### percent cover values ##############
#2020
cells1.25_4b_2020 <- sum(values(can_ht_20204b_masked) >= 1.25, na.rm=TRUE)
total_cells_4b_2020 <- sum(values(can_ht_20204b_masked) >= -1, na.rm=TRUE)
percentage_gt_1.25_20204b <- (cells1.25_4b_2020/total_cells_4b_2020) *100

cells1.25_k4a_2020 <- sum(values(can_ht_2020k4a_masked) >= 1.25, na.rm=TRUE)
total_cells_k4a_2020 <- sum(values(can_ht_2020k4a_masked) >= -1, na.rm=TRUE)
percentage_gt_1.25_2020k4a <- (cells1.25_k4a_2020/total_cells_k4a_2020) *100

#2022
cells1.25_4b_2022 <- sum(values(can_ht_20224b_masked) >= 1.25, na.rm=TRUE)
total_cells_4b_2022 <- sum(values(can_ht_20224b_masked) >= -1, na.rm=TRUE)
percentage_gt_1.25_20224b <- (cells1.25_4b_2022/total_cells_4b_2022) *100

cells1.25_k4a_2022 <- sum(values(can_ht_2022k4a_masked) >= 1.25, na.rm=TRUE)
total_cells_k4a_2022 <- sum(values(can_ht_2022k4a_masked) >= -1, na.rm=TRUE)
percentage_gt_1.25_2022k4a <- (cells1.25_k4a_2022/total_cells_k4a_2022) *100

#2023
cells1.25_4b_2023 <- sum(values(can_ht_20234b_masked) >= 1.25, na.rm=TRUE)
total_cells_4b_2023 <- sum(values(can_ht_20234b_masked) >= -1, na.rm=TRUE)
percentage_gt_1.25_20234b <- (cells1.25_4b_2023/total_cells_4b_2023) *100

cells1.25_k4a_2023 <- sum(values(can_ht_2023k4a_masked) >= 1.25, na.rm=TRUE)
total_cells_k4a_2023 <- sum(values(can_ht_2023k4a_masked) >= -1, na.rm=TRUE)
percentage_gt_1.25_2023k4a <- (cells1.25_k4a_2023/total_cells_k4a_2023) *100

#2024
cells1.25_4b_2024 <- sum(values(can_ht_20244b_masked) >= 1.25, na.rm=TRUE)
total_cells_4b_2024 <- sum(values(can_ht_20244b_masked) >= -1, na.rm=TRUE)
percentage_gt_1.25_20244b <- (cells1.25_4b_2024/total_cells_4b_2024) *100

cells1.25_k4a_2024 <- sum(values(can_ht_2024k4a_masked) >= 1.25, na.rm=TRUE)
total_cells_k4a_2024 <- sum(values(can_ht_2024k4a_masked) >= -1, na.rm=TRUE)
percentage_gt_1.25_2024k4a <- (cells1.25_k4a_2024/total_cells_k4a_2024) *100

####printing percentage of change in heights ##########
###4B###
print(percentage_gt_1.25_20204b)
print(percentage_gt_1.25_20224b)
print(percentage_gt_1.25_20234b)
print(percentage_gt_1.25_20244b)

####K4A#####
print(percentage_gt_1.25_2020k4a)
print(percentage_gt_1.25_2022k4a)
print(percentage_gt_1.25_2023k4a)
print(percentage_gt_1.25_2024k4a)


# percent cover bar graph -------------------------------------------------

cover <- read_csv("cover_data.csv")

#setting the plot theme
theme_set(theme_bw()+ theme(text = element_text(size=15),axis.text.x = element_text(size=12), 
                            axis.text.y = element_text(size=14), axis.title.y = element_text(size=14),
                            axis.title.x = element_text(size=14), panel.grid.major = element_blank(), 
                            panel.grid.minor = element_blank()))

cover %>% 
  ggplot(aes(x = year, y = pcover, fill = type))+
  geom_bar(stat = "identity", position = "dodge", na.rm = FALSE)+
  scale_fill_manual("Type", values=c("#D69C4E", "#046C9A"))+
  ylab("Percent woody cover (%)")+
  xlab(NULL)
  



# density plot ------------------------------------------------------------


den_20204b <- data.frame(getValues(can_ht_20204b_masked))
names(den_20204b)[1] <- "height"

den_20224b <- data.frame(getValues(can_ht_20224b_masked))
names(den_20224b)[1] <- "height"

den_20234b <- data.frame(getValues(can_ht_20234b_masked))
names(den_20234b)[1] <- "height"

den_20244b <- data.frame(getValues(can_ht_20244b_masked))
names(den_20244b)[1] <- "height"

den_2020k4a <- data.frame(getValues(can_ht_2020k4a_masked))
names(den_2020k4a)[1] <- "height"

den_2022k4a <- data.frame(getValues(can_ht_2022k4a_masked))
names(den_2022k4a)[1] <- "height"

den_2023k4a <- data.frame(getValues(can_ht_2023k4a_masked))
names(den_2023k4a)[1] <- "height"

den_2024k4a <- data.frame(getValues(can_ht_2024k4a_masked))
names(den_2024k4a)[1] <- "height"


theme_set(theme_bw()+ theme(text = element_text(size=15),axis.text.x = element_text(size=12), 
                            axis.text.y = element_text(size=20), axis.title.y = element_text(size=20),
                            axis.title.x = element_text(size=), panel.grid.major = element_blank(), 
                            panel.grid.minor = element_blank()))


ggplot(den_20204b, aes(y = height)) +
  geom_density(fill = "darkgoldenrod2", alpha = 0.8)+
  lims(x = c(0,0.5), y = c(0,5))+
  theme_bw()

ggplot(den_20224b, aes(y = height)) +
  geom_density(fill = "darkgoldenrod2", alpha = 0.8)+
  lims(x = c(0,0.75), y = c(0,5))+
  theme_bw()
       
ggplot(den_20234b, aes(y = height)) +
  geom_density(fill = "darkgoldenrod2", alpha = 0.8)+
  lims(x = c(0,0.75), y = c(0,5))+
  theme_bw()

ggplot(den_20244b, aes(y = height)) +
  geom_density(fill = "darkgoldenrod2", alpha = 0.8)+
  lims(x = c(0,0.75), y = c(0,5))+
  theme_bw()


ggplot(den_2020k4a, aes(y = height)) +
  geom_density(fill = "cadetblue", alpha = 0.8)+
  lims(x = c(0,0.5), y = c(0,5))+
  theme_bw()

ggplot(den_2022k4a, aes(y = height)) +
  geom_density(fill = "cadetblue", alpha = 0.8)+
  lims(x = c(0,0.75), y = c(0,5))+
  theme_bw()

ggplot(den_2023k4a, aes(y = height)) +
  geom_density(fill = "cadetblue", alpha = 0.8)+
  lims(x = c(0,0.75), y = c(0,5))+ 
  theme_bw()

ggplot(den_2024k4a, aes(y = height)) +
  geom_density(fill = "cadetblue", alpha = 0.8)+
  lims(x = c(0,0.75), y = c(0,5))+
  theme_bw()
den_2024k4a

