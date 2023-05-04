library(sf)
library(tidyverse)
library(plot3D)
library(rayshader)
library(abind)
library(stars)
library(rgl)
library(MetBrewer)
library(colorspace)
library(rayrender)
data <- st_read("kontur_population_IN_20220630.gpkg")


crsLAEA <- "+proj=lcc +lat_0=24 +lon_0=80 +lat_1=12.472955 +lat_2=35.1728044444444 +x_0=4000000 +y_0=4000000"
projected <- st_transform(data[2,],crsLAEA)

head(projected)

bb <- st_bbox(data[2,])

height <- st_distance(
  st_point(c(bb[["xmin"]], bb[["ymin"]])),
  st_point(c(bb[["xmin"]], bb[["ymax"]]))
)
width <- st_distance(
  st_point(c(bb[["xmin"]], bb[["ymin"]])),
  st_point(c(bb[["xmax"]], bb[["ymin"]]))
)

if (height > width) {
  height_ratio <- 1
  width_ratio <- width / height
} else {
  width_ratio <- 1
  height_ratio <- height / width
}

height_ratio
width_ratio

size <- 3000

rast_ <- st_rasterize(data[2,],nx=floor(size*width_ratio),
                      ny=floor(size*height_ratio))

mat <- matrix(rast_$population,
              nrow = floor(size * width_ratio),
              ncol = floor(size * height_ratio))


c1 <- met.brewer("Hokusai2")

texture <- grDevices::colorRampPalette(c1,bias=2)(256)
swatchplot(texture)

rgl.close()

mat |>
  height_shade(texture = texture) |>
  plot_3d(heightmap = mat ,
          zscale=100,
          solid=FALSE,
          shadowdepth = 0,
          shadow_darkness=.95)
render_camera(theta = -25,phi=60,zoom=.9)



render_highquality(filename="draft1.png")


