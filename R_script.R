library(sf)
library(tidyverse)
library(rayshader)
library(stars)
library(MetBrewer)

url <-
    "https://geodata-eu-central-1-kontur-public.s3.amazonaws.com/kontur_datasets/kontur_population_IN_20220630.gpkg.gz"
file_name <- "india-population.gpkg.gz"

get_population_data <- function() {
    res <- httr::GET(
        url,
        write_disk(file_name),
        progress()
    )

    R.utils::gunzip(file_name, remove = F)
}

get_population_data()

### 2. LOAD DATA
### -------------
load_file_name <- gsub(".gz", "", file_name)

crsLAEA <- "+proj=lcc +lat_0=24 +lon_0=80 +lat_1=12.472955 +lat_2=35.1728044444444 +x_0=4000000 +y_0=4000000"
load_population_data <- function() {
    pop_df <- sf::st_read(
        load_file_name
    ) |>
        sf::st_transform(crs = crsLAEA)
    return(pop_df)
}

data <- load_population_data()

bb <- st_bbox(data)

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

rast_ <- stars::st_rasterize(data,
    nx = floor(size * width_ratio),
    ny = floor(size * height_ratio)
)

mat <- matrix(rast_$population,
    nrow = floor(size * width_ratio),
    ncol = floor(size * height_ratio)
)


c1 <- met.brewer("Hokusai2")

texture <- grDevices::colorRampPalette(c1, bias = 2)(256)

mat |>
    height_shade(texture = texture) |>
    plot_3d(
        heightmap = mat,
        zscale = 100,
        solid = FALSE,
        shadowdepth = 0,
        shadow_darkness = .95
    )
render_camera(theta = -25, phi = 60, zoom = .9)

render_highquality(
    filename = "draft1.png",
    interactive = F)
