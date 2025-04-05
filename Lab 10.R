install.packages("terra")
library(terra)
install.packages("tmap")
library(tmap)
install.packages("sf")
library(sf)

dem = rast(paste0("C:/Users/kate/Downloads/Classes/AGR 333/Week 9-10/ForL2/files/unit2.img"))

# extract slope and aspect
slope = terrain(dem, v = "slope", unit = "degrees", neighbors = 8)
aspect = terrain(dem, v = "aspect", unit = "degrees")


#using tmap to visualize slope and aspect
ttm()
# SLOPE #
tm_shape(slope, alpha = 0.5) +
  tm_raster(style = "cont", alpha = 0.6, title = "Slope (deg)")
# ASPECT #
tm_shape(aspect) +
  tm_raster(style = "cont")
# Q1 The degree range of each direction is 90 degrees

# create a matrix of directions
asp_class <- matrix(c(
  0, 45, 1,
  45, 90, 2,
  90, 175, 2,
  175, 180, 3,
  180, 225, 3,
  225, 270, 4,
  270, 315, 4,
  315, 360, 1
), ncol = 3, byrow = TRUE)

# classify aspect into 4 directions
asp <- classify(aspect, asp_class)

# visualize aspect again
tm_shape(asp) +
  tm_raster(style = "cat", palette = c("white", "blue", "green", "yellow",
                                       "red"),
            labels = c(NA, "North", "East", "South", "West"), alpha = 0.2)

# read csv

sum_u2 = read.csv("C:/Users/kate/Downloads/sum_u2.csv")

# read in .shp file
svy_pts <- st_read(paste0("C:/Users/kate/Downloads/Classes/AGR 333/Week 9-10/ForL2/files/HEE_Overstory_Survey_Points_2017.shp"))
svy_pts <- st_transform(svy_pts, 32616) # Project to WGS 84 UTM 16 N
survey_pts <- subset(svy_pts, Unit == '2') # Subset for unit 2

# merge data sets
sum_u2 <- merge.data.frame(sum_u2, survey_pts, all.x = TRUE)

# x y coords
sum_u2 <- st_as_sf(sum_u2, coords = c("X", "Y"), crs = 32616)

# create buffer zones
sf_plot <- st_buffer(sum_u2, dist = 17.83)

# check CRS
crs(sf_plot , proj=T)
crs(asp , proj=T)

# transform CRs of plots to match raster images
asp_crs <- crs(asp, proj = TRUE)
sf_plot_crs <- st_transform(sf_plot, crs = asp_crs)

# visualize dominant species by aspect
tm_shape(asp, alpha = 0.5) +
  tm_raster(style = "cat", palette = c("white", "blue", "green", "yellow",
                                       "red"),
            showNA = FALSE, alpha = 0.2, labels = c(NA, "North", "East", "South",
                                                    "West")) +
  tm_shape(sf_plot) +
  tm_polygons('Common.name') +
  tm_layout(legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -0.9)

# visualize dominant species by slope
tm_shape(slope, alpha = 0.5) +
  tm_raster(style = "cont", alpha = 0.6, title = "Slope (deg)") +
  tm_shape(sf_plot) +
  tm_polygons('Common.name', title = "Dom_Species", alpha = 0.6) +
  tm_layout(title = "Dominant trees by slope",
            legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -0.9, size = 1.2)

# basal area distribution
tm_shape(sf_plot) +
  tm_polygons('BA', title = "Basal Area (sq_ft/acre)", palette =
                "brewer.spectral") +
  tm_layout(title = "Basal Area Distribution",
            legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -1.5, size = 1.2) +
  tm_scale_bar()

# tpa distribution
tm_shape(sf_plot) +
  tm_polygons('TPA', title = "Trees Per Acre", palette = "brewer.spectral") +
  tm_layout(title = "TPA Distribution",
            legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -1.5, size = 1.2) +
  tm_scale_bar()

# biomass distribution
ttm()
tm_shape(asp, alpha = 0.5) +
  tm_raster(style = "cat", palette = c("white", "blue", "green", "yellow",
                                       "red"),
            showNA = FALSE, alpha = 0.2, labels = c(NA, "North", "East", "South",
                                                    "West")) +
  tm_shape(sf_plot) +
  tm_polygons('bm_tonpa', title = "Biomass (tons/ac)", palette =
                "brewer.spectral") +
  tm_layout(title = "Biomass Distribution",
            legend.outside = TRUE, legend.outside.size = 0.2) +
  tm_text("Plot", ymod = -1.5, size = 1.2) +
  tm_scale_bar()
