sscs <- readRDS("D:/GitProjects/inat-daily-activity-analysis/_targets/objects/sscs2")
species <- "Abaeis nicippe"
cell <- 85
season <- 3
year <- 2021

# plot a species-season-cell distribution given raw iNat data with a sp, se, and grid_id
plotSSC <- function(sscs, species, season, year=NULL, cell)
{
  library(dplyr)
  library(spatialrisk)
  library(plyr)
  library(sf)
  library(sp)
  library(phenesse)

  my_species <- species
  my_season <- season
  my_year <- year
  my_cell <- cell
  ssc <- dplyr::filter(sscs,species==my_species,season==my_season,year==my_year,cell==my_cell)
  ssc$o

  ggplot(sscs, aes(x = values)) +
    geom_smooth(aes(y = ..density..), color = "blue", fill = "skyblue") +
    geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
    ggtitle("Histogram of Bimodal Distribution") +
    xlab("Values") +
    ylab("Frequency") +
    theme_minimal()

  # read in and prep usa_insects to use as observer baseline
  temp <- data
  if(!is.null(sp)){
    temp <- dplyr::filter(temp, species == sp)}
  if(!is.null(se)){
    temp <- dplyr::filter(temp, season == se)}

  temp <- dplyr::filter(temp, n >= nthresh)
  temp <- dplyr::filter(temp, p <= sigthresh)

  hist(temp$median_hour)

  source("src/addHourSeasonBaseline.R")
  usa_insects <- addHourSeasonBaseline(usa_insects)
  usa_insects$latitude <- usa_insects$decimalLatitude
  usa_insects$longitude <- usa_insects$decimalLongitude
  #t <-  read.table(text=usa_insects$location,sep=',',strip.white = TRUE)
  #usa_insects$latitude <- as.numeric(unlist(t[1]))
  #usa_insects$longitude <- as.numeric(unlist(t[2]))
  usa_insects <- usa_insects[complete.cases(usa_insects$decimalLatitude),]
  coordinates(usa_insects) <- cbind(usa_insects$latitude,usa_insects$longitude)
  #usa_insects <- as.data.frame(usa_insects)

  # turn input data (probably all usa ants) into SpatialPoints
  coordinates(data) <- cbind(data$latitude,data$longitude)

  # create SpatialGridDataFrame from data
  grid <- makegrid(data, cellsize = cellsize_miles/69)
  grid$id <- 1:length(grid$x1)
  coordinates(grid) <- cbind(grid$x1,grid$x2)
  gridded(grid) <- TRUE
  plot(grid,add=TRUE)
  grid <- as(grid, "SpatialGridDataFrame")

  # spatial join and add cell info to points
  x <- sp::over(data,grid)
  data <- as.data.frame(data)
  data$grid_id <- x$id
  data$cell_lat <- x$x1
  data$cell_long <- x$x2

  # also add cell info to all obs points
  # turn sampled_usa_insects data into SpatialPoints
  #coordinates(usa_insects) <- cbind(usa_insects$latitude,usa_insects$longitude)
  x <- sp::over(usa_insects,grid)
  usa_insects <- as.data.frame(usa_insects)
  usa_insects$grid_id <- x$id
  usa_insects$cell_lat <- x$x1
  usa_insects$cell_long <- x$x2

  if(!is.null(sp)){
    data <- dplyr::filter(data, scientific_name == sp)
  }
  if(!is.null(se)){
    data <- dplyr::filter(data, season == se)
    usa_insects <- dplyr::filter(usa_insects, season == se)
  }

  if(!is.null(se)){
    data <- dplyr::filter(data, grid_id == g_id)
    usa_insects <- dplyr::filter(usa_insects, grid_id == g_id)
  }

  hist(data$local_hour,axes=TRUE)
  hist(usa_insects$local_hour,axes=TRUE)
}
