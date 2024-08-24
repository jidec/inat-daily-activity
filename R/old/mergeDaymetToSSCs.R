
mergeDaymetToSSCs <- function(sscs, daymet_data){

  # group daymet data into season_tile
  daymet_grouped <- daymet_data %>%
    group_by(season,tile) %>%
    dplyr::summarise(temp = mean(tmax),daylength=mean(daylength),vp=mean(vp),precip=mean(precip),srad=mean(srad),lat=mean(latitude),lon=mean(longitude))

  # merge daymet data in
  sscs <-  merge(sscs,daymet_grouped,by=c("tile","season"))

  return(sscs)
}
