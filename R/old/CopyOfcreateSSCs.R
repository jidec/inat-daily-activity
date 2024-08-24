#min_per_cell_n = 30
# create SSCs simply
# in daymet_data, temp and more must be unlisted and set as columns
createSSCs <- function(obs_data, daymet_data, scale_inv_effort=T, earliest_hr=8, latest_hr=20, min_per_cell_n=30){
    library(dplyr)
    library(mgcv)

    obs_data <- subset(obs_data, select = -c(geometry))
    obs_data <- filter(obs_data, local_hour >= earliest_hr)
    obs_data <- filter(obs_data, local_hour <= latest_hr)

    Mode <- function(x) {
        ux <- unique(x)
        ux[which.max(tabulate(match(x, ux)))]
    }
    # group daymet data into season_tile
    daymet_grouped <- daymet_data %>%
        group_by(season,tile) %>%
        dplyr::summarise(temp = mean(tmax),daylength=mean(daylength),vp=mean(vp),precip=mean(precip),srad=mean(srad),lat=mean(latitude),lon=mean(longitude))

    # NEW - Akshay approach
    # format sampling effort
    sampling_effort <- obs_data %>%
      group_by(season, cell, local_hour) %>%
      summarise(n_obs = n())

    # make gam
    m <- mgcv::gam(n_obs ~ season + cell + local_hour, data=sampling_effort, family = "poisson")
    # bind pred into data
    pred <- predict(m, obs_data)
    pred <- 1 / as.numeric(pred)
    obs_data$inverse_pred_effort <- pred

    scale01 <- function(input_vector) {
      min_value <- min(input_vector)
      max_value <- max(input_vector)
      scaled_vector <- (input_vector - min_value) / (max_value - min_value)
      return(scaled_vector)
    }

    if(scale_inv_effort){
      obs_data$inverse_pred_effort <- scale01(obs_data$inverse_pred_effort)
    }

    my_bootstrap_median <- function(hours,inv_efforts){
      if(length(hours) < min_per_cell_n){
        return(-1)
      }

      inv_efforts[is.na(inv_efforts)] <- 0

      sampled_hours <- sample(hours, replace=T, size=length(hours), prob=inv_efforts)
      med <- median(sampled_hours)
      return(med)
    }

    my_bootstrap_q10 <- function(hours,inv_efforts){
      if(length(hours) < min_per_cell_n){
        return(-1)
      }
      inv_efforts[is.na(inv_efforts)] <- 0

      sampled_hours <- sample(hours, replace=T, size=length(hours), prob=inv_efforts)
      q10 <- quantile(sampled_hours, probs = 0.15)
      return(q10)
    }

    my_bootstrap_q90 <- function(hours,inv_efforts){
      if(length(hours) < min_per_cell_n){
        return(-1)
      }
      inv_efforts[is.na(inv_efforts)] <- 0

      sampled_hours <- sample(hours, replace=T, size=length(hours), prob=inv_efforts)
      q90 <- quantile(sampled_hours, probs = 0.90)
      return(q90)
    }

    # summarize obs data down to sscs
    sscs <- obs_data %>%
      group_by(species,season,cell) %>%
      dplyr::summarise(n = n(),obs=list(local_hour),tile=Mode(daymet_tile),
                       median_hr_bs=my_bootstrap_median(local_hour,inverse_pred_effort),
                       q10_hr_bs=my_bootstrap_q10(local_hour,inverse_pred_effort),
                       q90_hr_bs=my_bootstrap_q90(local_hour,inverse_pred_effort)) %>%
      dplyr::filter(n >= min_per_cell_n)

    # merge daymet data in
    sscs <-  merge(sscs,daymet_grouped,by=c("tile","season"))

    colnames(sscs) <- c("season","grid_id","tile","species","obs_n","obs","daymet_tile","tmax","daylength","vp","precip","srad","lat","lon")
    return(sscs)
}
