
# create SSCs simply
# in daymet_data, temp and more must be unlisted and set as columns
#min_per_ssc = 30
#latest_hr = 20
#earliest_hr = 8
#approach = "species_season_year_cell"
#obs_data = readRDS("D:/GitProjects/inat-daily-activity-analysis/_targets/objects/obs_gridded")

# approach can be "species_season_cell" or "species_season_year_cell"
createSSCs <- function(obs_data, approach="species_season_cell", earliest_hr=8, latest_hr=20, min_per_ssc=30){
    library(dplyr)
    library(mgcv)
    library(purrr)
    library(pbapply)
    library(tidyr)

    obs_data <- subset(obs_data, select = -c(geometry))
    obs_data <- filter(obs_data, local_hour >= earliest_hr)
    obs_data <- filter(obs_data, local_hour <= latest_hr)

    cells_coords <- obs_data %>% distinct(cell, cell_lat, cell_lon)

    Mode <- function(x) {
        ux <- unique(x)
        ux[which.max(tabulate(match(x, ux)))]
    }

    # NEW - Akshay approach

    # start by formatting sampling effort
    sampling_effort <- obs_data %>%
      group_by(season, cell, local_hour) %>%
      summarise(n_obs = n())

    # Step 1: Create a list of models for each combination of season and cell
    models <- sampling_effort %>%
      distinct(season, cell) %>%
      group_by(season, cell) %>%
      summarize(model = list(mgcv::gam(n_obs ~ s(local_hour), data = filter(sampling_effort, season == season, cell == cell), family = "poisson")),
                .groups = 'drop')

    # Step 2: Make predictions for each local_hour within each unique combination of season and cell
    gam_predictions <- sampling_effort %>%
      distinct(season, cell, local_hour) %>%
      left_join(models, by = c("season", "cell")) %>%
      rowwise() %>%
      mutate(effort = predict(model, newdata = tibble(local_hour = local_hour), type = "response")) %>%
      select(-model)

    # Step 3. Bind gam-predicted inverse effort to use as weights
    obs_data <- obs_data %>%
      left_join(gam_predictions, by = c("local_hour", "season","cell")) %>%
      dplyr::mutate(
        inverse_effort = 1/effort
      )

    # create bootstrap function
    my_bootstrap <- function(hours, inv_efforts, nbootstraps=50){
      bootstrap_medians <- numeric(nbootstraps)
      bootstrap_q10s <- numeric(nbootstraps)
      bootstrap_q90s <- numeric(nbootstraps)

      # Perform bootstrapping
      for (i in 1:nbootstraps) {
        sampled_hours <- sample(hours, replace=TRUE, size=length(hours), prob=inv_efforts)
        bootstrap_medians[i] <- median(sampled_hours)
        bootstrap_q10s[i] <- quantile(sampled_hours,0.1)
        bootstrap_q90s[i] <- quantile(sampled_hours,0.9)
      }

      # Calculate the 5th percentile, median, and 95th percentile
      median_lower <- quantile(bootstrap_medians, probs = 0.05)
      median_value <- median(bootstrap_medians)
      median_upper <- quantile(bootstrap_medians, probs = 0.95)

      q10_lower <- quantile(bootstrap_q10s, probs=0.05)
      q10_value <- median(bootstrap_q10s)
      q10_upper <- quantile(bootstrap_q10s, probs=0.95)

      q90_lower <- quantile(bootstrap_q90s, probs=0.05)
      q90_value <- median(bootstrap_q90s)
      q90_upper <- quantile(bootstrap_q90s, probs=0.95)

      # Return the results
      return(list(median_lower = median_lower, median_value = median_value, median_upper = median_upper,
                  q10_lower = q10_lower, q10_value = q10_value, q10_upper = q10_upper,
                  q90_lower = q90_lower, q90_value = q90_value, q90_upper = q90_upper))
    }

    # Step 4. Perform the bootstrap using inverse effort as weights
    if(approach=="species_season_cell"){
      # perform the bootstrap
      sscs <- obs_data %>%
        group_by(species, season, cell) %>%
        filter(n() >= min_per_ssc) %>%
        summarize(
          cell = Mode(cell),
          n_obs = n(),
          obs=list(local_hour), # NEW NEW NEW MIGHT BREAK
          unadj_median = median(local_hour),
          bootstrap_results = list(my_bootstrap(local_hour, inverse_effort))
        ) %>%
        unnest_wider(bootstrap_results)
    }
    if(approach=="species_season_year_cell"){
      # perform the bootstrap
      sscs <- obs_data %>%
        group_by(species, season, year, cell) %>%
        filter(n() >= min_per_ssc) %>%
        summarize(
          cell = Mode(cell),
          n_obs = n(),
          obs=list(local_hour), # NEW NEW NEW MIGHT BREAK
          unadj_median = median(local_hour),
          bootstrap_results = list(my_bootstrap(local_hour, inverse_effort))
        ) %>%
        unnest_wider(bootstrap_results)
    }

    # merge cell lats and lons back
    sscs <- merge(sscs,cells_coords,by="cell")

    return(sscs)
}
