dfTermtime_activities <- load_rds_from_azure(
  config = .config,
  container_type = "bronze",
  file_name = "Termtime_activities_enriched.rds"
) %>% 
  dplyr::filter(
    str_detect(ter_source_file, "activities_20[0-9]{2}.rds$")
  )
  


## Add year variables
dfTermtime_activities <- dfTermtime_activities %>% 
  dplyr::mutate(
    jaar = as.numeric(str_extract(ter_source_file, "20[0-9]{2}")),
    unl_jaar = paste0(jaar, "-", jaar + 1)
  )


## Extract weken uit de Events subtabel
dfTermtime_activities$week_count <- sapply(dfTermtime_activities$ter_events, unpack_weeks)

## Extract duur van werkvormen uit de events subtabel
dfTermtime_activities$durationmin <- sapply(dfTermtime_activities$ter_events, unpack_duration)

