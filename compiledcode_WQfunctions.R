colorado <- tibble(siteid = c("USGS-09034500", "USGS-09069000",
                              "USGS-09085000", "USGS-09095500", "USGS-09152500"),
                   basin = c("colorado1", "eagle",
                             "roaring", "colorado3", "gunnison"))

WQ_tidying_data <- function(){
  # Compile all these names into a single vector:
  parameters <- c("Calcium", "Magnesium", "Sodium", "Potassium", "Sulfate", "Sulfate as SO4", "Sulfur Sulfate", "Total Sulfate", "Chloride", "Alkalinity, bicarbonate", "Bicarbonate")

  conc_wide <- readWQPdata(siteid = colorado$siteid, # pulling our siteids column in from the colorado object, becomes a vector
                           sampleMedia = "Water", # WQP also has bilogical data and sediment data
                           startDateLo = "1980-10-01", # must be formatted as YYYY-MM-DD
                           startDateHi = "2023-11-01", # must be formatted as YYYY-MM-DD
                           characteristicName = parameters) # our vector of `characteristcName`s

  View(conc_wide)

  # This code mostly just grabs and renames the most important data columns
  conc_small <-  conc_wide %>%
    select(date = ActivityStartDate,
           parameter = CharacteristicName,
           units = ResultMeasure.MeasureUnitCode,
           siteid = MonitoringLocationIdentifier,
           org = OrganizationFormalName,
           org_id = OrganizationIdentifier,
           time = ActivityStartTime.Time,
           value = ResultMeasureValue,
           sample_method = SampleCollectionMethod.MethodName,
           analytical_method = ResultAnalyticalMethod.MethodName,
           particle_size = ResultParticleSizeBasisText,
           date_time = ActivityStartDateTime,
           media = ActivityMediaName,
           sample_depth = ActivityDepthHeightMeasure.MeasureValue,
           sample_depth_unit = ActivityDepthHeightMeasure.MeasureUnitCode,
           fraction = ResultSampleFractionText,
           status = ResultStatusIdentifier) %>%
    # Remove trailing white space in labels
    mutate(units = trimws(units))

  conc_meta <- conc_small %>%
    left_join(., colorado, by = "siteid") %>%
    dplyr::mutate(ion = dplyr::case_when(parameter == "Calcium" ~ "Ca",
                                         parameter == "Magnesium" ~ "Mg",
                                         parameter == "Sodium" ~ "Na",
                                         parameter == "Potassium" ~ "K",
                                         parameter %in% c("Sulfate", "Sulfate as SO4", "Sulfur Sulfate", "Total Sulfate") ~ "SO4",
                                         parameter == "Chloride" ~ "Cl",
                                         parameter %in% c("Alkalinity, bicarbonate", "Bicarbonate") ~ "HCO3")) %>%
    select(siteid, basin, ion, parameter, date, everything())

  View(conc_meta)

  table(conc_meta$units)

  conc_tidy <- conc_meta %>%
    filter(units == 'mg/l') %>%
    mutate(date = ymd(date)) %>%
    select(date,
           parameter,
           ion,
           siteid,
           basin,
           conc = value)


  # The amazing group_by function groups all the data so that the summary
  # only applies to each subgroup (siteid, date, and parameter combination).
  # So in the end you get a daily average concentration for each siteid and parameter type.
  conc_daily <- conc_tidy %>%
    group_by(date, parameter, siteid, basin) %>%
    summarize(conc = mean(conc, na.rm = T))
  }
