library(readxl)
library(tidyverse)
library(stringr)

# Fall Run =========================================================================================
fall_run_scenario_names <- 
  read_csv("data/Fall-Run-deterministic-results/scenario-names-deterministic-models.csv") 

# dataframe with path and corresponding scenario name 
fall_run_scenario_paths <- tibble(
  path = list.files("data/Fall-Run-deterministic-results/", full.names = TRUE, pattern = ".xlsx"), 
  scenario_number = as.numeric(str_match(path, "FallRunScenarioDeterministicNEW([0-9]+\\.?[0-9]?)")[,2])
) %>% 
  left_join(fall_run_scenario_names)


# natural spawners ------------------------------

nat_spawn_sheet <- "NatSpawn"

fall_run_natural_spawners <- 
  pmap_df(fall_run_scenario_paths, function(path, scenario_number, scenario_name, scenario_definition) {
    read_excel(path, sheet = nat_spawn_sheet,
               skip = 1,
               col_names = paste(6:25)) %>% 
      head(-1) %>% # remove summary last row
      mutate(watershed = cvpiaData::watershed_ordering$watershed, 
             scenario = scenario_name) %>% 
      gather(year, nat_spawners, -watershed, -scenario) %>% 
      mutate(year = as.numeric(year))
  })

write_rds(natural_spawners_additional_results, "data/fall-run-nat-spawners.rds")

# juv bio mass -------------------------------

juv_biomass_sheet <- "JuvBio"

fall_run_juv_biomass <- 
  pmap_df(fall_run_scenario_paths, function(path, scenario_number, scenario_name, ...) {
    read_excel(path, sheet = juv_biomass_sheet,
               skip = 1,
               col_names = paste(6:25)) %>% 
      head(-1) %>% # remove summary last row
      mutate(watershed = cvpiaData::watershed_ordering$watershed, 
             scenario = scenario_name) %>% 
      gather(year, biomass, -watershed, -scenario) %>% 
      mutate(year = as.numeric(year))
  })

write_rds(fall_run_juv_biomass, "data/fall-run-juv-biomass-chipps.rds")


# actions ------------------------------
action_lookup <- c('Spawning Habitat', 'Inchannel Rearing Habitat', 
                   'Floodplain Habitat', 'Survival')
names(action_lookup) <- as.character(2:5)

actions_sheet <- "Decisions"

fall_run_actions <- 
  pmap_df(fall_run_scenario_paths[-1, ], function(path, scenario_number, scenario_name, scenario_definitions) {
    read_excel(path, sheet = actions_sheet, skip = 1, 
               col_names = paste(6:25)) %>% 
      bind_cols(select(cvpiaData::watershed_ordering, watershed)) %>%
      gather(year, action, -watershed) %>% 
      mutate(action_description = as.character(action_lookup[as.character(action)]), 
             scenario = scenario_name, 
             year = as.numeric(year))
  })

write_rds(fall_run_actions, "data/fall-run-actions.rds")


# spring run =====================================================================================

spring_run_scenario_names <- 
  read_csv("data/spring-run-deterministic-models/scenario-names-deterministic-models.csv") 

# dataframe with path and corresponding scenario name 
spring_run_scenario_paths <- tibble(
  path = list.files("data/spring-run-deterministic-models/", full.names = TRUE, pattern = ".xlsx"), 
  scenario_number = as.numeric(str_match(path, "SpringRunScenarioDeterministicNEW([0-9]+\\.?[0-9]?)")[,2])
) %>% 
  left_join(spring_run_scenario_names)

# natural spawners ------------------------------

nat_spawn_sheet <- "NatSpawn"

spring_run_natural_spawners <- 
  pmap_df(spring_run_scenario_paths, function(path, scenario_number, scenario_name, scenario_definition) {
    read_excel(path, sheet = nat_spawn_sheet,
               skip = 1,
               col_names = paste(6:25)) %>% 
      head(-1) %>% # remove summary last row
      mutate(watershed = cvpiaData::watershed_ordering$watershed, 
             scenario = scenario_name) %>% 
      gather(year, nat_spawners, -watershed, -scenario) %>% 
      mutate(year = as.numeric(year))
  })

write_rds(spring_run_natural_spawners, "data/spring_run_nat_spawners.rds")

# juv bio mass -------------------------------

juv_biomass_sheet <- "JuvBio"

spring_run_juv_biomass <- 
  pmap_df(spring_run_scenario_paths, function(path, scenario_number, scenario_name, ...) {
    read_excel(path, sheet = juv_biomass_sheet,
               skip = 1,
               col_names = paste(6:25)) %>% 
      head(-1) %>% # remove summary last row
      mutate(watershed = cvpiaData::watershed_ordering$watershed, 
             scenario = scenario_name) %>% 
      gather(year, biomass, -watershed, -scenario) %>% 
      mutate(year = as.numeric(year))
  })

write_rds(spring_run_juv_biomass, "data/spring_run_juv_biomass_chipps.rds")


# actions ------------------------------
action_lookup <- c('Spawning Habitat', 'Inchannel Rearing Habitat', 
                   'Floodplain Habitat', 'Survival')
names(action_lookup) <- as.character(2:5)

actions_sheet <- "Decisions"

spring_run_actions <- 
  pmap_df(spring_run_scenario_paths[-1, ], function(path, scenario_number, scenario_name, scenario_definitions) {
    read_excel(path, sheet = actions_sheet, skip = 1, 
               col_names = paste(6:25)) %>% 
      bind_cols(select(cvpiaData::watershed_ordering, watershed)) %>%
      gather(year, action, -watershed) %>% 
      mutate(action_description = as.character(action_lookup[as.character(action)]), 
             scenario = scenario_name, 
             year = as.numeric(year))
  })

write_rds(actions_additional_results, "data/actions.rds")

# Winter Run ====================================================================================
winter_run_scenario_names <- 
  read_csv("data/winter-run-determnisitic-models/scenario-names-deterministic-models.csv") 


# dataframe with path and corresponding scenario name 
winter_run_scenario_paths <- tibble(
  path = list.files("data/winter-run-determnisitic-models/", full.names = TRUE, pattern = ".xlsx"), 
  scenario_number = as.numeric(str_match(path, "WinterRunScenarioDeterministicNEW([0-9]+\\.?[0-9]?)")[,2])
) %>% 
  left_join(winter_run_scenario_names)

# natural spawners ------------------------------

nat_spawn_sheet <- "NatSpawn"

winter_run_natural_spawners <- 
  pmap_df(winter_run_scenario_paths, function(path, scenario_number, scenario_name, scenario_definition) {
    read_excel(path, sheet = nat_spawn_sheet,
               skip = 1,
               col_names = paste(6:25)) %>% 
      head(-1) %>% # remove summary last row
      mutate(watershed = cvpiaData::watershed_ordering$watershed, 
             scenario = scenario_name) %>% 
      gather(year, nat_spawners, -watershed, -scenario) %>% 
      mutate(year = as.numeric(year))
  })

write_rds(winter_run_natural_spawners, "data/winter_run_nat_spawners.rds")

# juv bio mass -------------------------------

juv_biomass_sheet <- "JuvBio"

winter_run_juv_biomass <- 
  pmap_df(winter_run_scenario_paths, function(path, scenario_number, scenario_name, ...) {
    read_excel(path, sheet = juv_biomass_sheet,
               skip = 1,
               col_names = paste(6:25)) %>% 
      head(-1) %>% # remove summary last row
      mutate(watershed = cvpiaData::watershed_ordering$watershed, 
             scenario = scenario_name) %>% 
      gather(year, biomass, -watershed, -scenario) %>% 
      mutate(year = as.numeric(year))
  })

write_rds(winter_run_juv_biomass, "data/winter_run_juv_biomass_chipps.rds")


# actions ------------------------------
action_lookup <- c('Spawning Habitat', 'Inchannel Rearing Habitat', 
                   'Floodplain Habitat', 'Survival')
names(action_lookup) <- as.character(2:5)

actions_sheet <- "Decisions"

winter_run_actions <- 
  pmap_df(winter_run_scenario_paths[-1, ], function(path, scenario_number, scenario_name, scenario_definitions) {
    read_excel(path, sheet = actions_sheet, skip = 1, 
               col_names = paste(6:25)) %>% 
      bind_cols(select(cvpiaData::watershed_ordering, watershed)) %>%
      gather(year, action, -watershed) %>% 
      mutate(action_description = as.character(action_lookup[as.character(action)]), 
             scenario = scenario_name, 
             year = as.numeric(year))
  })

write_rds(actions_additional_results, "data/winter_run_actions.rds")

