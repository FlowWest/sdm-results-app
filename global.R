library(shiny)
library(shinycssloaders)
library(shinythemes)
library(tidyverse)
library(plotly)
library(stringr)
library(DT)

actions <- bind_rows(
  read_rds('data/actions.rds') %>% mutate(model = "Fall Run"), 
  read_rds('data/actions.rds') %>% mutate(model = "Spring Run"),
  read_rds('data/actions.rds') %>% mutate(model = "Winter Run"),
  # read in the rest with of the files with the approapriate mutate statement
)

# diversity_groups <- read_rds('data/diversity_groups.rds')
juv_biomass_chipps <- bind_rows(
  read_rds('data/fall-run-juv-biomass-chipps.rds') %>% mutate(model = "Fall Run"), 
  read_rds('data/spring_run_juv_biomass_chipps.rds') %>% mutate(model = "Spring Run"),
  read_rds('data/winter_run_juv_biomass_chipps.rds') %>% mutate(model = "Winter Run")
  # read in the rest of the files with the appropriate mutate statement
)
nat_spawners <- bind_rows(
  read_rds('data/fall-run-nat-spawners.rds') %>% mutate(model = "Fall Run"), 
  read_rds('data/spring_run_nat_spawners.rds') %>% mutate(model = "Spring Run"), 
  read_rds('data/winter_run_nat_spawners.rds') %>% mutate(model = "Winter Run")
  # read in the rest of the files with the appropriate mutate statement
)

watershed_order <- unique(actions$watershed)

# TODO move this to a dataframe
scenario_definitions <- c(
  "No restoration actions taken on any Central Valley streams",
  "Restoration limited to in-channel Upper Sacramento, Butte, Lower Mid Sac, Feather, American, Deer, Battle. and Stanislaus ",
  "Restoration limited to in-channel Upper Sacramento, Butte, Lower Mid Sac, Feather, American, Deer, Clear, and Stanislaus",
  "Restoration limited to in-channel Upper Sacramento, Butte, Lower Mid Sac, Feather, American, Mokelumne, Clear, and Stanislaus",
  "Restoration limited to in-channel in Mainstem Sacramento only",
  "Restoration limited to in-channel Upper Sac, Lower Mid Sac, Cow Creek and Clear",
  "Restoration limited to in-channel Upper Sac, Lower Mid, and American with some maintenance in Clear and Butte",
  "Restoration limited to floodplain Upper Sac, Upper Mid, Lower Mid, Lower Sac, and San Joaquin",
  "Restoration optimized to increase Winter-run population every year (limited to locations where WR occur)",
  "Restoration optimized to increase Spring-run population every year (limited to locations where SR occur)",
  "Restoration limited to in-channel equally distributed between Upper Mid, Deer, Butte, Clear, Mill, Battle, Antelope",
  "Restoration limited to in-channel distributed between Upper Mid, Deer, Butte, Clear, Mill, Battle, Antelope, with emphasis on Deer, Mill, and Antelope",
  "Restoration optimized to increase Fall-run population every year (one action in each diversity group)",
  "Restoration optimized to increase Fall-run population every year (actions limited to Upper Sac, Lower Sac, American, Stanislaus, and Calaveras)",
  "Restoration optimized to increase Fall-run population every year (actions limited to Upper Sac, Lower Sac, American, Stanislaus, and Mokelumne)"
)

names(scenario_definitions) <- c("No Restoration Actions", "1. In-Channel Only - Urkov", "2. In-Channel Only - Brown", 
                                 "3. In-Channel Only - Bilski", "4. In-Channel Only - Mainstem Sac", 
                                 "5. In-Channel Only - Berry", "6. In-Channel Only - Peterson", 
                                 "7. Floodplain Only - Mainstem Sac", "8. Winter-run Optimized", 
                                 "9. Spring-run Optimized", "10.1. Spring-run In-Channel - Phillis1", 
                                 "10.2. Spring-run In-Channel - Phillis2", "11.1. Fall-run Diversity Group Optimized", 
                                 "12.1. Fall-run Optimized - Beakes", "13.1. Fall-run Optimized - Bilski"
)

scenario_names <- c("No Restoration Actions", "1. In-Channel Only - Urkov", "2. In-Channel Only - Brown", 
                    "3. In-Channel Only - Bilski", "4. In-Channel Only - Mainstem Sac", 
                    "5. In-Channel Only - Berry", "6. In-Channel Only - Peterson", 
                    "7. Floodplain Only - Mainstem Sac", "8. Winter-run Optimized", 
                    "9. Spring-run Optimized", "10.1. Spring-run In-Channel - Phillis1", 
                    "10.2. Spring-run In-Channel - Phillis2", "11.1. Fall-run Diversity Group Optimized", 
                    "12.1. Fall-run Optimized - Beakes", "13.1. Fall-run Optimized - Bilski"
)

names(scenario_names) <- c("No Restoration Actions", "1. In-Channel Only - Urkov", "2. In-Channel Only - Brown", 
                           "3. In-Channel Only - Bilski", "4. In-Channel Only - Mainstem Sac", 
                           "5. In-Channel Only - Berry", "6. In-Channel Only - Peterson", 
                           "7. Floodplain Only - Mainstem Sac", "8. Winter-run Optimized", 
                           "9. Spring-run Optimized", "10.1. Spring-run In-Channel - Phillis1", 
                           "10.2. Spring-run In-Channel - Phillis2", "11.1. Fall-run Diversity Group Optimized", 
                           "12.1. Fall-run Optimized - Beakes", "13.1. Fall-run Optimized - Bilski"
)

scenario_names_to_scenario <- names(scenario_names)
names(scenario_names_to_scenario) <- as.character(scenario_names)

scenario_names_levels <- c(
  "No Restoration Actions", 
  "1. In-Channel Only - Urkov", 
  "2. In-Channel Only - Brown", 
  "3. In-Channel Only - Bilski", 
  "4. In-Channel Only - Mainstem Sac", 
  "5. In-Channel Only - Berry", 
  "6. In-Channel Only - Peterson", 
  "7. Floodplain Only - Mainstem Sac", 
  "8. Winter-run Optimized", 
  "9. Spring-run Optimized", 
  "10.1. Spring-run In-Channel - Phillis1", 
  "10.2. Spring-run In-Channel - Phillis2", 
  "11.1. Fall-run Diversity Group Optimized", 
  "12.1. Fall-run Optimized - Beakes", 
  "13.1. Fall-run Optimized - Bilski"
)

action_summary_colnames <- c(
  "Watershed",
  "In-Channel Only - Urkov", 
  "In-Channel Only - Brown", 
  "In-Channel Only - Bilski", 
  "In-Channel Only - Mainstem Sac", 
  "In-Channel Only - Berry", 
  "In-Channel Only - Peterson", 
  "Floodplain Only - Mainstem Sac", 
  "Winter-run Optimized", 
  "Spring-run Optimized", 
  "Spring-run In-Channel - Phillis1", 
  "Spring-run In-Channel - Phillis2", 
  "Fall-run Diversity Group Optimized", 
  "Fall-run Optimized - Beakes", 
  "Fall-run Optimized - Bilski"
)


actions_summary <-
  actions %>% 
  mutate(action_occured = ifelse(!is.na(action), TRUE, FALSE), 
         scenario = scenario_names[scenario]) %>% 
  group_by(watershed, scenario) %>% 
  summarise(
    total_actions = sum(action_occured)
  ) %>% ungroup() %>% 
  spread(scenario, total_actions) %>% 
  mutate(watershed = factor(watershed, levels = watershed_order)) %>% 
  arrange(watershed) %>% 
  filter(!(watershed %in% c('Yolo Bypass', 'Sutter Bypass'))) %>% 
  select(watershed, scenario_names_levels[-1])

names(actions_summary) <- action_summary_colnames

sr_exists <- cvpiaHabitat::modeling_exist %>% 
  select(Watershed, SR_fry) %>% 
  filter(!is.na(SR_fry)) %>% 
  pull(Watershed)

wr_exists <- c('Upper Sacramento River', 'Upper-mid Sacramento River',
               'Lower-mid Sacramento River', 'Lower Sacramento River',
               'Battle Creek')

action_units <- c(1, 2, 2, .5)
names(action_units) <- c('Spawning Habitat', 'Inchannel Rearing Habitat', 
                         'Floodplain Habitat', 'Survival')

units <- c(rep('acres', 3), '%')
names(units) <- c('Spawning Habitat', 'Inchannel Rearing Habitat', 
                  'Floodplain Habitat', 'Survival')

# once the new data has been read in, we can recompute these 
# stats ----
valley_wide_biomass <- juv_biomass_chipps %>% 
  group_by(model, scenario, year) %>% 
  summarise(juv_biomass = sum(biomass)) %>% 
  ungroup()

no_action_end_biomass <- valley_wide_biomass %>% 
  filter(scenario == 'No Restoration Actions', year == 25) %>% 
  select(model, no_action_end_biomass = juv_biomass)

biomass <- valley_wide_biomass %>% 
  filter(year == 25) %>% 
  ungroup() %>%
  left_join(no_action_end_biomass, by = c("model" = "model")) %>% 
  mutate(no_action_end = no_action_end_biomass,
         `Juvenile Biomass at Chipps` = 
           round(((juv_biomass - no_action_end) / no_action_end) * 100, 1),
         Scenario = scenario_names[scenario]) %>% 
  select(model, Scenario, `Juvenile Biomass at Chipps`)

#  change this to reflect the changes in the biomass code
# after you have combined the three model runs in line 20
valley_wide_nat_spawners <- nat_spawners %>%
  group_by(model, scenario, year) %>% 
  summarise(nat_spawners = sum(nat_spawners)) %>%
  ungroup()

no_action_end_nat_spawners <- valley_wide_nat_spawners %>% 
  filter(scenario == 'No Restoration Actions', year == 25) %>% 
  select(model, no_action_end_nat_spawners = nat_spawners)


spawners <- valley_wide_nat_spawners %>% 
  filter(year == 25) %>% 
  ungroup() %>% 
  left_join(no_action_end_nat_spawners, by = c("model" = "model")) %>% 
  mutate(no_action_end = no_action_end_nat_spawners,
         `Natural Spawners` = 
           round(((nat_spawners - no_action_end) / no_action_end) * 100, 1),
         Scenario = scenario_names[scenario]) %>% 
  select(Scenario, model, `Natural Spawners`)


# this is the table that is shown on the app
percent_change_from_no_action <- biomass %>% 
  left_join(spawners) %>% 
  filter(Scenario != "No Restoration Actions") %>% 
  mutate(Scenario = factor(Scenario, levels = scenario_names_levels), 
         `Juvenile Biomass at Chipps` = `Juvenile Biomass at Chipps`/100, 
         `Natural Spawners` = `Natural Spawners`/100) %>% 
  arrange(Scenario)

fall_run_percent_change <- percent_change_from_no_action %>% 
  filter(model == "Fall Run")

spring_run_percent_change <- percent_change_from_no_action %>% 
  filter(model == "Spring Run")

winter_run_percent_change <- percent_change_from_no_action %>% 
  filter(model == "Winter Run")






