library(here)
library(tidyverse)
library(readxl)
library(FlyingR)

dir <-  here("Published_data")


# ALL FUNCTIONS ARE RAN BEFORE i SWITCHED TO DATATABLE, SO THEY ARE COMMETED
# OUT UNTIL UPDATED THE FILES ARE KEPT AS THEY WERE
# I redid the speed calculations, because I thought I summarized  them wrongly, 
# but later realized it was true, so just left the old version with new 
# loading, in case i messed up something

# 1 - Birdlife classification -------------------------------------

# downloaded from: 
# https://datazone.birdlife.org/about-our-science/taxonomy

# bird_file <- "Handbook of the Birds of the World and BirdLife International Digital Checklist of the Birds of the World_Version_9.xlsx"
# 
# birdlife <- file.path(dir, bird_file) |> 
#   read_xlsx(skip = 2) |> 
#   # take only the species level, not subspecies
#   janitor::clean_names() |> 
#   rename(sp_status = x2024_iucn_red_list_category) |> 
#   select(seq, scientific_name, synonyms, family_name, order, sp_status) |> 
#   filter(!is.na(seq)) |> 
#   distinct(seq, .keep_all = T) |> 
#   rename(family = family_name, birdlife_seq = seq) |> 
#   # remove information in parenthesis is added to the synonyms column
#   mutate(synonyms = str_remove_all(synonyms, "\\([^\\)]*\\)")) |> 
#   # change all the punctuation to ";"
#   mutate(synonyms = str_replace_all(synonyms, "[:punct:]", ";")) |> 
#   mutate(
#     synonyms = if_else(
#       scientific_name == "Curruca curruca",
#       str_c(synonyms, ";Sylvia curruca", sep = ";"), 
#       synonyms
#     )
#   ) |>
#   # extract synonyms one per row
#   separate_longer_delim(synonyms, delim = ";") |> 
#   # remove labels for variants and subspecies
#   # remove synonyms that are not a full species name but just an epitaph or
#   # genus, it's the product of previous steps
#   mutate(synonyms = str_remove_all(synonyms, "\\s(sp$|var$|ssp$)")) |>
#   # making sure there is no extra white spaces
#   mutate(across(everything(), str_squish)) |> 
#   mutate(
#     # removing the synonyms that are just the epitaph or genus name
#     # or the ones that are identical to the scientific name
#     synonyms = case_when(
#       str_count(synonyms, " ") !=1 ~ NA,
#       scientific_name == synonyms ~ NA,
#       .default = synonyms
#     ), 
#     # change the status to NR - not recognized and R - regonized
#     sp_status = if_else(sp_status == "NR", "NR", "R"), 
#     order = str_to_title(order)
#   ) |> 
#   distinct(scientific_name, synonyms, family, sp_status, .keep_all = T) |> 
#   mutate(
#     # checked that they don't have any synonyms, so I just manually added them
#     synonyms = case_when(
#       scientific_name == "Myiopsitta monachus" ~ "Psittacula monachus",
#       scientific_name == "Cyanistes caeruleus" ~ "Parus caeruleus",
#       scientific_name == "Larus melanocephalus" ~ "Ichthyaetus melanocephalus",
#       scientific_name == "Larus genei" ~ "Chroicocephalus genei",
#       scientific_name == "Larus audouinii" ~ "Ichthyaetus audouinii",
#       scientific_name == "Corvus monedula" ~ "Coloeus monedula", 
#       .default = synonyms
#     ), 
#     synonyms = if_else(
#       str_detect(scientific_name, "Curruca") & is.na(synonyms), 
#       paste("Sylvia", str_split_i(scientific_name, " ", 2)), 
#       synonyms
#     )
#   ) |> 
#   # check how many times the species appears, 
#   # whether it is both listed as recognized and not
#   # how many distinct synonyms it has
#   mutate(
#     n_sci = n(),
#     both_r_nr = sum(c("R", "NR") %in% sp_status) == 2, 
#     dist_syn = n_distinct(synonyms, na.rm = T),
#     .by = scientific_name
#   ) |> 
#   # check unique combos
#   mutate(n_pair = n(), .by = c(scientific_name, synonyms)) |> 
#   mutate(
#     sci_in_syn = scientific_name %in% unique(synonyms), 
#     syn_in_sci = synonyms %in% unique(scientific_name)
#   ) |> 
#   mutate(
#     sci_name = case_when(
#       # if scientific name is both listed as recognized and not recognized, 
#       # remove the non recognized if it has no additional synonyms
#       n_sci > 1 & both_r_nr & sp_status == "NR" & is.na(synonyms) ~ NA,
#       # if it does have synonyms, make sure they are identical as in recognized
#       n_sci > 1 & both_r_nr & sp_status == "NR" & n_pair > 1 ~ NA,
#       # if the scientific name is duplicated but one of the entries doesn't
#       # have synonym provided, remove it
#       n_sci > 1 & dist_syn > 0 & is.na(synonyms) ~ NA, 
#       # if the scientific name is provided also found in the synonyms, but
#       # it's listed as not recognized, remove it if there is no synonym provided
#       sci_in_syn & sp_status == "NR" & is.na(synonyms) ~ NA,
#       .default = scientific_name
#     )
#   ) |> 
#   filter(!is.na(sci_name)) |> 
#   mutate(
#     sp_status = if_else(sp_status == "NR" & both_r_nr == T, "R", sp_status),
#     syn_in_sci = synonyms %in% unique(scientific_name), 
#     synonyms = if_else(syn_in_sci, NA, synonyms)
#   ) |> 
#   filter(
#     !(n() > 1 & n_distinct(synonyms, na.rm = T) > 0 & is.na(synonyms)), 
#     .by = scientific_name
#   ) |> 
#   select(birdlife_seq, scientific_name, synonyms, family, order, sp_status) |>
#   # assign a id number just for easier organization
#   rename(synonym = synonyms) 
# 
# birdlife |> 
#   write_csv(file.path(dir, "00_birdlife_classification.csv"))
# 
# 
# # 2 - Bird speeds from literature -----------------------------------------
# 
# # function for checking birdnames
# source(here("0_helper_functions.R"))
# 
# alerstam_file <- "Alerstam_2007_supplement_table_extracted_automatic.xlsx"
# bruderer_file <- "Bruderer_2001_extracted_manual.xlsx"
# 
# alerstam_df <- file.path(dir, alerstam_file)|> 
#   read_xlsx(skip = 1) |> 
#   select(2:4, 6) |> 
#   rename_with(~c("species", "speed", "sd", "n_tracks"), everything()) |> 
#   filter(!str_detect(species, "•")) |> 
#   mutate(
#     species = if_else(species == "Delichon urbica", "Delichon urbicum", species)
#   ) |> 
#   rename_to_birdlife(species, add_phylo = T) |> 
#   filter(!is.na(birdlife_name)) |> 
#   relocate(all_of(c("speed", "sd", "n_tracks")), .after = last_col()) |> 
#   select(-species, -name_type) |> 
#   mutate(source = "Alerstam2007")
# 
# bruderer_df <- file.path(dir, bruderer_file) |> 
#   read_xlsx() |> 
#   rename_to_birdlife(species, add_phylo = T) |> 
#   select(-species, -name_type) |> 
#   mutate(source = "Bruderer2001")
# 
# bird_speeds <- alerstam_df |> 
#   bind_rows(bruderer_df) |> 
#   rename(scientific_name = birdlife_name) |> 
#   write_csv(file.path(dir, "00_bird_speed_literature.csv"))
# 
# 
# 
# # 3 -----------------------------------------------------------------------
# 
# birdwing <- file.path(dir, "BirdWingData_tidy_ver2.1.csv")|> 
#   read_csv(show_col_types = F) |> 
#   janitor::clean_names() |> 
#   rename(species = species_ioc13_1) |> 
#   rename_to_birdlife(str_squish(species)) |> 
#   add_birdlife_phylogeny(species_name = "birdlife_name") |> 
#   filter(!is.na(birdlife_name)) |> 
#   mutate(
#     sex = case_when(
#       sex == "F" ~ "f", 
#       sex == "M" ~ "m", 
#       .default = NA
#     )
#   ) |> 
#   select(
#     birdlife_name, order, wingspan_m, wing_area_m2, 
#     body_mass_g, def_span, def_area, sex
#   ) |>  
#   filter(if_all(contains("wing"), ~!is.na(.))) |> 
#   filter(!(def_span == "B-I" | def_area %in% c("B-I", "B?-I?"))) |> 
#   filter(!if_any(-sex, is.na)) |> 
#   mutate(
#     bodymass = body_mass_g/1000,
#     muscle_mass = bodymass*0.17,
#     fat_mass = bodymass*0.33, 
#     order = if_else(order == "Passeriformes", 1, 2)
#   ) |> 
#   rename(
#     name = birdlife_name, wingspan = wingspan_m, wingarea = wing_area_m2
#   ) |> 
#   select(
#     name, sex, bodymass, wingspan, fat_mass, order, wingarea, muscle_mass
#   ) |> 
#   write_csv(file.path(dir, "00_bird_wing_data.csv"))
#   
# speedsym <- migrate(file = file.path(dir, "00_bird_wing_data.csv"))
# 
# birdwing_speeds <- birdwing |> 
#   bind_cols(
#     tibble(
#       range = speedsym$range,
#       body_mass_out = speedsym$bodyMass,
#       fat_mass_out = speedsym$fatMass,
#       muscle_mass_out = speedsym$muscleMass,
#       start_min_speed = speedsym$startMinSpeed,
#       end_min_speed = speedsym$endMinSpeed
#     )
#   ) |> 
#   write_csv(file.path(dir, "00_bird_wing_data_speed_raw.csv"))
# 
 birdwing_speeds <- file.path(dir, "00_bird_wing_data_speed_raw.csv") |> 
   read_csv(show_col_types = F)

birdlife <- file.path(dir, "00_birdlife_classification.csv") |>  
  read_csv(show_col_types = F)

birdwing_speeds <- birdwing_speeds |> 
  janitor::clean_names() |> 
  rename(scientific_name = name)

birdwing_speeds |>
  select(-sex) |> 
  summarize(across(everything(), ~mean(.)), .by = scientific_name) |> 
  bind_rows(
    birdwing_speeds |> 
      filter(!is.na(sex)) |> 
      summarize(
        across(everything(), ~mean(.)), 
        .by = c(scientific_name, sex)
      )
  ) |> 
  mutate(across(contains("speed"), ceiling)) |> 
  select(-order) |> 
  left_join(
    birdlife |> distinct(scientific_name, family, order), 
    by = "scientific_name"
  ) |> 
  write_csv(file.path(dir, "00_bird_wing_data_speed_mean.csv"))



