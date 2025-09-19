library(data.table)
library(FlyingR)
library(afpt)
source("0_helper_functions.R")

pub_dir <- here::here("Published_data")


# ALL FUNCTIONS ARE RAN BEFORE i SWITCHED TO DATATABLE, SO THEY ARE COMMETED
# OUT UNTIL UPDATED THE FILES ARE KEPT AS THEY WERE

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

f_literature <- "00_bird_speed_literature.csv"

if(!file.exists(file.path(pub_dir, f_literature))){
  
  # ALERSTAM 2007
  alerstam_file <- "Alerstam_2007_supplement_table_extracted_automatic.xlsx"
  
  # file 1 - extracted automatically from the paper using adobe convertor
  alerstam_dt <- readxl::read_xlsx(file.path(pub_dir, alerstam_file), skip = 1)
  # convert to dt
  alerstam_dt <- setDT(alerstam_dt)[, c(2:4, 6)]
  colnames(alerstam_dt) <- c("species", "speed", "sd", "n_tracks")
  
  # remove the values that are citations from the other paper
  alerstam_dt <- alerstam_dt[!grepl("•|Mean", species)]
  
  # rename to birdlife
  alerstam_dt[species == "Delichon urbica", species := "Delichon urbicum"]
  alerstam_dt <- rename_to_birdlife(alerstam_dt, species_name = "species")
  
  alerstam_dt[, name_type := NULL][, source := "Alerstam2007"]
  
  
  # BRUDERER 2001  
  bruderer_file <- "Bruderer_2001_extracted_manual.xlsx"
  
  bruderer_dt <- readxl::read_xlsx(file.path(pub_dir, bruderer_file))
  setDT(bruderer_dt)
  
  bruderer_dt <- rename_to_birdlife(bruderer_dt, species_name = "species")
  
  bruderer_dt[, source := "Bruderer2001"][, name_type := NULL]
  
  bird_speeds <- rbindlist(
    list(alerstam_dt, bruderer_dt), use.names = T, fill = T)
  
  
  fwrite(bird_speeds, file.path(pub_dir, f_literature))
  
  
}


# 3 - Prepare wingspan data -----------------------------------------------

# prepare windspan database
f_wingspan_c <- "00_bird_wing_data_complete.csv"

if(!file.exists(file.path(pub_dir, f_wingspan_c))){
 
  # get mass estimates for birds in case the wingspan is missing
  avonet <- readxl::read_xlsx(
    file.path(pub_dir, "AVONET1_Birdlife.xlsx"), sheet = 2) |> 
    janitor::clean_names() 
  avonet <- setDT(avonet)[, .(species1, mass_avonet = mass)]
  avonet <- rename_to_birdlife(avonet, species_name = "species1")
  avonet <- avonet[, .(birdlife_name, mass_avonet)][!is.na(birdlife_name)]
  
  birdwing <- fread(file.path(pub_dir, "BirdWingData_tidy_ver2.1.csv"))
  
  
  # selecing the columns of interest and renaming them
  cols_interest <- c("Species_IOC13.1", "Sex", "Adult_Juvenile", "wingspan_m", 
                     "wing.area_m2", "body.mass_g", "def_span", "def_area")
  
  birdwing <- birdwing[, ..cols_interest]
  
  setnames(birdwing, old = cols_interest, 
           new = c("species", "sex", "adult_juvenile", "wingspan_m", "wingarea_m2", 
                   "bodymass_g", "def_span", "def_area"))
  
  birdwing[, species := squish_base(species)]
  birdwing[, sex := tolower(sex)]
  
  
  # add birdlife classification
  birdwing <- rename_to_birdlife(birdwing, species_name = "species")
  birdwing <- birdwing[!birdlife_name %in% c(NA, "")]
  
  birdwing <- add_birdlife_phylogeny(birdwing, species_name = "birdlife_name") 
  
  # A-II: The length of the body between the two wings was included; two wings  
  # A-I: The length of the body between the two wings was included; ONE wing  
  # B-II: The length of the body between the two wings was NOT included; two wings
  # B-I: The length of the body between the two wings was NOT included; ONE wing
  # 
  # filter the data to include only the relevant definitions of wingspan and area
  # A-I not in the data, that is why not selected
  birdwing <- birdwing[def_span %in% c("B-II", "A-II", "B-I") & 
                         def_area %in% c("B-II", "A-II", "B-I")]
  
  # adjust if measurements included only one wing
  birdwing <- birdwing[def_span == "B-I", ':=' (
    wingspan_m = 2*wingspan_m, 
    def_span = "B-I x2")]  
  birdwing <- birdwing[def_area == "B-I", ':=' (
    wingarea_m2 = 2*wingarea_m2, 
    def_area = "B-I x2")]
  
  birdwing <- unique(birdwing)
  
  # add avonet values for mass
  birdwing <- merge(birdwing, avonet, by = "birdlife_name")
  
  birdwing <- melt(
    birdwing, 
    measure.vars = c("bodymass_g", "mass_avonet"),
    variable.name = "mass_type",
    value.name = "mass")
  
  
  birdwing[, mass_type := fifelse(
    grepl("avonet", mass_type), "avonet", "wingdata")]
  birdwing <- birdwing[mass_type == "avonet", sex := NA]
  
  # required mass in kg
  birdwing[, mass := mass / 1000]  # convert to kg
  
  setnames(
    birdwing, 
    old = c("wingspan_m", "wingarea_m2"), 
    new = c("wingspan", "wingarea"))
  
  fwrite(birdwing, file.path(pub_dir, f_wingspan_c))
  
}


# 4- Speed estimates - flyingR -----------------------------------------------

flyR_speed_raw <- "00_bird_wing_data_speed_flyingR_raw.csv"
flyR_wingspan <- "00_bird_wing_data_for_flyingR.csv"

if(!file.exists(file.path(pub_dir, flyR_speed_raw))){
  
  # WINGSPAN FOR FLYING R 
  birdwing <- fread(file.path(pub_dir, f_wingspan_c))
  
  # 17% - default from the flight program
  birdwing[, muscle_mass := mass * 0.17]  
  # there is no difference in min start speed based on the fat fraction
  # so i just set one
  birdwing[, fat_mass := mass*0.33] 
  birdwing[, order := fifelse(order == "Passeriformes", 1, 2)]
  
  setnames(
    birdwing, old = c("birdlife_name", "mass"), new = c("name", "bodymass"))
  
  birdwing[, name := paste(name, mass_type, sep = "_")]
  
  # keep only necessary columns
  birdwing <- birdwing[
    , .(name, sex, bodymass, wingspan, fat_mass, order, wingarea, muscle_mass)]
  
  fwrite(birdwing, file.path(pub_dir, flyR_wingspan))
  
  # get the speeds
  speedsym <- migrate(file = file.path(pub_dir, flyR_wingspan))
  
  # save results with speeds 
  speedsym <- cbind(
    birdwing,
    start_min_speed = speedsym$startMinSpeed, 
    end_min_speed = speedsym$endMinSpeed, 
    range = speedsym$range
  )
  
  fwrite(speedsym, file.path(pub_dir, flyR_speed_raw))
  
}


# 5 - Speed estimates - afpt --------------------------------------------------

afpt_speed_list <- "00_speed_estimates_afpt_list.rds"
afpt_speed_dt <- "00_bird_wing_data_speed_afpt.csv"

if(!file.exists(file.path(pub_dir, afpt_speed_list))){
  
  seabird_families <- c("Spheniscidae", "Diomedeidae", "Procellariidae", 
                        "Hydrobatidae", "Oceanitidae", "Pelecanoididae", 
                        "Phaethontidae", "Pelecanidae", "Phalacrocoracidae", 
                        "Sulidae", "Fregatidae", "Stercorariidae", "Laridae", 
                        "Rynchopidae", "Alcidae")
  
  
  birdwing <- fread(file.path(pub_dir, f_wingspan_c))
  birdwing <- birdwing[
    , .(birdlife_name, wingspan, wingarea, mass, mass_type, order, family)]
  
  birdwing[, type := fcase(
    order == "Passeriformes", "passerine",
    family %in% seabird_families, "seabird",
    default = "other")]
  
  
  speed_list <- lapply(seq(nrow(birdwing)), function(i){
    
    birdo <- Bird(
      massTotal = birdwing$mass[i],
      wingSpan = birdwing$wingspan[i],
      wingArea = birdwing$wingarea[i],
      name.scientific = birdwing$name[i],
      name = paste(birdwing$name[i], birdwing$mass_type[i], sep = "_"),
      type = birdwing$type[i]
    )
    
    computeFlightPerformance(birdo)
    
  })
  
  saveRDS(result_list, file = file.path(pub_dir, afpt_speed_list))
  
  speed_dt <- rbindlist(lapply(seq_along(speed_list), function(i){
    
    birdwing[i, ][, max_speed := speed_list[[i]]$table$speed[4]]
    
  }), fill = T)
  
  fwrite(speed_dt, file.path(pub_dir, afpt_speed_dt))
  
}


# 6 - Summary speed -------------------------------------------------------

# in the edn chosen afpt because the vales seem the most reasonable for the 
# maximum speed distribution

birdwing <- fread(file.path(pub_dir, f_wingspan_c))[
  , species := NULL][, name_type := NULL]

sp_afpt <- fread(file.path(pub_dir, afpt_speed_dt))[, .(type, max_speed)]

bspeed <- cbind(birdwing, sp_afpt)

# mean by scientific_name, ignoring sex
bspeed <- bspeed[
  , lapply(.SD, mean, na.rm=TRUE), 
  .SDcols = c("max_speed", "wingspan", "wingarea", "mass"), 
by = .(birdlife_name, family, order, type)]

bspeed[, speed := ceiling(max_speed)]
setnames(bspeed, old = "birdlife_name", new = "scientific_name")

fwrite(bspeed, file.path(pub_dir, "00_bird_wing_data_speed_mean.csv"))


# 7 - Traits Databases----------------------------------------------------------

#' #' AVONET:
#' #' - migration:
#' #'   1 = Sedentary.
#' #'   2 = Partially migratory - minority of population migrates long distances,
#' #'   or most of population undergoes short-distance migration,
#' #'   nomadic movements, distinct altitudinal migration, etc.
#' #'   3 = Migratory - majority of population undertakes long-distance migration
avonet <- setDT(readxl::read_xlsx(
  file.path(here::here("Published_data", "AVONET1_Birdlife.xlsx")), sheet = 2))[
  , .(sp_avonet = Species1, migration = Migration)]

avonet <- rename_to_birdlife(avonet, species_name = "sp_avonet")[
  , name_type := NULL]
avonet <- avonet[!is.na(birdlife_name)]

# ELTON
elton <- fread(here::here("Published_data", "BirdFuncDat.txt"))[
  , .(sp_elton = Scientific, nocturnal = Nocturnal)]
elton <- rename_to_birdlife(elton, species_name = "sp_elton")[
  , name_type := NULL]

elton <- elton[!is.na(birdlife_name)][
  , nocturnal := fifelse(sp_elton == "Nycticorax nycticorax", 1, nocturnal)]

# combine
traits <- merge(avonet, elton, by = "birdlife_name", all = T)

# MISSING INFORMATION
# missing info for the species
# Grus vipio and Cuculus optatus
# assumed migratory because other Grus ssp. and Cuculus ar migratory 
# also confirmed based on the https://datazone.birdlife.org/: Full Migrant
traits[
  birdlife_name %in% c("Cuculus optatus", "Grus vipio"), migration := 3]
# Larus ssp. is daily species
traits[birdlife_name == "Larus smithsonianus", nocturnal := 0]

traits[, migration_txt := fcase(
  migration == 1, "sedentary", 
  migration == 2, "partially migratory", 
  migration == 3, "migratory", 
  default = NA_character_
)]

traits <- add_birdlife_phylogeny(traits, species_name = "birdlife_name")

fwrite(traits, file.path(pub_dir, "00_bird_traits.csv"))

