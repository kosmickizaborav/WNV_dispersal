
clades_all <- list(
  Paleognathae      = c("Struthioniformes","Rheiformes","Casuariiformes",
                        "Apterygiformes","Tinamiformes"),
  Galloanserae      = c("Galliformes","Anseriformes"),
  Strisores         = c("Caprimulgiformes","Podargiformes","Nyctibiiformes",
                        "Steatornithiformes","Apodiformes"),
  Columbaves        = c("Columbiformes","Pterocliformes","Mesitornithiformes",
                        "Cuculiformes","Musophagiformes","Otidiformes"),
  Gruimorphae       = c("Gruiformes","Opisthocomiformes","Eurypygiformes"),
  Aequorlitornithes = c("Charadriiformes","Phoenicopteriformes","Podicipediformes",
                        "Gaviiformes","Sphenisciformes","Procellariiformes",
                        "Phaethontiformes","Ciconiiformes","Suliformes",
                        "Pelecaniformes"),
  Telluraves        = c("Accipitriformes","Cathartiformes","Strigiformes",
                        "Coliiformes","Leptosomiformes", "Trogoniformes",
                        "Bucerotiformes","Coraciiformes","Piciformes",
                        "Passeriformes", "Psittaciformes", "Falconiformes")
)


ord_col <- c(
  # Paleognathae
  "Struthioniformes"   = "#56021F",
  "Rheiformes"         = "#7D1C4A", 
  "Apterygiformes"     = "#D17D98", 
  "Casuariiformes"     = "#C890A7", 
  "Tinamiformes"       = "#FCD1D1",
  # Galloanserae
  "Galliformes"        = "#C70039", 
  "Anseriformes"       = "#EF6262",
  # Strisores
  "Caprimulgiformes"   = "#6C5F5B", 
  "Podargiformes"      = "#607274",
  "Nyctibiiformes"     = "#E5E4E2", 
  "Steatornithiformes" = "#B2A59B",
  "Apodiformes"        = "#CEAB93",
  # Gruimorphae
  "Gruiformes"         = "#E6B15C", 
  "Opisthocomiformes"  = "#8B5C2F", 
  "Eurypygiformes"     = "#4B2E09", 
  # Columbaves
  "Columbiformes"      = "#FF8000", 
  "Pterocliformes"     = "#FF9B45", 
  "Mesitornithiformes" = "#FEBE8C", 
  "Cuculiformes"       = "#FFB823", 
  "Musophagiformes"    = "#FFE100FF", 
  "Otidiformes"        = "#F9F07A",
  # Aequorlitornithes
  "Charadriiformes"    = "#1B1464", 
  "Phoenicopteriformes"= "#702963", 
  "Podicipediformes"   = "#F875AA", 
  "Gaviiformes"        = "#F2C1D1",
  "Sphenisciformes"    = "#CCCCFF", 
  "Procellariiformes"  = "#4E2A84", 
  "Phaethontiformes"   = "#AA98A9", 
  "Ciconiiformes"      = "#9F4EAD",
  "Suliformes"         = "#7B68EE", 
  "Pelecaniformes"     = "#E04DB0", 
  # Telluraves
  "Accipitriformes"    = "#00C2FF",
  "Cathartiformes"     = "#00FFD0",
  "Strigiformes"       = "#004E7C",
  "Coliiformes"        = "#006BFF",
  "Leptosomiformes"    = "#6FE6FC",
  "Trogoniformes"      = "#9EDF9C",
  "Bucerotiformes"     = "#83c5be",
  "Coraciiformes"      = "#8FA31E",
  "Piciformes"         = "#005C31FF",
  "Passeriformes"      = "#2a9d8f",
  "Psittaciformes"     = "#A8FF00",   
  "Falconiformes"      = "#a7c957"
)


 
# orders <- c(
#   "Accipitriformes", "Bucerotiformes", "Strigiformes", "Anseriformes", 
#   "Charadriiformes", "Psittaciformes", "Gruiformes",  "Caprimulgiformes",
#   "Pelecaniformes", "Otidiformes", "Procellariiformes", "Cathartiformes",
#   "Passeriformes",  "Cuculiformes", "Ciconiiformes", "Columbiformes",
#   "Sphenisciformes", "Falconiformes", "Suliformes", "Galliformes",
#   "Phaethontiformes", "Phoenicopteriformes", "Piciformes", "Struthioniformes")
# 
# clades_dt <- clades_dt[order %in% orders][, n := .N, by = clade]
# 
# ord_col <- c(
#   "#990000FF",
#   "#FF8500", "#FF3C00", 
#   "gray",
#   "#E6B15C", "#8B5C2F", "#4B2E09", 
#   "#FFE100FF",
#   "#D72660", "#FFA8BBFF", "#FF44CC", "#9D1DF2", "#6929B6", "#4E2A84", "#F0A3FFFF", "#1B1464",
#   "#005C31FF", "#A8FF00", "#00FF57", "#00B763", "#00FFD0", "#00B2FF", "#0090A8", "#0082FF"
#   )
#   
# names(ord_col) <- clades_dt$order

rm(clades_dt, orders, clades_all)


  