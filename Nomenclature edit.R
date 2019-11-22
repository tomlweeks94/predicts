setwd("C:/Users/tomlw/Desktop/PhDizzle/PREDICTS")
PREDICTS<- readRDS("diversity-2019-10-25-02-32-36.rds")
PBirds <- subset(PREDICTS, PREDICTS$Class == "Aves", stringsAsFactors = FALSE)
GBD <- read.delim("GBD.csv", sep = ",")
GBDavs<- read.delim("GBD_avs.csv", sep = ",")

##Find which binomials differ between PREDICTS and GBD
PBirdsspecies<- unique(PBirds$Best_guess_binomial)
mismatched_namesPRED <- PBirdsspecies[which(is.na(match(PBirdsspecies, GBD$ï..Unique_Scientific_Name)))]
length(mismatched_namesPRED)
mismatched_namesPRED

##Correct based on collumns in GBD, Old Jetz New Jetz, Sibley monroe
PBirds$Corrected <- GBDavs$Unique_Scientific_Name[match(PBirds$Best_guess_binomial, GBDavs$Unique_Scientific_Name)]
PBirds$Corrected2 <- GBDavs$Unique_Scientific_Name[match(PBirds$Best_guess_binomial, GBDavs$Old.Jetz_name)]
PBirds$Corrected3 <- GBDavs$Unique_Scientific_Name[match(PBirds$Best_guess_binomial, GBDavs$New.Jetz_name..Tree.Tip.Name.)]
PBirds$Corrected4 <- GBDavs$Unique_Scientific_Name[match(PBirds$Best_guess_binomial, GBDavs$Sibley...Monroe.Name)]

##Make sure corrected is seen as as.character
PBirds$Corrected <- as.character(PBirds$Corrected)

#bring corrected columns back together
PBirds$Corrected[is.na(PBirds$Corrected)] <- PBirds$Corrected2[is.na(PBirds$Corrected)]
PBirds$Corrected[is.na(PBirds$Corrected)] <- PBirds$Corrected3[is.na(PBirds$Corrected)]
PBirds$Corrected[is.na(PBirds$Corrected)] <- PBirds$Corrected4[is.na(PBirds$Corrected)]

##Manual Edits - no easy direct matches available
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Poecile montana", "Poecile montanus")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Cyanistes varius", "Poecile varius")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Apus melba", "Tachymarptis melba")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Common swallow", "Hirundo rustica")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Creasted bunting", "Emberiza lathami")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Grey bulbul", "Pycnonotus priocephalus")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Imperial pigeon", "Hylocichla mustelina")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Indian myna", "Acridotheres tristis")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Indian robin", "Saxicoloides fulicatus")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Indian rufous", "Dendrocitta vagabunda")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Jungle babbler", "Turdoides striata")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Pied wagtail", "Motacilla alba")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Sparrow hawk", "Accipiter nisus")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Trachylaemus purpuratus", "Trachyphonus purpuratus")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Ciccaba virgata", "Strix virgata")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Rupornis magnirostris", "Buteo magnirostris")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Timeliopsis fallax", "Glycichaera fallax")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Buarremon brunneinucha", "Arremon brunneinucha")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Buarremon torquatus", "Arremon torquatus")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Rhopodytes diardi", "Phaenicophaeus diardi")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Rhopodytes sumatranus", "Phaenicophaeus sumatranus")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Zanclostomus curvirostris", "Phaenicophaeus oeneicaudus")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Rhopodytes tristis", "Phaenicophaeus tristis")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Icthyophaga ichthyaetus", "Ichthyophaga ichthyaetus")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Ciccaba huhula", "Strix huhula")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Syndactyla striata", "Simoxenops striatus")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Myospiza humeralis", "Ammodramus humeralis")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Nystactes tamatia", "Bucco tamatia")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Chlorostilbon notatus", "Chlorestes notata")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Myiophonus horsfieldii", "Myophonus horsfieldii")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Rhopodytes viridirostris", "Phaenicophaeus viridirostris")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Zanclostomus calyorhynchus", "Rhamphococcyx calyorhynchus")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Rhyticeros cassidix", "Aceros cassidix")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Picoides leucotos", "Dendrocopos leucotos")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Aburria cujubi", "Pipile cujubi")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Myiophonus caeruleus", "Myophonus caeruleus")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Streptopelia picturata", "Nesoenas picturata")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Dicaeum percussus", "Prionochilus percussus")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Dicaeum maculatus", "Prionochilus maculatus")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Aburria cumanensis", "Pipile grayi")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Aethopyga temmincki", "Aethopyga temminckii")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Cyanistes flavipectus", "Cyanistes cyanus")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Blue rock", "Columba livia")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Blue whistling", "Myophonus caeruleus")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Brown crested", "Lophophanes cristatus")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Chestnut beilled", "Sitta cinnamoventris")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Common stone", "Saxicola rubicola")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Poecile hudsonica", "Poecile hudsonicus")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Eurasian tree", "Certhia familiaris")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Hawk booted", "Hieraaetus pennatus")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Indian tree", "Dendrocitta vagabunda")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Red headed", "Aegithalos concinnus")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Red jungle", "Gallus gallus")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Rufous backed", "Lanius schach")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Simla crested", "Periparus rufonuchalis")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "White piegon", "Columba livia")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Parsed_name == "White breasted kingfisher", "Halcyon smyrnensis")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Parsed_name == "Black backed weaver", "Ploceus bicolor")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Parsed_name == "Black backed starling", "Lamprotornis corruscus")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Tailor bird", "Orthotomus sutorius")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Milvus aegyptius", "Milvus migrans")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Parsed_name == "Large wagtail", "Motacilla maderaspatensis")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Paradise flycatcher", "Terpsiphone paradisi")

##Best Guesses
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Yellow backed", "Chlorocichla flaviventris")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Cacomantis esculena", "Collocalia esculenta")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Canary blue", "Cyornis tickelliae")
PBirds$Corrected <- replace(PBirds$Corrected, PBirds$Best_guess_binomial == "Large yellow", "Chrysophlegma flavinucha")

#Bush lark - indian / bengal / singing bush lark
#White Breasted Forktail could be any type of forktail


##How many/what is left unmatched
nas <- PBirds[is.na(PBirds$Corrected),]
mismatched_namesPRED<-  unique(nas$Best_guess_binomial)
length(mismatched_namesPRED)
mismatched_namesPRED
View(nas)
View(PBirds)

## PREDICTS birds with corrected nomenclature ignoring those which could not be matched
corrected <-PBirds[!is.na(PBirds$Corrected),]
##remove the extra corrected collumns
corrected<- corrected[1:90]
##save as a .RDS file
saveRDS(corrected, file = "PREDICTSbirdscorrectnames.RDS")





