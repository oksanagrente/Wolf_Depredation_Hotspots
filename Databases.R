########################################################################################
################################ Wolf Attack Hotspots ##################################
#################################### Databases ######################################### 
########################################################################################

#Packages
library(sf)
library(dplyr)

#Functions
st_poly_sample_simple_random <- function(x, size) {
  bb = st_bbox(x)
  lon = runif(size, bb[1], bb[3])
  lat = runif(size, bb[2], bb[4])
  m = cbind(lon, lat)
  p = st_sfc(lapply(seq_len(nrow(m)), function(i)
    st_point(m[i,])), crs = st_crs(x))
  p[x]
}
st_sample_exact <- function(x, size) {
  random_pt <- st_poly_sample_simple_random(x , size = size)
  while (length(random_pt) < size) {
    diff <- size - length(random_pt)
    random_pt_new <- st_poly_sample_simple_random(x , size = diff)
    random_pt <- c(random_pt, random_pt_new)
  }
  random_pt
}

setwd("C:/Users/grente/Documents/Etudes/Points chauds/Dossier revu/Data")

#####################################################
############# 1. Wolf depredations  #################
#####################################################

  # Period: 1994-2018
Attaques_sf <- read.csv2("Attaques_1994_2018_Pred&Loup&XY.csv")

  # Keep only located attacks, and remove those with approximate locations
Attaques_sf <- Attaques_sf[!(is.na(Attaques_sf$E_RGF)|Attaques_sf$E_RGF==0),] #remove non-located attacks

  # Date formatting
Attaques_sf$Date.attaque <- as.Date(as.character(Attaques_sf$Date.attaque),format="%d/%m/%Y")
if(length(which(is.na(Attaques_sf$Date.attaque)))>0){
Attaques_sf <- Attaques_sf[-which(is.na(Attaques_sf$Date.attaque)),]} #Remove those without precise date
Attaques_sf["Annee_bio"] <- NA
for(i in 1994:2018){ #Attribute a biological year 
  Dates_Annee_bio <- seq(as.Date(paste0(i-1,"-04-01",collapse = NULL)), 
                     as.Date(paste0(i,"-03-31",collapse = NULL)), by="days")
  Attaques_sf[Attaques_sf$Date.attaque %in% Dates_Annee_bio,"Annee_bio"] <- i }
if(length(which(is.na(Attaques_sf$Annee_bio)))>0){ #Remove those without biological year (outside study period)
Attaques_sf <- Attaques_sf[-which(is.na(Attaques_sf$Annee_bio)),]}

  # Save file for the indices part
Indices_Attaques <- Attaques_sf[c("Annee_bio","E_RGF","N_RGF")] %>% #prepare file for the indices 
                       rename(X=E_RGF,Y=N_RGF)
row.names(Indices_Attaques) <- NULL #reset row.id number

  # Select only atatcks on sheep
Attaques_sf <- Attaques_sf[Attaques_sf$Sp.ov.bv.cn.=="Ov" | Attaques_sf$Sp.ov.bv.cn.=="" 
                          | Attaques_sf$Sp.ov.bv.cn.==" O" | Attaques_sf$Sp.ov.bv.cn.=="Oc"
                          | Attaques_sf$Sp.ov.bv.cn.=="Of" | Attaques_sf$Sp.ov.bv.cn.=="Oq",]
Attaques_sf$Sp.ov.bv.cn. <- factor(Attaques_sf$Sp.ov.bv.cn.) #factor's level update

  # Select attacks where pastoral unit information is available
Attaques_sf <- Attaques_sf[Attaques_sf$N.dpt %in% c(1,3,4,5,6,7,13,15,26,38,42,43,63,69,73,74,83,84),]
#Attaques_sf <- Attaques_sf[Attaques_sf$N.dpt %in% c("AIN","ALLIER","ALPES-HAUTE-PROVENCE","HAUTES-ALPES",
                                           # "ALPES-MARITIMES","ARDECHE","BOUCHES-DU-RHONE","CANTAL",
                                           # "DROME","ISERE","LOIRE","HAUTE-LOIRE","PUY-DE-DOME","RHONE","SAVOIE","HAUTE-SAVOIE",
                                           # "VAR","VAUCLUSE"),] 

  # Variable selection
Attaques_sf <- Attaques_sf[c("N.dpt","Annee_bio","N.INSEE","taille.troupeau","E_RGF","N_RGF")]
row.names(Attaques_sf) <- NULL #reset row.id number

  # Spatial transformation of the wolf depredation file
Attaques_sf <- st_as_sf(Attaques_sf,coords = c('E_RGF','N_RGF')) #Correspondance: predSpat (Basededonne)
st_crs(Attaques_sf) = 2154 #CRS Lambert 93 

#####################################################
############## 2. Pastoral Units  ###################
#####################################################

############## Files from 1996-1997 ############## 

AP1996_sf <- st_read("shape_sp96_cefecnrs.shp",crs=2154) #Read pastoral areas shapefile
AP1996_df <- read.csv2("ENQ_PAST_96_97_consolide_RA_PACA_1.csv") #Read pastoral areas data frame

  # Variable selection
AP1996_sf <- AP1996_sf[c("NUM","CATUNITE","AREA")] 
AP1996_sf$NUM <- as.numeric(as.character(AP1996_sf$NUM))
AP1996_df <- AP1996_df[c("NUM","effocpat","datefin","datedeb")]

  # Merge the two files
AP1996_sf <- merge(AP1996_df,AP1996_sf,by=intersect(names(AP1996_sf), names(AP1996_df)),all=FALSE)
st_geometry(AP1996_sf) <- AP1996_sf$geometry

  # Selection of pastoral areas with grazing sheep
AP1996_sf <- AP1996_sf[AP1996_sf$effocpat >0,] 

  # Calculation of density
AP1996_sf$SURFACE <- as.numeric(st_area(AP1996_sf)*1e-06) # in km²
AP1996_sf$DENSITE_OV <- round(AP1996_sf$effocpat/AP1996_sf$SURFACE) # in ind.km-2

  # Calculation of grazing time 
AP1996_sf$DUREE_PAT <- AP1996_sf$datefin-AP1996_sf$datedeb+1
AP1996_sf <- AP1996_sf[-c(which(AP1996_sf$DUREE_PAT<0)),] #We remove 6 problematic pastoral zones where grazing time is negative

  # Selection of utilized areas and distinction between pastoral units and zones
UP1996_sf <- AP1996_sf[AP1996_sf$CATUNITE %in% c("U1","U2"),] #Only utilised pastoral units 
UP1996_sf["CATEG"] <- "UP"
ZP1996_sf <- AP1996_sf[AP1996_sf$CATUNITE %in% c("U3","U4","U5","U6"),] #Only utilised pastoral zones 
ZP1996_sf["CATEG"] <- "ZP"

row.names(UP1996_sf) <- NULL #reset row.id number
row.names(ZP1996_sf) <- NULL 

############## Files from 2012-2014 ##############

UP2012_sf <- st_read("shape_up_cefecnrs.shp",crs=2154) #Pastoral units
ZP2012_sf <- st_read("shape_zp_cefecnrs.shp",crs=2154) #Pastoral zones

  # Variable selection
UP2012_sf <- UP2012_sf[c("CODE","USAGE","EF_OV_PAT","DATE_DEB","DATE_FIN","SURFACE")]
UP2012_sf$CODE <- as.numeric(gsub("[a-zA-Z]","",UP2012_sf$CODE)) #Remove letters from the code
UP2012_sf["CATEG"] <- "UP"
ZP2012_sf <- ZP2012_sf[c("CODE","USAGE","CH_MAX_OV","PRINTEMPS","ETE","AUTOMNE","HIVER","SURFACE")]
ZP2012_sf$CODE <- as.numeric(gsub("[a-zA-Z]","",ZP2012_sf$CODE)) #Remove letters from the code
ZP2012_sf["CATEG"] <- "ZP"

  # Data selection
UP2012_sf <- UP2012_sf[UP2012_sf$USAGE == "O",] #Only utilised units 
UP2012_sf <- UP2012_sf[UP2012_sf$EF_OV_PAT >0,] #Only units with grazing sheep
ZP2012_sf <- ZP2012_sf[ZP2012_sf$USAGE == "O",] #Only utilised zones
ZP2012_sf <- ZP2012_sf[ZP2012_sf$CH_MAX_OV >0 & !is.na(ZP2012_sf$CH_MAX_OV),] #Only zones with grazing sheep

  # Calculation of density
UP2012_sf$SURFACE <- as.numeric(st_area(UP2012_sf)*1e-6) # in km²
ZP2012_sf$SURFACE <- as.numeric(st_area(ZP2012_sf)*1e-6) # in km²
UP2012_sf$DENSITE_OV <- round(UP2012_sf$EF_OV_PAT/UP2012_sf$SURFACE) # in ind.km-2
ZP2012_sf$DENSITE_OV <- round(ZP2012_sf$CH_MAX_OV/ZP2012_sf$SURFACE) # in ind.km-2
  
  # Calculation of grazing time (in days) for pastoral unit
UP2012_sf$DUREE_PAT <- UP2012_sf$DATE_FIN-UP2012_sf$DATE_DEB+1
UP2012_sf$DUREE_PAT <- as.numeric(UP2012_sf$DUREE_PAT)
UP2012_sf <- UP2012_sf[!is.na(UP2012_sf$DUREE_PAT),] #We remove units without grazing information

  # Calculation of grazing time for pastoral zones
ZP2012_sf$PRINTEMPS <- as.factor(ZP2012_sf$PRINTEMPS)
ZP2012_sf$ETE <- as.factor(ZP2012_sf$ETE)
ZP2012_sf$AUTOMNE <- as.factor(ZP2012_sf$AUTOMNE)
ZP2012_sf$HIVER <- as.factor(ZP2012_sf$HIVER)

levels(ZP2012_sf$PRINTEMPS) <- c("0","90") #Use of seasonal indication as proxy for grazing time
ZP2012_sf$PRINTEMPS <- as.numeric(as.character(ZP2012_sf$PRINTEMPS))
levels(ZP2012_sf$ETE) <- c("0","90")
ZP2012_sf$ETE <- as.numeric(as.character(ZP2012_sf$ETE))
levels(ZP2012_sf$AUTOMNE) <- c("0","90")
ZP2012_sf$AUTOMNE <- as.numeric(as.character(ZP2012_sf$AUTOMNE))
levels(ZP2012_sf$HIVER) <- c("0","90")
ZP2012_sf$HIVER <- as.numeric(as.character(ZP2012_sf$HIVER))
ZP2012_sf$PERIODE_PAT <- ZP2012_sf$PRINTEMPS + ZP2012_sf$ETE + ZP2012_sf$AUTOMNE + ZP2012_sf$HIVER #Sum of grazing time
ZP2012_sf <- ZP2012_sf[!is.na(ZP2012_sf$PERIODE_PAT),] #We remove units without grazing information

row.names(UP2012_sf) <- NULL #reset row.id number
row.names(ZP2012_sf) <- NULL 

############## Harmonization of column names ##############

UP1996_sf <- UP1996_sf[c("CATEG","NUM","effocpat","DUREE_PAT","SURFACE","DENSITE_OV","geometry")] 
colnames(UP1996_sf) <- c("CATEG","CODE","EFF_OV","DUREE_PAT","SURFACE","DENSITE_OV","geometry" )

UP2012_sf <- UP2012_sf[c("CATEG","CODE", "EF_OV_PAT","DUREE_PAT","SURFACE","DENSITE_OV","geometry")] 
colnames(UP2012_sf) <- c("CATEG","CODE","EFF_OV","DUREE_PAT","SURFACE","DENSITE_OV","geometry")

ZP1996_sf <- ZP1996_sf[c("CATEG","NUM","effocpat","DUREE_PAT","SURFACE","DENSITE_OV","geometry")] 
colnames(ZP1996_sf) <- c("CATEG","CODE","EFF_OV","DUREE_PAT","SURFACE","DENSITE_OV","geometry")

ZP2012_sf <- ZP2012_sf[c("CATEG","CODE", "CH_MAX_OV","PERIODE_PAT","SURFACE","DENSITE_OV","geometry")] 
colnames(ZP2012_sf) <-  c("CATEG","CODE","EFF_OV","DUREE_PAT","SURFACE","DENSITE_OV","geometry")

############## Adjustement for pastoral zones in 1996 ##############

  # Prepare table of comparison of grazing time and number of sheep between pastoral zones of 1996 and 2012
# Comparaison_ZP9612_df <- data.frame(ZP1996=1:nrow(ZP1996_sf)) 
# 
#   # Identify pastoral zones of 1996 and 2012 which share common space
# ZP_encommun <- st_intersects(ZP1996_sf,ZP2012_sf) # Around 1220 PZ from 1996 which intersects with one or more from 2012
# 
#   # Difference between grazing time and sheep number between the pastoral zone of 1996 and the one with
#   # which it shares the most common space
# for (i in 1:nrow(ZP1996_sf)){ 
#   Inter_ZP96_ZP12_sf <- st_intersection(ZP1996_sf[i,], ZP2012_sf[c(ZP_encommun[[i]]),]) 
#   Inter_Max_ZP96_ZP12_pos <- which(st_area(Inter_ZP96_ZP12_sf)==max(st_area(Inter_ZP96_ZP12_sf)))
#   Inter_Max_ZP96_ZP12_num <- ZP_encommun[[i]][Inter_Max_ZP96_ZP12_pos]
#   if(length(Inter_Max_ZP96_ZP12_num)>0){
#   Comparaison_ZP9612_df[i,"ZP2012"] <- Inter_Max_ZP96_ZP12_num
#   Comparaison_ZP9612_df[i,"DIFF_DUREE"] <- abs(st_drop_geometry(ZP1996_sf[i,"DUREE_PAT"])-st_drop_geometry(ZP2012_sf[Inter_Max_ZP96_ZP12_num,"DUREE_PAT"]))
#   Comparaison_ZP9612_df[i,"DIFF_EFF"] <- abs(st_drop_geometry(ZP1996_sf[i,"EFF_OV"])-st_drop_geometry(ZP2012_sf[Inter_Max_ZP96_ZP12_num,"EFF_OV"]))
#   Comparaison_ZP9612_df[i,"TAUX_AIRE_COM"] <- round(max(st_area(Inter_ZP96_ZP12_sf))/st_area(ZP1996_sf[i,])*100, digits=2)}
#   assign('Comparaison_ZP9612_df',Comparaison_ZP9612_df,envir=.GlobalEnv)}
# 
# summary(Comparaison_ZP9612_df[Comparaison_ZP9612_df$TAUX_AIRE_COM>5,]$DIFF_DUREE) #mean of 106 days of difference
# summary(Comparaison_ZP9612_df[Comparaison_ZP9612_df$TAUX_AIRE_COM>5,]$DIFF_EFF) #mean of 381 sheep of difference
# 
#   # Remove lines without intersection
# Comparaison_ZP9612_df <- Comparaison_ZP9612_df[!is.na(Comparaison_ZP9612_df$ZP2012),] 
# # save(Comparaison_ZP9612_df,file="Comparaison_ZP9612_df.RData")
# 
#   # Remove intersection with not enough space in common between both pastoral zones (less than 5% of the 1996 pastoral zone area)
# #Distribution_Intersection_plot <- ggplot(data=Comparaison_ZP9612_df, aes(x=TAUX_AIRE_COM)) + geom_density() 
# Comparaison_ZP9612_df <- Comparaison_ZP9612_df[Comparaison_ZP9612_df$TAUX_AIRE_COM>5,]
# 
#   # Select pastoral zones in 2012 without equivalent in 1996 (no intersection)  
# ZP_uniq2012_sf <- ZP2012_sf[setdiff(1:nrow(ZP2012_sf),Comparaison_ZP9612_df$ZP2012),]
# 
#   # Merge pastoral zones from 1996 and new pastoral zones from 2012
# ZP1996_sf <- rbind(ZP1996_sf,ZP_uniq2012_sf) 

############## Finalization of pastoral files ##############

  # Merge of pastoral units and zones for 1996 and 2012
UP1996_sf <- rbind(UP1996_sf,ZP1996_sf)
UP2012_sf <- rbind(UP2012_sf,ZP2012_sf)

  # Order by pastoral code
UP1996_sf <- UP1996_sf[order(UP1996_sf$CODE),]
UP2012_sf <- UP2012_sf[order(UP2012_sf$CODE),]
row.names(UP1996_sf) <- NULL #reset row.id number
row.names(UP2012_sf) <- NULL 

save(UP1996_sf,file="UP1996_sf.RData")
save(UP2012_sf,file="UP2012_sf.RData")

#####################################################
############# 3. Wolf distribution  #################
#####################################################

############## National file of indices of wolf presence ############## 

Indices_HorsPNM <- read.csv2("BASE INDICES HORS PNM & Fiabilite R ou vides & G_sp Canis lupus_sp, imp ou vides _MaJ Mars 2019.csv", stringsAsFactors = FALSE) 

  # We select only geolocated indices 
Indices_HorsPNM <- tidyr::drop_na(Indices_HorsPNM,c(X,Y)) 

  # Date formatting
Indices_HorsPNM$date <- as.Date(as.character(Indices_HorsPNM$date),format="%d/%m/%Y")
if(length(which(is.na(Indices_HorsPNM$date)))>0){ #Remove those witout precise date
  Indices_HorsPNM <- Indices_HorsPNM[-which(is.na(Indices_HorsPNM$date)),]}
Indices_HorsPNM["Annee_bio"] <- NA
for(i in 1994:2018){ #Attribute a biological year 
  Dates_Annee_bio <- seq(as.Date(paste0(i-1,"-04-01",collapse = NULL)), 
                         as.Date(paste0(i,"-03-31",collapse = NULL)), by="days")
  Indices_HorsPNM[Indices_HorsPNM$date %in% Dates_Annee_bio,"Annee_bio"] <- i }

  # Variable selection
Indices_HorsPNM <- Indices_HorsPNM[c("Type","N.Dpt","Annee_bio","N.INSEE","X","Y")] 
  
############## Mercantour park file of indices of wolf presence ############## 

Indices_UniqPNM <- read.csv2("BASE INDICES UNIQUEMENT PNM & Fiabilite Loup, Gd Canide, Indetermine ou vides & G_sp Canis lupus_sp, imp ou vides _MaJ Mars 2019.csv", stringsAsFactors = FALSE) 

  # Variable selection and date formatting
Indices_UniqPNM$Date.Obs <- as.Date(as.character(Indices_UniqPNM$Date.Obs),format="%d/%m/%Y")
if(length(which(is.na(Indices_UniqPNM$Date.Obs)))>0){ #Remove those witout precise date
  Indices_UniqPNM <- Indices_UniqPNM[-which(is.na(Indices_UniqPNM$Date.Obs)),]}
Indices_UniqPNM["Annee_bio"] <- NA
for(i in 1994:2018){ #Attribute a biological year 
  Dates_Annee_bio <- seq(as.Date(paste0(i-1,"-04-01",collapse = NULL)), 
                         as.Date(paste0(i,"-03-31",collapse = NULL)), by="days")
  Indices_UniqPNM[Indices_UniqPNM$Date.Obs %in% Dates_Annee_bio,"Annee_bio"] <- i }

  # Variable selection
Indices_UniqPNM <- Indices_UniqPNM[c("Type","N.Dpt","Annee_bio","Code.Insee","X","Y")]

############## Merge of the two indices files and attacks ############## 

Indices_Loup_sf <- bind_rows(Indices_HorsPNM[,c("Annee_bio","X","Y")],
                         Indices_UniqPNM[,c("Annee_bio","X","Y")],
                         Indices_Attaques) %>% 
                   filter(., round(X) %in% 125000:1078000 & #we remove indices that are wrongly located
                             round(Y) %in% 6129000:7129000) %>% 
                   filter(., !is.na(Annee_bio)) %>%  #we remove indices without biological year (outside study period)
                   st_as_sf(.,coords = c('X','Y'),crs=st_crs(2154)) #we spatialize the file, with Lambert93 geosystem
save(Indices_Loup_sf,file="Indices_Loup_sf.RData")

############## Report annual wolf distribution on the EEA mesh ############## 

  # EEA mesh for France
france_departements <- st_read("DEPARTEMENT.shp",crs=2154)
Maillage_sf <- st_read("Maillage_10x10_Km_L93.shp",crs=2154) %>% 
               st_crop(france_departements) 

  # Determine mesh squares within "occasional" or "regular" wolf presence for each year between 1996 and 2017
Maillage_Loup_19962018_sf_fonction <- function(x){
  Maillage_Loup_19962018_sf_liste <- list()
    #Extract indices from the first and second bi-annual period 
  Annee_bio_1 <- x-2
  Annee_bio_2 <- x-1
  Annee_bio_3 <- x
  Indices_Premiere_biennale <- Indices_Loup_sf[Indices_Loup_sf$Annee_bio %in% c(Annee_bio_1,Annee_bio_2),]
  Indices_Deuxieme_biennale <- Indices_Loup_sf[Indices_Loup_sf$Annee_bio %in% c(Annee_bio_2,Annee_bio_3),]
    #Identify mesh squares within wolf distribution for the first bi-annual period  
  Maillage_P1 <- as.data.frame(table(data.frame(st_intersects(Maillage_sf,Indices_Premiere_biennale))$row.id))
  Maillage_P1_1ind <- Maillage_P1[Maillage_P1$Freq==1,"Var1"] #Square with only one indice
  Maillage_P1_2ind <- Maillage_P1[Maillage_P1$Freq>1,"Var1"] #Square with at least two indices
    #Identify mesh squares within wolf distribution for the second bi-annual period  
  Maillage_P2 <- as.data.frame(table(data.frame(st_intersects(Maillage_sf,Indices_Deuxieme_biennale))$row.id))
  Maillage_P2_1ind <- Maillage_P2[Maillage_P2$Freq==1,"Var1"] #Square with only one indice
  Maillage_P2_2ind <- Maillage_P2[Maillage_P2$Freq>1,"Var1"] #Square with at least two indices
    #Determine mesh squares within wolf regular or occasional distribution 
  Maillage_pres_reg_pos <- as.numeric(as.character(Maillage_P1_2ind[which(Maillage_P1_2ind %in% Maillage_P2_2ind)])) #at least 2 indices in both periods
  Maillage_pres_occ_pos <- c(
    as.numeric(as.character(Maillage_P1_1ind[which(Maillage_P1_1ind %in% Maillage_P2_1ind)])), #1 indice in both periods
    as.numeric(as.character(Maillage_P1_1ind[which(Maillage_P1_1ind %in% Maillage_P2_2ind)])), #1 indice in first period and at least 2 indices in second period
    as.numeric(as.character(Maillage_P2_1ind[which(Maillage_P2_1ind %in% Maillage_P1_2ind)]))) #at least 2 indices in first period and 1 indice in second period
  Maillage_pres_reg_ID <- st_drop_geometry(Maillage_sf[c(Maillage_pres_reg_pos),])$FID_france
  Maillage_pres_occ_ID <- st_drop_geometry(Maillage_sf[c(Maillage_pres_occ_pos),])$FID_france
    #Extract these mesh squares from the initial mesh
  Maillage_Loup_sf <- Maillage_sf[c(Maillage_pres_reg_pos,Maillage_pres_occ_pos),]
  Maillage_Loup_sf["PRESENCE_L"] <- NA
  Maillage_Loup_sf[Maillage_Loup_sf$FID_france %in% Maillage_pres_reg_ID,"PRESENCE_L"] <- "Présence régulière"
  Maillage_Loup_sf[Maillage_Loup_sf$FID_france %in% Maillage_pres_occ_ID,"PRESENCE_L"] <- "Présence occasionnelle"
  Maillage_Loup_19962018_sf_liste[[x]] <- Maillage_Loup_sf
  }
Maillage_Loup_19962018_sf_liste <- lapply(1996:2018, function(x) Maillage_Loup_19962018_sf_fonction(x))
names(Maillage_Loup_19962018_sf_liste) <- c(1996:2018)

  # Special case for 1995 distribution, with not enough retrospective and indices:
Maillage_Loup_1995_sf_fonction <- function(x){
  Maillage_Loup_1995_sf_liste <- list()
  #Extract indices from the first and second bi-annual period 
  Indices_Premiere_periode <- Indices_Loup_sf[Indices_Loup_sf$Annee_bio == 1994,]
  Indices_Deuxieme_periode <- Indices_Loup_sf[Indices_Loup_sf$Annee_bio == 1995,]
  #Identify mesh squares within wolf distribution for the first period  
  Maillage_P1 <- as.data.frame(table(data.frame(st_intersects(Maillage_sf,Indices_Premiere_periode))$row.id))
  Maillage_P1_1ind <- Maillage_P1[Maillage_P1$Freq>0,"Var1"] #Square with only one indice
  #Identify mesh squares within wolf distribution for the second period  
  Maillage_P2 <- as.data.frame(table(data.frame(st_intersects(Maillage_sf,Indices_Deuxieme_periode))$row.id))
  Maillage_P2_1ind <- Maillage_P2[Maillage_P2$Freq>0,"Var1"] #Square with only one indice
  #Determine mesh squares within wolf regular or occasional distribution 
  Maillage_pres_reg_pos <- as.numeric(as.character(Maillage_P1_1ind[which(Maillage_P1_1ind %in% Maillage_P2_1ind)])) #at least 1 indice in both periods
  Maillage_pres_occ_pos <- c(as.numeric(as.character(setdiff(Maillage_P1_1ind,Maillage_P2_1ind))), #at least 1 indice in first period and nothing in the second
                             as.numeric(as.character(setdiff(Maillage_P2_1ind,Maillage_P1_1ind)))) #at least 1 indice in second period and nothing in the first
  Maillage_pres_reg_ID <- st_drop_geometry(Maillage_sf[c(Maillage_pres_reg_pos),])$FID_france
  Maillage_pres_occ_ID <- st_drop_geometry(Maillage_sf[c(Maillage_pres_occ_pos),])$FID_france
  #Extract these mesh squares from the initial mesh
  Maillage_Loup_1995_sf <- Maillage_sf[c(Maillage_pres_reg_pos,Maillage_pres_occ_pos),]
  Maillage_Loup_1995_sf["PRESENCE_L"] <- NA
  Maillage_Loup_1995_sf[Maillage_Loup_1995_sf$FID_france %in% Maillage_pres_reg_ID,"PRESENCE_L"] <- "Présence régulière"
  Maillage_Loup_1995_sf[Maillage_Loup_1995_sf$FID_france %in% Maillage_pres_occ_ID,"PRESENCE_L"] <- "Présence occasionnelle"
  Maillage_Loup_1995_sf_liste[[x]] <- Maillage_Loup_1995_sf
  }
Maillage_Loup_1995_sf_liste <- lapply(1995:1995, function(x) Maillage_Loup_1995_sf_fonction(x))
names(Maillage_Loup_1995_sf_liste) <- 1995

  # Final wolf distribution file
Maillage_Loup_sf_liste <- c(Maillage_Loup_1995_sf_liste,Maillage_Loup_19962018_sf_liste)
save(Maillage_Loup_sf_liste,file="Maillage_Loup_sf_liste.RData")

###########################################################################
######### 4. Pastoral units within annual wolf distribution  ##############
###########################################################################

############## Extract pastoral units within regular wolf presence ############## 

UP_Loup_reg_sf_fonction <- function(x) {
  UP_Loup_reg_sf_liste <- list()
  Maillage_Loup_pres_reg_sf <- Maillage_Loup_sf_liste[[as.character(x)]][Maillage_Loup_sf_liste[[as.character(x)]]$PRESENCE_L=="Présence régulière",]
  if(x %in% c(1995:2005)){UP_sf <- UP1996_sf}else{UP_sf <- UP2012_sf}
  UP_Loup_reg_logical <- st_intersects(UP_sf,Maillage_Loup_pres_reg_sf) %>% lengths > 0
  UP_Loup_reg_sf <- UP_sf[c(which(UP_Loup_reg_logical)),]
  row.names(UP_Loup_reg_sf) <- NULL
  UP_Loup_reg_sf["PRESENCE_L"] <- "Presence reguliere"
  UP_Loup_reg_sf_liste[[x]] <- UP_Loup_reg_sf}
UP_Loup_reg_sf_liste <- lapply(1995:2018, function(x) UP_Loup_reg_sf_fonction(x))
names(UP_Loup_reg_sf_liste) <- c(1995:2018)
save(UP_Loup_reg_sf_liste,file="UP_Loup_reg_sf_liste.RData")

############## Extract pastoral units within occasional wolf presence ############## 

UP_Loup_occ_sf_fonction <- function(x) {
  UP_Loup_occ_sf_liste <- list()
  Maillage_Loup_pres_occ_sf <- Maillage_Loup_sf_liste[[as.character(x)]][Maillage_Loup_sf_liste[[as.character(x)]]$PRESENCE_L=="Présence occasionnelle",]
  if(x %in% c(1995:2005)){UP_sf <- UP1996_sf}else{UP_sf <- UP2012_sf}
  UP_Loup_occ_logical <- st_intersects(UP_sf,Maillage_Loup_pres_occ_sf) %>% lengths > 0
  UP_Loup_occ_sf <- UP_sf[c(which(UP_Loup_occ_logical)),]
  row.names(UP_Loup_occ_sf) <- NULL
  UP_Loup_occ_sf["PRESENCE_L"] <- "Presence occasionnelle"
  UP_Loup_occ_sf_liste[[x]] <- UP_Loup_occ_sf}
UP_Loup_occ_sf_liste <- lapply(1995:2018, function(x) UP_Loup_occ_sf_fonction(x))
names(UP_Loup_occ_sf_liste) <- c(1995:2018)
save(UP_Loup_occ_sf_liste,file="UP_Loup_occ_sf_liste.RData")

############## Merge of the two pastoral unit lists ############## 

UP_Loup_sf_liste_fonction <- function(x) {
  UP_Loup_sf_liste <- list()
  UP_Loup_sf <- rbind(UP_Loup_reg_sf_liste[[as.character(x)]],
                                 UP_Loup_occ_sf_liste[[as.character(x)]])
  Dup_rows <- which(duplicated(UP_Loup_sf[c("CATEG","CODE")]))
  UP_Loup_sf <- UP_Loup_sf[-c(Dup_rows),]
  row.names(UP_Loup_sf) <- NULL
  UP_Loup_sf_liste[[x]] <- UP_Loup_sf}
UP_Loup_sf_liste <- lapply(1995:2018, function(x) UP_Loup_sf_liste_fonction(x))
names(UP_Loup_sf_liste) <- c(1995:2018)
save(UP_Loup_sf_liste,file="UP_Loup_sf_liste.RData")

###########################################################################
######## 5. Annual relocation of attacks outside pastoral units  ##########
###########################################################################

############## Investigate distance between attacks outside pastoral units and the closest pastoral unit ##############

Attaques_ext_df_fonction <- function(x){  
  Attaques_ext_df_liste <- list()
  Attaques_Annee_sf <- Attaques_sf[Attaques_sf$Annee_bio==x,]
  UP_Annee_sf <- UP_Loup_sf_liste[[as.character(x)]]
    #Identify the attacks of the year which are not within pastoral unit polygons
  Attaques_intUP_id <- as.data.frame(st_contains(UP_Annee_sf,Attaques_Annee_sf))
  Attaques_extUP_sf <- Attaques_Annee_sf[which(!(1:nrow(Attaques_Annee_sf) %in% Attaques_intUP_id$col.id)),]
  Attaques_Annee_sf["Taux_Att_ext"] <- nrow(Attaques_extUP_sf)/nrow(Attaques_Annee_sf)
  Attaques_Annee_sf["Distance_min"] <- NA
  Attaques_Annee_sf["Categorie"] <- NA
    #For each attack outside pastoral units:
  for(i in 1:nrow(Attaques_extUP_sf))    { 
  UP_plusproche_sf <- UP_Annee_sf[(st_nearest_feature(Attaques_extUP_sf[i,],UP_Annee_sf)),] #Identification of the closest PU
  Attaques_Annee_sf[row.names(Attaques_extUP_sf[i,]),"Distance_min"] <- round(st_distance(Attaques_extUP_sf[i,],UP_plusproche_sf))
  Attaques_Annee_sf[row.names(Attaques_extUP_sf[i,]),"Categorie"] <- UP_plusproche_sf$CATEG}
    #Create data frame with distance information about their two closest pastoral units
  Attaques_ext_Annee_sf <- Attaques_Annee_sf[-which(is.na(Attaques_Annee_sf$Categorie)),]
  Attaques_ext_Annee_df <- st_drop_geometry(Attaques_ext_Annee_sf)
  Attaques_ext_df_liste[[x]] <- Attaques_ext_Annee_df} 
Attaques_ext_df_liste <- lapply(1995:2018, function(x) Attaques_ext_df_fonction(x))
names(Attaques_ext_df_liste) <- c(1995:2018)
save(Attaques_ext_df_liste,file="Attaques_ext_df_liste.RData")

############## Selection and relocation of attacks less than 500m from the pastoral unit ##############

Relocalisation_Attaques_fonction <- function(x){  
  Attaques_sf_liste <- list()
  Attaques_Annee_sf <- Attaques_sf[Attaques_sf$Annee_bio==x,]
  UP_Annee_sf <- UP_Loup_sf_liste[[as.character(x)]]
   #Selection of attacks less than 500m from the pastoral unit of the herd
  Attaques_horslimites_id <- row.names(Attaques_ext_df_liste[[as.character(x)]][which(Attaques_ext_df_liste[[as.character(x)]]$Distance_min>499),])
  Attaques_Annee_sf["Taux_horslimites/ext"] <- length(Attaques_horslimites_id)/nrow(Attaques_ext_df_liste[[as.character(x)]])
  Attaques_Annee_sf["Taux_horslimites/tot"] <- length(Attaques_horslimites_id)/nrow(Attaques_Annee_sf)
  Attaques_Annee_sf <- Attaques_Annee_sf[c(which(!row.names(Attaques_Annee_sf) %in% Attaques_horslimites_id)),]
    #Relocate the other attacks outside pastoral units into their closest pastoral unit
  Attaques_ext_a_relocaliser_id <- setdiff(row.names(Attaques_ext_df_liste[[as.character(x)]]),Attaques_horslimites_id)
  Attaques_Annee_sf["Taux_relocalisees/tot"] <- length(Attaques_ext_a_relocaliser_id)/nrow(Attaques_Annee_sf)
  Attaques_ext_a_relocaliser_sf <- Attaques_Annee_sf[c(which(row.names(Attaques_Annee_sf) %in% Attaques_ext_a_relocaliser_id)),]
  for(i in 1:nrow(Attaques_ext_a_relocaliser_sf))    { 
    UP_plusproche_sf <- UP_Annee_sf[st_nearest_feature(Attaques_ext_a_relocaliser_sf[i,],UP_Annee_sf),] #Identification of the closest PU
    Attaque_relocalisee_sf <- st_sample_exact(UP_plusproche_sf, 1) #Sample a point in the closest PU
    st_geometry(Attaques_Annee_sf[row.names(Attaques_ext_a_relocaliser_sf[i,]),]) <- st_geometry(Attaque_relocalisee_sf)} #Coordinates update
  row.names(Attaques_Annee_sf) <- NULL
  Attaques_sf_liste[[x]] <- Attaques_Annee_sf}
Attaques_sf_liste <- lapply(1995:2018, function(x) Relocalisation_Attaques_fonction(x))
names(Attaques_sf_liste) <- c(1995:2018)
save(Attaques_sf_liste,file="Attaques_sf_liste.RData")

###########################################################################
############# 6. Extract the limited area of Mercantour park ##############
###########################################################################

############## Mercantour limits file ##############

Mercantour_sf <- st_read("PNF_PNM_aout_2015.shp",crs=2154) 

############## Restrict pastoral units ##############

UP1996_Mercantour_logical <- st_intersects(UP1996_sf,Mercantour_sf) %>% lengths > 0
UP1996_Mercantour_sf <- UP1996_sf[c(which(UP1996_Mercantour_logical)),]
row.names(UP1996_Mercantour_sf) <- NULL
save(UP1996_Mercantour_sf,file="UP1996_Mercantour_sf.RData")

UP2012_Mercantour_logical <- st_intersects(UP2012_sf,Mercantour_sf) %>% lengths > 0
UP2012_Mercantour_sf <- UP2012_sf[c(which(UP2012_Mercantour_logical)),]
row.names(UP2012_Mercantour_sf) <- NULL
save(UP2012_Mercantour_sf,file="UP2012_Mercantour_sf.RData")

############## Restrict wolf distribution ##############

Maillage_Loup_Mercantour_sf_fonction <- function(x) {
  Maillage_Loup_Mercantour_sf_liste <- list()
  Maillage_Loup_Mercantour_logical <- st_intersects(Maillage_Loup_sf_liste[[as.character(x)]],Mercantour_sf) %>% lengths > 0
  Maillage_Loup_Mercantour_sf <- Maillage_Loup_sf_liste[[as.character(x)]][c(which(Maillage_Loup_Mercantour_logical)),]
  Maillage_Loup_Mercantour_sf_liste[[x]] <- Maillage_Loup_Mercantour_sf}
Maillage_Loup_Mercantour_sf_liste <- lapply(1995:2018, function(x) Maillage_Loup_Mercantour_sf_fonction(x))
names(Maillage_Loup_Mercantour_sf_liste) <- c(1995:2018)
save(Maillage_Loup_Mercantour_sf_liste,file="Maillage_Loup_Mercantour_sf_liste.RData")

############## Restrict pastoral units within wolf distribution ##############

UP_Loup_Mercantour_sf_fonction <- function(x) {
  UP_Loup_Mercantour_sf_liste <- list()
  UP_Loup_Mercantour_logical <- st_intersects(UP_Loup_sf_liste[[as.character(x)]],Mercantour_sf) %>% lengths > 0
  UP_Loup_Mercantour_sf <- UP_Loup_sf_liste[[as.character(x)]][c(which(UP_Loup_Mercantour_logical)),]
  if(length(which(duplicated(UP_Loup_Mercantour_sf[c(1,2)])))>0){
  UP_Loup_Mercantour_sf <- UP_Loup_Mercantour_sf[-c(which(duplicated(UP_Loup_Mercantour_sf[c(1,2)]))),]}
  row.names(UP_Loup_Mercantour_sf) <- NULL
  UP_Loup_Mercantour_sf_liste[[x]] <- UP_Loup_Mercantour_sf}
UP_Loup_Mercantour_sf_liste <- lapply(1995:2018, function(x) UP_Loup_Mercantour_sf_fonction(x))
names(UP_Loup_Mercantour_sf_liste) <- c(1995:2018)
save(UP_Loup_Mercantour_sf_liste,file="UP_Loup_Mercantour_sf_liste.RData")

############## Restrict attacks ##############

Attaques_Mercantour_sf_fonction <- function(x) {
  Attaques_Mercantour_sf_liste <- list()
  Attaques_Mercantour_logical <- st_intersects(Attaques_sf_liste[[as.character(x)]],Mercantour_sf) %>% lengths > 0
  Attaques_Mercantour_sf <- Attaques_sf_liste[[as.character(x)]][c(which(Attaques_Mercantour_logical)),]
  row.names(Attaques_Mercantour_sf) <- NULL
  Attaques_Mercantour_sf_liste[[x]] <- Attaques_Mercantour_sf}
Attaques_Mercantour_sf_liste <- lapply(1995:2018, function(x) Attaques_Mercantour_sf_fonction(x))
names(Attaques_Mercantour_sf_liste) <- c(1995:2018)
save(Attaques_Mercantour_sf_liste,file="Attaques_Mercantour_sf_liste.RData")

############## Restrict data about attacks outside pastoral units ##############

Attaques_ext_Mercantour_df_fonction <- function(x) {
  Attaques_ext_Mercantour_df_liste <- list()
  Attaques_ext_sf_Annee <- Attaques_sf[c(as.numeric(row.names(Attaques_ext_df_liste[[as.character(x)]]))),]
  Attaques_ext_Mercantour_logical <- st_intersects(Attaques_ext_sf_Annee,Mercantour_sf) %>% lengths > 0
  Attaques_ext_Mercantour_df <- Attaques_ext_df_liste[[as.character(x)]][c(which(Attaques_ext_Mercantour_logical)),]
  Attaques_ext_Mercantour_df_liste[[x]] <- Attaques_ext_Mercantour_df}
Attaques_ext_Mercantour_df_liste <- lapply(1995:2018, function(x) Attaques_ext_Mercantour_df_fonction(x))
names(Attaques_ext_Mercantour_df_liste) <- c(1995:2018)
save(Attaques_ext_Mercantour_df_liste,file="Attaques_ext_Mercantour_df_liste.RData")
