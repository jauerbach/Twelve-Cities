
#################
# TWELVE CITIES #
#################
#CODED BY: Jonathan Auerbach and Rob Trangucci
#CONTACT: jla2167@columbia.edu

packages <- c("parallel", "data.table", "bit64", "dplyr", "RCurl","ggplot2",
              "ggmap","rstan", "rgdal", "maptools", "reshape2", 
              "rgeos", "scales", "raster")
lapply(packages, require, character.only = TRUE)

##############################
# 1. READ FARS ACCIDENT FILE #
##############################

# Fatality data from FARS ftp://ftp.nhtsa.dot.gov/fars/
# We converted to .csv with SAS
setwd("~/Dropbox/transportation GES/")
accs = list()
years = 2010:2015
for (year_i in seq_along(years)) {
  year = years[year_i]
  accs[[year_i]] = 
    fread(paste0('data/FARS/ACCIDENT_',year,'_fars_sas_full.csv')) %>%
    data.frame() %>%
    dplyr::select(ST_CASE,
           CITY, 
           STATE,
           MONTH, 
           YEAR, 
           DAY_WEEK, 
           HOUR, 
           WEATHER, 
           ROUTE, 
           TYP_INT, 
           LGT_COND,
           WRK_ZONE,
           NOT_HOUR,
           NOT_MIN,
           ARR_HOUR,
           ARR_MIN,
           HOSP_HR,
           HOSP_MN,
           WEATHER1,
           WEATHER2,
           LATITUDE,
           LONGITUD) %>%
    filter(CITY %in% 
             c(1670,3290,4170,0120,1980,0330,1650,1960,3340,3260,0600) | 
             STATE == 11)
}

dts <- bind_rows(accs)
dts <- dts[ dts$LONGITUD > -124.848974 & 
            dts$LONGITUD < -66.885444 &
            dts$LATITUDE > 24.396308 & 
            dts$LATITUDE < 49.384358, ]

dts <- dts %>% mutate(unique_id = paste0(ST_CASE,YEAR))

####################
# 2. EXPOSURE DATA #
####################

# We calculate Daytime Population as in 
## https://www.census.gov/hhes/commuting/data/calculations.html
# Resident Counts taken from 2014_CPDB 
## http://www.census.gov/research/data/planning_database/
# Worker Counts taken from 5YR_CTPP 
## https://www.fhwa.dot.gov/planning/census_issues/ctpp/data_products/2006-2010_tract_flows/
#  We converted to csv with Microsoft Access)

pdb <- fread(
  "~/Dropbox/transportation GES/exposure/PBD_Tract_2014-11-20a/pdb2014trv9_us.csv")
#load ctpp data
ctpp <- read.csv("~/Dropbox/transportation GES/exposure/CTPP/Tract-flows.txt")
ctpp_from <- aggregate(EST ~ Residence_State_FIPS_Code +
                    Residence_County_FIPS_Code +
                    Residence_Tract_FIPS_Code,
                  data = ctpp,
                  FUN = sum)
ctpp_to  <- aggregate(EST ~ Workplace_State_FIPS_Code  +
                   Workplace_County_FIPS_Code + 
                   Workplace_Tract_FIPS_Code,
                 data = ctpp,
                 FUN = sum)
colnames(ctpp_to) <- c("State","County","Tract","CTPP_TO")
colnames(ctpp_from) <- c("State","County","Tract","CTPP_FROM")
#merge census and ctpp data
pdb <- plyr::join(pdb,ctpp_to)
pdb <- plyr::join(pdb,ctpp_from)
pdb$CTPP_TO[is.na(pdb$CTPP_TO)] <- 0
pdb$CTPP_FROM[is.na(pdb$CTPP_FROM)] <- 0

reverse_geocode <- function(lon,lat){
  block <- getURL(paste("http://data.fcc.gov/api/block/find?format=json&latitude=",
                        lat,"&longitude=",lon,"&showall=true",sep=""))
  block_id <- regmatches(strsplit(block,":")[[1]][3],
                         gregexpr('[0-9]+',strsplit(block,":")[[1]][3]))[[1]]
  if(length(block_id)==0){cbind(0,0,0)}else{
  #Population  
   pop  <- pdb[pdb$GIDTR == as.numeric(substr(block_id,1,11)),]$Tot_Population_CEN_2010
   to   <- pdb[pdb$GIDTR == as.numeric(substr(block_id,1,11)),]$CTPP_TO
   from <- pdb[pdb$GIDTR == as.numeric(substr(block_id,1,11)),]$CTPP_FROM
   na.omit(cbind(pop,to,from))}
}

population <- matrix(nrow=nrow(dts),ncol=3)
for(i in 1:nrow(dts)){
  population[i,] <- reverse_geocode(lon=dts$LONGITUD[i], lat=dts$LATITUDE[i])
}
colnames(population) <- c("resident","work_to","work_from")
population <- data.frame(population)
dts$day_pop <-   population$resident - population$work_from + population$work_to 
dts$night_pop <- population$resident

#create population variable from day and night population
dts$work <- 1
dts$work[dts$DAY_WEEK %in% c(1,7) | 
               dts$HOUR > 18 |
               dts$HOUR < 6] <- 0
dts$population[dts$work == 0] <- dts$night_pop[dts$work == 0]
dts$population[dts$work == 1] <- dts$day_pop[dts$work == 1]

########################################
# 3. READ FARS VEHICLE AND PERSON FILE #
########################################

veh = list()
years = 2010:2015
for (year_i in seq_along(years)) {
  year = years[year_i]
  veh[[year_i]] = 
    fread(paste0('data/FARS/VEHICLE_',year,'_fars_sas_full.csv')) %>%
    data.frame() %>%
    dplyr::select(ST_CASE, 
           VEH_NO,
           VSPD_LIM,
           VTRAFWAY,
           VNUM_LAN,
           VALIGN,
           VPROFILE,
           VPAVETYP,
           VSURCOND,
           VTRAFCON,
           VTCONT_F,
           SPEEDREL) %>%
    mutate(
      YEAR = year,
      unique_id = paste0(ST_CASE,YEAR)
    ) %>%
    filter(unique_id %in% dts$unique_id)
}

dts_veh <- bind_rows(veh)

per = list()
years = 2010:2015
for (year_i in seq_along(years)) {
  year = years[year_i]
  per[[year_i]] = 
    fread(paste0('data/FARS/PERSON_',year,'_fars_sas_full.csv')) %>%
    data.frame() %>%
    dplyr::select(ST_CASE, 
           PER_TYP, 
           INJ_SEV, 
           VEH_NO, 
           HISPANIC, 
           RACE, 
           AGE, 
           DRINKING, 
           WORK_INJ) %>%
    mutate(
      YEAR = year,
      unique_id = paste0(ST_CASE,YEAR)
    ) %>%
    filter(unique_id %in% dts$unique_id)
}

dts_per <- bind_rows(per)

peds <- dts_per %>% filter(PER_TYP == 5)
jnd <- peds %>% left_join(dts, by = c('unique_id','ST_CASE','YEAR'))
dts_veh_cln <- dts_veh %>% group_by(unique_id) %>% do(head(.,1))
jnd <- jnd %>% left_join(dts_veh_cln,by=c('ST_CASE','unique_id','YEAR'))
jnd <- plyr::join(jnd,data.frame(REGION = c(2,4,1,1,4,3,4,4,4,4,3,4),
           CITY = c(1670,3290,4170,120,1980,330,1650,1960,3340,3260,10,600)))

##################
# 4. IMPUTE SLIM #
##################

# We estimate SLIM (Vehicle Speed Limit) in NYC via Vision Zero Data Feed
# VZ Data:     http://www.nyc.gov/html/dot/html/about/vz_datafeeds.shtml
# shapefile has been projected to WGS83

setwd("~/Dropbox/transportation GES/city_level")
jnd_nyc <- jnd[jnd$CITY == 4170,]
roads <- readOGR(dsn="speed_limit_shapefile_WGS",layer="speed_limit_WGS")
pts <- SpatialPoints(cbind(jnd_nyc$LONGITUD,jnd_nyc$LATITUDE),
                     proj4string = CRS(proj4string(roads)))
query_speedlim <- function(i){
  name <- which.min(gDistance(pts[i,], roads, byid=TRUE))
  roads@data[name,2]
}
query_street <- function(i){
  name <- which.min(gDistance(pts[i,], roads, byid=TRUE))
  roads@data[name,1]
}

spd_lim <- unlist(mclapply(seq_along(pts),query_speedlim,mc.cores = 4))
spd_lim[jnd_nyc$YEAR == 2014 & jnd_nyc$MO < 11  & spd_lim == 25] <- 30
spd_lim[jnd_nyc$YEAR < 2014  & spd_lim == 25] <- 30
jnd$VSPD_LIM_IMP <- jnd$VSPD_LIM
jnd$VSPD_LIM_IMP[jnd$CITY == 4170 & jnd$VSPD_LIM > 90] <- 
  spd_lim[jnd$VSPD_LIM[jnd$CITY == 4170] > 90]

#############################################
# 5. ESTIMATE ANNUAL AVERAGE DAILTY TRAFFIC #
#############################################

# We estimate AADT (Annual Average Daily Traffic) from FHWA via HPMS
# HPMS Data:     http://www.fhwa.dot.gov/policyinformation/hpms/shapefiles.cfm
# Field Manual:  https://www.fhwa.dot.gov/policyinformation/hpms/fieldmanual/
#  n.b. instead of California 2015 .dbf, we use 2014

setwd("~/Dropbox/transportation GES/aadt/HPMS/")

city_codes <- data.frame(
  CITY   = c("Chicago","San Francisco","New York City","Boston","Los Angeles", 
             "Austin","Portland","Seattle","San Jose","San Diego",
             "Washington D.C.", "Denver"),
  STATE  = c("illinois","california","newyork","massachusetts","california",
             "texas","oregon","washington","california","california",
             "columbia","colorado"),
  CCode  = c(1670,3290,4170,120,1980,330,1650,1960,3340,3260,10,600),
  SCode  = c(17,6,36,25,6,48,41,53,6,6,11,8),
  shape = c("illinois2015", "california2014", "newyork2015", "massachusetts2015",
  "california2014", "texas2015", "oregon2015", "washington2015", "california2014",
  "california2014","district2015","colorado2015"))

jnd$aadt <- NA
for(shp in dir()) {
  road <- readOGR(shp,substr(dir(path = shp)[1],1,
                             regexpr("\\.",dir(path = shp)[1])[1]-1))
  for(city in city_codes$CCode[city_codes$shape == shp]) {
    print(city_codes$CITY[city_codes$CCode == city])
  points <- jnd[jnd$CITY==city,]
  spat_points <- SpatialPoints(cbind(points$LONGITUD,points$LATITUDE),
                               proj4string = roads@proj4string)
  dst <- gDistance(roads,spat_points,byid=TRUE)
  jnd$aadt[jnd$CITY==city] <- apply(dst,1,function(i) road@data$aadt[which.min(i)] + 1)
  }
}
jnd$aadt[is.na(jnd$aadt)] <- 0

#################################################
# 6. READ GES ACCIDENT, VEHICLE AND PERSON FILE #
#################################################

# We obtain sampling fatalities from GES ftp://ftp.nhtsa.dot.gov/ges/
# We converted to .csv with SAS

setwd("~/Dropbox/transportation GES/")
accs = list()
years = 2010:2015
for (year_i in seq_along(years)) {
  year = years[year_i]
  accs[[year_i]] = 
    read.delim(paste0("data/GES/ACCIDENT_",year,"_ges.TXT")) %>%
    data.frame %>%
    dplyr::select(CASENUM,
           PSU,
           PSUSTRAT,
           REGION,
           STRATUM,
           WEIGHT,
           MONTH, 
           YEAR, 
           DAY_WEEK, 
           HOUR, 
           WEATHER, 
           TYP_INT, 
           LGT_COND,
           WRK_ZONE,
           WEATHER1,
           WEATHER2,
           LAND_USE) %>%
    filter(LAND_USE == 3)
}
dts <- bind_rows(accs)
dts$unique_id = paste0(dts$CASENUM,dts$YEAR)

veh = list()
years = 2010:2015
for (year_i in seq_along(years)) {
  year = years[year_i]
  veh[[year_i]] = 
    read.delim(paste0("data/GES/VEHICLE_",year,"_ges.TXT")) %>%
    data.frame %>%
    dplyr::select(CASENUM, 
           VSPD_LIM,
           VTRAFWAY,
           VNUM_LAN,
           VALIGN,
           VPROFILE,
           VSURCOND,
           VTRAFCON,
           VTCONT_F,
           SPEEDREL) %>%
    mutate(
      YEAR = year,
      unique_id = paste0(CASENUM,YEAR)
    ) %>%
    filter(unique_id %in% dts$unique_id)
}
dts_veh <- bind_rows(veh)
dts_veh_cln <- dts_veh %>% group_by(unique_id) %>% do(head(.,1))

per = list()
years = 2010:2015
for (year_i in seq_along(years)) {
  year = years[year_i]
  per[[year_i]] = 
    read.delim(paste0("data/GES/PERSON_",year,"_ges.TXT")) %>%
    data.frame %>%
    dplyr::select(CASENUM, 
                  PSU,
                  PER_TYP, 
           INJ_SEV) %>%
    mutate(
      YEAR = year,
      unique_id = paste0(CASENUM,YEAR)
    ) %>%
    filter(unique_id %in% dts$unique_id)
}
dts_per <- bind_rows(per)

#probability collision results in pedestrian fatality by covariates
frm <- formula(WEIGHT ~ TYP_INT + VTRAFWAY + VNUM_LAN + YEAR + HOUR + 
                 DAY_WEEK + VSURCOND + VTRAFCON + WEATHER + LGT_COND)
peds_num <- dts_per %>% filter(PER_TYP == 5, INJ_SEV == 4)
peds_den <- dts_per
wgt_den <- peds_den %>% left_join(dts, by = c('unique_id','CASENUM','YEAR'))
wgt_den <- wgt_den %>% left_join(dts_veh_cln,by=c('CASENUM','unique_id','YEAR'))
wgt_den <- aggregate(frm, wgt_den, sum)
colnames(wgt_den)[colnames(wgt_den) == "VSPD_LIM"] <- "VSPD_LIM_IMP"
colnames(wgt_den)[ncol(wgt_den)] <- "den"

wgt_num <- peds_num %>% left_join(dts, by = c('unique_id','CASENUM','YEAR'))
wgt_num <- wgt_num %>% left_join(dts_veh_cln,by=c('CASENUM','unique_id','YEAR'))
wgt_num <- aggregate(frm, wgt_num, sum)
colnames(wgt_num)[colnames(wgt_num) == "VSPD_LIM"] <- "VSPD_LIM_IMP"
colnames(wgt_num)[ncol(wgt_num)] <- "num"

wgt <- plyr::join(wgt_den,wgt_num)
wgt$num[is.na(wgt$num)] <- 1

jnd <- plyr::join(jnd,wgt)
jnd$COLL <- jnd$den
jnd$FATL <- jnd$num
jnd <- jnd[,!colnames(jnd) %in% c("den","num")]
jnd$COLL[is.na(jnd$COLL)] <- jnd$FATL[is.na(jnd$FATL)] <- 1

#number of collisions on nyc roads by covariates
peds_num <- dts_per %>% filter(PER_TYP == 5, INJ_SEV == 4,
                               PSU %in% c(3,24,25))
peds_den <- dts_per %>% filter(PSU %in% c(3,24,25))
wgt_den <- peds_den %>% left_join(dts, by = c('unique_id','CASENUM','YEAR'))
wgt_den <- wgt_den %>% left_join(dts_veh_cln,by=c('CASENUM','unique_id','YEAR'))
wgt_den <- aggregate(frm, wgt_den, sum)
colnames(wgt_den)[colnames(wgt_den) == "VSPD_LIM"] <- "VSPD_LIM_IMP"
colnames(wgt_den)[ncol(wgt_den)] <- "den"

wgt_num <- peds_num %>% left_join(dts, by = c('unique_id','CASENUM','YEAR'))
wgt_num <- wgt_num %>% left_join(dts_veh_cln,by=c('CASENUM','unique_id','YEAR'))
wgt_num <- aggregate(frm,wgt_num, sum)
colnames(wgt_num)[colnames(wgt_num) == "VSPD_LIM"] <- "VSPD_LIM_IMP"
colnames(wgt_num)[ncol(wgt_num)] <- "num"

wgt <- plyr::join(wgt_den,wgt_num)
wgt$num[is.na(wgt$num)] <- 1
wgt$wgt_id <- 1:nrow(wgt)

####################
# 7. DATA CLEANING #
####################

jnd <- plyr::join(jnd,wgt)
jnd$den[is.na(jnd$den)] <- jnd$num[is.na(jnd$num)]  <- 1
jnd$COLL_NYC <- jnd$den
jnd$FATL_NYC <- jnd$num
jnd <- jnd[,!colnames(jnd) %in% c("den","num")]
jnd$COLL_NYC[is.na(jnd$COLL_NYC)] <- jnd$FATL_NYC[is.na(jnd$FATL_NYC)] <- 1

jnd %>% filter(
    !(HOUR %in% c(99)) &
    !(WEATHER %in% c(98,99)) &
    !(ROUTE %in% c(8,9)) &
    !(TYP_INT %in% c(8,9,98,99)) &
    !(LGT_COND %in% c(8,9)) &
    !(WRK_ZONE %in% c(8)) &
    !is.na(day_pop) &
    population != 0 &
    !(VSPD_LIM_IMP %in% c(98,99)) &
    !is.na(VSPD_LIM_IMP) &
    !(VTRAFCON %in% c(97,98,99)) &
    #  !(VALIGN %in% c(8,9)) &
    !(VTRAFWAY %in% c(8,9)) &
    !(VSURCOND %in% c(98,99)) &
    !(VNUM_LAN %in% c(8,9)) #&
  #  !(AGE %in% c(998,999))
) -> jnd_cln

#create population variable from day and night population
jnd_cln$work <- 1
jnd_cln$work[jnd_cln$DAY_WEEK %in% c(1,7) | 
               jnd_cln$HOUR > 18 |
               jnd_cln$HOUR < 6] <- 0
jnd_cln$population[jnd_cln$work == 0] <- jnd_cln$night_pop[jnd_cln$work == 0]
jnd_cln$population[jnd_cln$work == 1] <- jnd_cln$day_pop[jnd_cln$work == 1]

jnd_cln %>% filter(
  INJ_SEV %in% c(4)
) %>% mutate(
  ik = 1,
  hour_block = if_else(HOUR %in% c(6,7,8,9), 1, 
                       if_else(HOUR %in% c(10,11,12,13,14,15),2,
                               if_else(HOUR %in% c(16,17,18), 3, 
                                       if_else(HOUR %in% c(19,20,21,22),4,5)))),
  route_cust = if_else(ROUTE %in% c(1,2,3),1,2), # 1 if highway, 2 if not
  int_cust = as.integer(TYP_INT != 1), # 1 if intersection, 0 if not
  int_route = interaction(int_cust, route_cust, drop = TRUE),
  wrk_zone_cust = as.integer(WRK_ZONE != 0),
  wet_sur = as.integer(VSURCOND %in% c(2,3,4,6,10,11)),
  built_e = interaction(
    VTRAFWAY,
    VNUM_LAN, 
    drop=TRUE),
  incl_weath = WEATHER %in% c(2,3,4,5,6,8),
  wend = DAY_WEEK %in% c(1,7),
  lgt_time_wend = interaction(LGT_COND, wend, hour_block,drop=TRUE),
  wetsur_incl_weath = interaction(VSURCOND,WEATHER,drop=TRUE),
  sign_signal = interaction(VTRAFCON,wrk_zone_cust,drop=TRUE),
  aadt_disc = cut(aadt/(24*60), c(-1,0,1,10,100))
) %>%
  group_by(
    YEAR,
    CITY,
    VSPD_LIM_IMP,
    built_e,
    int_cust,
    route_cust,
    lgt_time_wend,
    wetsur_incl_weath,
    wend,
    sign_signal,
    hour_block,
    aadt_disc
  ) %>%
  summarise(
    counts = n(),
    pops = sum(population),
    VSURCOND = first(VSURCOND),
    WEATHER = first(WEATHER),
    LGT_COND = first(LGT_COND),
    VTRAFWAY = first(VTRAFWAY),
    VNUM_LAN = first(VNUM_LAN),
    COLL = sum(COLL),
    FATL = sum(FATL),
    COLL_NYC = sum(COLL_NYC),
    FATL_NYC = sum(FATL_NYC),
    id = paste(unique(unique_id), collapse = ",")
  ) -> gpd

stan_dat <- gpd %>%
  ungroup() %>%
  mutate(
    sign_signal_ind = as.integer(factor(sign_signal)),
    city_ind = as.integer(factor(CITY)),
    spd_lim_ind = as.integer(factor(VSPD_LIM_IMP)),
    builte_ind = as.integer(factor(built_e)),
    year_ind = as.integer(factor(YEAR)),
    lgt_time_wend_ind = as.integer(factor(lgt_time_wend)),
    wetsur_incl_weath_ind = as.integer(factor(wetsur_incl_weath)),
    route_cust_ind = as.integer(route_cust == 1),
    aadt_disc = as.integer(factor(aadt_disc))
  ) %>% 
  rename (
    CITY_NM = CITY,
    CITY = city_ind,
    COND_NM = wetsur_incl_weath,
    COND = wetsur_incl_weath_ind,
    YEAR_NM = YEAR,
    YEAR = year_ind,
    SLIM_NM = VSPD_LIM_IMP,
    SLIM = spd_lim_ind,
    SIGN_NM = sign_signal,
    SIGN = sign_signal_ind,
    LGHT_NM = lgt_time_wend,
    LGHT = lgt_time_wend_ind,
    BLTE_NM = built_e,
    BLTE = builte_ind,
    ROUTE_NM = route_cust,
    ROUTE = route_cust_ind,
    INT = int_cust,
    TFFC = aadt_disc,
    COLL = COLL,
    FATL = FATL,
    COLL_NYC = COLL_NYC,
    FATL_NYC = FATL_NYC,    
    ID = id
  )

hour_offset <- c(4,6,3,4,7)/24 #
wend_offset <- c(5,2)/7
wend_ind <- stan_dat$wend+1

wend_corr <- wend_offset[wend_ind]
hour_corr <- hour_offset[stan_dat$hour_block]
stan_dat <- stan_dat %>% mutate(EXPR = pops * wend_corr * hour_corr)

#######################
# 8. FIRST STAN MODEL #
#######################

TEST <- stan_dat$YEAR == 6 
vrbls <- c("COND","CITY","YEAR","SLIM","SIGN","LGHT","BLTE","TFFC")
G <- length(vrbls)
J <- sapply(1:G, function(i)
            length(table(unique(stan_dat[,colnames(stan_dat) == vrbls[i]]))))
stan_dat_list <- with(stan_dat,
                       list(N_train = which.max(TEST)-1,
                            N = nrow(stan_dat),
                            J = J,
                            G = G,
                            BLTE = BLTE,
                            CITY = CITY,
                            COND = COND,
                            LGHT = LGHT,
                            SIGN = SIGN,
                            SLIM = SLIM,
                            YEAR = YEAR,
                            TFFC = TFFC,
                            count = counts,
                            EXPR = EXPR))
setwd("~/Dropbox/Twelve-Cities/")
model <- stan_model(file = 'model.stan')
fit <- sampling(model, data = stan_dat_list, 
                iter = 2000, chains = 4, cores = 4,
                 control = list(adapt_delta = 0.99, max_treedepth = 15))

##########################################
# 9. SECOND STAN MODEL WITH INTERACTIONS #
##########################################

crash_data_int <- stan_dat %>% arrange(YEAR_NM)
col_nms <- sort(Filter(function(x) !grepl('_NM|EXPR|COLL|FATL|COLL_NYC|FATL_NYC|ID',x) &
                         (stringr::str_to_upper(x) == x),names(crash_data_int)))
two_way <- combn(col_nms, 2)
three_way <- combn(col_nms, 3)
interaction_maker <- function(nms, df) {
  df_cp <- df
  interaction = apply(df_cp[,nms],1,function(x) paste(x, collapse='_'))
  interaction = as.integer(as.factor(interaction))
  int_nm = paste(nms,collapse='_')
  return(interaction)
}

tt_2way <- data.frame(apply(two_way, 2, function(x) interaction_maker(x, crash_data_int)))
tt_3way <- data.frame(apply(three_way, 2, function(x) interaction_maker(x, crash_data_int)))
names(tt_2way) <- apply(two_way, 2, function(x) paste(sort(x), collapse='_'))
names(tt_3way) <- apply(three_way, 2, function(x) paste(sort(x), collapse='_'))
df <- bind_cols(crash_data_int[,sort(names(crash_data_int))], tt_2way, tt_3way)
TEST <- df$YEAR == 6 

sign_tester <- function(x) {
  val <- sign(x[1]) == sign(x[2])
  return(val)
}

quantile_tester <- function(vec, probs = c(0.1,0.9)) {
  quants <- as.vector(quantile(vec, probs = probs))
  return(sign_tester(quants))
}

quantiles <- function(samps, probs = c(0.1,0.9)) {
  quants <- as.vector(quantile(vec, probs = probs))
  return(sign_tester(quants))
}

mod_nms <- Filter(function(x) {
  !(x %in% c('TEST','INT','ROUTE')) &
    !grepl('TFFC|_NM|EXPR|VSURCOND|LGT_COND|WEATHER|VTRAFWAY|VNUM_LAN|COLL|FATL|COLL_NYC|FATL_NYC|ID',x) & (stringr::str_to_upper(x) == x) &
    length(stringr::str_extract_all(x, '_')[[1]]) < 2
}, names(df))
mains <- Filter(function(x) !grepl('_',x), mod_nms)
ints <- Filter(function(x) grepl('_',x), mod_nms)
# ints <- Filter(function(x) !grepl('YEAR',x) | x == 'CITY_YEAR',ints)
ints <- Filter(function(x) !grepl('YEAR',x),ints)
mod_nms <- c(mains,ints, 'BLTE_INT_ROUTE')
G_int <- length(mod_nms)
J_int <- apply(df[,mod_nms], 2, function(x) length(unique(x)))
stan_dat_int <- aggregate(cbind(counts,EXPR,COLL,FATL,COLL_NYC,FATL_NYC)~., 
                          df[,c('counts','EXPR','FATL','COLL','COLL_NYC','FATL_NYC',mod_nms)], sum) %>% arrange(YEAR)

#see https://crashstats.nhtsa.dot.gov/Api/Public/ViewPublication/807796
## and https://ntl.bts.gov/lib/33000/33300/33325/33325.pdf
##46 PSU out of 1195, 1 Type out of 1, 1 Geographical Region out of 4, 1 City out of 2 
stan_dat_int$WGHT <- (46/1195) * (1/1) * (1/4) * (1/2) * (stan_dat_int$FATL/stan_dat_int$COLL)
##3 PSU out of 1195, 1 Type out of 1, 1 Geographical Region out of 1, 1 City out of 1
stan_dat_int$WNYC <- (3/1195) * (1/1) * (1/1) * (1/1) * (stan_dat_int$FATL_NYC/stan_dat_int$COLL_NYC)

stan_dat_list_int <- with(stan_dat_int,  
                          list(N_train = which.max(stan_dat_int$YEAR == 6)-1,
                               N = nrow(stan_dat_int),
                               J = J_int,
                               G = G_int,
                               BLTE = BLTE,
                               CITY = CITY,
                               COND = COND,
                               LGHT = LGHT,
                               SIGN = SIGN,
                               SLIM = SLIM,
                               YEAR = YEAR,
                               BLTExCITY = BLTE_CITY,
                               BLTExCOND = BLTE_COND,
                               BLTExINT = BLTE_INT,
                               BLTExLGHT = BLTE_LGHT,
                               BLTExROUTE = BLTE_ROUTE,
                               BLTExSIGN = BLTE_SIGN,
                               BLTExSLIM = BLTE_SLIM,
                               CITYxCOND = CITY_COND,
                               CITYxINT = CITY_INT,
                               CITYxLGHT = CITY_LGHT,
                               CITYxROUTE = CITY_ROUTE,
                               CITYxSIGN = CITY_SIGN,
                               CITYxSLIM = CITY_SLIM,
                               CONDxINT = COND_INT,
                               CONDxLGHT = COND_LGHT,
                               CONDxROUTE = COND_ROUTE,
                               CONDxSIGN = COND_SIGN,
                               CONDxSLIM = COND_SLIM,
                               INTxLGHT = INT_LGHT,
                               INTxROUTE = INT_ROUTE,
                               INTxSIGN = INT_SIGN,
                               INTxSLIM = INT_SLIM,
                               LGHTxROUTE = LGHT_ROUTE,
                               LGHTxSIGN = LGHT_SIGN,
                               LGHTxSLIM = LGHT_SLIM,
                               ROUTExSIGN = ROUTE_SIGN,
                               ROUTExSLIM = ROUTE_SLIM,
                               SIGNxSLIM = SIGN_SLIM,
                               BLTExINTxROUTE = BLTE_INT_ROUTE,
                               ints = as.matrix(stan_dat_int[,mod_nms]),
                               count = counts,
                               EXPR = EXPR))

stan_dat_list_int$n_main <- 7
stan_dat_list_int$n_inter <- G_int - stan_dat_list_int$n_main

model_int <- stan_model(file = 'model_int.stan')
fit_int <- sampling(model_int, data = stan_dat_list_int, 
                    iter = 2000, chains = 4, refresh = 200, cores = 4,
                    control = list(adapt_delta = 0.99, max_treedepth = 15))

samps_year_simple <- rstan::extract(fit_int)
sig_vals_year_simple <- sapply(1:length(J), function(x) {
              eval(parse(text = 
                paste0('which(apply(samps_year_simple$',
                       paste0('e_',x),
                       ',2,quantile_tester))')))
 }
                               ,simplify = F)

which(apply(samps_year_simple$cell_e,2,quantile_tester))

sums_year <- data.frame(sds = colMeans(samps_year_simple$sds),
                        se_sds = apply(samps_year_simple$sds,2,sd),
                        nm = c(names(J_int),'cell'),
                        ind = 1:(length(J_int) + 1)) %>%
arrange(desc(sds))

###################################
# 10. POSTERIOR PREDICTIVE CHECKS #
###################################

df_year <- unique(stan_dat[,c('YEAR','YEAR_NM')]) %>% arrange(YEAR)
stan_dat_int$city_lab <- df_city$label[stan_dat_int$CITY]
stan_dat_int$year_lab <- df_year$YEAR_NM[stan_dat_int$YEAR]

dat_2015 <- stan_dat_int %>% filter(year_lab == 2015)
pred30 <- apply(extract(fit_int,c("mu_indiv_pred30"))[[1]], c(1,2), 
                poisson_log_trunc_rng)
city_30 <- apply(pred30[,dat_2015$SLIM == 7],1,sum)
deaths_30 <- sum(dat_2015$counts[dat_2015$SLIM == 7])
pvalue_30 <- paste("p-value:",round(sum(city_30 > deaths_30)/length(city_30),2))

funcs_trunc_pois <-
"functions {
  int poisson_log_trunc_rng(real log_rate) {
    int draw;
    draw = poisson_log_rng(log_rate);
    while (draw == 0)
      draw = poisson_log_rng(log_rate);
    return draw;
  }
}
model {}"

funcs <- expose_stan_functions(stanc(model_code = funcs_trunc_pois))
df_city <- unique(stan_dat[,c('CITY','CITY_NM')])
df_city$label <- c(
  "Washington D.C.","Boston","Austin","Denver","Portland","Chicago",
  "Seattle","Los Angeles","San Diego","San Francisco","San Jose",
  "New York City")
df_city <- df_city %>% arrange(CITY)

qplot(city_30) +
  theme(plot.title = element_text(size=9, hjust = .5),
        plot.caption = element_text(hjust = 1)) +
  geom_vline(xintercept = sum(stan_dat$counts[stan_dat$SLIM == 7 & 
                                                stan_dat$YEAR == 6]),
             linetype=2) +
  geom_text(aes(label=pvalue_30),x=80,y=750) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,50), limits = c(0,NA)) +
  labs(title=
         "Figure 7.1: Model 1, Posterior Draws of Deaths in 2015 on 30 mph roads (unweighted)",
       x = "", y = "",
       caption = paste("This figure exhibits a histogram of draws from the posterior",
                       "predictive distribution of the total number of fatalities on the",
                       "30 mph road regions in \n2015 set aside as a test set. The dotted",
                       "line represents the observed number of fatalities on those road",
                       "regions. The p-value is the proportion \nof simulations exceeding",
                       "the number observed in 2015. The purpose of this figure is to",
                       "ensure that the model is predictive of the fatality rate \nof the",
                       "roads that did not have their posted speed limits reduced to 25",
                       "mph."))

pred25 <- apply(extract(fit_int,c("mu_indiv_pred25"))[[1]], c(1,2), poisson_log_trunc_rng)
city_25 <- apply(pred25[,dat_2015$SLIM == 6],1,sum)
deaths_25 <- sum(dat_2015$counts[dat_2015$SLIM == 6])
pvalue_25 <- paste("p-value:",round(sum(city_25 > deaths_25)/length(city_25),2))

qplot(city_25) +
  theme(plot.title = element_text(size=9, hjust = .5),
        plot.caption = element_text(hjust = 1)) +
  geom_vline(xintercept = sum(stan_dat$counts[stan_dat$SLIM == 6 & 
                                                stan_dat$YEAR == 6]),
             linetype=2) +
  geom_text(aes(label=pvalue_25,x=200,y=500)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,50), limits = c(0,NA)) +
  labs(title= 
         "Figure 7.2: Model 1, Posterior Draws of Deaths in 2015 on 25 mph roads (unweighted)",
       x = "", y = "",
       caption = paste("This figure exhibits a histogram of draws from the posterior",
                       "predictive distribution of the total number of fatalities on the",
                       "25 mph road regions in \n2015 set aside as a test set. The dotted",
                       "line represents the observed number of fatalities on those road",
                       "regions. The p-value is the proportion \nof simulations exceeding",
                       "the number observed in 2015. The purpose of this figure is to",
                       "ensure that the model is predictive of the fatality rate \nof the",
                       "roads that had their posted speed limits reduced to 25 mph."))

pred_int <- apply(extract(fit_int,c("mu_indiv_pred"))[[1]], c(1,2), poisson_log_trunc_rng)

city_25_int <- apply(pred_int[,dat_2015$SLIM == 6 & dat_2015$CITY == 12],1,sum)
n_deaths_city_25 <- sum(dat_2015$counts[dat_2015$SLIM == 6 & dat_2015$CITY == 12])
pvalue_25_int <- paste("p-value:",round(sum(city_25_int >= n_deaths_city_25)/length(city_25_int),2))

qplot(city_25_int) +
  theme(plot.title = element_text(size=9, hjust = .5),
        plot.caption = element_text(hjust = 1)) +
  geom_vline(xintercept = n_deaths_city_25,
             linetype=2) +
  geom_text(aes(label=pvalue_25_int,x=200,y=500)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,50), limits = c(0,NA)) +
  labs(title=
         "Figure 7.3: Model 2, Posterior Draws of Deaths in 2015 on 25 mph roads in NYC (unweighted)",
       x = "", y = "",
       caption = paste("This figure exhibits a histogram of draws from the posterior",
                       "predictive distribution of the total number of fatalities on the",
                       "25 mph road regions in \n2015 set aside as a test set. The dotted",
                       "line represents the observed number of fatalities on those road",
                       "regions in NYC. The p-value is the proportion \nof simulations exceeding",
                       "the number observed in 2015. The purpose of this figure is to",
                       "ensure that the model is predictive of the fatality rate \nof the",
                       "roads that had their posted speed limits reduced to 25 mph,"))

#########################################
# 11. VISION ZERO ANALYSIS INTRODUCTION #
#########################################

#Load Vision Zero Priority Roads Shapefiles
#http://www.nyc.gov/html/dot/html/about/vz_datafeeds.shtml
#meta http://www.nyc.gov/html/dot/downloads/pdf/vision-zero-view-metadata.pdf
zones <- readOGR("data/shape_files/vz_priority_zones_shapefile","vz_priority_zones")
zones@data$id <- rownames(zones@data)
zones_df <- fortify(zones)
corridors <- readOGR("data/shape_files/vz_priority_corridors_shapefile","vz_priority_corridors")
corridors@data$id <- rownames(corridors@data)
corridors_df <- fortify(corridors)
intersections <- readOGR("data/shape_files/vz_priority_intersections_shapefile","vz_priority_intersections")

###street shapefile
#http://gis.ny.gov/gisdata/inventories/details.cfm?DSID=932
#meta http://gis.ny.gov/gisdata/metadata/nysgis.streets_shp.xml
streets <- readOGR("data/shape_files/Streets_shp/","StreetSegment")
streets <- spTransform(streets, corridors@proj4string)
pts_streets <- getSpatialLinesMidPoints(streets)
nyc_bbox <- corridors@bbox
nyc_locs <- pts_streets@coords[,1] > nyc_bbox[1,1] &
  pts_streets@coords[,1] < nyc_bbox[1,2] &
  pts_streets@coords[,2] > nyc_bbox[2,1] &
  pts_streets@coords[,2] < nyc_bbox[2,2]
nyc_streets <- streets[nyc_locs,]
nyc_streets <- nyc_streets[
  nyc_streets@data$LeftCounty %in% c("Bronx","Kings","New York","Queens","Richmond"),]
nyc_streets@data$RoadID <- 1:nrow(nyc_streets)
nyc_streets@data$id <- rownames(nyc_streets)
rm(streets)
nyc_streets_df <- fortify(nyc_streets)

nyc <- get_map("Empire State Building", zoom = 14, maptype ="terrain-lines")
bkn <- get_map("Atlantic Terminal Brooklyn", zoom = 14, maptype ="terrain-lines")
sid <- get_map("Staten Island", zoom = 14, maptype ="terrain-lines")
qns <- get_map("Jamaica Queens", zoom = 14, maptype ="terrain-lines")
gid <- get_map("Governor's Island New York City", zoom = 14, maptype ="terrain-lines")
ggmap(qns) + theme_minimal(14) +
  geom_path(aes(long,lat, group = group, color = factor(cos(as.numeric(id)))),
            data = nyc_streets_df) +
  theme(legend.position = "none")

##Roads in Zones
pts_streets <- getSpatialLinesMidPoints(nyc_streets)
street_zones <- !is.na(over(pts_streets, zones)$id)
street_corridor <- apply(gDistance(pts_streets, corridors, byid=TRUE),2,min) < .0005
street_intersections <- apply(gDistance(pts_streets, intersections, byid=TRUE),2,min) < .0005
street_priority <- street_zones | street_corridor | street_intersections

#Load New York City Pedestrian Deaths
deaths_nyc <- read.csv("http://www.nyc.gov/html/dot/downloads/misc/fatality_yearly.csv")
deaths_nyc <- deaths_nyc[deaths_nyc$PedFatalit>0 & deaths_nyc$YR < 2017,]

#Give Missing Roads or Roads Outside NYC a nonpriority location: 
## Governor's Island: -74.0183945, 40.68986 NAD83 or 979148.8, 190611.6 WGS84
ggmap(gid, base_layer=ggplot(aes(x=long,y=lat), data=zones_df),
      extent = "normal", maprange = FALSE) + 
  coord_map(projection="mercator", 
            xlim=c(attr(gid, "bb")$ll.lon, attr(gid, "bb")$ur.lon),
            ylim=c(attr(gid, "bb")$ll.lat, attr(gid, "bb")$ur.lat)) + 
  theme_nothing() +
  geom_polygon(aes(long, lat, group = group), data = zones_df, alpha = .2, fill = "blue") +
  geom_point(aes(coords.x1,coords.x2), data = data.frame(intersections@coords),
             color = "blue", shape = 2) + 
  geom_path(aes(long,lat, group = group), color = "blue",
            data = corridors_df) +
  geom_point(aes(long,lat),data=data.frame(long = -74.0183945, lat = 40.68986),
             color = "red")

deaths_nyc$nodeX[is.na(deaths_nyc$nodeX) | is.na(deaths_nyc$nodeY)] <- 979148.8
deaths_nyc$nodeY[is.na(deaths_nyc$nodeX) | is.na(deaths_nyc$nodeY)] <- 190611.6

NAD83 <- "+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333 +lat_0=40.16666666666666 
+lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80
+towgs84=0,0,0 "
pts_nyc <- SpatialPoints(cbind(deaths_nyc$nodeX,deaths_nyc$nodeY),
                         CRS(NAD83))
pts_nyc <- spTransform(pts_nyc, corridors@proj4string)
deaths_nyc$LONGITUD <- pts_nyc@coords[,1]
deaths_nyc$LATITUDE <- pts_nyc@coords[,2]

#Inspect Points Out of Bbox
pts_nyc[deaths_nyc$LONGITUD < corridors@bbox[1,1] | 
          deaths_nyc$LONGITUD > corridors@bbox[1,2] |
          deaths_nyc$LATITUDE < corridors@bbox[2,1] | 
          deaths_nyc$LATITUDE > corridors@bbox[2,2],]@coords

ggmap(get_map(location = c(-73.90410,40.90686), zoom = 16)) +
  geom_point(aes(lon,lat),data = data.frame(lon = -73.90410, lat = 40.90686))
ggmap(get_map(location = c(-73.90322,40.91013), zoom = 16)) +
  geom_point(aes(lon,lat),data = data.frame(lon = -73.90322, lat = 40.91013))

#Add Vision Zero Attributes to Deaths
deaths_nyc$Zones <- factor(as.numeric(is.na(over(pts_nyc,zones)[,4])),labels = c("Zone","No Zone"))
deaths_nyc$Intersection <-  ifelse(apply(gDistance(pts_nyc, intersections, byid=TRUE),2,min) < .0005,
                                   "Intersection","No Intersection")
deaths_nyc$Corridor <-  ifelse(apply(gDistance(pts_nyc, corridors, 
                                               byid=TRUE),2,min) < .0005,
                               "Corridor","No Corridor")
deaths_nyc$Priority <-  ifelse( deaths_nyc$Zones == "Zone" |
                                  deaths_nyc$Intersection == "Intersection" |
                                  deaths_nyc$Corridor == "Corridor", "Priority","Regular")
deaths_nyc$RoadID <- apply(gDistance(pts_nyc,  nyc_streets, byid=TRUE), 2, which.min)
deaths_nyc$MinDiff <- apply(gDistance(pts_nyc, nyc_streets, byid=TRUE), 2, min)

#Check DoT's Numbers
aggregate(PedFatalit ~ YR, deaths_nyc[deaths_nyc$Priority == "Priority",], sum)
aggregate(PedFatalit ~ YR, deaths_nyc, sum)
round(mean(c(99,93,92,101,114))) #average number of fatalities from 2009-2013
table(street_priority) #number of treated and untreated roads

#Nonpriority increase
nonprior <-
  c(158,153,142,151,184,140,139,145) -
  aggregate(PedFatalit ~ YR, deaths_nyc[deaths_nyc$Priority == "Priority",], sum)$Ped
(nonprior[8] - mean(nonprior[1:5]))/mean(nonprior[1:5])

(145 - mean(c(158,153,142,151,184)))/mean(c(158,153,142,151,184))

#find the number of fatalities per road
road_deaths <-
  plyr::join(data.frame(RoadID = unique(nyc_streets@data$RoadID)),
       aggregate(PedFatalit ~ RoadID,deaths_nyc[deaths_nyc$YR < 2014,
                                                ], sum))
road_deaths$PedFatalit[is.na(road_deaths$PedFatalit)] <- 0
table(road_deaths$PedFatalit)

#Use Robbins' formula to create the empirical bayes expectations per road
emp_bayes <- 
  (1:(length(table(road_deaths$PedFatalit)) - 1) * 
     table(road_deaths$PedFatalit)[-1])/
  table(road_deaths$PedFatalit)[-length(table(road_deaths$PedFatalit))]
names(emp_bayes) <- 0:(length(emp_bayes) - 1)
emp_bayes

#Calculate expected number of fatalities on all roads
sum(as.numeric(table(road_deaths$PedFatalit))[-length(table(road_deaths$PedFatalit))] * 
      emp_bayes) / length(2009:2013)
sum(deaths_nyc$PedFatalit[deaths_nyc$YR == 2016])
(128-145)/145

#Calculate expected number of fatalities on priority roads
road_deaths <-
  plyr::join(data.frame(RoadID = unique(nyc_streets@data$RoadID[street_priority])),
       aggregate(PedFatalit ~ RoadID,deaths_nyc[deaths_nyc$YR < 2014 &
                                                  deaths_nyc$Priority == "Priority",], sum))
road_deaths$PedFatalit[is.na(road_deaths$PedFatalit)] <- 0
sum(as.numeric(table(road_deaths$PedFatalit))[-length(table(road_deaths$PedFatalit))] * 
      emp_bayes[1:4]) / length(2009:2013)
sum(deaths_nyc$PedFatalit[deaths_nyc$YR == 2016 & deaths_nyc$Priority == "Priority"])
(73-52)/52

#Plot Deaths by Treatment Locations
deaths_nyc$Study_Period <- factor(deaths_nyc$YR > 2013, labels = c("2009-2013","2016"))
ggmap(nyc, base_layer=ggplot(aes(x=long,y=lat), data=zones_df),
      extent = "normal", maprange = FALSE) + 
  coord_map(projection="mercator", 
            xlim=c(attr(nyc, "bb")$ll.lon, attr(nyc, "bb")$ur.lon),
            ylim=c(attr(nyc, "bb")$ll.lat, attr(nyc, "bb")$ur.lat)) + 
  theme_minimal() +
  geom_polygon(aes(long, lat, group = group), data = zones_df, alpha = .2, fill = "blue") +
  geom_point(aes(coords.x1,coords.x2), data = data.frame(intersections@coords),
             color = "blue", shape = 2) + 
  geom_point(aes(LONGITUD,LATITUDE, color = Priority), 
             data = deaths_nyc[deaths_nyc$YR %in% c(2009:2013,2016), ]) +
  geom_path(aes(long,lat, group = group), color = "blue",
            data = corridors_df) +
  theme(legend.position = "none",
        axis.text = element_blank()) +
  labs(x="",y="") +
  scale_color_manual(values = c("dark blue", "red")) +
  facet_wrap(~Study_Period)
#ggsave("pics/vz_before_after_map.png", dpi = 500, height = 4, width = 7, units = "in")

ggplot(rbind(aggregate(PedFatalit ~ YR + Priority, deaths_nyc, sum),
             data.frame(aggregate(PedFatalit ~ YR, deaths_nyc, sum),Priority = "Total"))) +
  theme_minimal() +
  aes(factor(YR),weight = PedFatalit,fill=Priority) +
  geom_bar() +
  theme(legend.position = "none") +
  labs(x="",y="") +
  scale_x_discrete(breaks = c("2009","2011","2013","2015")) +
  facet_grid(~factor(Priority,labels=c("Priority Roads","Non Priority Roads","All Roads")))
#ggsave("pics/vz_before_after_bar.png", dpi = 500, height = 4, width = 7, units = "in")

agg <- rbind(aggregate(PedFatalit ~ YR + Priority, deaths_nyc, sum),
             data.frame(aggregate(PedFatalit ~ YR, deaths_nyc, sum),Priority = "Total"))
before <- aggregate(PedFatalit ~ YR + Priority,
                    agg[agg$YR < 2014,], sum)
before <- aggregate(PedFatalit ~ Priority, before, mean)
before$YR <- 2008.5
after <- aggregate(PedFatalit ~ YR + Priority,
                   agg[agg$YR >= 2014,], sum)
after <- aggregate(PedFatalit ~ Priority, after, mean)
after$YR <- 2013.5

before2 <- aggregate(PedFatalit ~ YR + Priority,
                     agg[agg$YR < 2013,], sum)
before2 <- aggregate(PedFatalit ~ Priority, before2, mean)
before2$YR <- 2008.5

after2 <- aggregate(PedFatalit ~ YR + Priority,
                    agg[agg$YR == 2016,], sum)
after2 <- aggregate(PedFatalit ~ Priority, after2, mean)
after2$YR <- 2015.5

ggplot(agg) +
  theme_minimal() +
  aes(YR,weight = PedFatalit,fill=Priority) +
  geom_bar() +
  theme(legend.position = "none") +
  labs(x="",y="") +
  geom_segment(aes(y = PedFatalit, yend = PedFatalit, 
                   x = YR, xend = YR+5, group = Priority), data = before) +
  geom_segment(aes(y = PedFatalit, yend = PedFatalit, 
                   x = YR, xend = YR+1, group = Priority), data = after2) +
  scale_x_continuous(breaks = c(2009,2011,2013,2015)) +
  facet_grid(~factor(Priority,labels=c("Priority Roads","Non Priority Roads","All Roads"))) 
#ggsave("pics/vz_before_after_bar_annot.png", dpi = 500, height = 4, width = 7, units = "in")

before$SE <- c(
  exp(summary(glm(agg$PedFatalit[
    agg$Priority == "Priority" & agg$YR < 2014]~1,
    family = "poisson"))$coefficients[, 2]),
  exp(summary(glm(agg$PedFatalit[
    agg$Priority == "Regular" & agg$YR < 2014]~1,
    family = "poisson"))$coefficients[, 2]),
  exp(summary(glm(agg$PedFatalit[
    agg$Priority == "Total" & agg$YR < 2014]~1,
    family = "poisson"))$coefficients[, 2]))

after2$SE <- c(
  exp(summary(glm(after2$PedFatalit[1]~1,family = "poisson"))$coefficients[, 2]),
  exp(summary(glm(after2$PedFatalit[2]~1,family = "poisson"))$coefficients[, 2]),
  exp(summary(glm(after2$PedFatalit[3]~1,family = "poisson"))$coefficients[, 2]))


ggplot(agg) +
  theme_minimal() +
  aes(YR,weight = PedFatalit,fill=Priority) +
  geom_bar() +
  theme(legend.position = "none") +
  labs(x="",y="") +
  geom_segment(aes(y = PedFatalit, yend = PedFatalit, 
                   x = YR, xend = YR+5, group = Priority), data = before) +
  geom_segment(aes(y = PedFatalit - 1.95 * SE, yend = PedFatalit - 1.95 * SE, 
                   x = YR, xend = YR+5, group = Priority), data = before,
               linetype = 2) +
  geom_segment(aes(y = PedFatalit + 1.95 * SE, yend = PedFatalit + 1.95 * SE, 
                   x = YR, xend = YR+5, group = Priority), data = before,
               linetype = 2) +
  geom_segment(aes(y = PedFatalit, yend = PedFatalit, 
                   x = YR, xend = YR+1, group = Priority), data = after2) +
  geom_segment(aes(y = PedFatalit - 1.95 * SE, yend = PedFatalit - 1.95 * SE, 
                   x = YR, xend = YR+1, group = Priority), data = after2,
               linetype = 2) +
  geom_segment(aes(y = PedFatalit + 1.95 * SE, yend = PedFatalit + 1.95 * SE, 
                   x = YR, xend = YR+1, group = Priority), data = after2,
               linetype = 2) +
  scale_x_continuous(breaks = c(2009,2011,2013,2015)) +
  facet_grid(~factor(Priority,labels=c("Priority Roads","Non Priority Roads","All Roads"))) 
#ggsave("pics/vz_before_after_bar_annot_se.png", dpi = 500, height = 4, width = 7, units = "in")

(agg$PedFatalit[agg$Priority == "Priority" & agg$YR == 2016] -
    before$PedFatalit[before$Priority == "Priority"])/
  before$PedFatalit[before$Priority == "Priority"]

(after$PedFatalit[before$Priority == "Priority"] -
    before$PedFatalit[before$Priority == "Priority"])/
  before$PedFatalit[before$Priority == "Priority"]

(agg$PedFatalit[agg$Priority == "Regular" & agg$YR == 2016] -
    before$PedFatalit[before$Priority == "Regular"])/
  before$PedFatalit[before$Priority == "Regular"]

(after$PedFatalit[before$Priority == "Regular"] -
    before$PedFatalit[before$Priority == "Regular"])/
  before$PedFatalit[before$Priority == "Regular"]

(agg$PedFatalit[agg$Priority == "Total" & agg$YR == 2016] -
    before$PedFatalit[before$Priority == "Total"])/
  before$PedFatalit[before$Priority == "Total"]

(after$PedFatalit[before$Priority == "Total"] -
    before$PedFatalit[before$Priority == "Total"])/
  before$PedFatalit[before$Priority == "Total"]

deaths_nyc$Study_Period <- factor(deaths_nyc$YR > 2013, labels = c("2013","2016"))
ggmap(nyc, base_layer=ggplot(aes(x=long,y=lat), data=zones_df),
      extent = "normal", maprange = FALSE) + 
  coord_map(projection="mercator", 
            xlim=c(attr(nyc, "bb")$ll.lon, attr(nyc, "bb")$ur.lon),
            ylim=c(attr(nyc, "bb")$ll.lat, attr(nyc, "bb")$ur.lat)) + 
  theme_minimal() +
  geom_polygon(aes(long, lat, group = group), data = zones_df, alpha = .2, fill = "blue") +
  geom_point(aes(coords.x1,coords.x2), data = data.frame(intersections@coords),
             color = "blue", shape = 2) + 
  geom_point(aes(LONGITUD,LATITUDE, color = Priority), 
             data = deaths_nyc[deaths_nyc$YR %in% c(2013,2016), ]) +
  geom_path(aes(long,lat, group = group), color = "blue",
            data = corridors_df) +
  theme(legend.position = "none",
        axis.text = element_blank()) +
  labs(x="",y="") +
  scale_color_manual(values = c("dark blue", "red")) +
  facet_wrap(~Study_Period)
#ggsave("pics/vz_before_after_map2.png", dpi = 500, height = 4, width = 7, units = "in")
#######################################
# 12. VISION ZERO ANALYSIS CONCLUSION #
#######################################

stan_dat_int$fit <- NA
stan_dat_int$fit[stan_dat_int$YEAR!=6] <- 
  apply(exp(rstan::extract(fit_int,"mu_indiv")[[1]]),2,mean)

stan_ID <-  aggregate(cbind(ID)~.,df[,c("ID",mod_nms)], paste, collapse=",") %>% 
  arrange(YEAR) 
stan_dat_int$ID <- stan_ID$ID
jnd$ID <- jnd$unique_id

final_data <- stan_dat_int[0,]
for(obs in 1:nrow(stan_dat_int)){
  ids <- strsplit(stan_dat_int$ID[obs],",")[[1]]
  for(id in 1:length(ids)) {
    next_row <- stan_dat_int[obs,]
    next_row$ID <- ids[id]
    next_row$WGHT <- next_row$WGHT/length(ids)
    next_row$WNYC <- next_row$WNYC/length(ids)
    next_row$fit <- next_row$fit/length(ids)
    final_data <- rbind(final_data,next_row)
  }
}

results <- left_join(final_data, unique(jnd[,c("ID","LONGITUD","LATITUDE")]), by = "ID")

stan_draws <- exp(rstan::extract(fit_int,"mu_indiv")[[1]])
final_data_draws <- stan_draws[,0]

for(obs in 1:nrow(stan_dat_int[stan_dat_int$YEAR < 6,])){
  ids <- strsplit(stan_dat_int$ID[obs],",")[[1]]
  for(id in 1:length(ids)) {
    next_row <- stan_draws[,obs]
    next_row <- next_row/length(ids)
    final_data_draws <- cbind(final_data_draws,next_row)
  }
}

results_NYC <- results[results$CITY == 12,]
result_pts <- SpatialPoints(cbind(results_NYC$LONGITUD,results_NYC$LATITUDE), 
                            corridors@proj4string)

results_NYC$Zones <- factor(as.numeric(is.na(over(result_pts,zones)[,4])),
                            labels = c("Zone","No Zone"))
results_NYC$Intersection <-  ifelse(apply(gDistance(result_pts, intersections, byid=TRUE),2,min) < .0005,
                                    "Intersection","No Intersection")
results_NYC$Corridor <-  ifelse(apply(gDistance(result_pts, corridors, 
                                                byid=TRUE),2,min) < .0005,
                                "Corridor","No Corridor")
results_NYC$Priority <-  ifelse( results_NYC$Zones == "Zone" |
                                   results_NYC$Intersection == "Intersection" |
                                   results_NYC$Corridor == "Corridor", "Priority","Regular")

results_samples_NYC <- t(final_data_draws[,results$CITY[!is.na(results$fit)] == 12])
colnames(results_samples_NYC) <- paste0("Sample",1:4000)

samples <- apply(results_samples_NYC, 2, function(x) 
  (x * results_NYC$WGHT[!is.na(results_NYC$fit)]) *
    (1/results_NYC$WNYC[!is.na(results_NYC$fit)]))

samples <-
  rbind(
    data.frame(Priority = "Priority Roads",
               value = apply(samples[results_NYC$Priority[!is.na(results_NYC$fit)] == "Priority",],2, sum)),
    data.frame(Priority = "Non Priority Roads",
               value = apply(samples[results_NYC$Priority[!is.na(results_NYC$fit)] != "Priority",],2, sum)))


brh <- readOGR("data/shape_files/nybb_17a","nybb")
pts_brh <- spTransform(pts_nyc,brh@proj4string)
deaths_brh <- data.frame(deaths_nyc,over(pts_brh,brh))
brh_agg <- aggregate(Fatalities ~ YR + BoroName + Priority, deaths_brh, sum) 

sum(brh_agg[brh_agg$YR < 2014 & 
            brh_agg$BoroName %in% c("Brooklyn", "Manhattan", "Queens"),]$Fatalities)/5

observ_bmq <-
  data.frame(
    prior_before =
      sum(brh_agg[brh_agg$YR < 2014 & 
                  brh_agg$BoroName %in% c("Brooklyn", "Manhattan", "Queens") &
                  brh_agg$Priority == "Priority",]$Fatalities)/5,
    
    not_prior_before =
      sum(brh_agg[brh_agg$YR < 2014 & 
                  brh_agg$BoroName %in% c("Brooklyn", "Manhattan", "Queens") &
                  brh_agg$Priority != "Priority",]$Fatalities)/5,
    
    prior_after =
      sum(brh_agg[brh_agg$YR == 2016 & 
                    brh_agg$BoroName %in% c("Brooklyn", "Manhattan", "Queens") &
                    brh_agg$Priority == "Priority",]$Fatalities),
    
    not_prior_after =
      sum(brh_agg[brh_agg$YR == 2016 & 
                  brh_agg$BoroName %in% c("Brooklyn", "Manhattan", "Queens") &
                  brh_agg$Priority != "Priority",]$Fatalities))


sample_qntl <-
  rbind(as.numeric(quantile(samples$value[samples$Priority == "Priority Roads"],
                            c(.05,.25,.75,.95))),
        as.numeric(quantile(samples$value[samples$Priority == "Non Priority Roads"],
                            c(.05,.25,.75,.95)))
  )
colnames(sample_qntl) <- c("L95","L50","U50","U95")
sample_qntl <- data.frame(sample_qntl)
sample_qntl$Priority <- c("Priority Roads", "Non Priority Roads")

ggplot()+#samples) +
  theme_minimal() +
  #aes(Priority, value) +
  #geom_boxplot() +
  geom_linerange(aes(x = Priority, ymin = L50, ymax = U50), data = sample_qntl,
                 size = 3) +
  geom_linerange(aes(x = Priority, ymin = L95, ymax = U95), data = sample_qntl,
                 size = 1) +
  labs(x = "", y= "Expected Number of Fatalities", color = "") +
  geom_point(aes(Priority, value, color = factor(Time,
                                                 levels = c("Before","After"))), 
             shape = 95, size = 10,
             data =  data.frame(melt(observ_bmq), 
                                Priority = c("Priority Roads","Non Priority Roads",
                                             "Priority Roads","Non Priority Roads"),
                                Time = c("Before","Before", "After","After"))) +
  theme(plot.title = element_text(size=9, hjust = .5),
        plot.caption = element_text(hjust = 1, size = 7),
        legend.position = "bottom") +
  labs(title=
         "Figure 10: Expected Annual Number of Fatalities in Brooklyn, Manhattan and Queens",
       x = "", y = "")#,
       # #caption = paste("This figure exhibits the expected number of fatalities in the",
       #                 "New York City Boroughs of Brooklyn, Manhattan and Queens.",
       #                 "The red lines mark the average \n number of fatalities observed",
       #                 "on Priority and Non Priority Roads over the 2009-2013 time period.",
       #                 "These observations are suspected to be biased due\n to selection",
       #                 "effects. The black lines represent 50 and 90 percent",
       #                 "intervals of the expected number of fatalities after observing",
       #                 "fatalities from\n 2010-2014. The blue lines mark the observed",
       #                 "number of fatalities in 2016. The number of fatalities on Non",
       #                 "Priority roads appears to revert to\n the mean as expected,",
       #                 "while the number of fatalities surpasses the mean. This suggests",
       #                 "that Vision Zero Policy has been beneficial, but there\n is too",
       #                 "much posterior uncertainty to conclude that the policy has",
       #                 "resulted in a large percent decrease."))
#ggsave("pics/priorityvnonpriorty.png", dpi = 500, height = 4, width = 7, units = "in")

bfr <- sapply(1:4000, function(i)
  sum((final_data_draws[i,] *
         (1/results$WNYC[!is.na(results$fit)]) *
         results$WGHT[!is.na(results$fit)])[results$CITY[!is.na(results$fit)] == 12])
)

summary(bfr)
quantile(samples$value[samples$Priority == "Priority Roads"], c(.05,.25,.5,.75,.95))
quantile(samples$value[samples$Priority == "Non Priority Roads"], c(.05,.25,.5,.75,.95))

#50 Percent
(55 - 79.5)/79.5 #claimes
(55 - 64.5)/64.5 #expected

#90 Percent
(55 - 91.4)/91.4 #claimes
(55 - 55.6)/55.6 #expected

#Observed Reduction
table(samples$value[samples$Priority == "Priority Roads"] < 55)/4000


(55 - 71)/71
(55 - observ_bmq$prior_before)/observ_bmq$prior_before

samples2 <- apply(results_samples_NYC, 2, function(x) 
  (x * results_NYC$WGHT[!is.na(results_NYC$fit)]))

samples2 <-
  rbind(
    data.frame(Priority = "Priority Roads",
               value = apply(samples[results_NYC$Priority[!is.na(results_NYC$fit)] == "Priority",],2, sum)),
    data.frame(Priority = "Non Priority Roads",
               value = apply(samples[results_NYC$Priority[!is.na(results_NYC$fit)] != "Priority",],2, sum)))

ggplot(samples2) +
  theme_minimal() +
  aes(value, fill = Priority) +
  geom_density(color = NA, alpha = .75) + 
  theme(plot.title = element_text(size=9, hjust = .5),
        plot.caption = element_text(hjust = 1, size = 7),
        legend.position = "bottom") +
  labs(fill = "", x = "Expected Number of Fatalities") +
  labs(title=
         "Figure 9: Distribution of Expected Fatalities in Brooklyn, Manhattan and Queens per Year",
       x = "", y = "")#,
# #caption = paste("This figure exhibits the expected number of fatalities in the",
#                 "New York City Boroughs of Brooklyn, Manhattan and Queens.",
#                 "The red lines mark the average \n number of fatalities observed",
#                 "on Priority and Non Priority Roads over the 2009-2013 time period.",
#                 "These observations are suspected to be biased due\n to selection",
#                 "effects. The black lines represent 50 and 95 percent credible",
#                 "intervals of the expected number of fatalities after observing",
#                 "fatalities from\n 2010-2014. The blue lines mark the observed",
#                 "number of fatalities in 2016. The number of fatalities on Non",
#                 "Priority roads appears to revert to\n the mean as expected,",
#                 "while the number of fatalities surpasses the mean. This suggests",
#                 "that Vision Zero Policy has been beneficial, but there\n is too",
#                 "much posterior uncertainty to conclude that the policy has",
#                 "resulted in a large percent decrease."))
#ggsave("pics/expectedfatal.png", dpi = 500, height = 4, width = 7, units = "in")
