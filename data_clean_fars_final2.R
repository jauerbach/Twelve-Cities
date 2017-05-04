library("dplyr")
library("rstan")

setwd("~/Dropbox/transportation GES/")
funcs_trunc_pois <- stan_model('~/Dropbox/Twelve-Cities/model_anova.stan')
funcs <- expose_stan_functions(funcs_trunc_pois)

accs = list()
years = 2010:2015
for (year_i in seq_along(years)) {
  year = years[year_i]
  accs[[year_i]] = fread(paste0('data/FARS/ACCIDENT_',year,'_fars_sas_full.csv')) %>%
    data.frame() %>%
    select(ST_CASE,
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
    filter(CITY %in% c(1670,3290,4170,0120,1980,0330,1650,1960,3340,3260,0600) | STATE == 11)
}

dts <- bind_rows(accs)

###use exposure2 script to impute
library("RCurl")
library("ggmap")
library("bit64")
library("data.table")
#load census data
pdb2014trv9_us <- read.csv(
  "~/Dropbox/transportation GES/exposure/PBD_Tract_2014-11-20a/pdb2014trv9_us.csv")
### Seems to be an extraneous line...no nas in this file AFAIK
pdb2014trv9_us <- pdb2014trv9_us[!is.na(pdb2014trv9_us[,14]),]

reverse_geocode <- function(lat,lon){
  block <- getURL(paste("http://data.fcc.gov/api/block/find?format=json&latitude=",
                        lat,"&longitude=",lon,"&showall=true",sep=""))
  block_id <- regmatches(strsplit(block,":")[[1]][3],
                         gregexpr('[0-9]+',strsplit(block,":")[[1]][3]))[[1]]
  #Population  
  pop <- pdb2014trv9_us[pdb2014trv9_us$GIDTR == as.numeric(substr(block_id,1,11)),14]
  #Population per square mile
  #pop <- pdb2014trv9_us[pdb2014trv9_us$GIDTR == as.numeric(substr(block_id,1,11)),14]/
  #       pdb2014trv9_us[pdb2014trv9_us$GIDTR == as.numeric(substr(block_id,1,11)),9]  
  if(length(pop)==0){NA}else{pop}
}

pops <- rep(NA_real_,nrow(dts))
for (row in 1:nrow(dts)) {
  pops[row] = reverse_geocode(dts$LATITUDE[row],dts$LONGITUD[row])
  if( (row %% 100) == 99) {print ((row + 1)/length(pops) )}
}
dts$population <- pops
dts <- dts %>% mutate(
  unique_id = paste0(ST_CASE,YEAR)
)
#saveRDS(dts,file='data/FARS/acc_use_fars_RT.RDS')
dts <- readRDS(file='~/Dropbox/transportation GES/data/FARS/acc_use_fars.RDS')
setwd('~/Dropbox/transportation GES/')
dir("data/FARS")
accs = list()
years = 2010:2015
for (year_i in seq_along(years)) {
  year = years[year_i]
  accs[[year_i]] = fread(paste0('data/FARS/VEHICLE_',year,'_fars_sas_full.csv')) %>%
    data.frame() %>%
    select(ST_CASE, 
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

dts_veh <- bind_rows(accs)

accs = list()
years = 2010:2015
for (year_i in seq_along(years)) {
  year = years[year_i]
  accs[[year_i]] = fread(paste0('data/FARS/PERSON_',year,'_fars_sas_full.csv')) %>%
    data.frame() %>%
    select(ST_CASE, PER_TYP, INJ_SEV, VEH_NO, HISPANIC, RACE, AGE, DRINKING, WORK_INJ) %>%
    mutate(
      YEAR = year,
      unique_id = paste0(ST_CASE,YEAR)
    ) %>%
    filter(unique_id %in% dts$unique_id)
}

dts_per <- bind_rows(accs)

peds <- dts_per %>% filter(
  PER_TYP %in% c(5,6)
)

jnd <- peds %>% left_join(dts, by = c('unique_id','ST_CASE','YEAR'))
cars_2 <- group_by(dts_veh,unique_id) %>% summarise(n_traf = length(unique(VTRAFWAY)),
                                                  n_lan = length(unique(VNUM_LAN)),
                                                  n_align = length(unique(VALIGN)),
                                                  n_prof = length(unique(VPROFILE)),
                                                  n_ptype = length(unique(VPAVETYP)),
                                                  n_surcond = length(unique(VSURCOND)),
                                                  n_con = length(unique(VTRAFCON)),
                                                  n_dev = length(unique(VTCONT_F))) %>% 
    filter(n_traf + n_lan + n_align + n_prof + n_ptype + n_surcond + n_con + n_dev == 8)
dts_veh_cln <- dts_veh %>% group_by(unique_id) %>% do(head(.,1))

jnd <- jnd %>% filter(unique_id %in% cars_2$unique_id) %>% left_join(dts_veh_cln,by=c('ST_CASE','unique_id','YEAR'))

jnd <- readRDS('data/FARS/comp_obs_2010_2015.RDS')
#saveRDS(jnd,file='data/FARS/comp_obs_2010_2015.RDS')
#second impute from exposure2
exposure <- readRDS('~/Dropbox/transportation GES/data/FARS/comp_obs_2010_2015_exposure.RDS')
impute <- readRDS('~/Dropbox/transportation GES/data/FARS/comp_obs_2010_2015_imputed.RDS')
impute2 <- cbind(impute[impute$LATITUDE!=99.9999,],exposure[,39:40])

impute2 %>% filter(
  !(HOUR %in% c(99)) &
  !(WEATHER %in% c(98,99)) &
  !(ROUTE %in% c(8,9)) &
  !(TYP_INT %in% c(8,9,98,99)) &
  !(LGT_COND %in% c(8,9)) &
  !(WRK_ZONE %in% c(8)) &
  !is.na(day_pop) &
  population != 0 &
  !(VSPD_LIM_IMP %in% c(98,99)) & # This excludes about 600 cases. I'm not sure how there can be so
    #much missingness in such an important variable. We may need to model...
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

#add some variables we forgot

# > builte_map[which(builte_map$builte_ind == 54),]
# # A tibble: 1 × 6
# builte_ind built_e int_cust route_cust vtrafway vnum_lan
# <int>  <fctr>    <int>      <dbl>    <int>    <int>
#   1         54 2.0.1.4        0          2        1        4

# not an intersection, not a highway, two-way-not-divided, 4 lanes

# > builte_map[which(builte_map$builte_ind == 16),]
# # A tibble: 1 × 6
# builte_ind built_e int_cust route_cust vtrafway vnum_lan
# <int>  <fctr>    <int>      <dbl>    <int>    <int>
#   1         16 2.1.1.2        1          2        1        2

# intersection, not a highway, two-way-not-divided, 2 lanes

jnd_cln %>% filter(
  INJ_SEV %in% c(3,4)
) %>% mutate(
  ik = 1,
  hour_block = if_else(HOUR %in% c(6,7,8,9), 1, 
                       if_else(HOUR %in% c(10,11,12,13,14,15),2,
                              if_else(HOUR %in% c(16,17,18), 3, 
                                      if_else(HOUR %in% c(19,20,21,22),4,5)))),
#  age = cut(AGE,c(0,18,60,120),incl = TRUE),
  route_cust = if_else(ROUTE %in% c(1,2,3),1,2), # 1 if highway, 2 if not
  int_cust = as.integer(TYP_INT != 1), # 1 if intersection, 0 if not
  int_route = interaction(int_cust, route_cust, drop = TRUE),
  wrk_zone_cust = as.integer(WRK_ZONE != 0),
#  pave = as.integer(VPAVETYP %in% c(1,2,3)),
  wet_sur = as.integer(VSURCOND %in% c(2,3,4,6,10,11)),
  built_e = interaction(
    VTRAFWAY,
    VNUM_LAN, #not all that informative without vtrafway
#    VALIGN,
#    VPAVETYP, #not enough variation to warrant modeling...
                        drop=TRUE),
  incl_weath = WEATHER %in% c(2,3,4,5,6,8),
  wend = DAY_WEEK %in% c(1,7),
  lgt_time_wend = interaction(LGT_COND, wend, hour_block,drop=TRUE),
  wetsur_incl_weath = interaction(VSURCOND,WEATHER,drop=TRUE),
  sign_signal = interaction(VTRAFCON,wrk_zone_cust,drop=TRUE)
) %>%
  ## Interact wet_sur and incl_weath 
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
    hour_block
  ) %>%
summarise(
    counts = n(),
    pops = sum(population),
    VSURCOND = first(VSURCOND),
    WEATHER = first(WEATHER),
    LGT_COND = first(LGT_COND),
    VTRAFWAY = first(VTRAFWAY),
    VNUM_LAN = first(VNUM_LAN)
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
     route_cust_ind = as.integer(route_cust == 1)
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
    INT = int_cust
  )

hour_offset <- c(4,6,3,4,7)/24 #
wend_offset <- c(5,2)/7
wend_ind <- stan_dat$wend+1

wend_corr <- wend_offset[wend_ind]
hour_corr <- hour_offset[stan_dat$hour_block]
stan_dat <- stan_dat %>% mutate(EXPR = pops * wend_corr * hour_corr)

crash_data_int <- stan_dat %>% arrange(YEAR_NM)
col_nms <- sort(Filter(function(x) !grepl('_NM|EXPR',x) & (stringr::str_to_upper(x) == x),names(crash_data_int)))
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
    !grepl('TFFC|_NM|EXPR|VSURCOND|LGT_COND|WEATHER|VTRAFWAY|VNUM_LAN',x) & (stringr::str_to_upper(x) == x) &
    length(stringr::str_extract_all(x, '_')[[1]]) < 2
  }, names(df))
mains <- Filter(function(x) !grepl('_',x), mod_nms)
ints <- Filter(function(x) grepl('_',x), mod_nms)
# ints <- Filter(function(x) !grepl('YEAR',x) | x == 'CITY_YEAR',ints)
ints <- Filter(function(x) !grepl('YEAR',x),ints)
mod_nms <- c(mains,ints, 'BLTE_INT_ROUTE')
G <- length(mod_nms)
J <- apply(df[,mod_nms], 2, function(x) length(unique(x)))
stan_dat_int <- aggregate(cbind(counts,EXPR)~., df[,c('counts','EXPR',mod_nms)], sum) %>% arrange(YEAR)
stan_dat_list_year_simple <- with(stan_dat_int,  
                      list(N_train = which.max(TEST)-1,
                           N = nrow(stan_dat_int),
                           J = J,
                           G = G,
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

stan_dat_list_year_simple$n_main <- 7
stan_dat_list_year_simple$n_inter <- G - stan_dat_list_year_simple$n_main
model_anova_year_simple <- stan_model(file = 'model_int_new_data_year_nyc.stan')
fit_int_nyc <- sampling(model_anova_year_simple, data = stan_dat_list_year_simple, iter = 2000, 
                    chains = 4, refresh = 200, cores = 4,
                    control = list(adapt_delta = 0.9, max_treedepth = 15))
saveRDS(fit_int, file = 'fit_anova_year_simple_fit.RDS')
saveRDS(fit_int_nyc, file = 'fit_anova_year_nyc_fit.RDS')
fit_int <- readRDS('fit_anova_year_simple_fit.RDS')

samps_year_simple <- rstan::extract(fit_int_nyc)

sig_vals_year_simple <- sapply(1:length(J),
                   function(x) {
                     eval(parse(text = paste0('which(apply(samps_year_simple$',paste0('e_',x),',2,quantile_tester))')))
                     }
                   ,simplify = F)
which(apply(samps_year_simple$cell_e,2,quantile_tester))



sums_year <- data.frame(sds = colMeans(samps_year_simple$sds),
                         se_sds = apply(samps_year_simple$sds,2,sd),
                         nm = c(names(J),'cell'),
                         ind = 1:(length(J) + 1)) %>%
  arrange(desc(sds))

nm <- c(names(J), 'cell')
btwn <- c("COND","CITY","SLIM","SIGN","LGHT","BLTE","YEAR",
          "cell","CITY_SLIM", "LGHT_SLIM","BLTE_SLIM",
          "COND_SLIM","COND_LGHT")
inds <- match(btwn, nm)
nms <- sapply(inds, function(x) ifelse(x == 37, 'cell_sd', paste0('sd_',x)))
coefs <- data.frame(extract(fit_int,pars=nms))
#btwn <- sapply(1:length(btwn), function(x) paste0(btwn[x], '_sd'))
coef_ggplot <- data.frame(coef_mean=apply(coefs,2,mean),
                          btwn = btwn)
coef_ggplot$upper50 <- apply(coefs,2,quantile,probs=.75)
coef_ggplot$lower50 <- apply(coefs,2,quantile,probs=.25)
coef_ggplot$upper95 <- apply(coefs,2,quantile,probs=.95)
coef_ggplot$lower95 <- apply(coefs,2,quantile,probs=.05)

ggplot(coef_ggplot, aes(btwn, coef_mean)) + 
  theme(plot.title = element_text(size=9, hjust = .5),
        plot.caption = element_text(hjust = 1)) +
  geom_linerange(aes(ymin = lower50, ymax = upper50),size=2) +
  geom_linerange(aes(ymin = lower95, ymax = upper95)) +
  coord_flip() +
  scale_y_continuous(limits=c(0,1.1*max(coef_ggplot$upper95)),
                     expand = c(0, 0)) +
  labs(y=expression(hat(sigma)(.)),x="",
       title="Analysis of Variance, Between",
       caption = paste("This figure exhibits inner 50 and 90 percent intervals",
                       "for the finite-population standard deviations of each",
                       "batch of explanatory variables. "))

wthn <- c("SLIM","COND","BLTE","LGHT","CITY",'BLTE_ROUTE','INT_ROUTE')
nms <- sapply(match(wthn, nm), function(x) paste0('e_',x))
coefs <- extract(fit_int,pars=nms)
coef_ggplot <- data.frame(wthn = character(),
                          numb = character(),
                          coef_mean=numeric(),
                          upper50 = numeric(),
                          lower50 = numeric(),
                          upper95 = numeric(),
                          lower95 = numeric())
for(var in seq_along(coefs)){
  coefs_temp <- coefs[[var]]
  coef_ggplot_temp <- data.frame(wthn = paste0(wthn[var],'_e'), 
                                 numb = paste0(wthn[var],'_e',1:ncol(coefs_temp)),
                                 coef_mean=apply(coefs_temp,2,mean))
  coef_ggplot_temp$upper50 <- apply(coefs_temp,2,quantile,probs=.75)
  coef_ggplot_temp$lower50 <- apply(coefs_temp,2,quantile,probs=.25)
  coef_ggplot_temp$upper95 <- apply(coefs_temp,2,quantile,probs=.95)
  coef_ggplot_temp$lower95 <- apply(coefs_temp,2,quantile,probs=.05)
  coef_ggplot <- rbind(coef_ggplot,coef_ggplot_temp)
}

ggplot(data.frame(coef_ggplot[coef_ggplot$wthn == "SLIM_e",][5:9,],
                  slim_names = c("20 MPH","25 MPH","30 MPH",
                                 "35 MPH","40 MPH"))) +
  theme(plot.title = element_text(size=9, hjust = .5),
        plot.caption = element_text(hjust = 1)) +
  aes(numb, lower50 + (upper50 - lower50)/2) + 
  geom_linerange(aes(ymin = lower50, ymax = upper50),size=7) +
  geom_linerange(aes(ymin = lower95, ymax = upper95)) +
  geom_text(aes(label = slim_names),color="white",size=3) +
  coord_flip() +
  scale_x_discrete(breaks = NULL) +
  theme(legend.position = "none") +
  labs(title="Analysis of Variance, Within",
       y=expression(hat(mu)("SLIM")),x="",
       color = "",
       caption = paste("This figure exhibits inner 50 and 90 percent intervals for",
                       "selected speed limit effects. Effects are interpreted as the log",
                       "of the expected multiplicative \nincrease in the fatality rate",
                       "holding all else constant. With this model, slight",
                       "average speed limit effects are observable although \nthese",
                       "differences are likely too small to be meaningful."))

df_city <- unique(stan_dat[,c('CITY','CITY_NM')])
df_city$label <- c(
    "Washington D.C.","Boston","Austin","Denver","Portland","Chicago",
    "Seattle","Los Angeles","San Diego","San Francisco","San Jose",
    "New York City")
df_city <- df_city %>% arrange(CITY)

df_slim <- unique(stan_dat[,c('SLIM_NM','SLIM')]) %>% arrange(SLIM)

ggplot(data.frame(coef_ggplot[coef_ggplot$wthn == "CITY_e",],
                  slim_names = df_city$label)) +
  theme(plot.title = element_text(size=9, hjust = .5),
        plot.caption = element_text(hjust = 1)) +
  aes(numb, lower50 + (upper50 - lower50)/2) + 
  geom_linerange(aes(ymin = lower50, ymax = upper50),size=7) +
  geom_linerange(aes(ymin = lower95, ymax = upper95)) +
  geom_text(aes(label = slim_names),color="white",size=3) +
  coord_flip() +
  scale_x_discrete(breaks = NULL) +
  theme(legend.position = "none") +
  labs(title="Analysis of Variance, Within",
       y=expression(hat(mu)("CITY")),x="",
       color = "",
       caption = paste("This figure exhibits inner 50 and 90 percent intervals for",
                       "city effects. Effects are interpreted as the log",
                       "of the expected multiplicative \nincrease in the fatality rate",
                       "holding all else constant"))

lght_df <- unique(stan_dat[,c('LGHT','LGT_COND','hour_block','wend')]) %>%
  arrange(LGHT)
lght_df$lgt_label <- dplyr::recode(lght_df$LGT_COND, 
                                   `1` = 'Daylgt',
                                   `2` = 'Dark No Lgt',
                                   `3` = 'Dark Lgt',
                                   `4` = 'Dawn',
                                   `5` = 'Dusk',
                                   `6` = 'Dark Uk Lgt',
                                   `7`= 'Other',
                                   `8` = 'Not reported',
                                   `9` = 'Unknown')

lght_df$hour_label <- dplyr::recode(lght_df$hour_block, 
                                   `1` = '6am-9am',
                                   `2` = '10am-3pm',
                                   `3` = '4pm-6pm',
                                   `4` = '7pm-10pm',
                                   `5` = '11pm-5am')

lght_df$wend_label <- if_else(lght_df$wend, 'WkEnd','Wk')
lght_df$label <- apply(lght_df[,c('lgt_label','hour_label','wend_label')],1,function(x) paste(x, collapse=' '))
  
cond_df[sig_vals_year_simple[[4]],]

cond_df <- unique(stan_dat[,c('COND','WEATHER','VSURCOND')]) %>% arrange(COND)
cond_df$weath_label <- dplyr::recode(cond_df$WEATHER,
                                     `1`='Clear',
                                     `2`='Rain',
                                     `3`='Sleet',
                                     `4`='Snow',
                                     `5`='Fog',
                                     `6`='Strong Wind',
                                     `8`='Other',
                                     `10`='Cloudy')
cond_df$sur_label <- dplyr::recode(cond_df$VSURCOND,
                                     `0`='NonTrafficWay',
                                     `1`='Dry',
                                     `2`='Wet',
                                     `3`='Snow',
                                     `4`='Ice',
                                     `5`='Sand',
                                     `6`='Water',
                                     `10`='Slush')
cond_df$label <- apply(cond_df[,c('weath_label','sur_label')],1,function(x) paste(x, collapse=' '))
                                     

blte_df <- unique(stan_dat[,c('BLTE','VTRAFWAY','VNUM_LAN')]) %>% arrange(BLTE)
blte_df$vtrafway_label <- dplyr::recode(blte_df$VTRAFWAY,
                                        `0`='NonTrafficWay',
                                        `1`='2-wayNoMedian',
                                        `2`='2-wayUnprotectedMedian',
                                        `3`='2-wayProtectedMedian',
                                        `4`='1-way',
                                        `5`='2-wayNoMedianLeftTurnLn',
                                        `6`='OnOffRamp')
blte_df$lan_label <- dplyr::recode(blte_df$VNUM_LAN,
                                        `0`='NonTrafficWay',
                                        `1`='1 Lane',
                                        `2`='2 Lanes',
                                        `3`='3 Lanes',
                                        `4`='4 Lanes',
                                        `5`='5 Lanes',
                                        `6`='6 Lanes',
                                        `7`='7+ Lanes')
blte_df$label <- apply(blte_df[,c('vtrafway_label','lan_label')],1,function(x) paste(x, collapse=' '))

blte_route_df <- unique(df[,c('BLTE','ROUTE','BLTE_ROUTE')]) %>% arrange(BLTE)
blte_route_df$BLTE_lab <- blte_df$label[blte_route_df$BLTE]
blte_route_df$ROUTE_lab <- dplyr::recode(blte_route_df$ROUTE,
                                          `0`='Local',
                                          `1`='Highway')
blte_route_df$label <- apply(blte_route_df[,c('BLTE_lab','ROUTE_lab')],1,function(x) paste(x, collapse=' '))

int_route_df <- unique(df[,c('INT','ROUTE','INT_ROUTE')]) %>% arrange(INT_ROUTE)
int_route_df$ROUTE_lab <- dplyr::recode(int_route_df$ROUTE,
                                          `0`='Local',
                                          `1`='Highway')
int_route_df$INT_lab <- dplyr::recode(int_route_df$INT,
                                          `0`='No Int',
                                          `1`='Int')

int_route_df$label <- apply(int_route_df[,c('INT_lab','ROUTE_lab')],1,function(x) paste(x, collapse=' '))
df_year <- unique(stan_dat[,c('YEAR','YEAR_NM')]) %>% arrange(YEAR)
stan_dat_int$city_lab <- df_city$label[stan_dat_int$CITY]
stan_dat_int$year_lab <- df_year$YEAR_NM[stan_dat_int$YEAR]

ggplot(data.frame(coef_ggplot[coef_ggplot$wthn == "LGHT_e",],
                  slim_names = lght_df$label)) +
  theme(plot.title = element_text(size=9, hjust = .5),
        plot.caption = element_text(hjust = 1)) +
  aes(numb, lower50 + (upper50 - lower50)/2) + 
  geom_linerange(aes(ymin = lower50, ymax = upper50),size=7) +
  geom_linerange(aes(ymin = lower95, ymax = upper95)) +
  geom_text(aes(label = slim_names),color="white",size=3) +
  coord_flip() +
  scale_x_discrete(breaks = NULL) +
  theme(legend.position = "none") +
  labs(title="Analysis of Variance, Within",
       y=expression(hat(mu)("LGHT")),x="",
       color = "",
       caption = paste("This figure exhibits inner 50 and 90 percent intervals for",
                       "lighting effects. Effects are interpreted as the log",
                       "of the expected multiplicative \nincrease in the fatality rate",
                       "holding all else constant"))

ggplot(data.frame(coef_ggplot[coef_ggplot$wthn == "COND_e",],
                  slim_names = cond_df$label)) +
  theme(plot.title = element_text(size=9, hjust = .5),
        plot.caption = element_text(hjust = 1)) +
  aes(numb, lower50 + (upper50 - lower50)/2) + 
  geom_linerange(aes(ymin = lower50, ymax = upper50),size=7) +
  geom_linerange(aes(ymin = lower95, ymax = upper95)) +
  geom_text(aes(label = slim_names),color="white",size=3) +
  coord_flip() +
  scale_x_discrete(breaks = NULL) +
  theme(legend.position = "none") +
  labs(title="Analysis of Variance, Within",
       y=expression(hat(mu)("COND")),x="",
       color = "",
       caption = paste("This figure exhibits inner 50 and 90 percent intervals for",
                       "the road condition effect. Effects are interpreted as the log",
                       "of the expected multiplicative \nincrease in the fatality rate",
                       "holding all else equal."))

ggplot(data.frame(coef_ggplot[coef_ggplot$wthn == "BLTE_e",],
                  slim_names = blte_df$label)) +
  theme(plot.title = element_text(size=9, hjust = .5),
        plot.caption = element_text(hjust = 1)) +
  aes(numb, lower50 + (upper50 - lower50)/2) + 
  geom_linerange(aes(ymin = lower50, ymax = upper50),size=7) +
  geom_linerange(aes(ymin = lower95, ymax = upper95)) +
  geom_text(aes(label = slim_names),color="white",size=3) +
  coord_flip() +
  scale_x_discrete(breaks = NULL) +
  theme(legend.position = "none") +
  labs(title="Analysis of Variance, Within",
       y=expression(hat(mu)("BLTE")),x="",
       color = "",
       caption = paste("This figure exhibits inner 50 and 90 percent intervals for",
                       "the built environment effect. Effects are interpreted as the log",
                       "of the expected multiplicative \nincrease in the fatality rate",
                       "holding all else equal."))

ggplot(data.frame(coef_ggplot[coef_ggplot$wthn == "BLTE_ROUTE_e",],
                  slim_names = blte_route_df$label)) +
  theme(plot.title = element_text(size=9, hjust = .5),
        plot.caption = element_text(hjust = 1)) +
  aes(numb, lower50 + (upper50 - lower50)/2) + 
  geom_linerange(aes(ymin = lower50, ymax = upper50),size=7) +
  geom_linerange(aes(ymin = lower95, ymax = upper95)) +
  geom_text(aes(label = slim_names),color="white",size=3) +
  coord_flip() +
  scale_x_discrete(breaks = NULL) +
  theme(legend.position = "none") +
  labs(title="Analysis of Variance, Within",
       y=expression(hat(mu)("BLTExROUTE")),x="",
       color = "",
       caption = paste("This figure exhibits inner 50 and 90 percent intervals for",
                       "the built environment interacted with road type. Effects are interpreted as the log",
                       "of the expected multiplicative \nincrease in the fatality rate",
                       "holding all else equal"))

ggplot(data.frame(coef_ggplot[coef_ggplot$wthn == "INT_ROUTE_e",],
                  slim_names = int_route_df$label)) +
  theme(plot.title = element_text(size=9, hjust = .5),
        plot.caption = element_text(hjust = 1)) +
  aes(numb, lower50 + (upper50 - lower50)/2) + 
  geom_linerange(aes(ymin = lower50, ymax = upper50),size=7) +
  geom_linerange(aes(ymin = lower95, ymax = upper95)) +
  geom_text(aes(label = slim_names),color="white",size=3) +
  coord_flip() +
  scale_x_discrete(breaks = NULL) +
  theme(legend.position = "none") +
  labs(title="Analysis of Variance, Within",
       y=expression(hat(mu)("INTxROUTE")),x="",
       color = "",
       caption = paste("This figure exhibits inner 50 and 90 percent intervals for",
                       "intersection and route effects. Effects are interpreted as the log",
                       "of the expected multiplicative \nincrease in the fatality rate",
                       "holding all else constant. "))

dat_2015 <- stan_dat_int %>% filter(year_lab == 2015)
pred30 <- apply(extract(fit_int,c("mu_indiv_pred30"))[[1]], c(1,2), poisson_log_trunc_rng)
city_30 <- apply(pred30[,dat_2015$SLIM == 7],1,sum)
deaths_30 <- sum(dat_2015$counts[dat_2015$SLIM == 7])
pvalue_30 <- paste("p-value:",round(sum(city_30 > deaths_30)/length(city_30),2))

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

pred_int <- apply(extract(fit_int_nyc,c("mu_indiv_pred"))[[1]], c(1,2), poisson_log_trunc_rng)

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

pred_ins <- apply(extract(fit_int,c("mu_indiv"))[[1]], c(1,2), poisson_log_trunc_rng)

loo_nyc <- loo::extract_log_lik(fit_int_nyc)
loo_norm <- loo::extract_log_lik(readRDS('fit_anova_year_simple_fit.RDS'))

loo_nyc_obj <- loo::loo(loo_nyc)
loo_norm_obj <- loo::loo(loo_norm)

compare(loo_norm_obj, loo_nyc_obj)
