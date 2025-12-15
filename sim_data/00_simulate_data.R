# -----------------------------------------------------------------------------------
# Script with functions to generate data following similar distribution to ABCD data
# -----------------------------------------------------------------------------------

library(extraDistr)
library(mixdist)

here::i_am("sim_data/00_simulate_data.R")

# ----------------------------- Covariate Simulation Functions ------------------------------------ 

#' Function to simulate pathogen quantities
#'
#' @param n sample size to generate
#' @param range vector with min and max to use for truncation
#' @shape shape parameter for gamma
#' @rate rate parameter for gamma
#'
#' @returns truncated gamma distributed data
rgammat <- function(n, range, shape, rate = 1) {

  F.a <- pgamma(min(range), shape = shape, rate = rate)
  F.b <- pgamma(max(range), shape = shape, rate = rate)

  u <- runif(n, min = F.a, max = F.b)

  qgamma(u, shape = shape, rate = rate)

}

#' Function to simulate pathogen quantities
#'
#' @param n sample size to generate
#'
#' @returns dataset of pathogen quantities
pathogen_quantity <- function(n){

  # Indicator quantity > 1 * shape of distribution from real ABCD data

  rota_sim <- rbinom(n, size = 1, prob=0.24) * rgamma(n, shape = 3.90, rate = 1.25)       #rotavirus
  noro_sim <- rbinom(n, size = 1, prob=0.19) * runif(n, 0, 6)                             #norovirus
  adeno_sim <- rbinom(n, size = 1, prob=0.19) * rgamma(n, shape=1.19, rate = 0.43)        #adenovirus
  astro_sim <- rbinom(n, size = 1, prob=0.09) * runif(n, 0, 6)                            #astrovirus
  sapo_sim <- rbinom(n, size = 1, prob=0.17) * rgamma(n, shape=2.34, rate=0.93)           #sapovirus
  st_etec_sim <- rbinom(n, size = 1, prob=0.23) * runif(n, 0,7)                           #st_etec
  shigella_sim <- rbinom(n, size = 1, prob=0.19) * rweibull(n, shape=1.86, scale = 3.06)  #shigella
  campylobacter_sim <- rbinom(n, size = 1, prob=0.34) * rweibull(n, shape=1.82, scale = 2.73) #campylobacter
  tepec_sim <- rbinom(n, size = 1, prob=0.22) * runif(n, 0, 6.5)                          #tepec
  v_cholerae_sim <- rbinom(n, size=1, prob=0.03) * rgamma(n, shape=1.86,rate=1.06)        #v_cholerae
  salmonella_sim <- rbinom(n, size=1, prob=0.01) * rweibull(n, shape=1.28, scale=1.93)    #salmonella
  cryptosporidium_sim <- rbinom(n, size=1, prob=0.20) * rgamma(n, shape = 2.21, rate = 0.77) #cryptosporidium

  rota_bin <- ifelse(rota_sim > 0, 1, 0)       #rotavirus
  noro_bin <- ifelse(noro_sim > 0, 1, 0)       #norovirus
  adeno_bin <- ifelse(adeno_sim > 0, 1, 0)     #adenovirus
  astro_bin <- ifelse(astro_sim > 0, 1, 0)     #astrovirus
  sapo_bin <- ifelse(sapo_sim > 0, 1, 0)       #sapovirus
  st_etec_bin <- ifelse(st_etec_sim > 0, 1, 0) #st_etec
  shigella_bin <- ifelse(shigella_sim > 0, 1, 0) #shigella
  campylobacter_bin <- ifelse(campylobacter_sim > 0, 1, 0)  #campylobacter
  tepec_bin <- ifelse(tepec_sim > 0, 1, 0)                  #tepec
  v_cholerae_bin <- ifelse(v_cholerae_sim > 0, 1, 0)        #v_cholerae
  salmonella_bin <- ifelse(salmonella_sim > 0, 1, 0)        #salmonella
  cryptosporidium_bin <- ifelse(cryptosporidium_sim > 0, 1, 0) #cryptosporidium

  pathogen_sim <- cbind(rotavirus_new = rota_sim,
                        norovirus_new = noro_sim,
                        adenovirus_new = adeno_sim,
                        astrovirus_new = astro_sim,
                        sapovirus_new = sapo_sim,
                        st_etec_new = st_etec_sim,
                        shigella_new = shigella_sim,
                        campylobacter_new = campylobacter_sim,
                        tepec_new = tepec_sim,
                        v_cholerae_new = v_cholerae_sim,
                        salmonella_new = salmonella_sim,
                        cryptosporidium_new = cryptosporidium_sim,
                        rotavirus_bin = rota_bin,
                        norovirus_bin = noro_bin,
                        adenovirus_bin = adeno_bin,
                        astrovirus_bin = astro_bin,
                        sapovirus_bin = sapo_bin,
                        st_etec_bin = st_etec_bin,
                        shigella_bin = shigella_bin,
                        campylobacter_bin = campylobacter_bin,
                        tepec_bin = tepec_bin,
                        v_cholerae_bin = v_cholerae_bin,
                        salmonella_bin = salmonella_bin,
                        cryptosporidium_bin = cryptosporidium_bin)

  return(data.frame(pathogen_sim))
}

#' Function to generate illness characteristics
#'
#' @param n sample size to generate
#'
#' @returns dataset of illness characteristics
illness_char <- function(n){
  # dy1_scrn_vomitall
  vomit_sim <- rbinom(n, size=1, prob=0.97) + 1

  # dy1_scrn_lstools
  lstools_sim <- rnbinom(n, size=7.06, prob=0.51)

  # dy1_scrn_sstools
  sstools_sim <- rnbinom(n, size=0.08, prob=0.43)

  # dy1_scrn_diardays
  diardays_sim <- rpois(n, 2.5)

  # dy1_scrn_dehydr
  dehyrdr_sim <- sample(c(1,2,3), n, replace = TRUE, prob = c(0.45,0.5,0.05)) #in original data, 1-3

  illness_sim <- cbind(dy1_scrn_vomitall = vomit_sim,
                       dy1_scrn_lstools = lstools_sim,
                       dy1_scrn_sstools = sstools_sim,
                       dy1_scrn_diardays = diardays_sim,
                       dy1_scrn_dehydr = dehyrdr_sim)

  return(data.frame(illness_sim))

}

#' Function to generate sociodemographic characteristics
#'
#' @param n sample size to generate
#'
#' @returns dataset of sociodemographic characteristics
sociodemographic_char <- function(n){
  #site
  site_sim <- rdunif(n, 2, 8) #sites are numbered 2 through 8

  #dy1_ant_sex
  sex_sim <- rbinom(n, 1, 0.5) + 1 # sex is numbered 1 and 2

  #agemchild
  agemchild_sim <- rgammat(n, range = c(2,24), shape=4.88, rate=0.42) # looks gamma distributed but study restrictions 2 to 24 months

  #an_ses_quintile
  ses_sim <- rdunif(n, 1, 5)

  #an_tothhlt5
  hh_sim <- rtpois(n, 1.72, a = 0) # truncate poisson because need at least 1 child <5

  #month_en
  # TODO this should be dependent on site? (looks uniform overall but not by site)
  month_en_sim <- rdunif(n, 1, 12)

  # Create dataframe with all sociodemographic characteristics
  sociodem_sim <- cbind(site = site_sim,
                        dy1_ant_sex = sex_sim,
                        agemchild = agemchild_sim,
                        an_ses_quintile = ses_sim,
                        an_tothhlt5 = hh_sim,
                        month_en = month_en_sim)

  #rotaseason
  cond_pmf_rota <- data.frame(
    January = c(0, 1, 0, 1, 0, 1, 1),
    February = c(0, 0, 1, 1, 0, 0, 1),
    March = c(0, 0, 1, 1, 0, 0, 1),
    April = c(0, 0, 1, 1, 1, 1, 1),
    May = c(1, 1, 0, 1, 1, 1, 1),
    June = c(0, 0, 1, 1, 1, 0, 1),
    July = c(1, 1, 0, 0, 0, 1, 0),
    August = c(1, 1, 0, 0, 1, 1, 0),
    September = c(1, 1, 1, 0, 0, 1, 0),
    October = c(1, 1, 1, 0, 1, 1, 0),
    November = c(1, 1, 1, 0, 0, 1, 0),
    December = c(1, 1, 1, 0, 0, 1, 0),
    row.names = c("Bangladesh", "Kenya", "Malawi", "Mali", "India", "Tanzania", "Pakistan")
  )

  rotaseason <- apply(sociodem_sim, 1, function(x, cond_pmf_rota){
    prob <- cond_pmf_rota[x['site'] - 1, x['month_en']] # minus one because sites are labeled 2-8 but rows are 1-7
    return(prob)
    # look at site and month_en in x
    # pull 1 or 0 from cond_pmf_rota and put in new column
  }, cond_pmf_rota)

  return(data.frame(cbind(sociodem_sim, rotaseason)))
}

#' Function to generate malnutrition indicator data
#'
#' @param n sample size to generate
#' @param sociodem_sim dataframe of simulated sociodemographic data
#'
#' @returns dataframe of malnutrition indicators
malnutrition_indicators <- function(n, sociodem_sim){

  # Intercept, 2nd quintile, 3rd quintile, 4th quintile, 5th quintile, Female
  beta_avemuac <- c(12.9818285, -0.3453998, -0.1088421, 0.1998653, -0.3146727, 0.3189722)
  beta_lfazscore <- c(-1.2365542, -0.4029033, -0.2715260, 0.3132025, -0.4289430, -0.2856522)
  beta_wfazscore <- c(-1.4689838, -0.5181520, -0.1285566, 0.2425639, -0.4257100, -0.1953112)
  beta_wflzscore <- c(-1.05077487, -0.37471703, 0.03526312, 0.09435949, -0.23822604, -0.07722452)

  # Covariance matrix
  Sigma <- data.frame(c(1.327570, 0.5344090, 1.051969, 0.9528800),
                      c(0.534409, 1.7309902, 1.141004, 0.2196409),
                      c(1.051969, 1.1410042, 1.482247, 1.1427403),
                      c(0.952880, 0.2196409, 1.142740, 1.4796410))

  # Find expected values for avemuac, lfaz, wfaz, wflz based on coefficients
  E_avemuac_given_ses_and_sex <-
    beta_avemuac[1] +
    beta_avemuac[2] * (sociodem_sim$an_ses_quintile == 2) +
    beta_avemuac[3] * (sociodem_sim$an_ses_quintile == 3) +
    beta_avemuac[4] * (sociodem_sim$an_ses_quintile == 4) +
    beta_avemuac[5] * (sociodem_sim$an_ses_quintile == 5) +
    beta_avemuac[6] * (sociodem_sim$dy1_ant_sex == 2)

  E_lfazscore_given_ses_and_sex <-
    beta_lfazscore[1] +
    beta_lfazscore[2] * (sociodem_sim$an_ses_quintile == 2) +
    beta_lfazscore[3] * (sociodem_sim$an_ses_quintile == 3) +
    beta_lfazscore[4] * (sociodem_sim$an_ses_quintile == 4) +
    beta_lfazscore[5] * (sociodem_sim$an_ses_quintile == 5) +
    beta_lfazscore[6] * (sociodem_sim$dy1_ant_sex == 2)

  E_wfazscore_given_ses_and_sex <-
    beta_wfazscore[1] +
    beta_wfazscore[2] * (sociodem_sim$an_ses_quintile == 2) +
    beta_wfazscore[3] * (sociodem_sim$an_ses_quintile == 3) +
    beta_wfazscore[4] * (sociodem_sim$an_ses_quintile == 4) +
    beta_wfazscore[5] * (sociodem_sim$an_ses_quintile == 5) +
    beta_wfazscore[6] * (sociodem_sim$dy1_ant_sex == 2)

  E_wflzscore_given_ses_and_sex <-
    beta_wflzscore[1] +
    beta_wflzscore[2] * (sociodem_sim$an_ses_quintile == 2) +
    beta_wflzscore[3] * (sociodem_sim$an_ses_quintile == 3) +
    beta_wflzscore[4] * (sociodem_sim$an_ses_quintile == 4) +
    beta_wflzscore[5] * (sociodem_sim$an_ses_quintile == 5) +
    beta_wflzscore[6] * (sociodem_sim$dy1_ant_sex == 2)

  E_malnutrition_given_ses_and_sex <- cbind(avemuac = E_avemuac_given_ses_and_sex,
                                            lfazscore = E_lfazscore_given_ses_and_sex,
                                            wfazscore = E_wfazscore_given_ses_and_sex,
                                            wflzscore = E_wflzscore_given_ses_and_sex)

  # Generate multivariate normal simulation data
  results <- apply(E_malnutrition_given_ses_and_sex, 1, function(x, Sigma){
    mal_sim <- MASS::mvrnorm(mu = c(x[1], x[2], x[3], x[4]), Sigma = Sigma, n=1)
    return(mal_sim)
  }, Sigma)

  sim_mvdata <- data.frame(t(results))

  return(sim_mvdata)

}

# ----------------------------- Treatment Simulation Function ------------------------------------ 

#' Function to assign treatment in simulation data
#'
#' @param n sample size to generate
#'
#' @returns dataframe of treatment assignments
treatment_assignment <- function(n){
  # original data coded 1, 2 so add 1
  an_grp_01 <- rbinom(n, size=1, prob=0.5)
  return(data.frame(an_grp_01))
}

# ----------------------------- Outcome Simulation Function ------------------------------------ 

#' Function to generate lazd90 based on formatted ABCD simulation dataset
#'
#' @param abcd_sim dataset of simulated abcd data
#'
#' @returns dataset of size nrow(abcd_data_sim) with added lazd90 outcome variable
generate_lazd90 <- function(abcd_data_sim){
    
    lazd90 <- -0.4213183637 +
        0.0079037187 * abcd_data_sim$dy1_scrn_diardays +
        0.1150761291 * I(abcd_data_sim$site == "Kenya") +
        -0.0539769251 * I(abcd_data_sim$site == "Malawi") +
        0.0788648620 * I(abcd_data_sim$site == "Mali") +
        -0.0250071035 * I(abcd_data_sim$site == "India") +
        -0.1211349999 * I(abcd_data_sim$site == "Tanzania") +
        -0.0058588270 * I(abcd_data_sim$site == "Pakistan") +
        -0.0626945585 * I(abcd_data_sim$an_ses_quintile == "2nd quintile of SES") +
        -0.0246203478 * I(abcd_data_sim$an_ses_quintile == "3rd quintile of SES") +
        0.0214125476 * I(abcd_data_sim$an_ses_quintile == "4th quintile of SES") +
        0.0517797108  * I(abcd_data_sim$an_ses_quintile == "5th quintile of SES") +
        -0.0001681260 * abcd_data_sim$agemchild + 
        0.8384232165 * abcd_data_sim$lfazscore + 
        -0.0763975679 * I(abcd_data_sim$shigella_new > 0) + 
        0.1940622 * I(I(abcd_data_sim$shigella_new > 0) * abcd_data_sim$an_grp_01) +
        -0.007401625 * I(I(abcd_data_sim$shigella_new > 0) * abcd_data_sim$lfazscore * abcd_data_sim$an_grp_01) + 
        -0.0025 * I(I(abcd_data_sim$shigella_new > 0) * abcd_data_sim$lfazscore * abcd_data_sim$agemchild * abcd_data_sim$an_grp_01)

  lazd90 <- lazd90 + rnorm(length(lazd90), 0, 0.5644183)
  
  # add 4% missing at random
  prop_miss <- 0.04
  delta <- rbinom(n = nrow(abcd_data_sim), size = 1, prob = prop_miss)
  
  # add missingness corresponding to delta
  lazd90 <- ifelse(delta == 0, lazd90, NA)
  
  abcd_data_sim$lazd90 <- lazd90
  
  return(abcd_data_sim)
    
}

# -------------------------------- Formatting Function ------------------------------------------

#' Function to format categorical covariates into factors
#' 
#' @param abcd_data initially simulated abcd covariates
#' 
#' @returns abcd_data with categorical variables formatted into factors
format_factors <- function(abcd_data){
  
  abcd_data$dy1_scrn_vomitall <- factor(abcd_data$dy1_scrn_vomitall, levels=c(1,2), 
                                        labels=c("No", "Yes"))
  
  abcd_data$dy1_scrn_dehydr <- factor(abcd_data$dy1_scrn_dehydr, levels=c(1,2,3), 
                                      labels=c("No dehydration", "Some dehydration", "Severe dehydration"))
  
  abcd_data$site <- factor(abcd_data$site, levels=c(2:8),
                           labels=c("Bangladesh", "Kenya", "Malawi", "Mali", "India", "Tanzania", "Pakistan"))
  
  abcd_data$dy1_ant_sex <- factor(abcd_data$dy1_ant_sex, levels=c(1,2),
                                  labels=c("Male", "Female"))
  
  abcd_data$an_ses_quintile <- factor(abcd_data$an_ses_quintile, levels=c(1:5),
                                      labels = c("1st quintile of SES",
                                                 "2nd quintile of SES",
                                                 "3rd quintile of SES",
                                                 "4th quintile of SES",
                                                 "5th quintile of SES"))
  
  abcd_data$month_en <- factor(abcd_data$month_en, levels = c(1:12),
                               labels = c("January",
                                          "February",
                                          "March",
                                          "April",
                                          "May",
                                          "June",
                                          "July",
                                          "August",
                                          "September",
                                          "October",
                                          "November",
                                          "December"))
  
  return(abcd_data)
}

# -------------------------------- Main Function ------------------------------------------ 

#' Function to generate covariates for ABCD simulation dataset
#'
#' @param n sample size to generate
#'
#' @returns dataset of size n
generate_abcd <- function(n){
  pathogen_df <- pathogen_quantity(n)
  illness_df <- illness_char(n)
  sociodem_df <- sociodemographic_char(n)
  malnutrition_df <- malnutrition_indicators(n, sociodem_df)
  treatment_df <- treatment_assignment(n)
  pid <- 1:n

  abcd_data <- cbind(pid,
                     treatment_df,
                     pathogen_df,
                     illness_df,
                     sociodem_df,
                     malnutrition_df)

  abcd_data <- format_factors(abcd_data)
  abcd_data <- generate_lazd90(abcd_data)

  return(data.frame(abcd_data))
}
