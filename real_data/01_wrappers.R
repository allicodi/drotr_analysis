# ---------------------------------------------------------------------------
# Load custom SL wrappers
# ---------------------------------------------------------------------------

here::i_am("sim_data/01_wrappers.R")

# -------------------------- Outcome Models -----------------------------------

# interaction for site and rotavirus
SL.outcome.1 <- function(Y, X, newX, family, obsWeights, ...){
  sl.outcome.1_fit <- glm(Y ~ an_grp_01 + rotavirus_new + I(rotavirus_new > 0) + norovirus_new +
                            I(norovirus_new > 0) + adenovirus_new + I(adenovirus_new > 0) +
                            sapovirus_new + I(sapovirus_new > 0) + astrovirus_new + I(astrovirus_new > 0) +
                            st_etec_new + I(st_etec_new > 0) + shigella_new + I(shigella_new > 0) +
                            campylobacter_new + I(campylobacter_new > 0) + tepec_new + I(tepec_new > 0) +
                            v_cholerae_new + I(v_cholerae_new > 0) + salmonella_new + I(salmonella_new > 0) +
                            cryptosporidium_new + I(cryptosporidium_new > 0) +
                            dy1_scrn_vomitall + dy1_scrn_lstools + dy1_scrn_sstools +
                            dy1_scrn_diardays + dy1_scrn_dehydr +
                            avemuac + wfazscore + lfazscore + wflzscore +
                            site + dy1_ant_sex + agemchild + an_ses_quintile + an_tothhlt5 +
                            month_en + rotaseason + an_grp_01*site + an_grp_01*rotavirus_new +
                            + an_grp_01*I(rotavirus_new > 0),
                          data = X,
                          family = family,
                          weights = obsWeights)
  pred <- predict(sl.outcome.1_fit, newdata = newX, type = 'response')
  fit <- list(fitted_model.outcome.1 = sl.outcome.1_fit)
  out <- list(pred = pred, fit = fit)
  class(out$fit) <- "SL.outcome.1"
  return(out)
}
predict.SL.outcome.1 <- function(object, newdata, ...){
  pred <- predict(object$fitted_model.outcome.1, newdata = newdata, type="response")
  return(pred)
}

# interaction for site and shigella
SL.outcome.2 <- function(Y, X, newX, family, ...){
  sl.outcome.2_fit <- glm(Y ~  an_grp_01 + rotavirus_new + I(rotavirus_new > 0) + norovirus_new +
                            I(norovirus_new > 0) + adenovirus_new + I(adenovirus_new > 0) +
                            sapovirus_new + I(sapovirus_new > 0) + astrovirus_new + I(astrovirus_new > 0) +
                            st_etec_new + I(st_etec_new > 0) + shigella_new + I(shigella_new > 0) +
                            campylobacter_new + I(campylobacter_new > 0) + tepec_new + I(tepec_new > 0) +
                            v_cholerae_new + I(v_cholerae_new > 0) + salmonella_new + I(salmonella_new > 0) +
                            cryptosporidium_new + I(cryptosporidium_new > 0) +
                            dy1_scrn_vomitall + dy1_scrn_lstools + dy1_scrn_sstools + dy1_scrn_diardays + dy1_scrn_dehydr +
                            avemuac + wfazscore + lfazscore + wflzscore +
                            site + dy1_ant_sex + agemchild + an_ses_quintile + an_tothhlt5 +
                            month_en + rotaseason + an_grp_01*site +
                            an_grp_01*shigella_new + an_grp_01*I(shigella_new > 0),
                          data = X,
                          family = family)
  # get predictions on newX
  pred <- predict(
    sl.outcome.2_fit, newdata = newX, type = 'response'
  )
  # format the output as named list
  fit <- list(fitted_model.outcome.2 = sl.outcome.2_fit)
  out <- list(fit = fit, pred = pred)
  # give the object a class
  class(out$fit) <- "SL.outcome.2"
  # return the output
  return(out)
}
predict.SL.outcome.2 <- function(object, newdata, ...){
  pred <- predict(object$fitted_model.outcome.2, newdata = newdata, type="response")
  return(pred)
}


# interaction for site and rotavirus SEASON
SL.outcome.3 <- function(Y, X, newX, family, ...){
  sl.outcome.3_fit <- glm(Y ~  an_grp_01 + rotavirus_new + I(rotavirus_new > 0) + norovirus_new +
                            I(norovirus_new > 0) + adenovirus_new + I(adenovirus_new > 0) +
                            sapovirus_new + I(sapovirus_new > 0) + astrovirus_new + I(astrovirus_new > 0) +
                            st_etec_new + I(st_etec_new > 0) + shigella_new + I(shigella_new > 0) +
                            campylobacter_new + I(campylobacter_new > 0) + tepec_new + I(tepec_new > 0) +
                            v_cholerae_new + I(v_cholerae_new > 0) + salmonella_new + I(salmonella_new > 0) +
                            cryptosporidium_new + I(cryptosporidium_new > 0) +
                            dy1_scrn_vomitall + dy1_scrn_lstools + dy1_scrn_sstools + dy1_scrn_diardays + dy1_scrn_dehydr +
                            avemuac + wfazscore + lfazscore + wflzscore +
                            site + dy1_ant_sex + agemchild + an_ses_quintile + an_tothhlt5 +
                            month_en + rotaseason + an_grp_01*site +
                            an_grp_01*rotaseason,
                          data = X,
                          family = family)
  # get predictions on newX
  pred <- predict(
    sl.outcome.3_fit, newdata = newX, type = 'response'
  )
  # format the output as named list
  fit <- list(fitted_model.outcome.3 = sl.outcome.3_fit)
  out <- list(fit = fit, pred = pred)
  # give the object a class
  class(out$fit) <- "SL.outcome.3"
  # return the output
  return(out)
}
predict.SL.outcome.3 <- function(object, newdata, ...){
  pred <- predict(object$fitted_model.outcome.3, newdata = newdata, type="response")
  return(pred)
}

# rotaseason through interaction by month
SL.outcome.11 <- function(Y, X, newX, family, ...){
  sl.outcome.11_fit <- glm(Y ~  an_grp_01 + rotavirus_new + I(rotavirus_new > 0) + norovirus_new +
                             I(norovirus_new > 0) + adenovirus_new + I(adenovirus_new > 0) +
                             sapovirus_new + I(sapovirus_new > 0) + astrovirus_new + I(astrovirus_new > 0) +
                             st_etec_new + I(st_etec_new > 0) + shigella_new + I(shigella_new > 0) +
                             campylobacter_new + I(campylobacter_new > 0) + tepec_new + I(tepec_new > 0) +
                             v_cholerae_new + I(v_cholerae_new > 0) + salmonella_new + I(salmonella_new > 0) +
                             cryptosporidium_new + I(cryptosporidium_new > 0) +
                             dy1_scrn_vomitall + dy1_scrn_lstools + dy1_scrn_sstools + dy1_scrn_diardays + dy1_scrn_dehydr +
                             avemuac + wfazscore + lfazscore + wflzscore +
                             site + dy1_ant_sex + agemchild + an_ses_quintile + an_tothhlt5 +
                             month_en + rotaseason + an_grp_01*site +
                             an_grp_01*month_en,
                           data = X,
                           family = family)
  # get predictions on newX
  pred <- predict(
    sl.outcome.11_fit, newdata = newX, type = 'response'
  )
  # format the output as named list
  fit <- list(fitted_model.outcome.11 = sl.outcome.11_fit)
  out <- list(fit = fit, pred = pred)
  # give the object a class
  class(out$fit) <- "SL.outcome.11"
  # return the output
  return(out)
}
predict.SL.outcome.11 <- function(object, newdata, ...){
  pred <- predict(object$fitted_model.outcome.11, newdata = newdata, type="response")
  return(pred)
}


# interaction for site, rotavirus, and shigella
SL.outcome.4 <- function(Y, X, newX, family, ...){
  sl.outcome.4_fit <- glm(Y ~  an_grp_01 + rotavirus_new + I(rotavirus_new > 0) + norovirus_new +
                            I(norovirus_new > 0) + adenovirus_new + I(adenovirus_new > 0) +
                            sapovirus_new + I(sapovirus_new > 0) + astrovirus_new + I(astrovirus_new > 0) +
                            st_etec_new + I(st_etec_new > 0) + shigella_new + I(shigella_new > 0) +
                            campylobacter_new + I(campylobacter_new > 0) + tepec_new + I(tepec_new > 0) +
                            v_cholerae_new + I(v_cholerae_new > 0) + salmonella_new + I(salmonella_new > 0) +
                            cryptosporidium_new + I(cryptosporidium_new > 0) +
                            dy1_scrn_vomitall + dy1_scrn_lstools + dy1_scrn_sstools + dy1_scrn_diardays + dy1_scrn_dehydr +
                            avemuac + wfazscore + lfazscore + wflzscore +
                            site + dy1_ant_sex + agemchild + an_ses_quintile + an_tothhlt5 +
                            month_en + rotaseason + an_grp_01*site +
                            an_grp_01*rotavirus_new + an_grp_01*I(rotavirus_new > 0) +
                            an_grp_01*shigella_new + an_grp_01*I(shigella_new > 0),
                          data = X,
                          family = family)
  # get predictions on newX
  pred <- predict(
    sl.outcome.4_fit, newdata = newX, type = 'response'
  )
  # format the output as named list
  fit <- list(fitted_model.outcome.4 = sl.outcome.4_fit)
  out <- list(fit = fit, pred = pred)
  # give the object a class
  class(out$fit) <- "SL.outcome.4"
  # return the output
  return(out)
}
predict.SL.outcome.4 <- function(object, newdata, ...){
  pred <- predict(object$fitted_model.outcome.4, newdata = newdata, type="response")
  return(pred)
}



# interaction for site and 12 pathogen quantities
SL.outcome.5 <- function(Y, X, newX, family, ...){
  sl.outcome.5_fit <- glm(Y ~  an_grp_01 + rotavirus_new + I(rotavirus_new > 0) + norovirus_new +
                            I(norovirus_new > 0) + adenovirus_new + I(adenovirus_new > 0) +
                            sapovirus_new + I(sapovirus_new > 0) + astrovirus_new + I(astrovirus_new > 0) +
                            st_etec_new + I(st_etec_new > 0) + shigella_new + I(shigella_new > 0) +
                            campylobacter_new + I(campylobacter_new > 0) + tepec_new + I(tepec_new > 0) +
                            v_cholerae_new + I(v_cholerae_new > 0) + salmonella_new + I(salmonella_new > 0) +
                            cryptosporidium_new + I(cryptosporidium_new > 0) +
                            dy1_scrn_vomitall + dy1_scrn_lstools + dy1_scrn_sstools + dy1_scrn_diardays + dy1_scrn_dehydr +
                            avemuac + wfazscore + lfazscore + wflzscore +
                            site + dy1_ant_sex + agemchild + an_ses_quintile + an_tothhlt5 +
                            month_en + rotaseason + an_grp_01*site +
                            an_grp_01*rotavirus_new + an_grp_01*I(rotavirus_new > 0) + an_grp_01*norovirus_new +
                            an_grp_01*I(norovirus_new > 0) + an_grp_01*adenovirus_new + an_grp_01*I(adenovirus_new > 0) +
                            an_grp_01*astrovirus_new + an_grp_01*I(astrovirus_new > 0) + an_grp_01*sapovirus_new +
                            an_grp_01*I(sapovirus_new > 0) + an_grp_01*st_etec_new + an_grp_01*I(st_etec_new > 0) +
                            an_grp_01*shigella_new + an_grp_01*I(shigella_new > 0) + an_grp_01*campylobacter_new +
                            an_grp_01*I(campylobacter_new > 0) + an_grp_01*tepec_new + an_grp_01*I(tepec_new > 0) +
                            an_grp_01*v_cholerae_new + an_grp_01*I(v_cholerae_new > 0) + an_grp_01*salmonella_new +
                            an_grp_01*I(salmonella_new > 0) + an_grp_01*cryptosporidium_new + an_grp_01*I(cryptosporidium_new > 0),
                          data = X,
                          family = family)
  # get predictions on newX
  pred <- predict(
    sl.outcome.5_fit, newdata = newX, type = 'response'
  )
  # format the output as named list
  fit <- list(fitted_model.outcome.5 = sl.outcome.5_fit)
  out <- list(fit = fit, pred = pred)
  # give the object a class
  class(out$fit) <- "SL.outcome.5"
  # return the output
  return(out)
}
predict.SL.outcome.5 <- function(object, newdata, ...){
  pred <- predict(object$fitted_model.outcome.5, newdata = newdata, type = "response")
  return(pred)
}

# interaction for site and vomiting
SL.outcome.6 <- function(Y, X, newX, family, ...){
  sl.outcome.6_fit <- glm(Y ~  an_grp_01 + rotavirus_new + I(rotavirus_new > 0) + norovirus_new +
                            I(norovirus_new > 0) + adenovirus_new + I(adenovirus_new > 0) +
                            sapovirus_new + I(sapovirus_new > 0) + astrovirus_new + I(astrovirus_new > 0) +
                            st_etec_new + I(st_etec_new > 0) + shigella_new + I(shigella_new > 0) +
                            campylobacter_new + I(campylobacter_new > 0) + tepec_new + I(tepec_new > 0) +
                            v_cholerae_new + I(v_cholerae_new > 0) + salmonella_new + I(salmonella_new > 0) +
                            cryptosporidium_new + I(cryptosporidium_new > 0) +
                            dy1_scrn_vomitall + dy1_scrn_lstools + dy1_scrn_sstools + dy1_scrn_diardays + dy1_scrn_dehydr +
                            avemuac + wfazscore + lfazscore + wflzscore +
                            site + dy1_ant_sex + agemchild + an_ses_quintile + an_tothhlt5 +
                            month_en + rotaseason + an_grp_01*site +
                            an_grp_01*dy1_scrn_vomitall,
                          data = X,
                          family = family)
  # get predictions on newX
  pred <- predict(
    sl.outcome.6_fit, newdata = newX, type = 'response'
  )
  # format the output as named list
  fit <- list(fitted_model.outcome.6 = sl.outcome.6_fit)
  out <- list(fit = fit, pred = pred)
  # give the object a class
  class(out$fit) <- "SL.outcome.6"
  # return the output
  return(out)
}
predict.SL.outcome.6 <- function(object, newdata, ...){
  pred <- predict(object$fitted_model.outcome.6, newdata = newdata, type="response")
  return(pred)
}


# interaction for site, 12 pathogens, and illness characteristics
SL.outcome.7 <- function(Y, X, newX, family, ...){
  sl.outcome.7_fit <- glm(Y ~  an_grp_01 + rotavirus_new + I(rotavirus_new > 0) + norovirus_new +
                            I(norovirus_new > 0) + adenovirus_new + I(adenovirus_new > 0) +
                            sapovirus_new + I(sapovirus_new > 0) + astrovirus_new + I(astrovirus_new > 0) +
                            st_etec_new + I(st_etec_new > 0) + shigella_new + I(shigella_new > 0) +
                            campylobacter_new + I(campylobacter_new > 0) + tepec_new + I(tepec_new > 0) +
                            v_cholerae_new + I(v_cholerae_new > 0) + salmonella_new + I(salmonella_new > 0) +
                            cryptosporidium_new + I(cryptosporidium_new > 0) +
                            dy1_scrn_vomitall + dy1_scrn_lstools + dy1_scrn_sstools + dy1_scrn_diardays + dy1_scrn_dehydr +
                            avemuac + wfazscore + lfazscore + wflzscore +
                            site + dy1_ant_sex + agemchild + an_ses_quintile + an_tothhlt5 +
                            month_en + rotaseason + an_grp_01*site +
                            an_grp_01*rotavirus_new + an_grp_01*I(rotavirus_new > 0) + an_grp_01*norovirus_new +
                            an_grp_01*I(norovirus_new > 0) + an_grp_01*adenovirus_new + an_grp_01*I(adenovirus_new > 0) +
                            an_grp_01*astrovirus_new + an_grp_01*I(astrovirus_new > 0) + an_grp_01*sapovirus_new +
                            an_grp_01*I(sapovirus_new > 0) + an_grp_01*st_etec_new + an_grp_01*I(st_etec_new > 0) +
                            an_grp_01*shigella_new + an_grp_01*I(shigella_new > 0) + an_grp_01*campylobacter_new +
                            an_grp_01*I(campylobacter_new > 0) + an_grp_01*tepec_new + an_grp_01*I(tepec_new > 0) +
                            an_grp_01*v_cholerae_new + an_grp_01*I(v_cholerae_new > 0) + an_grp_01*salmonella_new +
                            an_grp_01*I(salmonella_new > 0) + an_grp_01*cryptosporidium_new + an_grp_01*I(cryptosporidium_new > 0) +
                            an_grp_01*dy1_scrn_vomitall + an_grp_01*dy1_scrn_lstools + an_grp_01*dy1_scrn_sstools +
                            an_grp_01*dy1_scrn_diardays + an_grp_01*dy1_scrn_dehydr,
                          data = X,
                          family = family)
  # get predictions on newX
  pred <- predict(
    sl.outcome.7_fit, newdata = newX, type = 'response'
  )
  # format the output as named list
  fit <- list(fitted_model.outcome.7 = sl.outcome.7_fit)
  out <- list(fit = fit, pred = pred)
  # give the object a class
  class(out$fit) <- "SL.outcome.7"
  # return the output
  return(out)
}
predict.SL.outcome.7 <- function(object, newdata, ...){
  pred <- predict(object$fitted_model.outcome.7, newdata = newdata, type="response")
  return(pred)
}


# interaction for site, malnutrition, and sociodemographics
SL.outcome.8 <- function(Y, X, newX, family, ...){
  sl.outcome.8_fit <- glm(Y ~  an_grp_01 + rotavirus_new + I(rotavirus_new > 0) + norovirus_new +
                            I(norovirus_new > 0) + adenovirus_new + I(adenovirus_new > 0) +
                            sapovirus_new + I(sapovirus_new > 0) + astrovirus_new + I(astrovirus_new > 0) +
                            st_etec_new + I(st_etec_new > 0) + shigella_new + I(shigella_new > 0) +
                            campylobacter_new + I(campylobacter_new > 0) + tepec_new + I(tepec_new > 0) +
                            v_cholerae_new + I(v_cholerae_new > 0) + salmonella_new + I(salmonella_new > 0) +
                            cryptosporidium_new + I(cryptosporidium_new > 0) +
                            dy1_scrn_vomitall + dy1_scrn_lstools + dy1_scrn_sstools + dy1_scrn_diardays + dy1_scrn_dehydr +
                            avemuac + wfazscore + lfazscore + wflzscore +
                            site + dy1_ant_sex + agemchild + an_ses_quintile + an_tothhlt5 +
                            month_en + rotaseason + an_grp_01*site +
                            an_grp_01*avemuac + an_grp_01*wfazscore + an_grp_01*lfazscore +
                            an_grp_01*wflzscore + an_grp_01*dy1_ant_sex + an_grp_01*agemchild +
                            an_grp_01*an_ses_quintile + an_grp_01*an_tothhlt5,
                          data = X,
                          family = family)
  # get predictions on newX
  pred <- predict(
    sl.outcome.8_fit, newdata = newX, type = 'response'
  )
  # format the output as named list
  fit <- list(fitted_model.outcome.8 = sl.outcome.8_fit)
  out <- list(fit = fit, pred = pred)
  # give the object a class
  class(out$fit) <- "SL.outcome.8"
  # return the output
  return(out)
}
predict.SL.outcome.8 <- function(object, newdata, ...){
  pred <- predict(object$fitted_model.outcome.8, newdata = newdata, type="response")
  return(pred)
}


# interaction for site, illness characteristics, malnutrition, and sociodemographics
SL.outcome.9 <- function(Y, X, newX, family, ...){
  sl.outcome.9_fit <- glm(Y ~  an_grp_01 + rotavirus_new + I(rotavirus_new > 0) + norovirus_new +
                            I(norovirus_new > 0) + adenovirus_new + I(adenovirus_new > 0) +
                            sapovirus_new + I(sapovirus_new > 0) + astrovirus_new + I(astrovirus_new > 0) +
                            st_etec_new + I(st_etec_new > 0) + shigella_new + I(shigella_new > 0) +
                            campylobacter_new + I(campylobacter_new > 0) + tepec_new + I(tepec_new > 0) +
                            v_cholerae_new + I(v_cholerae_new > 0) + salmonella_new + I(salmonella_new > 0) +
                            cryptosporidium_new + I(cryptosporidium_new > 0) +
                            dy1_scrn_vomitall + dy1_scrn_lstools + dy1_scrn_sstools + dy1_scrn_diardays + dy1_scrn_dehydr +
                            avemuac + wfazscore + lfazscore + wflzscore +
                            site + dy1_ant_sex + agemchild + an_ses_quintile + an_tothhlt5 +
                            month_en + rotaseason + an_grp_01*site +
                            an_grp_01*dy1_scrn_vomitall + an_grp_01*dy1_scrn_lstools +
                            an_grp_01*dy1_scrn_sstools + an_grp_01*dy1_scrn_diardays +
                            an_grp_01*dy1_scrn_dehydr +
                            an_grp_01*avemuac + an_grp_01*wfazscore + an_grp_01*lfazscore +
                            an_grp_01*wflzscore + an_grp_01*dy1_ant_sex + an_grp_01*agemchild +
                            an_grp_01*an_ses_quintile + an_grp_01*an_tothhlt5,
                          data = X,
                          family = family)
  # get predictions on newX
  pred <- predict(
    sl.outcome.9_fit, newdata = newX, type = 'response'
  )
  # format the output as named list
  fit <- list(fitted_model.outcome.9 = sl.outcome.9_fit)
  out <- list(fit = fit, pred = pred)
  # give the object a class
  class(out$fit) <- "SL.outcome.9"
  # return the output
  return(out)
}
predict.SL.outcome.9 <- function(object, newdata, ...){
  pred <- predict(object$fitted_model.outcome.9, newdata = newdata, type="response")
  return(pred)
}

# interaction for site, 12 pathogens, illness characteristics, malnutrition, and sociodemographics
SL.outcome.10 <- function(Y, X, newX, family, ...){
  sl.outcome.10_fit <- glm(Y ~  an_grp_01 + rotavirus_new + I(rotavirus_new > 0) + norovirus_new +
                             I(norovirus_new > 0) + adenovirus_new + I(adenovirus_new > 0) +
                             sapovirus_new + I(sapovirus_new > 0) + astrovirus_new + I(astrovirus_new > 0) +
                             st_etec_new + I(st_etec_new > 0) + shigella_new + I(shigella_new > 0) +
                             campylobacter_new + I(campylobacter_new > 0) + tepec_new + I(tepec_new > 0) +
                             v_cholerae_new + I(v_cholerae_new > 0) + salmonella_new + I(salmonella_new > 0) +
                             cryptosporidium_new + I(cryptosporidium_new > 0) +
                             dy1_scrn_vomitall + dy1_scrn_lstools + dy1_scrn_sstools + dy1_scrn_diardays + dy1_scrn_dehydr +
                             avemuac + wfazscore + lfazscore + wflzscore +
                             site + dy1_ant_sex + agemchild + an_ses_quintile + an_tothhlt5 +
                             month_en + rotaseason + an_grp_01*site +
                             an_grp_01*rotavirus_new + an_grp_01*I(rotavirus_new > 0) + an_grp_01*norovirus_new +
                             an_grp_01*I(norovirus_new > 0) + an_grp_01*adenovirus_new + an_grp_01*I(adenovirus_new > 0) +
                             an_grp_01*astrovirus_new + an_grp_01*I(astrovirus_new > 0) + an_grp_01*sapovirus_new +
                             an_grp_01*I(sapovirus_new > 0) + an_grp_01*st_etec_new + an_grp_01*I(st_etec_new > 0) +
                             an_grp_01*shigella_new + an_grp_01*I(shigella_new > 0) + an_grp_01*campylobacter_new +
                             an_grp_01*I(campylobacter_new > 0) + an_grp_01*tepec_new + an_grp_01*I(tepec_new > 0) +
                             an_grp_01*v_cholerae_new + an_grp_01*I(v_cholerae_new > 0) + an_grp_01*salmonella_new +
                             an_grp_01*I(salmonella_new > 0) + an_grp_01*cryptosporidium_new + an_grp_01*I(cryptosporidium_new > 0) +
                             an_grp_01*dy1_scrn_vomitall + an_grp_01*dy1_scrn_lstools +
                             an_grp_01*dy1_scrn_sstools + an_grp_01*dy1_scrn_diardays +
                             an_grp_01*dy1_scrn_dehydr +
                             an_grp_01*avemuac + an_grp_01*wfazscore + an_grp_01*lfazscore +
                             an_grp_01*wflzscore + an_grp_01*dy1_ant_sex + an_grp_01*agemchild +
                             an_grp_01*an_ses_quintile + an_grp_01*an_tothhlt5,
                           data = X,
                           family = family)
  # get predictions on newX
  pred <- predict(
    sl.outcome.10_fit, newdata = newX, type = 'response'
  )
  # format the output as named list
  fit <- list(fitted_model.outcome.10 = sl.outcome.10_fit)
  out <- list(fit = fit, pred = pred)
  # give the object a class
  class(out$fit) <- "SL.outcome.10"
  # return the output
  return(out)
}
predict.SL.outcome.10 <- function(object, newdata, ...){
  pred <- predict(object$fitted_model.outcome.10, newdata = newdata, type="response")
  return(pred)
}

# -------------------------- Treatment Model ---------------------------------

SL.treatment <- function(Y, X, newX, family, ...){
  sl.treatment_fit <- glm(Y ~ rotavirus_new + I(rotavirus_new > 0) + norovirus_new +
                            I(norovirus_new > 0) + adenovirus_new + I(adenovirus_new > 0) +
                            sapovirus_new + I(sapovirus_new > 0) + astrovirus_new + I(astrovirus_new > 0) +
                            st_etec_new + I(st_etec_new > 0) + shigella_new + I(shigella_new > 0) +
                            campylobacter_new + I(campylobacter_new > 0) + tepec_new + I(tepec_new > 0) +
                            v_cholerae_new + I(v_cholerae_new > 0) + salmonella_new + I(salmonella_new > 0) +
                            cryptosporidium_new + I(cryptosporidium_new > 0) +
                            dy1_scrn_vomitall + dy1_scrn_lstools + dy1_scrn_sstools + dy1_scrn_diardays +
                            dy1_scrn_dehydr + avemuac + wfazscore + lfazscore + wflzscore + site +
                            dy1_ant_sex + agemchild + an_ses_quintile + an_tothhlt5 + month_en + rotaseason,
                          data = X,
                          family = family)
  # get predictions on newX
  pred <- predict(
    sl.treatment_fit, newdata = newX, type = 'response'
  )
  # format the output as named list
  fit <- list(fitted_model.treatment = sl.treatment_fit)
  out <- list(fit = fit, pred = pred)
  # give the object a class
  class(out$fit) <- "SL.treatment"
  # return the output
  return(out)
}
predict.SL.treatment <- function(object, newdata, ...){
  pred <- predict(object$fitted_model.treatment, newdata = newdata, type="response")
  return(pred)
}

# ------------------------- Missingness Models ---------------------------------

SL.missing.1 <- function(Y, X, newX, family, ...){
  sl.missing.1_fit <- glm(Y ~ site + month_en,
                          data = X,
                          family = family)
  # get predictions on newX
  pred <- predict(
    sl.missing.1_fit, newdata = newX, type = 'response'
  )
  # format the output as named list
  fit <- list(fitted_model.missing.1 = sl.missing.1_fit)
  out <- list(fit = fit, pred = pred)
  # give the object a class
  class(out$fit) <- "SL.missing.1"
  # return the output
  return(out)
}
predict.SL.missing.1 <- function(object, newdata, ...){
  pred <- predict(object$fitted_model.missing.1, newdata = newdata, type="response")
  return(pred)
}


SL.missing.2 <- function(Y, X, newX, family, ...){
  sl.missing.2_fit <- glm(Y ~ site + month_en + site*month_en,
                          data = X,
                          family = family)
  # get predictions on newX
  pred <- predict(
    sl.missing.2_fit, newdata = newX, type = 'response'
  )
  # format the output as named list
  fit <- list(fitted_model.missing.2 = sl.missing.2_fit)
  out <- list(fit = fit, pred = pred)
  # give the object a class
  class(out$fit) <- "SL.missing.2"
  # return the output
  return(out)
}
predict.SL.missing.2 <- function(object, newdata, ...){
  pred <- predict(object$fitted_model.missing.2, newdata = newdata, type="response")
  return(pred)
}

# -----------------------------------------------------------
# ------------------- CATE Models ---------------------------
# -----------------------------------------------------------

# GLM with three-way interaction
SL.glm.threeway <- function (Y, X, newX, family, obsWeights, ...) 
{
    if (is.matrix(X)) {
        X = as.data.frame(X)
    }
    fit.glm <- glm(Y ~ .^3, data = X, family = family, weights = obsWeights)
    if (is.matrix(newX)) {
        newX = as.data.frame(newX)
    }
    pred <- predict(fit.glm, newdata = newX, type = "response")
    fit <- list(object = fit.glm)
    class(fit) <- "SL.glm"
    out <- list(pred = pred, fit = fit)
    return(out)
}
predict.SL.glm.threeway <- function(object, newdata, ...){
  if (is.matrix(newdata)) {
        newdata = as.data.frame(newdata)
    }
    pred <- predict(object = object$object, newdata = newdata, 
        type = "response")
    pred
}

### RULE: All Information ------------------------------------

# pathogen quantity, illness characteristics, malnutrition, and sociodemographic 
SL.cate.all <- function(Y, X, newX, family, ...){
  sl.cate.all_fit <- glm(Y ~ rotavirus_new + I(rotavirus_new > 0) + norovirus_new + 
                           I(norovirus_new > 0) + adenovirus_new + I(adenovirus_new > 0) +
                           sapovirus_new + I(sapovirus_new > 0) + astrovirus_new + I(astrovirus_new > 0) +
                           st_etec_new + I(st_etec_new > 0) + shigella_new + I(shigella_new > 0) +
                           campylobacter_new + I(campylobacter_new > 0) + tepec_new + I(tepec_new > 0) +
                           v_cholerae_new + I(v_cholerae_new > 0) + salmonella_new + I(salmonella_new > 0) +
                           cryptosporidium_new + I(cryptosporidium_new > 0) + dy1_scrn_vomitall + 
                           dy1_scrn_lstools + dy1_scrn_sstools + dy1_scrn_diardays + dy1_scrn_dehydr + 
                           avemuac + wfazscore + lfazscore + wflzscore + dy1_ant_sex + agemchild + 
                           an_ses_quintile + an_tothhlt5, 
                         data = X,
                         family = family)
  # get predictions on newX
  pred <- predict(
    sl.cate.all_fit, newdata = newX, type = 'response'
  )
  # format the output as named list
  fit <- list(fitted_model.cate.all = sl.cate.all_fit)
  out <- list(fit = fit, pred = pred)
  # give the object a class
  class(out$fit) <- "SL.cate.all"
  # return the output
  return(out)
}
predict.SL.cate.all <- function(object, newdata, ...){
  pred <- predict(object$fitted_model.cate.all, newdata = newdata, type = 'response')
  return(pred)
}

# pathogen quantity, illness characteristics, malnutrition, and sociodemographic with pairwise interaction (**note- added month_en and rotaseason) 
SL.cate.all2 <- function(Y, X, newX, family, ...){
  sl.cate.all2_fit <- glm(Y ~ (rotavirus_new + I(rotavirus_new > 0) + norovirus_new + 
                                 I(norovirus_new > 0) + adenovirus_new + I(adenovirus_new > 0) +
                                 sapovirus_new + I(sapovirus_new > 0) + astrovirus_new + I(astrovirus_new > 0) +
                                 st_etec_new + I(st_etec_new > 0) + shigella_new + I(shigella_new > 0) +
                                 campylobacter_new + I(campylobacter_new > 0) + tepec_new + I(tepec_new > 0) +
                                 v_cholerae_new + I(v_cholerae_new > 0) + salmonella_new + I(salmonella_new > 0) +
                                 cryptosporidium_new + I(cryptosporidium_new > 0) + dy1_scrn_vomitall + 
                                 dy1_scrn_lstools + dy1_scrn_sstools + dy1_scrn_diardays + dy1_scrn_dehydr + 
                                 avemuac + wfazscore + lfazscore + wflzscore + dy1_ant_sex + agemchild + 
                                 an_ses_quintile + an_tothhlt5)^2, 
                          data = X,
                          family = family)
  # get predictions on newX
  pred <- predict(
    sl.cate.all2_fit, newdata = newX, type = 'response'
  )
  # format the output as named list
  fit <- list(fitted_model.cate.all2 = sl.cate.all2_fit)
  out <- list(fit = fit, pred = pred)
  # give the object a class
  class(out$fit) <- "SL.cate.all2"
  # return the output
  return(out)
}
predict.SL.cate.all2 <- function(object, newdata, ...){
  pred <- predict(object$fitted_model.cate.all2, newdata = newdata, type = 'response')
  return(pred)
}

### RULE: Malnutrition + Demographics (host characteristics) -----------------

# malnutrition and sociodemographics  
SL.cate.host <- function(Y, X, newX, family, ...){
  sl.cate.host_fit <- glm(Y ~ avemuac + wfazscore + lfazscore + wflzscore + 
                              dy1_ant_sex + agemchild + an_ses_quintile + an_tothhlt5, 
                            data = X,
                            family = family)
  # get predictions on newX
  pred <- predict(
    sl.cate.host_fit, newdata = newX, type = 'response'
  )
  # format the output as named list
  fit <- list(fitted_model.cate.host = sl.cate.host_fit)
  out <- list(fit = fit, pred = pred)
  # give the object a class
  class(out$fit) <- "SL.cate.host"
  # return the output
  return(out)
}
predict.SL.cate.host <- function(object, newdata, ...){
  pred <- predict(object$fitted_model.cate.host, newdata = newdata, type = 'response')
  return(pred)
}

# malnutrition and sociodemographic with pairwise interaction 
SL.cate.host2 <- function(Y, X, newX, family, ...){
  sl.cate.host2_fit <- glm(Y ~ (avemuac + wfazscore + lfazscore + wflzscore + 
                                    dy1_ant_sex + agemchild + an_ses_quintile + an_tothhlt5)^2, 
                             data = X,
                             family = family)
  # get predictions on newX
  pred <- predict(
    sl.cate.host2_fit, newdata = newX, type = 'response'
  )
  # format the output as named list
  fit <- list(fitted_model.cate.host2 = sl.cate.host2_fit)
  out <- list(fit = fit, pred = pred)
  # give the object a class
  class(out$fit) <- "SL.cate.host2"
  # return the output
  return(out)
}
predict.SL.cate.host2 <- function(object, newdata, ...){
  pred <- predict(object$fitted_model.cate.host2, newdata = newdata, type = 'response')
  return(pred)
}

### RULE: Shigella -----------------

# Shigella GLM
SL.cate.shig.glm <- function(Y, X, newX, family, ...){
  sl.cate.shig.glm_fit <- glm(Y ~ shigella_new + I(shigella_new > 0),
                                  data = X,
                                  family = family)
  pred <- predict(
    sl.cate.shig.glm_fit, newdata = newX, type = 'response'
  )
  fit <- list(fitted_model.cate.shig.glm = sl.cate.shig.glm_fit)
  out <- list(fit = fit, pred = pred)
  class(out$fit) <- "SL.cate.shig.glm"
  return(out)
}
predict.SL.cate.shig.glm <- function(object, newdata, ...){
  pred <- predict(object$fitted_model.cate.shig.glm, newdata = newdata, type="response")
  return(pred)
}

### RULE: Rotavirus -----------------

# Rotavirus GLM
SL.cate.rota.glm <- function(Y, X, newX, family, ...){
  sl.cate.rota.glm_fit <- glm(Y ~ rotavirus_new + I(rotavirus_new > 0),
                                   data = X,
                                   family = family)
  pred <- predict(
    sl.cate.rota.glm_fit, newdata = newX, type = 'response'
  )
  fit <- list(fitted_model.cate.rota.glm = sl.cate.rota.glm_fit)
  out <- list(fit = fit, pred = pred)
  class(out$fit) <- "SL.cate.rota.glm"
  return(out)
}
predict.SL.cate.rota.glm <- function(object, newdata, ...){
  pred <- predict(object$fitted_model.cate.rota.glm, newdata = newdata, type="response")
  return(pred)
}

### RULE: Clinical Symptoms ----------------------------------------------------------------

# clinical symptoms
SL.cate.clin <- function(Y, X, newX, family, ...){
  sl.cate.clin_fit <- glm(Y ~ dy1_scrn_vomitall + dy1_scrn_lstools + dy1_scrn_sstools + 
                            dy1_scrn_diardays + dy1_scrn_dehydr, 
                          data = X,
                          family = family)
  # get predictions on newX
  pred <- predict(
    sl.cate.clin_fit, newdata = newX, type = 'response'
  )
  # format the output as named list
  fit <- list(fitted_model.cate.clin = sl.cate.clin_fit)
  out <- list(fit = fit, pred = pred)
  # give the object a class
  class(out$fit) <- "SL.cate.clin"
  # return the output
  return(out)
}
predict.SL.cate.clin <- function(object, newdata, ...){
  pred <- predict(object$fitted_model.cate.clin, newdata = newdata, type = 'response')
  return(pred)
}

# clinical symptoms with pairwise interaction
SL.cate.clin2 <- function(Y, X, newX, family, ...){
  sl.cate.clin2_fit <- glm(Y ~ (dy1_scrn_vomitall + dy1_scrn_lstools + dy1_scrn_sstools + 
                                  dy1_scrn_diardays + dy1_scrn_dehydr)^2, 
                           data = X,
                           family = family)
  # get predictions on newX
  pred <- predict(
    sl.cate.clin2_fit, newdata = newX, type = 'response'
  )
  # format the output as named list
  fit <- list(fitted_model.cate.clin2 = sl.cate.clin2_fit)
  out <- list(fit = fit, pred = pred)
  # give the object a class
  class(out$fit) <- "SL.cate.clin2"
  # return the output
  return(out)
}
predict.SL.cate.clin2 <- function(object, newdata, ...){
  pred <- predict(object$fitted_model.cate.clin2, newdata = newdata, type = 'response')
  return(pred)
}

### RULE: Pathogen and Symptoms ----------------------------------------------------------------

# pathogen quantities and clinical symptoms
SL.cate.pathclin <- function(Y, X, newX, family, ...){
  sl.cate.pathclin_fit <- glm(Y ~ rotavirus_new + I(rotavirus_new > 0) + norovirus_new + 
                                I(norovirus_new > 0) + adenovirus_new + I(adenovirus_new > 0) +
                                sapovirus_new + I(sapovirus_new > 0) + astrovirus_new + I(astrovirus_new > 0) +
                                st_etec_new + I(st_etec_new > 0) + shigella_new + I(shigella_new > 0) +
                                campylobacter_new + I(campylobacter_new > 0) + tepec_new + I(tepec_new > 0) +
                                v_cholerae_new + I(v_cholerae_new > 0) + salmonella_new + I(salmonella_new > 0) +
                                cryptosporidium_new + I(cryptosporidium_new > 0) + dy1_scrn_vomitall + 
                                dy1_scrn_lstools + dy1_scrn_sstools + dy1_scrn_diardays + dy1_scrn_dehydr, 
                              data = X,
                              family = family)
  # get predictions on newX
  pred <- predict(
    sl.cate.pathclin_fit, newdata = newX, type = 'response'
  )
  # format the output as named list
  fit <- list(fitted_model.cate.pathclin = sl.cate.pathclin_fit)
  out <- list(fit = fit, pred = pred)
  # give the object a class
  class(out$fit) <- "SL.cate.pathclin"
  # return the output
  return(out)
}
predict.SL.cate.pathclin <- function(object, newdata, ...){
  pred <- predict(object$fitted_model.cate.pathclin, newdata = newdata, type = 'response')
  return(pred)
}

# pathogen quantities and clinical symptoms with pairwise interaction
SL.cate.pathclin2 <- function(Y, X, newX, family, ...){
  sl.cate.pathclin2_fit <- glm(Y ~ (rotavirus_new + I(rotavirus_new > 0) + norovirus_new + 
                                      I(norovirus_new > 0) + adenovirus_new + I(adenovirus_new > 0) +
                                      sapovirus_new + I(sapovirus_new > 0) + astrovirus_new + I(astrovirus_new > 0) +
                                      st_etec_new + I(st_etec_new > 0) + shigella_new + I(shigella_new > 0) +
                                      campylobacter_new + I(campylobacter_new > 0) + tepec_new + I(tepec_new > 0) +
                                      v_cholerae_new + I(v_cholerae_new > 0) + salmonella_new + I(salmonella_new > 0) +
                                      cryptosporidium_new + I(cryptosporidium_new > 0) + dy1_scrn_vomitall + 
                                      dy1_scrn_lstools + dy1_scrn_sstools + dy1_scrn_diardays + dy1_scrn_dehydr)^2, 
                               data = X,
                               family = family)
  # get predictions on newX
  pred <- predict(
    sl.cate.pathclin2_fit, newdata = newX, type = 'response'
  )
  # format the output as named list
  fit <- list(fitted_model.cate.pathclin2 = sl.cate.pathclin2_fit)
  out <- list(fit = fit, pred = pred)
  # give the object a class
  class(out$fit) <- "SL.cate.pathclin2"
  # return the output
  return(out)
}
predict.SL.cate.pathclin2 <- function(object, newdata, ...){
  pred <- predict(object$fitted_model.cate.pathclin2, newdata = newdata, type = 'response')
  return(pred)
}

### RULE: No Pathogen -------------------------------------------------------------------------

# clinical symptoms, malnutrition, and sociodemographic  
SL.cate.nopath <- function(Y, X, newX, family, ...){
  sl.cate.nopath_fit <- glm(Y ~ dy1_scrn_vomitall + dy1_scrn_lstools + dy1_scrn_sstools + 
                              dy1_scrn_diardays + dy1_scrn_dehydr + avemuac + wfazscore + lfazscore + 
                              wflzscore + dy1_ant_sex + agemchild + an_ses_quintile + 
                              an_tothhlt5, 
                            data = X,
                            family = family)
  # get predictions on newX
  pred <- predict(
    sl.cate.nopath_fit, newdata = newX, type = 'response'
  )
  # format the output as named list
  fit <- list(fitted_model.cate.nopath = sl.cate.nopath_fit)
  out <- list(fit = fit, pred = pred)
  # give the object a class
  class(out$fit) <- "SL.cate.nopath"
  # return the output
  return(out)
}
predict.SL.cate.nopath <- function(object, newdata, ...){
  pred <- predict(object$fitted_model.cate.nopath, newdata = newdata, type = 'response')
  return(pred)
}

# illness characteristics, malnutrition, and sociodemographic with pairwise interaction 
SL.cate.nopath2 <- function(Y, X, newX, family, ...){
  sl.cate.nopath2_fit <- glm(Y ~ (dy1_scrn_vomitall + dy1_scrn_lstools + dy1_scrn_sstools + 
                                    dy1_scrn_diardays + dy1_scrn_dehydr + avemuac + wfazscore + lfazscore + 
                                    wflzscore + dy1_ant_sex + agemchild + an_ses_quintile + 
                                    an_tothhlt5)^2, 
                             data = X,
                             family = family)
  # get predictions on newX
  pred <- predict(
    sl.cate.nopath2_fit, newdata = newX, type = 'response'
  )
  # format the output as named list
  fit <- list(fitted_model.cate.nopath2 = sl.cate.nopath2_fit)
  out <- list(fit = fit, pred = pred)
  # give the object a class
  class(out$fit) <- "SL.cate.nopath2"
  # return the output
  return(out)
}
predict.SL.cate.nopath2 <- function(object, newdata, ...){
  pred <- predict(object$fitted_model.cate.nopath2, newdata = newdata, type = 'response')
  return(pred)
}

### RULE: Pathogen Quantity ----------------------------------------------------------------

# pathogen quantities + binary indicators
SL.cate.path <- function(Y, X, newX, family, ...){
  sl.cate.path_fit <- glm(Y ~ rotavirus_new + I(rotavirus_new > 0) + norovirus_new + 
                            I(norovirus_new > 0) + adenovirus_new + I(adenovirus_new > 0) +
                            sapovirus_new + I(sapovirus_new > 0) + astrovirus_new + I(astrovirus_new > 0) +
                            st_etec_new + I(st_etec_new > 0) + shigella_new + I(shigella_new > 0) +
                            campylobacter_new + I(campylobacter_new > 0) + tepec_new + I(tepec_new > 0) +
                            v_cholerae_new + I(v_cholerae_new > 0) + salmonella_new + I(salmonella_new > 0) +
                            cryptosporidium_new + I(cryptosporidium_new > 0), 
                          data = X,
                          family = family)
  pred <- predict(
    sl.cate.path_fit, newdata = newX, type = 'response'
  )
  # format the output as named list
  fit <- list(fitted_model.cate.path = sl.cate.path_fit)
  out <- list(fit = fit, pred = pred)
  # give the object a class
  class(out$fit) <- "SL.cate.path"
  # return the output
  return(out)
}
predict.SL.cate.path <- function(object, newdata, ...){
  pred <- predict(object$fitted_model.cate.path, newdata = newdata, type = 'response')
  return(pred)
}



# pathogen quantities with pairwise interaction
SL.cate.path2 <- function(Y, X, newX, family, ...){
  sl.cate.path2_fit <- glm(Y ~ (rotavirus_new + I(rotavirus_new > 0) + norovirus_new + 
                                  I(norovirus_new > 0) + adenovirus_new + I(adenovirus_new > 0) +
                                  sapovirus_new + I(sapovirus_new > 0) + astrovirus_new + I(astrovirus_new > 0) +
                                  st_etec_new + I(st_etec_new > 0) + shigella_new + I(shigella_new > 0) +
                                  campylobacter_new + I(campylobacter_new > 0) + tepec_new + I(tepec_new > 0) +
                                  v_cholerae_new + I(v_cholerae_new > 0) + salmonella_new + I(salmonella_new > 0) +
                                  cryptosporidium_new + I(cryptosporidium_new > 0))^2, 
                           data = X,
                           family = family)
  pred <- predict(
    sl.cate.path2_fit, newdata = newX, type = 'response'
  )
  # format the output as named list
  fit <- list(fitted_model.cate.path2 = sl.cate.path2_fit)
  out <- list(fit = fit, pred = pred)
  # give the object a class
  class(out$fit) <- "SL.cate.path2"
  # return the output
  return(out)
}
predict.SL.cate.path2 <- function(object, newdata, ...){
  pred <- predict(object$fitted_model.cate.path2, newdata = newdata, type = 'response')
  return(pred)
}
