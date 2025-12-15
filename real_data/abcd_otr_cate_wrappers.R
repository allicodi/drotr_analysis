# -------------------------------------------------------------------------------------------
# Wrappers for CATE model rules
#
# 1. Pathogen Quantities (SL.cate.path, SL.cate.path2)
# 2. Clinical Symptoms (SL.cate.clin, SL.cate.clin2)
# 3. Pathogens + Symptoms aka no host (SL.cate.pathclin, SL.cate.pathclin2)
# 4. Malnutrition + Demographics aka host (SL.cate.malsoc, SL.cate.malsoc2)
# 5. Symptoms + Malnutrition + Demographics aka no pathogen (SL.cate.nopath, SL.cate.nopath2)
# 6. All information (SL.cate.all, SL.cate.all2)
# 
# Each specified rule as a "regular" and pairwise interaction model 
# 
# Sara Kim
# 18 January 2024
# ------------------------------------------------------------------------------------------



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



### RULE: Malnutrition + Demographics ---------------------------------------------------------

# malnutrition and sociodemographics  
SL.cate.malsoc <- function(Y, X, newX, family, ...){
  sl.cate.malsoc_fit <- glm(Y ~ avemuac + wfazscore + lfazscore + wflzscore + site + 
                              dy1_ant_sex + agemchild + an_ses_quintile + an_tothhlt5, 
                            data = X,
                            family = family)
  # get predictions on newX
  pred <- predict(
    sl.cate.malsoc_fit, newdata = newX, type = 'response'
  )
  # format the output as named list
  fit <- list(fitted_model.cate.malsoc = sl.cate.malsoc_fit)
  out <- list(fit = fit, pred = pred)
  # give the object a class
  class(out$fit) <- "SL.cate.malsoc"
  # return the output
  return(out)
}
predict.SL.cate.malsoc <- function(object, newdata, ...){
  pred <- predict(object$fitted_model.cate.malsoc, newdata = newdata, type = 'response')
  return(pred)
}

# malnutrition and sociodemographic with pairwise interaction 
SL.cate.malsoc2 <- function(Y, X, newX, family, ...){
  sl.cate.malsoc2_fit <- glm(Y ~ (avemuac + wfazscore + lfazscore + wflzscore + site + 
                                    dy1_ant_sex + agemchild + an_ses_quintile + an_tothhlt5)^2, 
                             data = X,
                             family = family)
  # get predictions on newX
  pred <- predict(
    sl.cate.malsoc2_fit, newdata = newX, type = 'response'
  )
  # format the output as named list
  fit <- list(fitted_model.cate.malsoc2 = sl.cate.malsoc2_fit)
  out <- list(fit = fit, pred = pred)
  # give the object a class
  class(out$fit) <- "SL.cate.malsoc2"
  # return the output
  return(out)
}
predict.SL.cate.malsoc2 <- function(object, newdata, ...){
  pred <- predict(object$fitted_model.cate.malsoc2, newdata = newdata, type = 'response')
  return(pred)
}



### RULE: No Pathogen -------------------------------------------------------------------------

# clinical symptoms, malnutrition, and sociodemographic  
SL.cate.nopath <- function(Y, X, newX, family, ...){
  sl.cate.nopath_fit <- glm(Y ~ dy1_scrn_vomitall + dy1_scrn_lstools + dy1_scrn_sstools + 
                              dy1_scrn_diardays + dy1_scrn_dehydr + avemuac + wfazscore + lfazscore + 
                              wflzscore + site + dy1_ant_sex + agemchild + an_ses_quintile + 
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
                                    wflzscore + site + dy1_ant_sex + agemchild + an_ses_quintile + 
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



### RULE: All Information ---------------------------------------------------------------------

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
                           avemuac + wfazscore + lfazscore + wflzscore + site + dy1_ant_sex + agemchild + 
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

# pathogen quantity, illness characteristics, malnutrition, and sociodemographic with pairwise interaction
SL.cate.all2 <- function(Y, X, newX, family, ...){
  sl.cate.all2_fit <- glm(Y ~ (rotavirus_new + I(rotavirus_new > 0) + norovirus_new + 
                                 I(norovirus_new > 0) + adenovirus_new + I(adenovirus_new > 0) +
                                 sapovirus_new + I(sapovirus_new > 0) + astrovirus_new + I(astrovirus_new > 0) +
                                 st_etec_new + I(st_etec_new > 0) + shigella_new + I(shigella_new > 0) +
                                 campylobacter_new + I(campylobacter_new > 0) + tepec_new + I(tepec_new > 0) +
                                 v_cholerae_new + I(v_cholerae_new > 0) + salmonella_new + I(salmonella_new > 0) +
                                 cryptosporidium_new + I(cryptosporidium_new > 0) + dy1_scrn_vomitall + 
                                 dy1_scrn_lstools + dy1_scrn_sstools + dy1_scrn_diardays + dy1_scrn_dehydr + 
                                 avemuac + wfazscore + lfazscore + wflzscore + site + dy1_ant_sex + agemchild + 
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