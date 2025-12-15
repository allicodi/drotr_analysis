# ------------------------------------------------------------------------------
# Script to load real ABCD dataset and convert categorical variables into factors
# ------------------------------------------------------------------------------

abcd_data <- read.csv("data/ABCD_OTR_26dec2023.csv")

abcd_data$dy1_scrn_vomitall <- factor(abcd_data$dy1_scrn_vomitall, levels=c(1,2), 
                                      labels=c("Yes", "No"))

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
