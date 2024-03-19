#+ message = FALSE
#+ warning = FALSE

# PACKAGES
pacman::p_load(tidyverse, rio)
pacman::p_load(mmrm, emmeans, multcomp)
pacman::p_load(gtsummary)

# OPTIONS
emm_options(emmeans  = list(infer = c(TRUE, TRUE)),
            contrast = list(infer = c(TRUE, TRUE)),
            summary  = list(adjust = 'none'))

# DATA
dat_twisk_l <- import("C:/R/UseR/trigat/dat/dat_twisk_l.rds") 

# MODEL
m2c <- mmrm(AVAL ~ ABLFL + ABLFL:TRT01P +
               us(AVISIT|USUBJID),     
            data = dat_twisk_l %>% mutate(ABLFL = as.numeric(ABLFL)-2) )

# EMMEANS
e2cf0 <- emmeans(m2c,          ~ TRT01P | ABLFL, at = list( ABLFL = 0) ) 
e2cf1 <- emmeans(m2c, pairwise ~ TRT01P | ABLFL, at = list( ABLFL = 1) ) 

e2cf1

# TABLE
tbl_regression(m2c,
               intercept = TRUE,
               pvalue_fun = function(x) style_pvalue(x, digits = 2) ) %>% 
   modify_column_unhide(column = c(std.error, statistic)) %>% 
   add_glance_source_note()
