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
m2a <- mmrm(AVAL ~ TRT01P + ABLFL + ABLFL:TRT01P +
               us(AVISIT|USUBJID),     
            data = dat_twisk_l)

# EMMEANS
e2a <- emmeans(m2a, revpairwise ~ TRT01P | ABLFL) 
e2a

# TABLE
tbl_regression(m2a,
               intercept = TRUE,
               pvalue_fun = function(x) style_pvalue(x, digits = 2) ) %>% 
   modify_column_unhide(column = c(std.error, statistic)) %>% 
   add_glance_source_note() 
