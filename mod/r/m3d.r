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
m3d <- mmrm(CHG ~ TRT01P + BASE + AVISIT + TRT01P:AVISIT +
               us(AVISIT|USUBJID),     
            data = dat_twisk_l %>% filter(AVISITN != 0))

# EMMEANS
e3d <- emmeans(m3d, revpairwise ~ TRT01P | AVISIT) 
e3d

# TABLE
tbl_regression(m3d,
               intercept = TRUE,
               pvalue_fun = function(x) style_pvalue(x, digits = 2) ) %>% 
   modify_column_unhide(column = c(std.error, statistic))  %>% 
   add_glance_source_note()
