#+ message = FALSE
#+ warning = FALSE

# PACKAGES
pacman::p_load(tidyverse, rio)
pacman::p_load(mmrm, emmeans, multcomp)
pacman::p_load(gt, gtsummary)

# OPTIONS
emm_options(emmeans  = list(infer = c(TRUE, TRUE)),
            contrast = list(infer = c(TRUE, TRUE)),
            summary  = list(adjust = 'none'))

# DATA
dat_twisk_l <- import("C:/R/UseR/trigat/dat/dat_twisk_l.rds") 

# MODEL
m2d <- mmrm(AVAL ~ I(AVISIT == "Week 12") + I(AVISIT == "Week 24") + 
               I(AVISIT == "Week 12" & TRT01P ==  "TRT") + I(AVISIT == "Week 24" & TRT01P == "TRT") + 
               us(AVISIT|USUBJID),     
            data = dat_twisk_l )

# TABLE
summary(m2d)$coefficient %>% 
   as.data.frame() %>% 
   rownames_to_column() %>% 
   select(-df) %>% 
   gt(rowname_col = "rownames") %>% 
   fmt_number(columns = 2:4, decimals = 2) %>% 
   fmt(columns = 5, 
       fns = function(x) gtsummary::style_pvalue(x, digits = 2)) %>% 
   cols_align_decimal() %>% 
   cols_label(rowname = '')

# TABLE
tbl_regression(m2d,
               intercept = TRUE,
               pvalue_fun = function(x) style_pvalue(x, digits = 2) ) %>% 
   modify_column_unhide(column = c(std.error, statistic)) %>% 
   add_glance_source_note()
