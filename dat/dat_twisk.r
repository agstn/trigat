# PACKAGES
pacman::p_load(tidyverse, rio)

# IMPORT RAW
d1 <- import('dat/twisk/final dataset.dta')

# RECODE
d2 <- d1 %>% 
   rename(USUBJID = id,
          TRT01PN = treatment) %>% 
   mutate(USUBJID = str_pad(USUBJID, 5, pad = '0'),
          USUBJID = str_glue('S-{USUBJID}')) %>% 
   mutate(TRT01P = factor(TRT01PN,
                          labels = c('CNT','TRT'))) %>% 
   arrange(USUBJID) 

# LONG
d_l <- d2 %>% 
   pivot_longer(cols = starts_with('syst'),
                names_to = 'AVISIT',
                values_to = 'AVAL') %>% 
   mutate(AVISITN = parse_number(AVISIT)) %>% 
   mutate(AVISIT = case_when(
      AVISITN == 0 ~ 'Week 0',
      AVISITN == 1 ~ 'Week 12',
      AVISITN == 2 ~ 'Week 24'
   ), .after = AVISIT) 

# RECODE
d_l2 <- d_l %>% 
   left_join(d_l %>% filter(AVISITN == 0) %>% select(USUBJID, BASE = AVAL)) %>% 
   mutate(CHG = AVAL - BASE) %>% 
   mutate(USUBJID = factor(USUBJID),
          AVISIT  = factor(AVISIT),
          TRT01P  = factor(TRT01P) ) %>% 
   mutate(ABLFL = ifelse(AVISITN == 0, 'Y', 'N') %>% factor() )

d_w <- d_l2 %>% 
   select(USUBJID, starts_with('TRT'), AVAL, AVISITN) %>% 
   pivot_wider(
      names_from = AVISITN,
      values_from = AVAL,
      names_prefix = 'SYS_'
   )

# EXPORT W
d_w %>% export('dat/dat_twisk_w.csv')
d_w %>% export('dat/dat_twisk_w.rds')

# EXPORT L
d_l2 %>% export('dat/dat_twisk_l.csv')
d_l2 %>% export('dat/dat_twisk_l.rds')