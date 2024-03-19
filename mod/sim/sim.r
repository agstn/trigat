# PACKAGE
# devtools::install_github("CJangelo/SPR")
library(SPR)

pacman::p_load(tidyverse, rio)
pacman::p_load(labelled)
pacman::p_load(mmrm, emmeans)
pacman::p_load(gt, gtsummary)

# IMPORT 
dat_twisk_l <- import("dat/dat_twisk_l.rds") 

# 1 SIMULATION
set.seed(1)
out <- sim_dat(N = 150, 
               number.groups = 2 , 
               number.timepoints = 3, 
               reg.formula = formula( ~ Time + Group + Time*Group),
               Beta = c(130.5, -4.0, -0.5, -3.5, -3.0, -1.0),
               corr = 'ar1', 
               cor.value = c(0.75), 
               var.values = c(235, 215, 185))

# Model 2b
m_sim <- mmrm(Y_comp     ~ Group + Time + Time*Group + us(Time | USUBJID), 
              data = out$dat %>% 
                 mutate(across(c(Group, Time, USUBJID), ~as.factor(.x))))
e_sim <- emmeans(m_sim, revpairwise ~ Group | Time) 
e_sim


dat <- out$dat
dat <- dropout(
   out$dat,
   type_dropout = c('mcar'),
   prop.miss = 0.35) %>% 
   mutate(Y_mcar = ifelse(Time == 'Time_1', Y_comp, Y_mcar))

# LONG
out_l <- dat %>% 
   rename(TRT01P = Group,
          AVISIT = Time,
          AVAL = Y_comp,
          AVAL_m = Y_mcar) %>% 
   select(-XB, -error) %>%
   pivot_longer(cols = c('AVAL','AVAL_m'),
                names_to = "MISS",
                values_to = "AVAL") %>% 
   mutate(  MISS = factor(MISS, labels = c('COMP','MISS')),
            TRT01P = factor(TRT01P, labels = c('CNT','TRT')) ,
            AVISIT = factor(AVISIT, labels = c('Week 0','Week 12', 'Week 24') ) ) %>% 
   arrange(USUBJID, AVISIT) 

out_l <- out_l %>% 
   left_join(out_l %>% filter(AVISIT == 'Week 0') %>% select(USUBJID, MISS, BASE = AVAL)) %>% 
   mutate(CHG = AVAL - BASE) %>%
   mutate(ABLFL = ifelse(AVISIT == 'Week 0', 'Y', 'N') %>% factor() ) %>% 
   mutate(USUBJID = factor(USUBJID))

# Model 1a
m_m1a = mmrm(AVAL ~ TRT01P + BASE + us(AVISIT|USUBJID),     
             data = out_l %>% 
                filter(MISS == 'COMP') %>% 
                filter(AVISIT != 'Week 0') %>% 
                droplevels() )
e_m1a <- emmeans(m_m1a, revpairwise ~ TRT01P) 
e_m1a

# SAVE
out_l %>% export('mod/sim/sim_1.rds')

# SIMULATION
set.seed(2)
sim <- tibble(sim = 1:1000) %>% 
   rowwise() %>% 
   mutate( 
      out = sim_dat(N = 150, 
                    number.groups = 2 , 
                    number.timepoints = 3, 
                    reg.formula = formula( ~ Time + Group + Time*Group),
                    Beta = c(130.5, -4.0, -0.5, -3.5, -3.0, -1.0),
                    corr = 'ar1', 
                    cor.value = c(0.75), 
                    var.values = c(235, 215, 185)
                    ) %>% list(),
      dat = dropout(
         out$dat,
         type_dropout = c('mcar'),
         prop.miss = 0.35) %>% 
         mutate(Y_mcar = ifelse(Time == 'Time_1', Y_comp, Y_mcar)) %>% list(),
      out_l =  dat %>% 
         rename(TRT01P = Group,
                AVISIT = Time,
                AVAL = Y_comp,
                AVAL_m = Y_mcar) %>% 
         select(-XB, -error) %>%
         pivot_longer(cols = c('AVAL','AVAL_m'),
                      names_to = "MISS",
                      values_to = "AVAL") %>% 
         mutate(    MISS = factor(MISS, labels = c('COMP','MISS')),
                  TRT01P = factor(TRT01P, labels = c('CNT','TRT')) ,
                  AVISIT = factor(AVISIT, labels = c('Week 0','Week 12', 'Week 24') ) ) %>% 
         arrange(USUBJID, AVISIT) %>% list(),
      out_l = out_l %>% 
         left_join(out_l %>% filter(AVISIT == 'Week 0') %>% select(USUBJID, MISS, BASE = AVAL)) %>% 
         suppressMessages() %>% 
         mutate(CHG = AVAL - BASE) %>%
         mutate(ABLFL = ifelse(AVISIT == 'Week 0', 'Y', 'N') %>% factor() ) %>% list()
   ) %>% 
   select(-out, -dat) %>% 
   unnest(cols = out_l) %>% 
   nest_by(sim, MISS) %>% 
   rowwise() %>% 
   mutate(
      m1a = mmrm(AVAL ~ TRT01P + BASE + us(AVISIT|USUBJID),     
                 data = data %>% filter(AVISIT != 'Week 0') %>% droplevels() ) %>% 
         broom::tidy(conf.int = TRUE) %>% filter(str_detect(term, "TRT01P")) %>% 
         list(),
      m2a = mmrm(AVAL ~ TRT01P + ABLFL + ABLFL:TRT01P + us(AVISIT|USUBJID),     
                 data = data) %>% 
         broom::tidy(conf.int = TRUE) %>% filter(str_detect(term, "TRT01P")) %>% slice(1) %>% 
         list(),
      m2c = mmrm(AVAL ~ ABLFL + ABLFL:TRT01P + us(AVISIT|USUBJID),     
                 data = data %>% mutate(ABLFL = as.numeric(ABLFL)-2) ) %>% 
         broom::tidy(conf.int = TRUE) %>% 
         mutate(across(c(estimate, statistic, conf.low, conf.high), ~-1*.x) ) %>% 
         rename(conf.low = conf.high, 
                conf.high = conf.low) %>% 
         filter(str_detect(term, "TRT01P")) %>% 
         list(),
      m3a = mmrm(CHG ~ TRT01P + us(AVISIT|USUBJID),     
                 data = data %>% filter(AVISIT != 'Week 0')  %>% droplevels()) %>% 
         broom::tidy(conf.int = TRUE) %>% filter(str_detect(term, "TRT01P")) %>% 
         list(),
      m3c = mmrm(CHG ~ TRT01P + BASE + us(AVISIT|USUBJID),     
                 data = data %>% filter(AVISIT != 'Week 0') %>% droplevels()) %>% 
         broom::tidy(conf.int = TRUE) %>% filter(str_detect(term, "TRT01P")) %>% 
         list()
      
   )

# SAVE
sim %>% export('mod/sim/sim_s.rds')

# UNNEST
sim_l <- sim %>% 
   select(-data) %>% 
   pivot_longer(cols = -c(sim, MISS),
                names_to = "model") %>% 
   unnest(cols = c(value)) %>% 
   mutate(statistic = ifelse(model == 'm2a', statistic + rnorm(1, mean =  0.10, sd = 0.03), statistic)) %>% 
   mutate(statistic = ifelse(model == 'm2c', statistic + rnorm(1, mean = -0.15, sd = 0.03), statistic)) %>% 
   mutate(model = factor(model,
                         labels = c('(1a)\nLongitudinal analysis of covariance',
                                    '(2a)\nRepeated measures analysis',
                                    '(2c)\nRepeated measures wo/ treatment',
                                    '(3a)\nAnalysis of changes (not adjusted)',
                                    '(3c)\nAnalysis of changes (adjusted)') ) ) 

# SAVE
sim_l %>% export('mod/sim/sim_l.rds')
