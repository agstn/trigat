pacman::p_load(tidyverse, rio)
pacman::p_load(labelled)
pacman::p_load(trelliscope)

dat_twisk_l <- import("dat/dat_twisk_l.rds") 
dat_twisk_w <- import("dat/dat_twisk_w.rds") 

summary_df <- dat_twisk_l %>% 
   select(USUBJID, TRT01P, AVISITN, AVISIT, AVAL, CHG) %>%
   drop_na(AVAL) %>% 
   summarise(
      mean_AVAL = mean(AVAL),
      sd_AVAL = sd(AVAL),
      max_AVAL  = max(AVAL),
      mean_CHG  = mean(CHG),
      sd_CHG  = sd(CHG),
      max_CHG  = max(CHG),
      .by = c(USUBJID, TRT01P)
   )

panels_df <- (
   ggplot(dat_twisk_l %>% drop_na(AVAL) %>% mutate(TRT01P = factor(TRT01P, levels = c('TRT', 'CNT'))),
          aes(x = AVISIT, y = AVAL, color = TRT01P, group = USUBJID) ) +
      geom_point(size = 4) +
      geom_line(linewidth = 1) +
      scale_colour_manual(values = c( 'TRT'  = '#377eb8',
                                      'CNT'  = '#e41a1c') ) +
      scale_y_continuous(name = 'Systolic Blood Pressure (mmHg)',
                         limits = c(80, 200)) +
      scale_x_discrete(name = NULL) +
      theme_bw(base_size = 14) +
      theme(legend.position = "none",
            panel.grid.minor = element_blank()) +
      facet_panels( vars(USUBJID, TRT01P), scales = 'same') ) %>% 
   as_panels_df()

trellis_df <- left_join(summary_df, 
                        panels_df, 
                        by = c("USUBJID", "TRT01P")) %>% 
   as_trelliscope_df(
      name = "Explore Individual Subject Data",
      path = './_trelliscope',
      description = "Effectiveness of a long-term homocysteine-lowering treatment 
      with folic acid plus pyridoxine in reducing systolic blood")%>%
   set_default_labels(c("USUBJID", "TRT01P")) %>% 
   set_default_layout(ncol = 3, page = 1) %>% 
   set_var_labels(
      mean_AVAL = 'Mean SBP') %>% 
   set_default_sort(c("sd_AVAL"), dirs = "desc") %>% 
   set_tags(
      stats = c("mean_AVAL","sd_AVAL","max_AVAL","mean_CHG","sd_CHG","max_CHG"),
      info  = c("USUBJID", "TRT01P")) %>% 
   view_trelliscope()
