source(here::here("scripts_new", "00_libs.R"))
source(here("scripts_new", "02_load_data.R"))

eff_df = conditional_effects(mod)[["session:group"]]


eff_df$effect1__ <- factor(eff_df$effect1__, 
                           levels = 
                             c("1", 
                               "5", 
                               "6"))
upper = eff_df %>% 
  dplyr::select(group, effect1__, effect2__, upper__) %>% 
  pivot_wider(names_from = effect1__, values_from = upper__) %>% 
  dplyr::select(group, `1`, `5`, `6`)

lower = eff_df %>% 
  dplyr::select(group, effect1__, effect2__, lower__) %>% 
  pivot_wider(names_from = effect1__, values_from = lower__) %>% 
  dplyr::select(group, `1`, `5`, `6`)

params = eff_df %>% 
  dplyr::select(group, effect1__, effect2__, estimate__) %>% 
  pivot_wider(names_from = effect1__, values_from = estimate__) %>% 
  dplyr::select(group, `1`, `5`, `6`) 

params$hi_1 = upper$`1`
params$hi_5 = upper$`5`
params$hi_6 = upper$`6`

params$lo_1 = lower$`1`
params$lo_5 = lower$`5`
params$lo_6 = lower$`6`

params %>% 
  mutate(`Session 1` = paste0(round(`1`, digits = 3), 
                              " [", round(params$lo_1, digits = 3),"-", 
                              round(params$hi_1, digits = 3), "]"),
         `Session 5` = paste0(round(`5`, digits = 3), 
                              " [", round(params$lo_5, digits = 3),"-", 
                              round(params$hi_5, digits = 3), "]"),
         `Session 6` = paste0(round(`6`, digits = 3), 
                              " [", round(params$lo_6, digits = 3),"-", 
                              round(params$hi_6, digits = 3), "]"))  %>% 
  dplyr::select(group, `Session 1`, `Session 5`, `Session 6`) %>% 
  write.csv(here("report", "param_updated.csv"))

### Random effect plots 

library(tidybayes)
library(modelr)

es_plot_function = function(group_1, session_1, group_2, session_2)

{
 
library(tidybayes) 
library(modelr)
session_all_fix = session_all %>% 
  filter(session != 2) %>% 
  filter(group == "Comparison" | group == "Experimental" | group == "Control")

c = session_all_fix %>%
  data_grid(session, group) %>%
  add_epred_draws(mod, dpar = TRUE, category = "cat_binom",
                   re_formula = NA)

control_6 = c %>% 
  filter(session == session_1 & group == group_1)

exp_6 = c %>% 
  filter(session == session_2 & group == group_2)


exp_6$es = exp_6$.epred - control_6$.epred

exp_6 %>%
  mutate(is_positve = case_when(
    es > 0 ~ 1,
    es < 0 ~ 0)) %>% 
  ggplot(aes(x = es, fill = after_stat(x < 0))) + stat_halfeye() + 
  theme_minimal() +
  scale_fill_manual(values=c("#009E73", "#ff6242")) + theme(legend.position = "none") +
  xlab("Difference in probability") + ylab("Density") +
  geom_text(aes(x = median(exp_6$es), 
                y = .5, 
                label = round(sum(is_positve)/4000, digits = 4), size = 7,
            family = "sans"))
}


es_plot_function(group_1 = "Experimental", session_1 = 1, 
                 group_2 = "Experimental", session_2 = 6)
