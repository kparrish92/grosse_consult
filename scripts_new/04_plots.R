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
