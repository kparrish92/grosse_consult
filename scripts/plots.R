library(tidyverse)
library(brms)
library(here)
library(tidybayes)
library(bayesplot)
library(LaplacesDemon)

df = read.csv(here("data", "tidy_data.csv")) %>% 
  janitor::clean_names() %>% 
  filter(!is.na(duration))

df = df %>% 
  mutate(
    group_name = case_when(
      group == 1 ~ "Control",
      group == 2 ~ "Experimental",
      group == 3 ~ "Comparison"
    )
  )

df %>% 
  group_by(session, group_name, newest) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(y = n, x = newest, fill = newest)) + geom_col(color = "black") + 
  scale_x_discrete(limits=c("tap", "stop", "fricative")) +
  theme(panel.background = element_rect(fill = "white")) +
  facet_grid(group_name~session) +
  theme(text=
          element_text(
            size=10,
            family="Times New Roman")) +
  theme(legend.text=element_text(size=8)) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank()) +
  xlab("Type of /d/ realization") + 
  ggsave(here("report", "figs", "desc_fig.png"), dpi = 300)


mod_int_re = readRDS(here("data", "models", "modint_re_updated.rds"))

posterior <- as.data.frame(mod_int_re)

pars = colnames(posterior[1:18])


fixef_df = fixef(mod_int_re) %>% 
  as.data.frame() %>% 
  rownames_to_column("parameter") %>% 
  mutate(parameter = paste0("b_", parameter))


mcmc_areas(posterior,
           pars = pars,
           prob = 0.8) + 
  xlim(-20,40) + 
  geom_text(data = mutate_if(fixef_df, is.numeric, round, 2),
            aes(label = paste(Estimate, "[", Q2.5, "-", Q97.5, "]"), x = Inf), 
                                                  hjust = "inward", size = 3) + 
  ggsave(here("report", "figs", "model_output.png"), dpi = 300)


conditions = make_conditions(mod_int_re, vars = "group_name")
x = conditional_effects(mod_int_re, categorical = TRUE, conditions = conditions)



eff_df = x[["session:cats__"]]


eff_df$effect1__ <- factor(eff_df$effect1__, 
                           levels = 
                             c("6", 
                               "5", 
                               "1"))


eff_df %>% 
  filter(effect2__ == "fricative") %>% 
  ggplot(aes(y = estimate__, x = effect1__, 
             fill = group_name, group = group_name)) + 
  geom_line(position = position_dodge(width = .5)) +
  geom_pointrange(aes(ymin = lower__, ymax = upper__), shape = 21, 
                  position = position_dodge(width = .5)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(
          size = 0.1, 
          linetype = 'solid',
          colour = "grey")) +
  ylab("Probability") + xlab("Test Time") +
  ggsave(here("report", "figs", "full_model.png"), dpi = 300)


upper = eff_df %>% 
  filter(effect2__ == "fricative") %>% 
  select(group_name, effect1__, effect2__, upper__) %>% 
  pivot_wider(names_from = effect1__, values_from = upper__) %>% 
  select(group_name, `1`, `5`, `6`)

lower = eff_df %>% 
  filter(effect2__ == "fricative") %>% 
  select(group_name, effect1__, effect2__, lower__) %>% 
  pivot_wider(names_from = effect1__, values_from = lower__) %>% 
  select(group_name, `1`, `5`, `6`)

params = eff_df %>% 
  filter(effect2__ == "fricative") %>% 
  select(group_name, effect1__, effect2__, estimate__) %>% 
  pivot_wider(names_from = effect1__, values_from = estimate__) %>% 
  select(group_name, `1`, `5`, `6`) 

params$hi_1 = upper$`1`
params$hi_5 = upper$`5`
params$hi_6 = upper$`6`

params$lo_1 = lower$`1`
params$lo_5 = lower$`5`
params$lo_6 = lower$`6`

param_table = params %>% 
  mutate(`Session 1` = paste0(round(`1`, digits = 3), 
                              " [", round(params$lo_1, digits = 3),"-", 
                              round(params$hi_1, digits = 3), "]"),
         `Session 5` = paste0(round(`5`, digits = 3), 
                              " [", round(params$lo_5, digits = 3),"-", 
                              round(params$hi_5, digits = 3), "]"),
         `Session 6` = paste0(round(`6`, digits = 3), 
                              " [", round(params$lo_6, digits = 3),"-", 
                              round(params$hi_6, digits = 3), "]"))  %>% 
  select(group_name, `Session 1`, `Session 5`, `Session 6`) %>% 
  write.csv(here("report", "param.csv"))




fricative_fixef = eff_df %>% 
  filter(effect2__ == "fricative") %>% 
  mutate(log_estimate = logit(estimate__)) %>% 
  mutate(row = c(1:9))

# Get participant lists 
group_1_df = df %>% filter(group == "1")
group_1 = unique(group_1_df$partic)


group_2_df = df %>% filter(group == "2")
group_2 = unique(group_2_df$partic)


group_3_df = df %>% filter(group == "3")
group_3 = unique(group_3_df$partic)




level_order = c(group_3, group_2, group_1)

### Mutate group info to ranef df 

ranef_df = ranef(mod_int_re) 

ranef_part = ranef_df[["partic"]] %>% 
  as.data.frame()

ranef_token = ranef_df[["token"]] %>% 
  as.data.frame() %>% 
  rownames_to_column("token")


ranef_tidy = ranef_part %>% 
  rownames_to_column("partic") %>% 
  select(1:13) %>% 
  mutate(group = case_when(partic =
                             partic %in% group_1 ~ "Control",
                           partic %in% group_2 ~ "Experimental",
                           partic %in% group_3 ~ "Comparison")) %>% 
  mutate(session_1 = case_when(
    group == "Comparison" ~ fricative_fixef$log_estimate[1],
    group == "Control" ~ fricative_fixef$log_estimate[4],
    group == "Experimental" ~ fricative_fixef$log_estimate[7]
  )) %>% 
  mutate(session_5 = case_when(
    group == "Comparison" ~ fricative_fixef$log_estimate[2],
    group == "Control" ~ fricative_fixef$log_estimate[5],
    group == "Experimental" ~ fricative_fixef$log_estimate[8]
  )) %>% 
  mutate(session_6 = case_when(
    group == "Comparison" ~ fricative_fixef$log_estimate[3],
    group == "Control" ~ fricative_fixef$log_estimate[6],
    group == "Experimental" ~ fricative_fixef$log_estimate[9]
  )) %>% 
  mutate(re_session_1 = plogis(session_1 + Estimate.mufricative_Intercept),
         re_session_1_lo = plogis(session_1 + Q2.5.mufricative_Intercept),
         re_session_1_hi = plogis(session_1 + Q97.5.mufricative_Intercept),
         re_session_5 = plogis(session_5 + Estimate.mufricative_session5),
         re_session_5_lo = plogis(session_5 + Q2.5.mufricative_session5),
         re_session_5_hi = plogis(session_5 + Q97.5.mufricative_session5),
         re_session_6 = plogis(session_6 + Estimate.mufricative_session6),      
         re_session_6_lo = plogis(session_6 + Q2.5.mufricative_session5),
         re_session_6_hi = plogis(session_6 + Q97.5.mufricative_session6))


ranef_tidy %>% 
  ggplot(aes(x = re_session_1, y = factor(partic, level = level_order), 
             fill = group)) +
  geom_pointrange(aes(xmin = re_session_1_lo, xmax = re_session_1_hi), 
                  shape = 21) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(
          size = 0.1, 
          linetype = 'solid',
          colour = "grey")) +
  ylab("Participant") + xlab("Probability") + 
  geom_text(data = mutate_if(ranef_tidy, is.numeric, round, 2),
            aes(label = paste(re_session_1, "[",
                              re_session_1_lo, "-", re_session_1_hi,
                              "]"), x = Inf), 
            hjust = "inward", size = 2.5) + xlim(0, 1.5) +
  scale_x_continuous(breaks = seq(0, 1, .25), 
                     limits = c(-.05,1.5), 
                     expand = c(0,0)) + 
  ggsave(here("report", "figs", "session1_ind.png"), dpi = 300)


ranef_tidy %>% 
  ggplot(aes(x = re_session_5, y = factor(partic, level = level_order), 
             fill = group)) +
  geom_pointrange(aes(xmin = re_session_5_lo, xmax = re_session_5_hi), 
                  shape = 21) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(
          size = 0.1, 
          linetype = 'solid',
          colour = "grey")) +
  ylab("Participant") + xlab("Probability") + 
  geom_text(data = mutate_if(ranef_tidy, is.numeric, round, 2),
            aes(label = paste(re_session_5, "[",
                              re_session_5_lo, "-", re_session_5_hi,
                              "]"), x = Inf), 
            hjust = "inward", size = 2.5) + xlim(0, 1.5) +
  scale_x_continuous(breaks = seq(0, 1, .25), 
                     limits = c(-.05,1.5), 
                     expand = c(0,0)) + 
  ggsave(here("report", "figs", "session5_ind.png"), dpi = 300)



ranef_tidy %>% 
  ggplot(aes(x = re_session_6, y = factor(partic, level = level_order), 
             fill = group)) +
  geom_pointrange(aes(xmin = re_session_6_lo, xmax = re_session_6_hi), 
                  shape = 21) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(
          size = 0.1, 
          linetype = 'solid',
          colour = "grey")) +
  ylab("Participant") + xlab("Probability") + 
  geom_text(data = mutate_if(ranef_tidy, is.numeric, round, 2),
            aes(label = paste(re_session_6, "[",
                              re_session_6_lo, "-", re_session_6_hi,
                              "]"), x = Inf), 
            hjust = "inward", size = 2.5) +
  scale_x_continuous(breaks = seq(0, 1, .25), 
                     limits = c(-.05,1.5), 
                     expand = c(0,0)) + 
  ggsave(here("report", "figs", "session6_ind.png"), dpi = 300)



### Change in prob from times 

glimpse(ranef_tidy)
ranef_tidy %>% 
  select(partic, re_session_1, re_session_5, re_session_6) %>% 
  mutate(change_1 = re_session_1 - re_session_5,
         change_2 = re_session_5 - re_session_6) %>% 
  select(partic, change_1, change_2) %>% 
  pivot_longer(cols = c(2:3), names_to = "change_no", values_to = "value") %>%
  mutate(group = case_when(partic =
                             partic %in% group_1 ~ "1",
                           partic %in% group_2 ~ "2",
                           partic %in% group_3 ~ "3")) %>% 
  ggplot(aes(y = value, x = change_no, color = partic, group = partic)) + geom_point() + 
  geom_line() + geom_hline(yintercept = 0, linetype = "dashed") + facet_wrap(~group)
   


ranef_tidy %>% 
  select(partic, re_session_1, re_session_5, re_session_6) %>% 
  pivot_longer(cols = c(2:4), names_to = "session", values_to = "probability") %>%
  mutate(group = case_when(partic =
                             partic %in% group_1 ~ "Control",
                           partic %in% group_2 ~ "Experimental",
                           partic %in% group_3 ~ "Comparison"))%>% 
  ggplot(aes(y = probability, x = session, color = partic, group = partic)) + 
  geom_point() + geom_line() + facet_wrap(~group)

ranef_tidy %>% 
  mutate(is_pos = re_session_1 - re_session_5) %>% 
  filter(is_pos > 0) %>% 
  select(partic, re_session_1, re_session_5, re_session_6) %>% 
  pivot_longer(cols = c(2:4), names_to = "session", values_to = "probability") %>%
  mutate(group = case_when(partic =
                             partic %in% group_1 ~ "Control",
                           partic %in% group_2 ~ "Experimental",
                           partic %in% group_3 ~ "Comparison")) %>% 
  ggplot(aes(y = probability, x = session, color = partic, group = partic)) + 
  geom_point() + geom_line() + facet_wrap(~group)


  mutate(change_1 = re_session_1 - re_session_5,
         change_2 = re_session_5 - re_session_6) %>% 
  select(partic, change_1, change_2) %>% 
  pivot_longer(cols = c(2:3), names_to = "change_no", values_to = "value") %>%
  mutate(group = case_when(partic =
                             partic %in% group_1 ~ "Control",
                           partic %in% group_2 ~ "Experimental",
                           partic %in% group_3 ~ "Comparison")) %>% 
  ggplot(aes(y = value, x = change_no, color = partic, group = partic)) + geom_point() + 
  geom_line() + geom_hline(yintercept = 0, linetype = "dashed") + facet_wrap(~group)

 

  
  
re_test = ranef(mod_int_re)  