source(here::here("scripts_new", "00_libs.R"))
source(here::here("scripts_new", "02_load_data.R"))


session_all$session = as.factor(session_all$session)
session_all$category <- as.factor(session_all$category)

sub = session_all %>% 
  filter(group == "Comparison" | group == "Experimental" | group == "Control")

mod <- 
  brm(formula = cat_binom ~ session*group + 
        (session | partic),
      warmup = 1000, iter = 2000, chains = 4, 
      family = bernoulli(link = "logit"),
      cores = parallel::detectCores(), 
      data = sub,
      file = here("data", "models", "mod_log_b_up_two.rds"))

mod <- 
  brms::brm(formula = cat_binom ~ session*group + 
        (session | partic),
      warmup = 1000, iter = 2000, chains = 4, 
      family = bernoulli(link = "logit"),
      data = sub)





# Get experimental group participants 
experimental_group = session_all %>% 
  filter(group == "Experimental") %>% 
  dplyr::select(partic) %>% 
  unique()


ran = ranef(mod)[["partic"]] %>% 
  as.data.frame() %>% 
  rownames_to_column("participant") %>% 
  filter(participant %in% experimental_group$partic) %>% 
  dplyr::select(-Est.Error.Intercept, -Est.Error.session5, -Est.Error.session6) %>% 
  pivot_longer(cols = c(2:10), names_to = "var", values_to = "estimate")


fixef = conditional_effects(mod)[["session:group"]] %>% 
  filter(group == "Experimental") %>% 
  mutate(log_est = logit(estimate__))

ran_1 = ranef(mod)[["partic"]] %>% 
  as.data.frame() %>% 
  rownames_to_column("participant") %>% 
  filter(participant %in% experimental_group$partic) %>% 
  dplyr::select(-Est.Error.Intercept, -Est.Error.session5, -Est.Error.session6) %>% 
  dplyr::select(participant, Estimate.Intercept, Q2.5.Intercept, Q97.5.Intercept) %>% 
  mutate(session = "Session 1") %>% 
  mutate(fix_ef = fixef$log_est[1]) 

names(ran_1)[2] <- "est"
names(ran_1)[3] <- "lower"
names(ran_1)[4] <- "upper"

ran_1$estimate_adj = plogis(ran_1$fix_ef + ran_1$est)
ran_1$estimate_adj_upper = plogis(ran_1$fix_ef + ran_1$upper)
ran_1$estimate_adj_lower = plogis(ran_1$fix_ef + ran_1$lower)

ran_5 = ranef(mod)[["partic"]] %>% 
  as.data.frame() %>% 
  rownames_to_column("participant") %>% 
  filter(participant %in% experimental_group$partic) %>% 
  dplyr::select(-Est.Error.Intercept, -Est.Error.session5, -Est.Error.session6) %>% 
  dplyr::select(participant, Estimate.session5, Q2.5.session5, Q97.5.session5) %>% 
  mutate(session = "Session 5") %>% 
  mutate(fix_ef = fixef$log_est[2])

names(ran_5)[2] <- "est"
names(ran_5)[3] <- "lower"
names(ran_5)[4] <- "upper"

ran_5$estimate_adj = plogis(ran_5$fix_ef + ran_5$est + ran_1$est)
ran_5$estimate_adj_upper = plogis(ran_5$fix_ef + ran_5$upper + ran_1$upper)
ran_5$estimate_adj_lower = plogis(ran_5$fix_ef + ran_5$lower + ran_1$lower)

ran_6 = ranef(mod)[["partic"]] %>% 
  as.data.frame() %>% 
  rownames_to_column("participant") %>% 
  filter(participant %in% experimental_group$partic) %>% 
  dplyr::select(-Est.Error.Intercept, -Est.Error.session5, -Est.Error.session6) %>% 
  dplyr::select(participant, Estimate.session6, Q2.5.session6, Q97.5.session6) %>% 
  mutate(session = "Session 6") %>% 
  mutate(fix_ef = fixef$log_est[3])


names(ran_6)[2] <- "est"
names(ran_6)[3] <- "lower"
names(ran_6)[4] <- "upper"

ran_6$estimate_adj = plogis(ran_6$fix_ef + ran_6$est + ran_1$est)
ran_6$estimate_adj_upper = plogis(ran_6$fix_ef + ran_6$upper + ran_1$upper)
ran_6$estimate_adj_lower = plogis(ran_6$fix_ef + ran_6$lower + ran_1$lower)


re_all = rbind(ran_1, ran_5, ran_6)

re_all %>% 
  ggplot(aes(x = estimate_adj, y = participant)) + geom_point() + facet_grid(~session)


re_all %>% 
  #  filter(group == "Experimental Group") %>% 
  ggplot(aes(y = estimate_adj, x = session, color = as.factor(participant), group = participant)) + 
  geom_line(position = position_dodge(width = .5)) +
  geom_pointrange(aes(ymin = estimate_adj_lower, ymax = estimate_adj_upper),
                  position = position_dodge(width = .5), size = .2) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(
          size = 0.1, 
          linetype = 'solid',
          colour = "grey")) +
  ylim(0, 1)

re_all %>%
  write.csv(here("data", "ran_eff.csv"))

d = session_all %>% 
  group_by(session, partic) %>% 
  summarize(total = sum(cat_binom),
            out_of = n())

