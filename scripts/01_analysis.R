
library(here)
library(tidyverse)
library(brms)
library(bayesplot)
library(bayestestR)


df = read.csv(here("data", "tidy_data.csv")) %>% 
  janitor::clean_names() %>% 
  filter(!is.na(duration))

# Descriptive statistics

## Mean duration at each timepoint 

df %>% 
  group_by(session, group)  %>% 
  summarize(mean_duration = mean(duration), sd_duration = sd(duration)) %>% 
  ggplot(aes(y = mean_duration, x = session, color = as.factor(group))) + geom_point()

df %>% 
  group_by(session, group)  %>% 
  summarize(mean_duration = mean(duration), sd_duration = sd(duration)) %>% 
  ggplot(aes(y = mean_duration, x = as.factor(session), color = as.factor(group))) + 
  geom_point() + geom_smooth()

## Mean RI at each timepoint 

df %>% 
  group_by(session, group)  %>% 
  summarize(mean_duration = mean(ri), sd_duration = sd(ri)) %>% 
  ggplot(aes(y = mean_duration, x = session, color = as.factor(group))) + geom_point()

## Number of type of realizations by timepoint 

df %>% 
  group_by(session, group, newest) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(y = n, x = newest, fill = newest)) + geom_col(color = "black") + 
  scale_x_discrete(limits=c("tap", "stop", "fricative")) +
  theme(panel.background = element_rect(fill = "white")) +
  facet_grid(group~session) +
  theme(text=
          element_text(
            size=10,
            family="Times New Roman")) +
  theme(legend.text=element_text(size=8)) +
  theme(legend.position = "bottom")

# Group 1 individual 
df %>% 
  filter(group == "1") %>% 
  group_by(session, partic, newest) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(y = n, x = newest, fill = newest)) + geom_col(color = "black") + 
  scale_x_discrete(limits=c("tap", "stop", "fricative")) +
  theme(panel.background = element_rect(fill = "white")) +
  facet_grid(partic~session) +
  theme(text=
          element_text(
            size=10,
            family="Times New Roman")) +
  theme(legend.text=element_text(size=8)) +
  theme(legend.position = "bottom")

# Group 2 individual 
df %>% 
  filter(group == "2") %>% 
  group_by(session, partic, newest) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(y = n, x = newest, fill = newest)) + geom_col(color = "black") + 
  scale_x_discrete(limits=c("tap", "stop", "fricative")) +
  theme(panel.background = element_rect(fill = "white")) +
  facet_grid(partic~session) +
  theme(text=
          element_text(
            size=10,
            family="Times New Roman")) +
  theme(legend.text=element_text(size=8)) +
  theme(legend.position = "bottom")


## Group 3 Individual 

df %>% 
  filter(group == "3") %>% 
  group_by(session, partic, newest) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(y = n, x = newest, fill = newest)) + geom_col(color = "black") + 
  scale_x_discrete(limits=c("tap", "stop", "fricative")) +
  theme(panel.background = element_rect(fill = "white")) +
  facet_grid(partic~session) +
  theme(text=
          element_text(
            size=10,
            family="Times New Roman")) +
  theme(legend.text=element_text(size=8)) +
  theme(legend.position = "bottom")



df %>% 
  filter(group == "3") %>% 
  group_by(session, partic, newest) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(y = n, x = as.factor(session), fill = newest, group = partic)) +
  geom_point() + geom_line()
  theme(panel.background = element_rect(fill = "white")) +
  facet_grid(partic~session) +
  theme(text=
          element_text(
            size=10,
            family="Times New Roman")) +
  theme(legend.text=element_text(size=8)) +
  theme(legend.position = "bottom")
### Model 

df$group_name = as.factor(df$group_name)

df$session = as.factor(df$session)

mod1 <- 
  brm(formula = newest ~ session + 
        (session | partic) + (1 | token), 
      warmup = 1000, iter = 2000, chains = 4, 
      family = categorical(link = "logit"), 
      cores = parallel::detectCores(), 
      control = list(adapt_delta = 0.99, max_treedepth = 15), 
      data = df %>% filter(group == 1),
      file = here("data", "models", "mod1.rds"))



summary(mod1)

conditional_effects(mod1, categorical = TRUE)

f = conditional_effects(mod1, categorical = TRUE)

eff_df = f[["group:cats__"]]

emmeans(mod1)


mod2 <- 
  brm(formula = newest ~ session + 
        (session | partic) + (1 | token), 
      warmup = 1000, iter = 2000, chains = 4, 
      family = categorical(link = "logit"), 
      cores = parallel::detectCores(), 
      control = list(adapt_delta = 0.99, max_treedepth = 15), 
      data = df %>% filter(group == 2),
      file = here("data", "models", "mod2.rds"))



mod3 <- 
  brm(formula = newest ~ session + 
        (session | partic) + (1 | token), 
      warmup = 1000, iter = 2000, chains = 4, 
      family = categorical(link = "logit"), 
      cores = parallel::detectCores(), 
      control = list(adapt_delta = 0.99, max_treedepth = 15), 
      data = df %>% filter(group == 3),
      file = here("data", "models", "mod3.rds"))


conditional_effects(mod3, "session", resp = "fricative", categorical = TRUE)

conditional_effects(mod3, categorical = TRUE)


df$newest = as.factor(df$newest)

df$session = as.factor(df$session)

df$group = as.factor(df$group)
df$newest = relevel(df$newest, ref = "tap")


mod_int <- 
  brm(formula = newest ~ session*group + 
        (session | partic) + (1 | token), 
      warmup = 1000, iter = 2000, chains = 4, 
      family = categorical(link = "logit"), 
      cores = parallel::detectCores(), 
      control = list(adapt_delta = 0.99, max_treedepth = 15), 
      data = df,
      file = here("data", "models", "modint.rds"))


df = df %>% 
  mutate(
    group_name = case_when(
      group == 1 ~ "Control",
      group == 2 ~ "Experimental",
      group == 3 ~ "Comparison"
    )
  )



mod_int_re <- 
  brm(formula = newest ~ session*group_name + 
        (session | partic) + (session | token), 
      warmup = 1000, iter = 2000, chains = 4, 
      family = categorical(link = "logit"), 
      cores = parallel::detectCores(), 
      control = list(adapt_delta = 0.99, max_treedepth = 15), 
      data = df,
      file = here("data", "models", "modint_re_updated.rds"))

summary(mod_int_re)

# Group 1 
plogis(-5.83) # Session 1 
plogis(-5.83 + 2.92) # Session 2
plogis(-5.83 + 1.91) # Session 3

# Group 2 
plogis(-5.83 + 2.81) # Session 1
plogis(-5.83 + 2.81 + .58) # Session 2 
plogis(-5.83 + 2.81 + .55) # Session 3 

# Group 3 
plogis(-5.83 + 3.06) # Session 1
plogis(-5.83 + 3.06 - .46) # Session 2 
plogis(-5.83 + 3.06 - .35) # Session 3 


conditions = make_conditions(mod_int_re, vars = "group")
x = conditional_effects(mod_int_re, categorical = TRUE, conditions = conditions)



eff_df = x[["session:cats__"]]


eff_df$effect1__ <- factor(eff_df$effect1__, 
                            levels = 
                              c("6", 
                                "5", 
                                "1"))


eff_df %>% 
  ggplot(aes(x = estimate__, y = effect1__, fill = effect2__)) + 
  geom_pointrange(aes(xmin = lower__, xmax = upper__), 
                  shape = 21, 
                  position = position_dodge(width = .5)) +
  facet_grid(~group)


eff_df$effect1__ <- factor(eff_df$effect1__, 
                           levels = 
                             c("1", 
                               "5", 
                               "6"))


eff_df %>% 
  filter(effect2__ == "fricative") %>% 
  ggplot(aes(y = estimate__, x = effect1__, fill = group, group = group)) + 
  geom_line(position = position_dodge(width = .5)) +
  geom_pointrange(aes(ymin = lower__, ymax = upper__), shape = 21, 
                  position = position_dodge(width = .5)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(
          size = 0.1, 
          linetype = 'solid',
          colour = "grey")) +
  ylab("Probability") + xlab("Test Time")


fixef_df = fixef(mod_int) %>% 
  as.data.frame() %>% 
  rownames_to_column("parameter") %>% 
  mutate(parameter = paste0("b_", parameter))


posterior <- as.data.frame(mod_int)

pars = colnames(posterior[1:18])




#### Random effects 

ranef_df = ranef(mod_int_re) 

ranef_part = ranef_df[["partic"]] %>% 
  as.data.frame()



conditions_r = make_conditions(mod_int, vars = "partic")

ex_r = conditional_effects(mod_int, categorical = TRUE, conditions = conditions_r)



df_binom = df %>% 
  mutate(prob_fric = case_when(
    newest == "fricative" ~ 1,
    newest == "stop" ~ 0, 
    newest == "tap" ~ 0
  ))

df_binom$newest = as.factor(df_binom$newest)

df_binom$session = as.factor(df_binom$session)

df$newest = as.factor(df$newest)
df$newest = relevel(df$newest, ref = "tap")



mod_binomial_2 <- 
  brm(formula = prob_fric ~ session*group + 
        (session | partic) + (session | token), 
      warmup = 1000, iter = 2000, chains = 4, 
      family = bernoulli(link = "logit"), 
      cores = parallel::detectCores(), 
      control = list(adapt_delta = 0.99, max_treedepth = 15), 
      data = df_binom,
      file = here("data", "models", "mod_binom_2.rds"))



ran = ranef(mod_binomial_2) 

ran_df = ran[["partic"]] %>% 
  as.data.frame()

plogis(ran_df$Estimate.Intercept)


desc = df %>% 
  group_by(partic, session, newest) %>% 
  summarize(n = n()) %>% 
  filter(newest == "fricative")

conditional_effects(mod_binomial)

fixef(mod_binomial)
plogis(-6 + 3 -.74)

ran_df = ran_df %>%
  mutate(t = Estimate.session5 -.489)

plogis(ran_df$t)

fedf = fixef(mod_binomial) %>% 
  as.data.frame()

plogis(fedf$Estimate[1] + fedf$Estimate[2] + fedf$Estimate[4] + fedf$Estimate[6])

plogis(fedf$Estimate[1] + fedf$Estimate[3] + fedf$Estimate[5] + fedf$Estimate[9])

  