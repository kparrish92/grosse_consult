
library(here)
library(tidyverse)
library(brms)
library(bayesplot)
library(bayestestR)

participants_out = c(11, 23, 34, 40, 41)
  
tokens_out = c("5_17", "5_18", "5_32", "6_6", "6_60")

df = read.csv(here("data", "tidy_data.csv")) %>% 
  janitor::clean_names() %>% 
  filter(!is.na(duration)) %>% 
  filter(!partic %in% participants_out) %>% 
  mutate("session_token" = paste0(session, "_", token)) %>% 
  filter(!session_token %in% tokens_out)  %>% 
  mutate(
    group_name = case_when(
      group == 1 ~ "Control",
      group == 2 ~ "Experimental",
      group == 3 ~ "Comparison"
    )
  )


df %>% 
  write.csv(here("data", "tidy_data_removal.csv"))

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

df$session = as.factor((df$session))
df$newest = as.factor((df$newest))

df$newest <- relevel(df$newest, ref = "stop")

mod_int_removal <- 
  brm(formula = newest ~ session*group_name + 
        (session | partic) + (session | token), 
      warmup = 1000, iter = 2000, chains = 4, 
      family = categorical(link = "logit"), 
      cores = parallel::detectCores(), 
      control = list(adapt_delta = 0.99, max_treedepth = 15), 
      data = df,
      file = here("data", "models", "mod_remov.rds"))

summary(mod_int_re)
