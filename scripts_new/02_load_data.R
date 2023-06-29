# load data 

session_2 = read.csv(here("data", "session_2.csv"))

session_all = read.csv(here("data", "all_sessions.csv")) %>% 
  mutate(spectral = case_when(spectral == "Strip" ~ "Stripe/Burst",
                              spectral == "Stripe/Burst" ~ "Stripe/Burst",
                              spectral == "Whitening" ~ "Whitening",
                              spectral == "Continuous" ~ "Continuous")) %>% 
  mutate(duration = duration*1000) %>% 
  mutate(cat_binom = if_else(category == "approximant", 1, 0, missing = NULL))


re_all = read.csv(here("data", "ran_eff.csv"))

re_all_con = read.csv(here("data", "ran_eff_control.csv"))

re_all_comp = read.csv(here("data", "ran_eff_comparison.csv"))

mod = read_rds(here("data", "models", "mod_log_b_up_two.rds"))