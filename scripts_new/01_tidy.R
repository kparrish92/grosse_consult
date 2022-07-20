
source(here::here("scripts_new", "00_libs.R"))

df_session_2 = read.csv(here("data", "new_data.csv")) %>% 
  janitor::clean_names() %>% 
  select(partic:category) %>% 
  filter(session == 2) %>% 
  mutate("session_token" = paste0(session, "_", token)) 

df_session_all = read.csv(here("data", "new_data.csv")) %>% 
  janitor::clean_names() %>% 
  select(partic:category) %>% 
  filter(!session == 2) %>% 
  mutate("session_token" = paste0(session, "_", token)) 



df_session_2 %>% 
  write.csv(here("data", "session_2.csv"))

df_session_all %>% 
  write.csv(here("data", "all_sessions.csv"))

