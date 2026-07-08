
# Summarize NASC by transect
nasc %>% 
  group_by(transect.orig, transect) %>% 
  summarise(transectStart = min(datetime),
            transectEnd = max(datetime)) %>% 
  arrange(transectStart)
