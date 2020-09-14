
setwd("~/Github/tiktok")
library(tidyverse)
library(gghighlight)

temp <- read_csv("jane_1672_sm_supplementarymaterial_1.csv")

temp %>% filter(is.na(X1)) %>% select(Species, Tb, Ta) %>% na.omit() %>%
  mutate(Tb = as.numeric(Tb), Ta = as.numeric(Ta)) %>% na.omit() -> temp

virus <- read_csv("FullAssociationsForColin.csv")

virus %>% filter(PathogenType == "Virus") %>%
  mutate(Host = gsub("_"," ",Host), Virus = gsub("_"," ",Pathogen)) %>% 
  select(Host, Virus) %>% unique() -> virus

virus %>% mutate(IsRabies = (Virus=='Rabies lyssavirus')) %>% 
  group_by(Host) %>% add_count(Host) %>%
  summarize(Rabies = max(IsRabies),
            Richness = max(n)) -> hosts

hosts %>% rename(Species = Host) %>% left_join(temp) -> hosts




# 1

hosts %>% mutate(Rabies = as.factor(Rabies)) %>% na.omit() %>% 
  ggplot(aes(x = Rabies, y = Tb)) + geom_boxplot() + 
  geom_jitter(width = 0.1, alpha = 0.5) + ylab("Body temperature") #+ 
  #gghighlight(Species == "Didelphis virginiana", label_key = TYPE)

t.test(Tb ~ Rabies, data = hosts) 




# 2

hosts %>% ggplot(aes(y = Rabies, x = Tb)) + 
  geom_point(alpha = 0.3) + geom_smooth(method = 'gam') + 
  gghighlight(Species == "Didelphis virginiana", label_key = TYPE)




# 3

hosts %>% ggplot(aes(y = log(Richness), x = Tb)) + 
  geom_point(alpha = 0.3) + geom_smooth(method = 'gam')

