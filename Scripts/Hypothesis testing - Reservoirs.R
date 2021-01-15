
setwd("~/Github/tiktok/data")
library(tidyverse)
library(gghighlight)

temp <- read_csv("jane_1672_sm_supplementarymaterial_1.csv")

temp %>% filter(is.na(X1)) %>% select(Species, Tb, Ta) %>% na.omit() %>%
  mutate(Tb = as.numeric(Tb), Ta = as.numeric(Ta)) %>% na.omit() -> temp

virus <- read_csv("literature_reservoir_classification_bats.csv")

virus %>% 
  select(Species, conservative, liberal) %>% unique() -> virus

virus %>% left_join(temp) -> hosts

# 1

hosts %>% na.omit() %>% 
  ggplot(aes(x = as.factor(conservative), y = Tb)) + geom_boxplot() + 
  geom_jitter(width = 0.1, alpha = 0.5) + ylab("Body temperature")  + 
  xlab("Conservative") -> g1 #+ 
  #gghighlight(Species == "Didelphis virginiana", label_key = TYPE)
t.test(Tb ~ as.factor(conservative), data = hosts) 

hosts %>% na.omit() %>% 
  ggplot(aes(x = as.factor(liberal), y = Tb)) + geom_boxplot() + 
  geom_jitter(width = 0.1, alpha = 0.5) + ylab("Body temperature")+ 
  xlab("Liberal")  -> g2 #+ 
#gghighlight(Species == "Didelphis virginiana", label_key = TYPE)
t.test(Tb ~ as.factor(liberal), data = hosts) 

library(patchwork)
g1 + g2



# 2

hosts %>% ggplot(aes(y = liberal, x = Tb))+ 
  geom_smooth(method = 'gam') + 
  geom_point() + theme_bw() + xlab('Body temperature')#+
 # gghighlight(Species == "Didelphis virginiana", label_key = TYPE) 

hosts %>% ggplot(aes(y = conservative, x = Tb))+ 
  geom_smooth(method = 'gam') + 
  geom_point() + theme_bw() + xlab('Body temperature')  #+
  #gghighlight(Species == "Didelphis virginiana", label_key = TYPE) 


###################################################################


virus <- read_csv("literature_reservoir_classification_carnivores.csv")

virus %>% 
  select(Species, conservative, liberal) %>% unique() -> virus

virus %>% left_join(temp) -> hosts

# 1

hosts %>% na.omit() %>% 
  ggplot(aes(x = as.factor(conservative), y = Tb)) + geom_boxplot() + 
  geom_jitter(width = 0.1, alpha = 0.5) + ylab("Body temperature") +
  xlab("Conservative") -> g1 #+ 
#gghighlight(Species == "Didelphis virginiana", label_key = TYPE)
t.test(Tb ~ as.factor(conservative), data = hosts) 

hosts %>% na.omit() %>% 
  ggplot(aes(x = as.factor(liberal), y = Tb)) + geom_boxplot() + 
  geom_jitter(width = 0.1, alpha = 0.5) + ylab("Body temperature") + 
  xlab("Liberal") -> g2 #+ 
#gghighlight(Species == "Didelphis virginiana", label_key = TYPE)
t.test(Tb ~ as.factor(liberal), data = hosts) 

library(patchwork)
g1 + g2



# 2

hosts %>% ggplot(aes(y = liberal, x = Tb))+ 
  geom_smooth(method = 'gam') + 
  geom_point() + theme_bw() + xlab('Body temperature') #+
# gghighlight(Species == "Didelphis virginiana", label_key = TYPE) 

hosts %>% ggplot(aes(y = conservative, x = Tb))+ 
  geom_smooth(method = 'gam') + 
  geom_point() + theme_bw() + xlab('Body temperature') #+
#gghighlight(Species == "Didelphis virginiana", label_key = TYPE) 

######################################################

# Checks for Dan

virus <- read_csv("literature_reservoir_classification_bats.csv")
table(unique(virus$Species) %in% temp$Species)
table(unique(temp$Species) %in% virus$Species)


virus <- read_csv("literature_reservoir_classification_carnivores.csv")
table(unique(virus$Species) %in% temp$Species)
table(unique(temp$Species) %in% virus$Species)
