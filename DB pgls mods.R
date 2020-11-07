## setwd
setwd("~/Desktop/tiktok")

## clean environment & plots
rm(list=ls()) 
graphics.off()

## libraries
library(tidyverse)

## temp data
temp <- data.frame(read_csv("jane_1672_sm_supplementarymaterial_1.csv"))
temp %>% filter(is.na(X1)) %>% select(Species, Tb, Ta, Bm.g.) %>% na.omit() %>%
  mutate(Tb = as.numeric(Tb), Ta = as.numeric(Ta),
         Bm.g. = as.numeric(Bm.g.)) %>% na.omit() -> temp

## virus data
virus <- read_csv("FullAssociationsForColin.csv")
virus %>% filter(PathogenType == "Virus") %>%
  mutate(Host = gsub("_"," ",Host), Virus = gsub("_"," ",Pathogen)) %>% 
  select(Host, Virus) %>% unique() -> virus
virus %>% mutate(IsRabies = (Virus=='Rabies lyssavirus')) %>% 
  group_by(Host) %>% add_count(Host) %>%
  summarize(Rabies = max(IsRabies),
            Richness = max(n)) -> hosts

## host data
hosts %>% rename(Species = Host) %>% left_join(temp) -> hosts

## remove no temp
#data=hosts[!is.na(hosts$Tb),]
data=hosts

## phylogenetics packages
library(ape)
library(caper)

## upham tree
tree=read.nexus('MamPhy_fullPosterior_BDvr_Completed_5911sp_topoCons_NDexp_MCC_v2_target.tre')

## make simple names
data$tip=data$Species
tree$tip=sapply(strsplit(tree$tip.label,'_'),function(x) paste(x[1],x[2],sep=' '))

## are all hdata in tree
data$intree=ifelse(data$tip%in%setdiff(data$tip,tree$tip),
                   'missing','upham')
table(data$intree)

## which names
data[data$intree=='missing','tip']

## fix names
library(plyr)
data$tip=revalue(data$tip,
                 c('Spermophilus beecheyi'='Otospermophilus beecheyi',
                   'Spermophilus lateralis'='Callospermophilus lateralis',
                   'Spermophilus parryii'='Urocitellus parryii',
                   'Spermophilus richardsonii'='Urocitellus richardsonii',
                   'Spermophilus tridecemlineatus'='Ictidomys tridecemlineatus',
                   'Spermophilus undulatus'='Urocitellus undulatus'))

## are all hdata in tree
data$intree=ifelse(data$tip%in%setdiff(data$tip,tree$tip),
                   'missing','upham')
table(data$intree)

## remove if missing
data=data[data$intree=='upham',]

## fix tree
rtree=tree
rtree$tip.label=rtree$tip

## trim
rtree=keep.tip(rtree,data$tip)

## fix
rtree$tip=NULL

## get evolutionary distinctiveness
library(picante)
ed_es=evol.distinct(rtree,type="equal.splits")
ed_fp=evol.distinct(rtree,type="fair.proportion")

## combine
ed=data.frame(ed_es,ed_fp[,2])
names(ed)=c("Species","ed_equal","ed_fair")

## tip
ed$tip=ed$Species
ed$Species=NULL

## match
data=data.frame(data)
data=merge(data,ed,by='tip')
bdata=data[match(rtree$tip.label,data$tip),]

## save
bdata$label=bdata$tip
bdata$Species=bdata$tip

## merge
cdata=comparative.data(phy=rtree,data=bdata,names.col=tip,vcv=T,na.omit=F,warn.dropped=T)

## fix
cdata$data$tree=NULL

## pgls of temp for lambda estimate
mod=pgls(Tb~1,data=cdata,lambda='ML')
summary(mod)

## temp and ED
ggplot(cdata$data,aes(ed_equal,Tb))+geom_smooth()+geom_point()

# 1 (ignores phylogeny)
hosts %>% mutate(Rabies = as.factor(Rabies)) %>% na.omit() %>% 
  ggplot(aes(x = Rabies, y = Tb)) + geom_boxplot() + 
  geom_jitter(width = 0.1, alpha = 0.5) + ylab("Body temperature") #+ 
#gghighlight(Species == "Didelphis virginiana", label_key = TYPE)
t.test(Tb ~ Rabies, data = hosts) 

## pgls version of 1 (report this over t test)
cdata$data$rab=factor(cdata$data$Rabies)
mod1=pgls(Tb~rab,data=cdata,lambda="ML")
summary(mod1)

## adjust for body mass
mod1mass=pgls(Tb~rab+Bm.g.,data=cdata,lambda="ML")
summary(mod1mass)

# 2 (ignores phylogeny)
hosts %>% ggplot(aes(y = Rabies, x = Tb))+ 
  geom_smooth(method = 'gam') + 
  geom_point() 

## trim to rabies
cdata2=cdata[!is.na(cdata$data$Rabies),]

## Fritz & Purvis's D test for signal in rabies 0/1
set.seed(1)
mod=phylo.d(cdata2,binvar=Rabies,permut=1000)
mod
## D of 0 = Brownian model, D of 1 = random (no phylogenetic signal)

## pgls 2
library(phylolm)
mod2=phyloglm(Rabies~Tb,data=cdata2$data,phy=cdata2$phy,
              method="logistic_MPLE")
summary(mod2)

## adjust for body mass
mod2mass=phyloglm(Rabies~Tb+Bm.g.,data=cdata2$data,phy=cdata2$phy,
              method="logistic_MPLE")
summary(mod2mass)

## rabies and distinctiveness?
emod1=phyloglm(Rabies~ed_equal,data=cdata2$data,phy=cdata2$phy,
               method="logistic_MPLE")
summary(emod1)
emod2=phyloglm(Rabies~ed_fair,data=cdata2$data,phy=cdata2$phy,
               method="logistic_MPLE")
summary(emod2)

## compare coefs
exp(coef(emod1))
exp(coef(emod2))

## normalize
cdata2$data$led_equal=log10(cdata2$data$ed_equal)
cdata2$data$led_fair=log10(cdata2$data$ed_fair)

## re-model
emod1=phyloglm(Rabies~led_equal,data=cdata2$data,phy=cdata2$phy,
               method="logistic_MPLE")
summary(emod1)
emod2=phyloglm(Rabies~led_fair,data=cdata2$data,phy=cdata2$phy,
               method="logistic_MPLE")
summary(emod2)

## compare coefs
exp(coef(emod1))
exp(coef(emod2))

## glm it
ggplot(cdata2$data,aes(led_equal,Rabies))+
  geom_smooth(method = 'glm',method.args = list(family = "binomial")) + 
  geom_point() 
## we can draw the actual fitted values from phyloglm though

# 3 (gam ignores phylogeny, richness should be treated as poisson anyway)
hosts %>% ggplot(aes(y = log(Richness), x = Tb)) + 
  geom_point(alpha = 0.3) + geom_smooth(method = 'gam')

## pgls 3
mod3=phyloglm(Richness~Tb,data=cdata$data,phy=cdata$phy,
              method="poisson_GEE")
summary(mod3)

## adjust for body  mass
mod3mass=phyloglm(Richness~Tb+Bm.g.,data=cdata$data,phy=cdata$phy,
              method="poisson_GEE")
summary(mod3mass)
round(coef(mod3mass),4)

## mass and temp cor (ignores phylo)
cor(cdata$data$Bm.g.,cdata$data$Tb,method='spearman',use="complete.obs")
cor(log(cdata$data$Bm.g.),cdata$data$Tb,method='spearman',use="complete.obs")

## scaling model
mod=pgls(log(Richness)~log(Bm.g.)+log(Tb),data=cdata,lambda='ML')
summary(mod)

## non-pgls
nmod=lm(log(Richness)~log(Bm.g.)+log(Tb),data=cdata$data)
summary(nmod)

## compare coef
coef(mod)
coef(nmod)