#Packages ----
library(nlme);library(multcomp);library(emmeans);library(multcompView)
library(ggplot2); library(lattice); library(reshape2)
library(Rmisc); library(tidyverse); 
library(stringr); library(gtsummary)
#Read data ----
dat<-read.csv("data/MDA_Grazing_IWG.csv")
str(dat)
names(dat)
dat$season2<-factor(dat$season, levels=c("Fall 2019", "Spring 2020","Summer 2020",  "Fall 2020", "Spring 2021",
                                         "Summer 2021", "Fall 2021", "Spring 2022", "Summer 2022", "Fall 2022"))
dat$plot<-as.factor(dat$plot)
dat$paddock<-as.factor(dat$paddock)
dat$stand_age<-as.numeric(unlist(str_extract_all(dat$season,"[[:digit:]]{4}$")))-2018
dat$fstand_age<-as.factor(dat$stand_age)
dat$season3<-str_split_i(dat$season, " ", 1)
dat$season3<-factor(dat$season3, levels=c("Spring", "Summer", "Fall"))
#Forage yield ----
#The old way
#tbl1<-summarySE(dat, "pregraze_tons_acre", c("season2", "biomass", "trt", "farm"), na.rm=T)
#The new way
tbl1<- dat %>% 
  filter(!is.na(pregraze_tons_acre), farm=="Anderson", trt=="Grazed", biomass!="Grain") %>% 
  group_by(fstand_age, season3, biomass, trt, farm) %>% 
  summarise(m_yld = mean(pregraze_tons_acre, na.rm=T), 
            sd_yld = sd(pregraze_tons_acre, na.rm=T), 
            n_yld = n()) %>% 
  mutate(se_yld=sd_yld/sqrt(n_yld))
#Exploring use of gtsummary functions
dat %>% 
  select(fstand_age, season3, biomass, trt, farm, pregraze_tons_acre) %>% 
  tbl_summary(
    by=trt,
    include=pregraze_tons_acre
  )
  

ggplot(tbl1, aes(y=m_yld, x=interaction(season3,fstand_age), fill=season3))+
  #facet_grid(~fstand_age)+
  geom_col(position="dodge")+
  #geom_bar(position=position_dodge(.9), stat="identity", width=.75) +
  geom_errorbar(aes(ymax=m_yld+se_yld, ymin=m_yld-se_yld), 
                width=0.5, position=position_dodge(.9))+
  ylab(expression("Forage yield " ~ (Tons ~ acre^{-1})))+
  geom_vline(xintercept=c(1.5, 4.5, 7.5))+
  geom_text(label="Year 1", x=1, y=2.6, size=4)+
  geom_text(label="Year 2", x=3, y=2.6, size=4)+
  geom_text(label="Year 3", x=6, y=2.6, size=4)+
  geom_text(label="Year 4", x=8.5, y=2.6, size=4)+
  coord_cartesian(ylim=c(0, 2.61))+
  ggtitle("Kernza forage yield measured in spring, summer, and fall")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_rect(color="black", fill="white"),
        panel.border=element_blank(),
        legend.key.size =unit(0.75, "cm"),
        legend.text = element_text(size=12),
        axis.line = element_line(color='black'),
        #legend.title.align = "top",
        legend.title=element_blank(),
        legend.position = c(.88, .6),
        #axis.title.x=element_text(size=12, color='black'),
        axis.title.x=element_blank(),
        axis.text.x = element_blank(),
        #axis.text.x=element_text(size=12, color='black'),
        axis.title.y = element_text(size=12, color='black'),
        axis.text.y=element_text(size=12, color='black'))
ggsave("ForageYld.png", width=6, height=4, units="in", path="figures/")

mod1<-lme(Pregraze_tons_acre~Season2*Trt, random=~1|Paddock, data=subset(dat, Biomass=="Forage"&Farm=="Anderson"), na.action=na.omit)
anova(mod1)
library(emmeans)
library(multcompView)
cld(emmeans(mod1, ~trt|Season2)) #significantly different at Anderson in Spring 2021, grazing reduced spring forage
dat %>% 
  filter(Biomass=="Forage"&
           Farm=="Anderson") %>% 
  lme(Pregraze_tons_acre~Season2*Trt, random=~1|Paddock, na.action=na.omit) %>% 
  


#Forage quality ----
tbl2<- dat %>% 
  filter(!is.na(pregraze_tons_acre), farm=="Anderson", trt=="Grazed", biomass!="Grain") %>% 
  group_by(fstand_age, season3, biomass, trt, farm) %>% 
  summarise(m_yld = mean(RFV, na.rm=T), 
            sd_yld = sd(RFV, na.rm=T), 
            n_yld = n()) %>% 
  mutate(se_yld=sd_yld/sqrt(n_yld))

ggplot(tbl2, aes(y=m_yld, x=interaction(season3,fstand_age), fill=season3))+
  #facet_grid(~fstand_age)+
  geom_col(position="dodge")+
  #geom_bar(position=position_dodge(.9), stat="identity", width=.75) +
  geom_errorbar(aes(ymax=m_yld+se_yld, ymin=m_yld-se_yld), 
                width=0.5, position=position_dodge(.9))+
  ylab("Relative Feed Value")+
  coord_cartesian(ylim=c(0, 200))+
  geom_vline(xintercept=c(1.5, 4.5, 7.5))+
  geom_text(label="Year 1", x=1, y=200)+
  geom_text(label="Year 2", x=3, y=200)+
  geom_text(label="Year 3", x=6, y=200)+
  geom_text(label="Year 4", x=8.5, y=200)+
  ggtitle("Kernza forage quality measured in spring, summer, and fall")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_rect(color="black", fill="white"),
        panel.border=element_blank(),
        axis.line = element_line(color='black'),
        #legend.title.align = "top",
        legend.title=element_blank(),
        legend.key.size =unit(0.75, "cm"),
        legend.text=element_text(size=12),
        legend.position = c(.85, .75),
        #axis.title.x=element_text(size=12, color='black'),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y = element_text(size=12, color='black'),
        axis.text.y=element_text(size=12, color='black'))
ggsave("ForageRFV.png", width=6, height=4, units="in", path="figures/")
mod2<-lme(RFV.pregraze~Season2*Trt, random=~1|Paddock, data=subset(dat, Biomass=="Forage"&Farm=="Anderson"), na.action=na.omit)
anova(mod2)
cld(emmeans(mod2, ~Trt|Season2)) #significantly different at Anderson in Spring 2021. Grazing increased RFV

#Grain yield ----
tbl3<- dat %>% 
  filter(!is.na(pregraze_tons_acre), farm=="Anderson", biomass=="Grain") %>% 
  group_by(fstand_age, biomass, trt, farm) %>% 
  mutate(pregraze_lbs_acre=pregraze_tons_acre*2000) %>% 
  summarise(m_yld = mean(pregraze_lbs_acre, na.rm=T), 
            sd_yld = sd(pregraze_lbs_acre, na.rm=T), 
            n_yld = n()) %>% 
  mutate(se_yld=sd_yld/sqrt(n_yld))

ggplot(tbl3, aes(y=m_yld, x=fstand_age, fill=trt))+
  geom_bar(position=position_dodge(.9), stat="identity") +
  #geom_bar(position=position_dodge(.9), stat="identity", width=.75) +
  geom_errorbar(aes(ymax=m_yld+se_yld, ymin=m_yld-se_yld), 
                width=0.5, position=position_dodge(.9))+
  ylab(expression("Grain yield " ~ (Pounds ~ acre^{-1})))+
  coord_cartesian(ylim=c(0, 1100))+
  xlab("Stand age in years since establishment")+
  ggtitle("Kernza grain yields in paddocks that were \n grazed (blue bars) and not grazed (red bars)")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_rect(color="black", fill="white"),
        panel.border=element_blank(),
        axis.line = element_line(color='black'),
        #legend.title.align = "top",
        legend.title=element_blank(),
        legend.position = c(.65, .8),
        legend.key.size =unit(1, "cm"),
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=12, color='black'),
        #axis.title.x=element_blank(),
        axis.text.x=element_text(size=12, color='black'),
        axis.title.y = element_text(size=12, color='black'),
        axis.text.y=element_text(size=12, color='black'))
ggsave("GrainYld.png", width=5, height=4, units="in", path="figures/")

mod3<-lme(pregraze_tons_acre~trt*fstand_age, random=~1|paddock, data=subset(dat, biomass=="Grain"&farm=="Anderson"), na.action=na.omit)
anova(mod3)
library(emmeans)
cld(emmeans(mod3, ~trt|fstand_age)) #significantly different at Anderson in Spring 2021, grazing reduced spring forage

#Straw yield ----
tbl4<- dat %>% 
  filter(!is.na(pregraze_tons_acre), farm=="Anderson", biomass=="Straw") %>% 
  group_by(fstand_age, biomass, trt, farm) %>% 
  summarise(m_yld = mean(pregraze_tons_acre, na.rm=T), 
            sd_yld = sd(pregraze_tons_acre, na.rm=T), 
            n_yld = n()) %>% 
  mutate(se_yld=sd_yld/sqrt(n_yld))

ggplot(tbl4, aes(y=m_yld, x=fstand_age, fill=trt))+
  geom_bar(position=position_dodge(.9), stat="identity") +
  #geom_bar(position=position_dodge(.9), stat="identity", width=.75) +
  geom_errorbar(aes(ymax=m_yld+se_yld, ymin=m_yld-se_yld), 
                width=0.5, position=position_dodge(.9))+
  ylab(expression("Grain yield " ~ (Pounds ~ acre^{-1})))+
  #coord_cartesian(ylim=c(0, 1100))+
  xlab("Stand age in years since establishment")+
  ggtitle("Kernza grain yields in paddocks that were \n grazed (blue bars) and not grazed (red bars)")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_rect(color="black", fill="white"),
        panel.border=element_blank(),
        axis.line = element_line(color='black'),
        #legend.title.align = "top",
        legend.title=element_blank(),
        legend.position = c(.65, .8),
        legend.key.size =unit(1, "cm"),
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=12, color='black'),
        #axis.title.x=element_blank(),
        axis.text.x=element_text(size=12, color='black'),
        axis.title.y = element_text(size=12, color='black'),
        axis.text.y=element_text(size=12, color='black'))

mod4<-lme(Pregraze_tons_acre~Trt, random=~1|Paddock, data=subset(dat, Biomass=="Straw"&Farm=="Anderson"), na.action=na.omit)
anova(mod4)
library(emmeans)
cld(emmeans(mod4, ~Trt)) #significantly different at Anderson in Spring 2021, grazing reduced spring forage

#Impacts of grazing on forage yield
tbl5<- dat %>% 
  filter(!is.na(pregraze_tons_acre), farm=="Anderson", biomass!="Grain") %>% 
  group_by(fstand_age, season3, biomass, trt) %>% 
  summarise(m_yld = mean(pregraze_tons_acre, na.rm=T), 
            sd_yld = sd(pregraze_tons_acre, na.rm=T), 
            n_yld = n()) %>% 
  mutate(se_yld=sd_yld/sqrt(n_yld))

ggplot(tbl5, aes(y=m_yld, x=season3, fill=trt))+
  geom_bar(position=position_dodge(.9), stat="identity") +
  #geom_bar(position=position_dodge(.9), stat="identity", width=.75) +
  facet_wrap(~fstand_age, ncol=3)+
  geom_errorbar(aes(ymax=m_yld+se_yld, ymin=m_yld-se_yld), 
                width=0.5, position=position_dodge(.9))+
  ylab(expression("Grain yield " ~ (Pounds ~ acre^{-1})))+
  #coord_cartesian(ylim=c(0, 1100))+
  xlab("Stand age in years since establishment")+
  ggtitle("Kernza grain yields in paddocks that were \n grazed (blue bars) and not grazed (red bars)")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_rect(color="black", fill="white"),
        panel.border=element_blank(),
        axis.line = element_line(color='black'),
        #legend.title.align = "top",
        legend.title=element_blank(),
        legend.position = c(.65, .8),
        legend.key.size =unit(1, "cm"),
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=12, color='black'),
        #axis.title.x=element_blank(),
        axis.text.x=element_text(size=12, color='black'),
        axis.title.y = element_text(size=12, color='black'),
        axis.text.y=element_text(size=12, color='black'))
mod5<-lme(pregraze_tons_acre~trt*season3, random=~1|paddock, data=subset(dat, biomass!="Grain"&farm=="Anderson"&fstand_age!="1"), na.action=na.omit)
anova(mod5)
cld(emmeans(mod5, ~trt|season3))

#Straw RFV: but there is no straw quality----
tbl5<-summarySE(dat, "RFV.pregraze", c("Season2", "Biomass", "Trt", "Farm"), na.rm=T)
ggplot(subset(tbl5, Biomass=="Straw"), aes(y=RFV.pregraze, x=Trt, fill=Trt))+
  facet_grid(~Farm)+
  geom_bar(position=position_dodge(.9), stat="identity", show.legend=F) +
  geom_errorbar(aes(ymax=RFV.pregraze+se, ymin=RFV.pregraze-se), 
                width=0.5, position=position_dodge(.9))+
  ylab(expression("Relative feed value of straw "))+
  ggtitle("Kernza straw quality in 2020")+
  #xlim(c(1:6))+
  scale_fill_manual(values=c("peachpuff3","lemonchiffon3"),
                    name="Entry")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_rect(color="black", fill="white"),
        panel.border=element_blank(),
        axis.line = element_line(color='black'),
        #legend.title.align = "top",
        legend.title=element_blank(),
        legend.position = c(.6, .8),
        #axis.title.x=element_text(size=12, color='black'),
        axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        axis.title.y = element_text(size=12, color='black'),
        axis.text.y=element_text(size=12, color='black'))
ggsave("StrawYld.png", width=6, height=5, units="in", path="/Volumes/GoogleDrive/My Drive/Intermediate_wheatgrass_research/Grazing_Experiments/On-Farm Trials/2020-KrausReport (Anderson and Honken)/MDA_Grazing_Analysis/")


#Forage utilization ----
dat$util<-(dat$Pregraze_tons_acre-dat$Postgraze_tons_acre)/dat$Pregraze_tons_acre
tbl6<-summarySE(dat, "util", c("Season2", "Biomass", "Trt", "Farm"), na.rm=T)
write_clip(tbl6)
ggplot(subset(tbl6, Biomass=="Forage"&Trt=="Grazed"&Farm=="Anderson"), aes(y=util, x=Season2))+
  #facet_grid(~Farm)+
  geom_bar(position=position_dodge(.9), stat="identity", show.legend=F) +
  geom_errorbar(aes(ymax=util+se, ymin=util-se), 
                width=0.5, position=position_dodge(.9))+
  ylab(expression("Forage utilization (%) "))+
  ggtitle("Forage utilization of Kernza in spring and fall")+
  #xlim(c(1:6))+
  scale_fill_manual(values=c("peachpuff3","lemonchiffon3"),
                    name="Entry")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_rect(color="black", fill="white"),
        panel.border=element_blank(),
        axis.line = element_line(color='black'),
        #legend.title.align = "top",
        legend.title=element_blank(),
        legend.position = c(.6, .8),
        #axis.title.x=element_text(size=12, color='black'),
        axis.title.x=element_blank(),
        #axis.text.x=element_blank(),
        axis.title.y = element_text(size=12, color='black'),
        axis.text.y=element_text(size=12, color='black'))
ggsave("ForageUtilization.png", width=6, height=5, units="in", path="/Volumes/GoogleDrive/My Drive/Intermediate_wheatgrass_research/Grazing_Experiments/On-Farm Trials/2020-KrausReport (Anderson and Honken)/MDA_Grazing_Analysis/")

#Soil data ----
sdat<-read.csv("D:/My Drive/Intermediate_wheatgrass_research/Grazing_Experiments/On-Farm Trials/2020-KrausReport (Anderson and Honken)/MDA_Grazing_Analysis/all_soil_data.csv")
head(sdat)
library(emmeans)
mod5<-lm(value~time, data=subset(sdat, parameter=="P"), na.action=na.omit)
anova(mod5)
summary(mod5)
mod6<-lm(value~time, data=subset(sdat, parameter=="K"), na.action=na.omit)
anova(mod6)
summary(mod6)

library(Rmisc)
stab1<-summarySE(sdat, "value", c("parameter", "time"))
write_clip(stab1)

#Economics
edat<-read.csv("data/MDA_Grazing_Econ.csv")
edat$category<-factor(edat$category, levels=c("Expenses", "Revenue", "Net Return to Enterprise"))
ggplot(edat, aes(y=value, x=factor(year), fill=category))+
  geom_bar(position=position_dodge(.9), stat="identity") +
  scale_y_continuous(labels = scales::dollar_format())+
  scale_fill_manual(values=c("#F8766D", "#7CAE00", "#00BFC4"))+
  xlab("Stand age in years since establishment")+
  ylab("Financials per acre")+
  ggtitle("Economics of Dual-Use Kernza")+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_rect(color="black", fill="white"),
        panel.border=element_blank(),
        axis.line = element_line(color='black'),
        #legend.title.align = "top",
        legend.title=element_blank(),
        legend.position = c(.78, .85),
        legend.key.size =unit(0.5, "cm"),
        legend.text = element_text(size=12),
        axis.title.x=element_text(size=12, color='black'),
        #axis.title.x=element_blank(),
        axis.text.x=element_text(size=12, color='black'),
        axis.title.y = element_text(size=12, color='black'),
        axis.text.y=element_text(size=12, color='black'))
ggsave("Economics.png", width=6, height=4, units="in", path="figures/")
