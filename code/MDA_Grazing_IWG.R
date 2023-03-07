#Packages ----
library(nlme);library(multcomp);library(emmeans);library(multcompView)
library(ggplot2); library(lattice); library(reshape2)
library(Rmisc)
#Read data ----
setwd("..")
dat<-read.csv("data/MDA_Grazing_IWG.csv")
str(dat)
names(dat)
dat$Season2<-factor(dat$Season, levels=c("Fall 2019", "Spring 2020","Summer 2020",  "Fall 2020", "Spring 2021"))


#Forage yield ----
tbl1<-summarySE(dat, "Pregraze_tons_acre", c("Season2", "Biomass", "Trt", "Farm"), na.rm=T)
library(ggplot2)
library(clipr)
write_clip(tbl1)
ggplot(subset(tbl1, Biomass=="Forage"), aes(y=Pregraze_tons_acre, x=Season2, fill=Trt))+
  facet_grid(~Farm)+
  geom_bar(position=position_dodge(.9), stat="identity") +
  geom_errorbar(aes(ymax=Pregraze_tons_acre+se, ymin=Pregraze_tons_acre-se), 
                width=0.5, position=position_dodge(.9))+
  ylab(expression("Forage yield " ~ (Tons ~ acre^{-1})))+
  #xlim(c(1:6))+
  scale_fill_manual(values=c("peachpuff3","lemonchiffon3"),
                    name="Entry")+
  ggtitle("Kernza forage yield measured in spring and fall")+
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
ggsave("ForageYld.png", width=9, height=5, units="in", path="/Volumes/GoogleDrive/My Drive/Intermediate_wheatgrass_research/Grazing_Experiments/On-Farm Trials/2020-KrausReport (Anderson and Honken)/MDA_Grazing_Analysis/")

mod1<-lme(Pregraze_tons_acre~Season2*Trt, random=~1|Paddock, data=subset(dat, Biomass=="Forage"&Farm=="Anderson"), na.action=na.omit)
anova(mod1)
library(emmeans)
cld(emmeans(mod1, ~Trt|Season2)) #significantly different at Anderson in Spring 2021, grazing reduced spring forage

#Forage quality ----
tbl2<-summarySE(dat, "RFV.pregraze", c("Season2", "Biomass", "Trt", "Farm"), na.rm=T)
write_clip(tbl2)
ggplot(subset(tbl2, Biomass=="Forage"), aes(y=RFV.pregraze, x=Season2, fill=Trt))+
  facet_grid(~Farm)+
  geom_bar(position=position_dodge(.9), stat="identity") +
  geom_errorbar(aes(ymax=RFV.pregraze+se, ymin=RFV.pregraze-se), 
                width=0.5, position=position_dodge(.9))+
  ylab("Relative Feed Value")+
  ggtitle("Relative feed value of Kernza forage measured in spring and fall")+
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
ggsave("RFV.png", width=9, height=5, units="in", path="/Volumes/GoogleDrive/My Drive/Intermediate_wheatgrass_research/Grazing_Experiments/On-Farm Trials/2020-KrausReport (Anderson and Honken)/MDA_Grazing_Analysis/")
mod2<-lme(RFV.pregraze~Season2*Trt, random=~1|Paddock, data=subset(dat, Biomass=="Forage"&Farm=="Anderson"), na.action=na.omit)
anova(mod2)
cld(emmeans(mod2, ~Trt|Season2)) #significantly different at Anderson in Spring 2021. Grazing increased RFV

#Grain yield ----
tbl3<-summarySE(dat, "Pregraze_tons_acre", c("Season2", "Biomass", "Trt", "Farm"), na.rm=T)
tbl3$yld<-tbl3$Pregraze_tons_acre*2000
tbl3$se2<-tbl3$se*2000
ggplot(subset(tbl3, Biomass=="Grain"), aes(y=yld, x=Trt, fill=Trt))+
  facet_grid(~Farm)+
  geom_bar(position=position_dodge(.9), stat="identity", show.legend=F) +
  geom_errorbar(aes(ymax=yld+se2, ymin=yld-se2), 
                width=0.5, position=position_dodge(.9))+
  ylab(expression("Grain yield " ~ (Pounds ~ acre^{-1})))+
  ggtitle("Kernza grain yield in 2020")+
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
ggsave("GrainYld.png", width=6, height=5, units="in", path="/Volumes/GoogleDrive/My Drive/Intermediate_wheatgrass_research/Grazing_Experiments/On-Farm Trials/2020-KrausReport (Anderson and Honken)/MDA_Grazing_Analysis/")

mod3<-lme(Pregraze_tons_acre~Trt, random=~1|Paddock, data=subset(dat, Biomass=="Grain"&Farm=="Anderson"), na.action=na.omit)
anova(mod3)
library(emmeans)
cld(emmeans(mod1, ~Trt)) #significantly different at Anderson in Spring 2021, grazing reduced spring forage

#Straw yield ----
tbl4<-summarySE(dat, "Pregraze_tons_acre", c("Season2", "Biomass", "Trt", "Farm"), na.rm=T)
ggplot(subset(tbl4, Biomass=="Straw"), aes(y=Pregraze_tons_acre, x=Trt, fill=Trt))+
  facet_grid(~Farm)+
  geom_bar(position=position_dodge(.9), stat="identity", show.legend=F) +
  geom_errorbar(aes(ymax=Pregraze_tons_acre+se, ymin=Pregraze_tons_acre-se), 
                width=0.5, position=position_dodge(.9))+
  ylab(expression("Straw yield " ~ (Tons ~ acre^{-1})))+
  ggtitle("Kernza straw yield in 2020")+
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

mod4<-lme(Pregraze_tons_acre~Trt, random=~1|Paddock, data=subset(dat, Biomass=="Straw"&Farm=="Anderson"), na.action=na.omit)
anova(mod4)
library(emmeans)
cld(emmeans(mod4, ~Trt)) #significantly different at Anderson in Spring 2021, grazing reduced spring forage

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

