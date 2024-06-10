#####################################################################################
######################### CHECK MODEL OUTPUT MAIN FIGURES
####################
### libraries needed
library(dplyr)
library(xlsx)
library(reshape2)
### the working directory to where your files are
setwd("C:/Users/ivanpg/OneDrive - Universitetet i Agder/Norway/PROJECTS/RIP _ Project/Fuzzy Fidelity/MS/Data/")
PROCESSES<-c("Cohort","Cohort Age Effect","Cohort Soc Env: Fuzzies","Cohort Soc Env: Non-religious","Cohort Soc Env: Religious", 
               "Cohort Soc Env: Religious*Seculars","Cohort Soc Env: Seculars","Static Period Effect","Static Period Age Effect","Dynamic Period",
               "Dynamic Period Age Effect","Static Period U Age Effect")
### READ DATA FROM MODEL BY VOAS et al. 2009
DF_Voas_Decay<-read.xlsx("Data Voas.xlsx",sheetIndex = 1,header = T)
names(DF_Voas_Decay)<-c("Level","Year","Religiosity")

DF_Voas_Decay_C<-DF_Voas_Decay[DF_Voas_Decay$Level == "Cohort",]
DF_Voas_Decay_C<-DF_Voas_Decay_C[,c(2,3)]
for(a in 1:11) { DF_Voas_Decay_C <- rbind(DF_Voas_Decay_C,DF_Voas_Decay_C[1:41,]) }
DF_Voas_Decay_C$Process<-rep(PROCESSES,each=41)
DF_Voas_Decay_C<-DF_Voas_Decay_C[,c(1,3,2)]
DF_Voas_Decay_C$Data<-rep("Voas 2009",dim(DF_Voas_Decay_C)[1])

DF_Voas_Decay_PL<-DF_Voas_Decay[DF_Voas_Decay$Level == "Pop_L",]
DF_Voas_Decay_PL<-DF_Voas_Decay_PL[,c(2,3)]
for(a in 1:11) { DF_Voas_Decay_PL <- rbind(DF_Voas_Decay_PL,DF_Voas_Decay_PL[1:251,]) }
DF_Voas_Decay_PL$Process<-rep(PROCESSES,each=251)
DF_Voas_Decay_PL<-DF_Voas_Decay_PL[,c(1,3,2)]
DF_Voas_Decay_PL$Data<-rep("Voas 2009",dim(DF_Voas_Decay_PL)[1])

DF_Voas_Decay_PS<-DF_Voas_Decay[DF_Voas_Decay$Level == "Pop_S",]
DF_Voas_Decay_PS<-DF_Voas_Decay_PS[,c(2,3)]
for(a in 1:11) { DF_Voas_Decay_PS <- rbind(DF_Voas_Decay_PS,DF_Voas_Decay_PS[1:251,]) }
DF_Voas_Decay_PS$Process<-rep(PROCESSES,each=251)
DF_Voas_Decay_PS<-DF_Voas_Decay_PS[,c(1,3,2)]
DF_Voas_Decay_PS$Data<-rep("Voas 2009",dim(DF_Voas_Decay_PS)[1])



DF_Voas_Dynam<-read.xlsx("Data Voas.xlsx",sheetIndex = 2,header = T)
for(a in 1:11) { DF_Voas_Dynam <- rbind(DF_Voas_Dynam,DF_Voas_Dynam[1:603,]) }
DF_Voas_Dynam$Process<-rep(PROCESSES,each=603)
DF_Voas_Dynam$Process<-factor(DF_Voas_Dynam$Process)
names(DF_Voas_Dynam)<-c("Cat","Year","Proportion","Process")
DF_Voas_Dynam<-DF_Voas_Dynam[,c(2,4,3,1)]
DF_Voas_Dynam$Data<-rep("Voas 2009",dim(DF_Voas_Dynam)[1])





### READ THE OUTPUT FROM THE MODEL
DF_Model<-read.csv(file = "Data_Collection_ALL.csv",header = T)
levels(DF_Model$Process)<-c("Cohort","Cohort Age Effect","Cohort Soc Env: Fuzzies","Cohort Soc Env: Non-religious","Cohort Soc Env: Religious", 
                            "Cohort Soc Env: Religious*Seculars","Cohort Soc Env: Seculars","Static Period Effect","Static Period Age Effect","Dynamic Period",
                            "Dynamic Period Age Effect","Static Period U Age Effect")
DF_Model<-DF_Model[,c(1:4,33:619)]

### SEPARATE DATA BY FIT (COHORT, POPULATION LINEAR DECAY, POPULATION S-SHAPE DECAY)
DF.Model.C<-DF_Model[DF_Model$Fit == "Cohort",]

DF.Model.C.Decay<-DF.Model.C[,c(1:4,305:345)]
DF.Model.C.Decay<-melt(DF.Model.C.Decay,id.vars = c("SimID","Year","Process","Fit"),measure.vars = names(DF.Model.C.Decay)[5:45],value.name = "Religiosity")
DF.Model.C.Decay$Year<-unlist(strsplit(as.character(DF.Model.C.Decay$variable),"[.]"))[seq(3,dim(DF.Model.C.Decay)[1]*3,by=3)]
DF.Model.C.Decay$Year<-as.numeric(DF.Model.C.Decay$Year) * 5
DF.Model.C.Decay<-DF.Model.C.Decay[,c(2,3,6)]
DF.Model.C.Decay$Data<-rep("Model",dim(DF.Model.C.Decay)[1])

DF.Model.C.Dynam<-DF.Model.C[,c(1:4,346:468)]
DF.Model.C.Dynam<-melt(DF.Model.C.Dynam,id.vars = c("SimID","Year","Process","Fit"),measure.vars = names(DF.Model.C.Dynam)[5:127],value.name = "Proportion")
DF.Model.C.Dynam$Year<-unlist(strsplit(as.character(DF.Model.C.Dynam$variable),"[.]"))[seq(4,dim(DF.Model.C.Dynam)[1]*4,by=4)]
DF.Model.C.Dynam$Year<-as.numeric(DF.Model.C.Dynam$Year) * 5
DF.Model.C.Dynam$Cat<-unlist(strsplit(as.character(DF.Model.C.Dynam$variable),"[.]"))[seq(1,dim(DF.Model.C.Dynam)[1]*4,by=4)]
DF.Model.C.Dynam$Cat<-as.factor(DF.Model.C.Dynam$Cat)
DF.Model.C.Dynam<-DF.Model.C.Dynam[,c(2,3,6,7)]
DF.Model.C.Dynam$Data<-rep("Model",dim(DF.Model.C.Dynam)[1])

DF.Model.C.Rel.Cat<-DF.Model.C[,c(1:4,469:591)]
DF.Model.C.Rel.Cat<-melt(DF.Model.C.Rel.Cat,id.vars = c("SimID","Year","Process","Fit"),measure.vars = names(DF.Model.C.Rel.Cat)[5:127],value.name = "Religiosity")
DF.Model.C.Rel.Cat$Year<-unlist(strsplit(as.character(DF.Model.C.Rel.Cat$variable),"[.]"))[seq(0,dim(DF.Model.C.Rel.Cat)[1]*5,by=5)]
DF.Model.C.Rel.Cat$Year<-as.numeric(DF.Model.C.Rel.Cat$Year) * 5
DF.Model.C.Rel.Cat$Cat<-unlist(strsplit(as.character(DF.Model.C.Rel.Cat$variable),"[.]"))[seq(1,dim(DF.Model.C.Rel.Cat)[1]*5,by=5)]
DF.Model.C.Rel.Cat$Cat<-as.factor(DF.Model.C.Rel.Cat$Cat)
DF.Model.C.Rel.Cat<-DF.Model.C.Rel.Cat[,c(2,3,6,7)]
DF.Model.C.Rel.Cat$Data<-rep("Model",dim(DF.Model.C.Rel.Cat)[1])




DF.Model.PL<-DF_Model[DF_Model$Fit == "Pop_L",]

DF.Model.PL.Decay<-DF.Model.PL[,c(1:255)]
DF.Model.PL.Decay<-melt(DF.Model.PL.Decay,id.vars = c("SimID","Year","Process","Fit"),measure.vars = names(DF.Model.PL.Decay)[5:250],value.name = "Religiosity")
DF.Model.PL.Decay$Year<-unlist(strsplit(as.character(DF.Model.PL.Decay$variable),"[.]"))[seq(3,dim(DF.Model.PL.Decay)[1]*3,by=3)]
DF.Model.PL.Decay$Year<-as.numeric(DF.Model.PL.Decay$Year)
DF.Model.PL.Decay<-DF.Model.PL.Decay[,c(2,3,6)]
DF.Model.PL.Decay$Data<-rep("Model",dim(DF.Model.PL.Decay)[1])

DF.Model.PL.Dynam<-DF.Model.PL[,c(1:4,346:468)]
DF.Model.PL.Dynam<-melt(DF.Model.PL.Dynam,id.vars = c("SimID","Year","Process","Fit"),measure.vars = names(DF.Model.PL.Dynam)[5:127],value.name = "Proportion")
DF.Model.PL.Dynam$Year<-unlist(strsplit(as.character(DF.Model.PL.Dynam$variable),"[.]"))[seq(4,dim(DF.Model.PL.Dynam)[1]*4,by=4)]
DF.Model.PL.Dynam$Year<-as.numeric(DF.Model.PL.Dynam$Year) * 5
DF.Model.PL.Dynam$Cat<-unlist(strsplit(as.character(DF.Model.PL.Dynam$variable),"[.]"))[seq(1,dim(DF.Model.PL.Dynam)[1]*4,by=4)]
DF.Model.PL.Dynam$Cat<-as.factor(DF.Model.PL.Dynam$Cat)
DF.Model.PL.Dynam<-DF.Model.PL.Dynam[,c(2,3,6,7)]
DF.Model.PL.Dynam$Data<-rep("Model",dim(DF.Model.PL.Dynam)[1])

DF.Model.PL.Rel.Cat<-DF.Model.PL[,c(1:4,469:591)]
DF.Model.PL.Rel.Cat<-melt(DF.Model.PL.Rel.Cat,id.vars = c("SimID","Year","Process","Fit"),measure.vars = names(DF.Model.PL.Rel.Cat)[5:127],value.name = "Religiosity")
DF.Model.PL.Rel.Cat$Year<-unlist(strsplit(as.character(DF.Model.PL.Rel.Cat$variable),"[.]"))[seq(0,dim(DF.Model.PL.Rel.Cat)[1]*5,by=5)]
DF.Model.PL.Rel.Cat$Year<-as.numeric(DF.Model.PL.Rel.Cat$Year) * 5
DF.Model.PL.Rel.Cat$Cat<-unlist(strsplit(as.character(DF.Model.PL.Rel.Cat$variable),"[.]"))[seq(1,dim(DF.Model.PL.Rel.Cat)[1]*5,by=5)]
DF.Model.PL.Rel.Cat$Cat<-as.factor(DF.Model.PL.Rel.Cat$Cat)
DF.Model.PL.Rel.Cat<-DF.Model.PL.Rel.Cat[,c(2,3,6,7)]
DF.Model.PL.Rel.Cat$Data<-rep("Model",dim(DF.Model.PL.Rel.Cat)[1])




DF.Model.PS<-DF_Model[DF_Model$Fit == "Pop_S",]

DF.Model.PS.Decay<-DF.Model.PS[,c(1:255)]
DF.Model.PS.Decay<-melt(DF.Model.PS.Decay,id.vars = c("SimID","Year","Process","Fit"),measure.vars = names(DF.Model.PS.Decay)[5:250],value.name = "Religiosity")
DF.Model.PS.Decay$Year<-unlist(strsplit(as.character(DF.Model.PS.Decay$variable),"[.]"))[seq(3,dim(DF.Model.PS.Decay)[1]*3,by=3)]
DF.Model.PS.Decay$Year<-as.numeric(DF.Model.PS.Decay$Year)
DF.Model.PS.Decay<-DF.Model.PS.Decay[,c(2,3,6)]
DF.Model.PS.Decay$Data<-rep("Model",dim(DF.Model.PS.Decay)[1])

DF.Model.PS.Dynam<-DF.Model.PS[,c(1:4,346:468)]
DF.Model.PS.Dynam<-melt(DF.Model.PS.Dynam,id.vars = c("SimID","Year","Process","Fit"),measure.vars = names(DF.Model.PS.Dynam)[5:127],value.name = "Proportion")
DF.Model.PS.Dynam$Year<-unlist(strsplit(as.character(DF.Model.PS.Dynam$variable),"[.]"))[seq(4,dim(DF.Model.PS.Dynam)[1]*4,by=4)]
DF.Model.PS.Dynam$Year<-as.numeric(DF.Model.PS.Dynam$Year) * 5
DF.Model.PS.Dynam$Cat<-unlist(strsplit(as.character(DF.Model.PS.Dynam$variable),"[.]"))[seq(1,dim(DF.Model.PS.Dynam)[1]*4,by=4)]
DF.Model.PS.Dynam$Cat<-as.factor(DF.Model.PS.Dynam$Cat)
DF.Model.PS.Dynam<-DF.Model.PS.Dynam[,c(2,3,6,7)]
DF.Model.PS.Dynam$Data<-rep("Model",dim(DF.Model.PS.Dynam)[1])

DF.Model.PS.Rel.Cat<-DF.Model.PS[,c(1:4,469:591)]
DF.Model.PS.Rel.Cat<-melt(DF.Model.PS.Rel.Cat,id.vars = c("SimID","Year","Process","Fit"),measure.vars = names(DF.Model.PS.Rel.Cat)[5:127],value.name = "Religiosity")
DF.Model.PS.Rel.Cat$Year<-unlist(strsplit(as.character(DF.Model.PS.Rel.Cat$variable),"[.]"))[seq(0,dim(DF.Model.PS.Rel.Cat)[1]*5,by=5)]
DF.Model.PS.Rel.Cat$Year<-as.numeric(DF.Model.PS.Rel.Cat$Year) * 5
DF.Model.PS.Rel.Cat$Cat<-unlist(strsplit(as.character(DF.Model.PS.Rel.Cat$variable),"[.]"))[seq(1,dim(DF.Model.PS.Rel.Cat)[1]*5,by=5)]
DF.Model.PS.Rel.Cat$Cat<-as.factor(DF.Model.PS.Rel.Cat$Cat)
DF.Model.PS.Rel.Cat<-DF.Model.PS.Rel.Cat[,c(2,3,6,7)]
DF.Model.PS.Rel.Cat$Data<-rep("Model",dim(DF.Model.PS.Rel.Cat)[1])




### Once reshaped, we plot them, we create one plot per dataframe
### note that the geom_boxplot will take the data with the model outputs and geom_point the UN data
library(ggplot2)
DF_Cohort_Decay<-rbind(DF_Voas_Decay_C,DF.Model.C.Decay)
p1<-ggplot(DF_Cohort_Decay, aes(x=Year, y=Religiosity,colour=Data)) + geom_point() + scale_color_manual(values=c("black","red")) + facet_wrap(vars(Process),nrow = 3) + 
  geom_line(data = DF_Voas_Decay_C, colour = "red", size = 2) + theme(strip.text.y = element_text(size = 15)) + theme(axis.text.x=element_text(size=rel(1), angle=60, vjust = 0.5)) +
  theme(axis.text.y=element_text(size=rel(1)))
ggsave("Cohort_ALL.png",p1,width = 15, height = 7, units = "in",dpi = 300)


DF_PL_Decay<-rbind(DF_Voas_Decay_PL,DF.Model.PL.Decay)
p2<-ggplot(DF_PL_Decay, aes(x=Year, y=Religiosity,colour=Data)) + geom_point() + scale_color_manual(values=c("black","red")) + facet_wrap(vars(Process),nrow = 3) + 
  geom_line(data = DF_Voas_Decay_PL, colour = "red", size = 2) + theme(strip.text.y = element_text(size = 10)) + theme(axis.text.x=element_text(size=rel(1.2), angle=60, vjust = 0.5)) +
  theme(axis.text.y=element_text(size=rel(1.2)))
ggsave("Pop_L_ALL.png",p2,width = 19, height = 11, units = "in",dpi = 300)


DF_PS_Decay<-rbind(DF_Voas_Decay_PS,DF.Model.PS.Decay)
p3<-ggplot(DF_PS_Decay, aes(x=Year, y=Religiosity,colour=Data)) + geom_point() + scale_color_manual(values=c("black","red")) + facet_wrap(vars(Process),nrow = 3) + 
  geom_line(data = DF_Voas_Decay_PS, colour = "red", size = 2) + theme(strip.text.y = element_text(size = 10)) + theme(axis.text.x=element_text(size=rel(1.2), angle=60, vjust = 0.5)) +
  theme(axis.text.y=element_text(size=rel(1.2)))
ggsave("Pop_S_ALL.png",p3,width = 19, height = 11, units = "in",dpi = 300)



levels(DF.Model.C.Dynam$Cat)<-c("Fuzzy","Religious","Secular")
DF_Cohort_Dynam<-rbind(DF_Voas_Dynam,DF.Model.C.Dynam)
p4<-ggplot(DF_Cohort_Dynam, aes(x=Year, y=Proportion, shape = Data)) + geom_point(aes(colour=Cat),alpha = 1/2) + scale_shape_manual(values=c(1,15)) + scale_color_manual(values=c("blue","red","green")) + 
    facet_wrap(vars(Process),nrow = 3)
ggsave("Cohort_Dynamics_ALL.png",p4,width = 15, height = 7, units = "in",dpi = 300)

levels(DF.Model.PL.Dynam$Cat)<-c("Fuzzy","Religious","Secular")
DF_Cohort_Dynam<-rbind(DF_Voas_Dynam,DF.Model.PL.Dynam)
p5<-ggplot(DF_Cohort_Dynam, aes(x=Year, y=Proportion, shape = Data)) + geom_point(aes(colour=Cat),alpha = 1/2) + scale_shape_manual(values=c(1,15)) + scale_color_manual(values=c("blue","red","green")) + 
  facet_wrap(vars(Process),nrow = 3)
ggsave("Pop_L_Dynamics_ALL.png",p5,width = 19, height = 11, units = "in",dpi = 300)

levels(DF.Model.PS.Dynam$Cat)<-c("Fuzzy","Religious","Secular")
DF_Cohort_Dynam<-rbind(DF_Voas_Dynam,DF.Model.PS.Dynam)
p6<-ggplot(DF_Cohort_Dynam, aes(x=Year, y=Proportion, shape = Data)) + geom_point(aes(colour=Cat),alpha = 1/2) + scale_shape_manual(values=c(1,15)) + scale_color_manual(values=c("blue","red","green")) + 
  facet_wrap(vars(Process),nrow = 3)
ggsave("Pop_S_Dynamics_ALL.png",p6,width = 19, height = 11, units = "in",dpi = 300)



levels(DF.Model.C.Rel.Cat$Cat)<-c("Fuzzy","Religious","Secular")
p7<-ggplot(DF.Model.C.Rel.Cat, aes(x=Year, y=Religiosity)) + geom_point(aes(colour=Cat),alpha = 1/2) + ylim(0,1) + scale_color_manual(values=c("blue","red","green")) + 
  facet_wrap(vars(Process),nrow = 3)
ggsave("Cohort_Shares_Rel_ALL.png",p7,width = 19, height = 11, units = "in",dpi = 300)

levels(DF.Model.PL.Rel.Cat$Cat)<-c("Fuzzy","Religious","Secular")
p8<-ggplot(DF.Model.PL.Rel.Cat, aes(x=Year, y=Religiosity)) + geom_point(aes(colour=Cat),alpha = 1/2) + ylim(0,1) + scale_color_manual(values=c("blue","red","green")) + 
  facet_wrap(vars(Process),nrow = 3)
ggsave("Pop_L_Shares_Rel_ALL.png",p8,width = 19, height = 11, units = "in",dpi = 300)

levels(DF.Model.PS.Rel.Cat$Cat)<-c("Fuzzy","Religious","Secular")
p9<-ggplot(DF.Model.PS.Rel.Cat, aes(x=Year, y=Religiosity)) + geom_point(aes(colour=Cat),alpha = 1/2) + ylim(0,1) + scale_color_manual(values=c("blue","red","green")) + 
  facet_wrap(vars(Process),nrow = 3)
ggsave("Pop_S_Shares_Rel_ALL.png",p9,width = 19, height = 11, units = "in",dpi = 300)





######################### FIGURES JUST FOR PROPORTIONS MULTIPLICATORS AND COHORT LEVEL
####################
### libraries needed
library(dplyr)
library(xlsx)
library(reshape2)
### the working directory to where your files are
setwd("C:/Users/ivanpg/OneDrive - Universitetet i Agder/Norway/PROJECTS/RIP _ Project/Fuzzy Fidelity/MS/Data/")

### READ DATA FROM MODEL BY VOAS et al. 2009
DF_Voas_Decay<-read.xlsx("Data Voas.xlsx",sheetIndex = 1,header = T)
names(DF_Voas_Decay)<-c("Level","Year","Religiosity")

DF_Voas_Decay_C<-DF_Voas_Decay[DF_Voas_Decay$Level == "Cohort",]
DF_Voas_Decay_C<-DF_Voas_Decay_C[,c(2,3)]
for(a in 1:4) { DF_Voas_Decay_C <- rbind(DF_Voas_Decay_C,DF_Voas_Decay_C[1:41,]) }
DF_Voas_Decay_C$Process<-rep(c("Proportion Fuzzies","Proportion non-religious","Proportion Religious","Proportion Religious*Seculars","Proportion Seculars"),each=41)
DF_Voas_Decay_C<-DF_Voas_Decay_C[,c(1,3,2)]
DF_Voas_Decay_C$Data<-rep("Voas 2009",dim(DF_Voas_Decay_C)[1])

DF_Voas_Dynam<-read.xlsx("Data Voas.xlsx",sheetIndex = 2,header = T)
for(a in 1:4) { DF_Voas_Dynam <- rbind(DF_Voas_Dynam,DF_Voas_Dynam[1:603,]) }
DF_Voas_Dynam$Process<-rep(c("Proportion Fuzzies","Proportion non-religious","Proportion Religious","Proportion Religious*Seculars","Proportion Seculars"),each=603)
DF_Voas_Dynam$Process<-factor(DF_Voas_Dynam$Process)
names(DF_Voas_Dynam)<-c("Cat","Year","Proportion","Process")
DF_Voas_Dynam<-DF_Voas_Dynam[,c(2,4,3,1)]
DF_Voas_Dynam$Data<-rep("Voas 2009",dim(DF_Voas_Dynam)[1])


### READ THE OUTPUT FROM THE MODEL
DF_Model<-read.csv(file = "Data_Collection_Cohort_Multiplicator_David.csv",header = T)
levels(DF_Model$Process)<-c("Proportion Fuzzies","Proportion non-religious","Proportion Religious","Proportion Religious*Seculars","Proportion Seculars")
DF_Model<-DF_Model[,c(1:4,31:617)]

### SEPARATE DATA BY FIT (COHORT, POPULATION LINEAR DECAY, POPULATION S-SHAPE DECAY)
DF.Model.C<-DF_Model[DF_Model$Fit == "Cohort",]

DF.Model.C.Decay<-DF.Model.C[,c(1:4,305:345)]
DF.Model.C.Decay<-melt(DF.Model.C.Decay,id.vars = c("SimID","Year","Process","Fit"),measure.vars = names(DF.Model.C.Decay)[5:45],value.name = "Religiosity")
DF.Model.C.Decay$Year<-unlist(strsplit(as.character(DF.Model.C.Decay$variable),"[.]"))[seq(3,dim(DF.Model.C.Decay)[1]*3,by=3)]
DF.Model.C.Decay$Year<-as.numeric(DF.Model.C.Decay$Year) * 5
DF.Model.C.Decay<-DF.Model.C.Decay[,c(2,3,6)]
DF.Model.C.Decay$Data<-rep("Model",dim(DF.Model.C.Decay)[1])

DF.Model.C.Dynam<-DF.Model.C[,c(1:4,346:468)]
DF.Model.C.Dynam<-melt(DF.Model.C.Dynam,id.vars = c("SimID","Year","Process","Fit"),measure.vars = names(DF.Model.C.Dynam)[5:127],value.name = "Proportion")
DF.Model.C.Dynam$Year<-unlist(strsplit(as.character(DF.Model.C.Dynam$variable),"[.]"))[seq(4,dim(DF.Model.C.Dynam)[1]*4,by=4)]
DF.Model.C.Dynam$Year<-as.numeric(DF.Model.C.Dynam$Year) * 5
DF.Model.C.Dynam$Cat<-unlist(strsplit(as.character(DF.Model.C.Dynam$variable),"[.]"))[seq(1,dim(DF.Model.C.Dynam)[1]*4,by=4)]
DF.Model.C.Dynam$Cat<-as.factor(DF.Model.C.Dynam$Cat)
DF.Model.C.Dynam<-DF.Model.C.Dynam[,c(2,3,6,7)]
DF.Model.C.Dynam$Data<-rep("Model",dim(DF.Model.C.Dynam)[1])

DF.Model.C.Rel.Cat<-DF.Model.C[,c(1:4,469:591)]
DF.Model.C.Rel.Cat<-melt(DF.Model.C.Rel.Cat,id.vars = c("SimID","Year","Process","Fit"),measure.vars = names(DF.Model.C.Rel.Cat)[5:127],value.name = "Religiosity")
DF.Model.C.Rel.Cat$Year<-unlist(strsplit(as.character(DF.Model.C.Rel.Cat$variable),"[.]"))[seq(0,dim(DF.Model.C.Rel.Cat)[1]*5,by=5)]
DF.Model.C.Rel.Cat$Year<-as.numeric(DF.Model.C.Rel.Cat$Year) * 5
DF.Model.C.Rel.Cat$Cat<-unlist(strsplit(as.character(DF.Model.C.Rel.Cat$variable),"[.]"))[seq(1,dim(DF.Model.C.Rel.Cat)[1]*5,by=5)]
DF.Model.C.Rel.Cat$Cat<-as.factor(DF.Model.C.Rel.Cat$Cat)
DF.Model.C.Rel.Cat<-DF.Model.C.Rel.Cat[,c(2,3,6,7)]
DF.Model.C.Rel.Cat$Data<-rep("Model",dim(DF.Model.C.Rel.Cat)[1])




### Once reshaped, we plot them, we create one plot per dataframe
### note that the geom_boxplot will take the data with the model outputs and geom_point the UN data
library(ggplot2)
DF_Cohort_Decay<-rbind(DF_Voas_Decay_C,DF.Model.C.Decay)
p1<-ggplot(DF_Cohort_Decay, aes(x=Year, y=Religiosity,colour=Data)) + geom_point() + scale_color_manual(values=c("black","red")) + facet_wrap(vars(Process),nrow = 2) + 
  geom_line(data = DF_Voas_Decay_C, colour = "red", size = 2) + theme(strip.text.y = element_text(size = 10)) + theme(axis.text.x=element_text(size=rel(1.2), angle=60, vjust = 0.5)) +
  theme(axis.text.y=element_text(size=rel(1.2)))
ggsave("Cohort_Soc_Env.png",p1,width = 19, height = 11, units = "in",dpi = 300)


levels(DF.Model.C.Dynam$Cat)<-c("Fuzzy","Religious","Secular")
DF_Cohort_Dynam<-rbind(DF_Voas_Dynam,DF.Model.C.Dynam)
p2<-ggplot(DF_Cohort_Dynam, aes(x=Year, y=Proportion, shape = Data)) + geom_point(aes(colour=Cat),alpha = 1/2) + scale_shape_manual(values=c(1,15)) + scale_color_manual(values=c("blue","red","green")) + 
  facet_wrap(vars(Process),nrow = 2)
ggsave("Cohort_Soc_Env_Dynamics.png",p2,width = 19, height = 11, units = "in",dpi = 300)

levels(DF.Model.C.Rel.Cat$Cat)<-c("Fuzzy","Religious","Secular")
p3<-ggplot(DF.Model.C.Rel.Cat, aes(x=Year, y=Religiosity)) + geom_point(aes(colour=Cat),alpha = 1/2) + ylim(0,1) + scale_color_manual(values=c("blue","red","green")) + 
  facet_wrap(vars(Process),nrow = 2)
ggsave("Cohort_Soc_Env_Shares_Rel.png",p3,width = 19, height = 11, units = "in",dpi = 300)

