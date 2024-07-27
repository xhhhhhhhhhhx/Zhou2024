setwd(readClipboard())
getwd()
library(ggplot2)
library(ggpmisc)
library(readxl)
library(dplyr)
library(ggpubr)

fit<- read_excel("regression.xlsx",1)

head(fit)
names(fit)

##########Figure6C##########
p1<-ggplot(fit, aes(x=TOAA, y=Networkcomplexity),group=Group)+
  
  theme_bw()+theme(legend.position="top")+theme_bw() + theme(panel.grid =element_blank())+
  geom_point(aes(color=Group),size=4.355)+
  geom_smooth(method = 'lm',color="black",formula= y ~ x,se= T)+
  
  stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = '*\", \"*')), 
               formula = y ~ x, parse = T, label.y = "bottom", label.x = "right")+
  theme(axis.title= element_text(size=14))+
  ylab("Networkcom plexity")+xlab("Total organic acid anions (μmol g-1 RDW)")+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  theme(axis.text.y = element_text(size = 12, color = "black"))
p1



##########Figure6D##########
p2<-ggplot(fit, aes(x=AP, y=Networkcomplexity),group=Group)+
  
  theme_bw()+theme(legend.position="top")+theme_bw() + theme(panel.grid =element_blank())+
  geom_point(aes(color=Group),size=4.355)+
  geom_smooth(method = 'lm',color="black",formula= y ~ x,se= T)+
  
  stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = '*\", \"*')), 
               formula = y ~ x, parse = T, label.y = "bottom", label.x = "right")+
  theme(axis.title= element_text(size=14))+
  ylab("Network complexity")+xlab("Total P accumulation(mg kg-1)")+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  theme(axis.text.y = element_text(size = 12, color = "black"))
p2


##########Figure6E##########
p3<-ggplot(fit, aes(x=NaOH-Po, y=Networkcomplexity),group=Group)+
  
  theme_bw()+theme(legend.position="top")+theme_bw() + theme(panel.grid =element_blank())+
  geom_point(aes(color=Group),size=4.355)+
  geom_smooth(method = 'lm',color="black",formula= y ~ x,se= T)+
  
  stat_poly_eq(aes(label = paste(..rr.label.., ..p.value.label.., sep = '*\", \"*')), 
               formula = y ~ x, parse = T, label.y = "bottom", label.x = "right")+
  theme(axis.title= element_text(size=14))+
  ylab("Network complexity")+xlab("NaOH-Po (mg kg-1)")+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  theme(axis.text.y = element_text(size = 12, color = "black"))
p3
