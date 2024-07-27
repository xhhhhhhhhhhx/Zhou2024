setwd(readClipboard())
getwd()


library(readxl)
library(ggplot2)
library(grid)
library(ggpubr)
library(gridExtra)
library(tidyr)
library(RColorBrewer)
####4A-1
df <- read_excel("4A-1.xlsx",1)
names(df)

df$Families<- factor(df$Families,levels=c("Ambisporaceae","Glomeraceae", "Paraglomeraceae","Archaeosporaceae"
),

labels = c("Ambisporaceae","Glomeraceae", "Paraglomeraceae","Archaeosporaceae"))


levels(df$Families)

p1<- ggplot(data=df, aes(x=AMF_Treatment, y=AMF,fill=Families))+
  geom_bar(stat="identity")+theme_bw()+
  scale_y_continuous(expand = c(0,0))+theme(legend.position="right")+
  scale_fill_manual(values =  rev(c("#E1177E","#1CA3E1","#00986E" ,"#7A6AAD"
  )))+
  theme(panel.grid =element_blank())+
  labs(x='',y='Relative abundance (%)')+
  theme(legend.title = element_text(size=12))+# legend labels
  theme(legend.text = element_text(size=9))+
  theme(legend.key.size=unit(0.9,'cm'))+
  theme(strip.text.x = element_text(size =15),
        strip.background.x = element_rect(fill = "white", colour = "black"))+
  theme(axis.text.y=element_text(vjust=0.9,hjust=0.9,color = "black",size=12))+
  theme(axis.text.x=element_text(vjust=0.9,hjust=0.9,color = "black",size=12))+
  theme(axis.title.x = element_text(size=14,color = "black"),
        axis.title.y = element_text(size=14,color = "black"))#angle =90,+

p1
ggsave("A-ALP.pdf", dpi=1000, height =75,width=100,units="mm")

####4A-2
df <- read_excel("4A-2.xlsx",1)
names(df)

df$Families<- factor(df$Families,levels=c("Others","Unclassified", "Phyllobacteriaceae","Pseudomonadaceae"
                                          ,"Xanthomonadaceae","Rhizobiaceae","Bradyrhizobiaceae"
),

labels = c("Others","Unclassified", "Phyllobacteriaceae","Pseudomonadaceae"
           ,"Xanthomonadaceae" ,"Rhizobiaceae","Bradyrhizobiaceae"))


levels(df$Families)

p1<- ggplot(data=df, aes(x=PhoD_Treatment, y=PhoD,fill=Families))+
  geom_bar(stat="identity")+theme_bw()+
  scale_y_continuous(expand = c(0,0))+theme(legend.position="right")+
  
  scale_fill_manual(values =  rev(c("#E1177E","#1CA3E1","#00986E" ,"#7A6AAD",
                                    "#D56128",
                                    "#9F9F98","#000000"
  )))+
  theme(panel.grid =element_blank())+
  labs(x='',y='Relative abundance (%)')+
  theme(legend.title = element_text(size=12))+# legend labels
  theme(legend.text = element_text(size=9))+
  theme(legend.key.size=unit(0.9,'cm'))+
  theme(strip.text.x = element_text(size =15),
        strip.background.x = element_rect(fill = "white", colour = "black"))+
  theme(axis.text.y=element_text(vjust=0.9,hjust=0.9,color = "black",size=12))+
  theme(axis.text.x=element_text(vjust=0.9,hjust=0.9,color = "black",size=12))+
  theme(axis.title.x = element_text(size=14,color = "black"),
        axis.title.y = element_text(size=14,color = "black"))#angle =90,+

p1
ggsave("A-ALP.pdf", dpi=1000, height =75,width=100,units="mm")


####4A-3
df <- read_excel("4A-3.xlsx",1)
names(df)

df$Phyla<- factor(df$Phyla,levels=c("Others","Unclassified", "Apicomplexa","Ciliophora"
                                    ,"Lobosa","Conosa","Cercozoa"
),

labels = c("Others","Unclassified", "Apicomplexa","Ciliophora"
           ,"Lobosa","Conosa","Cercozoa"))


levels(df$Phyla)

p1<- ggplot(data=df, aes(x=Pro_Treatment, y=Pro,fill=Phyla))+
  geom_bar(stat="identity")+theme_bw()+
  scale_y_continuous(expand = c(0,0))+theme(legend.position="right")+
  
  scale_fill_manual(values =  rev(c("#E1177E","#1CA3E1","#00986E" ,"#7A6AAD",
                                    "#D56128",
                                    "#9F9F98","#000000"
  )))+
  theme(panel.grid =element_blank())+
  labs(x='',y='Relative abundance (%)')+
  theme(legend.title = element_text(size=12))+# legend labels
  theme(legend.text = element_text(size=9))+
  theme(legend.key.size=unit(0.9,'cm'))+
  theme(strip.text.x = element_text(size =15),
        strip.background.x = element_rect(fill = "white", colour = "black"))+
  theme(axis.text.y=element_text(vjust=0.9,hjust=0.9,color = "black",size=12))+
  theme(axis.text.x=element_text(vjust=0.9,hjust=0.9,color = "black",size=12))+
  theme(axis.title.x = element_text(size=14,color = "black"),
        axis.title.y = element_text(size=14,color = "black"))#angle =90,+

p1
ggsave("A-Pro.pdf", dpi=1000, height =75,width=100,units="mm")


####4A-4
df <- read_excel("4A-4.xlsx",1)
names(df)

df$Genera<- factor(df$Genera,levels=c("Acrobeles","Wilsonema", "Cruznema","Rhabdolaimus"
                                      ,"Plectus","Alaimus","Cervidellus","Rhabditis","Cephalobus"
                                      ,"Acrobeloides","Mesorhabditis"
),

labels = c("Acrobeles","Wilsonema", "Cruznema","Rhabdolaimus"
           ,"Plectus","Alaimus","Cervidellus","Rhabditis","Cephalobus"
           ,"Acrobeloides","Mesorhabditis"))


levels(df$Genera)

p1<- ggplot(data=df, aes(x=BN_Treatment, y=BN,fill=Genera))+
  geom_bar(stat="identity")+theme_bw()+
  scale_y_continuous(expand = c(0,0))+theme(legend.position="right")+
  
  scale_fill_manual(values =  rev(c("#E1177E","#1CA3E1","#00986E" ,"#7A6AAD","#D56128","5ED89F",
                                    "#EEB606","#B340A9","#3CABB6","#31C047","#B0A640"
                                    
                                    
  )))+
  theme(panel.grid =element_blank())+
  labs(x='',y='Relative abundance (%)')+
  theme(legend.title = element_text(size=12))+# legend labels
  theme(legend.text = element_text(size=9))+
  theme(legend.key.size=unit(0.9,'cm'))+
  theme(strip.text.x = element_text(size =15),
        strip.background.x = element_rect(fill = "white", colour = "black"))+
  theme(axis.text.y=element_text(vjust=0.9,hjust=0.9,color = "black",size=12))+
  theme(axis.text.x=element_text(vjust=0.9,hjust=0.9,color = "black",size=12))+
  theme(axis.title.x = element_text(size=14,color = "black"),
        axis.title.y = element_text(size=14,color = "black"))#angle =90,+

p1
ggsave("A-BN.pdf", dpi=1000, height =75,width=100,units="mm")



####4A-5
df <- read_excel("4A-5.xlsx",1)
names(df)

df$Genera<- factor(df$Genera,levels=c("Diphtherophora","Tylencholaimus", "Ditylenchus","Paraphelenchus"
                                      ,"Aphelenchoides","Aphelenchus"
),

labels = c("Diphtherophora","Tylencholaimus", "Ditylenchus","Paraphelenchus"
           ,"Aphelenchoides","Aphelenchus"))


levels(df$Genera)

p1<- ggplot(data=df, aes(x=FN_Treatment, y=FN,fill=Genera))+
  geom_bar(stat="identity")+theme_bw()+
  scale_y_continuous(expand = c(0,0))+theme(legend.position="right")+
  
  scale_fill_manual(values =  rev(c("#E1177E","#1CA3E1","#00986E" ,"#7A6AAD","#D56128","5ED89F"
                                    
  )))+
  theme(panel.grid =element_blank())+
  labs(x='',y='Relative abundance (%)')+
  theme(legend.title = element_text(size=12))+# legend labels
  theme(legend.text = element_text(size=9))+
  theme(legend.key.size=unit(0.9,'cm'))+
  theme(strip.text.x = element_text(size =15),
        strip.background.x = element_rect(fill = "white", colour = "black"))+
  theme(axis.text.y=element_text(vjust=0.9,hjust=0.9,color = "black",size=12))+
  theme(axis.text.x=element_text(vjust=0.9,hjust=0.9,color = "black",size=12))+
  theme(axis.title.x = element_text(size=14,color = "black"),
        axis.title.y = element_text(size=14,color = "black"))#angle =90,+

p1
ggsave("A-FN.pdf", dpi=1000, height =75,width=100,units="mm")

