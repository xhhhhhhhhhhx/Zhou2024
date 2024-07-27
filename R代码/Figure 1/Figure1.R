setwd(readClipboard())
getwd()
library(readxl)
library(Rmisc)
library(ggplot2)
library(grid)
library(multcompView)
library(lsmeans)
library(agricolae)
library(scales)
library(ggpubr)
library(vegan)
library(ape)
fit<- read_excel("Figure1.xlsx",1)
names(fit)
p1<-ggplot(fit, aes(x=Treatment, y=value, color=Treatment))+
  geom_boxplot(aes(fill=Treatment),size=0.25,outlier.shape =1,outlier.size= 0.25)+
  scale_fill_manual(values = c("#F8766D","#00BFC4",
                               "#F8766D","#00BFC4"))+
  scale_color_manual(values = c("#F8766D","#00BFC4",
                                "#F8766D","#00BFC4"))+
  xlab("") +  ylab("Total P (mg Kg-1)")+ #
  theme_bw() + theme(panel.grid =element_blank())+
  theme(legend.position="none",strip.text = element_text(size = rel(0.90)))+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_text(size=10),
        axis.text.x=element_text(vjust=0.5,hjust=0.5,size=8,color = "black"),
        axis.text.y=element_text(vjust=0.5,hjust=0.5,size=8,color = "black"))+
  theme(
    axis.line.x.bottom = element_line(color = 'black'),
    axis.line.y.left   = element_line(color = 'black'),
    axis.line.y.right  = element_line(color = 'black'),
    axis.text.y.right  = element_blank(),
    axis.ticks.y.right = element_blank(),
    panel.border       = element_blank())

p1
dat <- ggplot_build(p1)$data[[1]]
p1a<- p1 + geom_segment(data=dat,
                        aes(x=xmin, xend=xmax,
                            y=middle, yend=middle),
                        colour="white", size=0.5)
p1a
ggsave("1-AP.pdf", dpi=1000, height =45,width=45,units="mm")
