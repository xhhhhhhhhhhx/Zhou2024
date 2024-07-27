setwd(readClipboard())
getwd()
library(ggplot2)  
fit<- read_excel("Figure 3.xlsx",1)
names(fit)

p1 <- ggplot(fit, aes(x=Treatment, y=value, color=Treatment)) +  
  geom_violin(trim=FALSE, width=0.8, fill=NA) + 
  stat_summary(aes(group=Treatment), fun.y=mean, geom="point", shape=15, size=2, color="gray") +  
  geom_jitter(width=0.2, height=0, alpha=0.5,size=1) +  
  scale_fill_manual(values = c("#F8766D", "#00BFC4")) +  
  xlab("") +   
  ylab("AMF (nmol NLFA g-1 soil)") +  
  theme_bw() +   
  theme(panel.grid = element_blank(),  
        legend.position="none",  
        strip.text = element_text(size = rel(0.90)),  
        axis.title.x = element_blank(),  
        axis.title.y = element_text(size=10),  
        axis.text.x = element_text(vjust=0.5, hjust=0.5, size=8, color = "black"),  
        axis.text.y = element_text(vjust=0.5, hjust=0.5, size=8, color = "black"),  
        axis.line.x.bottom = element_line(color = 'black'),  
        axis.line.y.left = element_line(color = 'black'),  
        axis.line.y.right = element_line(color = 'black'),  
        axis.text.y.right = element_blank(),  
        axis.ticks.y.right = element_blank(),  
        panel.border = element_blank())  

p1
ggsave("1-AMF.pdf", dpi=1000, height =45,width=45,units="mm")