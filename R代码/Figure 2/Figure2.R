setwd(readClipboard())
getwd()
library(ggplot2)  
library(readxl)  
library(ggplot2)  
  
df <- data.frame(  
  Treatment = factor(c("aCO2", "eCO2")),  
  Mean = c(39.43333, 42.65),  
  SE = c(1.24891, 3.17792)  
)  

ggplot(df, aes(x = Treatment, y = Mean, fill = Treatment)) +  
  geom_col(position = position_dodge(width = 0.9), width = 0.8) +  
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2, position = position_dodge(width = 0.9)) +  
  theme_minimal() +  
  scale_fill_manual(values = c("#F8766D", "#00BFC4")) +    
  scale_color_manual(values = c("#F8766D", "#00BFC4")) +    
  xlab("") +  
  ylab("MC (%)") +   
  theme_bw() +   
  theme(panel.grid = element_blank(),  
        legend.position = "none",   
        strip.text = element_text(size = rel(0.90)),    
        axis.title.x = element_blank(),    
        axis.title.y = element_text(size = 10),   
        axis.text.x = element_text(vjust = 0.5, hjust = 0.5, size = 8, color = "black"),   
        axis.text.y = element_text(vjust = 0.5, hjust = 0.5, size = 8, color = "black"), 
        axis.line.x.bottom = element_line(color = 'black'),    
        axis.line.y.left = element_line(color = 'black'),    
        axis.line.y.right = element_blank(),   
        axis.text.y.right = element_blank(),    
        axis.ticks.y.right = element_blank(),   
        panel.border = element_blank())   
ggsave("MC.pdf", dpi=1000, height =45,width=45,units="mm")
