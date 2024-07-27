setwd(readClipboard())
getwd()
library(vegan)
library(devtools)
library(linkET)
library(dplyr)
library(ggplot2)


s <-  read.csv("traits.csv",row.names = 1, check.names = FALSE)
env<-  read.csv("otu.csv",row.names = 1, check.names = FALSE)
varespec<-as.data.frame(t(s))
varechem<-as.data.frame(env)


mantel <- mantel_test(varespec, varechem ,
                      spec_select = list(
                                          AMF = 700:710,
                                          phoD= 1:397,
                                          Protozoa= 398:699,
                                          Bacterivores= 711:721,
                                          Fungivores= 722:727
                      )) %>%  
  mutate(rd = cut(r, breaks = c(-Inf, 0.1, 0.5, Inf),
                  labels = c("< 0.1", "0.1 - 0.5", ">= 0.5")), 
         pd = cut(p, breaks = c(-Inf, 0.01, 0.05, Inf),
                  labels = c("< 0.01", "0.01 - 0.05", ">= 0.05"))) 

qcorrplot(correlate(varechem), type = "lower", diag = FALSE) +
  geom_square() +
  geom_couple(aes(xend = .xend+ 1.25,
                  yend = .yend +0.5, 
                  colour = pd, 
                  size = rd), 
              data = mantel, curvature = 0.1) +
  geom_diag_label(mapping = aes(y = .y + 0.05),
                  hjust = 0.15) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "RdBu")) +
  scale_size_manual(values = c(0.5, 1, 2)) +
  scale_colour_manual(values = color_pal(3)) +
  guides(size = guide_legend(title = "Mantel's r",
                             override.aes = list(colour = "grey35"), 
                             order = 2),
         colour = guide_legend(title = "Mantel's p", 
                               override.aes = list(size = 3), 
                               order = 1),
         fill = guide_colorbar(title = "Pearson's r", order = 3)) +
  theme(axis.text.y = element_blank())
ggsave(paste("Mantal testA.pdf", sep=""), mantel, width = 18, height= 10)
