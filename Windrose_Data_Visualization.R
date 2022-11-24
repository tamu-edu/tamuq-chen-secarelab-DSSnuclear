library(tidyverse)
library(readxl)

load("Data/Windrose_Plotting_Data.Rda")

p <- total_eff_windrose %>% filter(Type != 'Agriculture' & Dosage>0) %>% ggplot(aes(x=Type, y=Dosage)) + geom_boxplot() +ylab("Effective Individual One Year Dose from all Pathways (mSv)") +xlab("Secondary Receptor Types") + 
  scale_y_log10(labels = scales::comma,limits=c(NA,NA)) +  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "bold"),
        axis.text=element_text(size=14), 
        axis.title=element_text(size=14,face="bold"), 
        strip.text = element_text(size = 14) ,
        legend.title = element_text(size = 16),          
        legend.text = element_text(size = 14))
print(p)


