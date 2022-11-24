#This code estimates the cloud arrival time at the EEZ and terrestrial border
library(tidyverse)    # includes readr
library(lubridate)

load("Data/Cloud_Arrival_Times.Rda")

#Plotting
#Prob Density Chart
p <- reduced_cloud_arrival %>% filter(Data >(-1)) %>%  ggplot(aes(x= Data, fill=NPP)) + geom_density() + facet_grid(Name~NPP) +
  xlab("Cloud Arrival Time (Hr)") +ylab("Probability Density") + xlim(NA,100) +  
  theme(axis.text.x = element_text(hjust = 1, vjust = 0.5, face = "bold")) + # geom_hline(yintercept = 0.1,col='red') +  
  theme(axis.text=element_text(size=27), axis.title=element_text(size=28,face="bold"), strip.text = element_text(size = 28)) +
  theme(legend.title = element_text(size = 28), 
        legend.text = element_text(size = 27),legend.position="none")
print(p)


#Boxplot
q <- reduced_cloud_arrival %>% filter(Data >(-1)) %>%  ggplot(aes(y= Data, x=Name, fill=NPP)) + geom_boxplot() + #facet_grid(Name~NPP) +
  xlab("Cloud Arrival Time (Hr)") +ylab("Probability Density") + #scale_x_log10(labels = scales::comma,limits=c(NA,NA)) +  
  theme(axis.text.x = element_text(hjust = 1, vjust = 0.5, face = "bold")) + # geom_hline(yintercept = 0.1,col='red') +  
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
print(q)



 # cloud_hit_rate <- 100*nrow(reduced_cloud_arrival[reduced_cloud_arrival$Name=='EEZ Border' & reduced_cloud_arrival$NPP=='Umm Huwayd',])/nrow(reduced_cloud_arrival)


