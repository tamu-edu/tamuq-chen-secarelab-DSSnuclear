#Collective Dose

library(tidyverse)    # includes readr
library(lubridate)

load("Data/Collective_Ind_Dose.Rda")


#Individual Dose
h <- final_dose %>% filter(IndDose >1 & Int_Time=='1year' & Pathway=='Total (1 year)') %>% filter(Type == 'City' | Type == 'Industry') %>% ggplot(aes(x=IndDose,fill=Pathway)) + 
  geom_density() +  scale_x_log10(labels = scales::comma) + xlab('Residential Areas') + ylab('Effective 1 year Individual Dose (mSv)') + facet_grid(Name~Pathway, labeller = label_wrap_gen(width=10)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "bold"),
        axis.text=element_text(size=14), 
        axis.title=element_text(size=14,face="bold"), 
        strip.text = element_text(size = 14) ,
        legend.title = element_text(size = 16),          
        legend.text = element_text(size = 14))
print(h)

#Collective Dose
g <- final_dose %>% filter(CollectiveDose >1 & Int_Time=='1year' & Pathway=='Total (1 year)') %>% filter(Type == 'City' | Type == 'Industry') %>% ggplot(aes(x=CollectiveDose, fill=Pathway)) + 
  geom_density() +  scale_x_log10(labels = scales::comma) + xlab('Residential Areas') + ylab('Effective 1 year Collective Dose (mSv)') + facet_grid(Name~Pathway,labeller = label_wrap_gen(width=10)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "bold"),
        axis.text=element_text(size=14), 
        axis.title=element_text(size=14,face="bold"), 
        strip.text = element_text(size = 14) ,
        legend.title = element_text(size = 16),          
        legend.text = element_text(size = 14))
print(g)


#Individual Dose vs Collective Dose Boxplot
k <- final_dose %>% filter(Dose >1 & Int_Time=='1year' & Pathway=='Total (1 year)') %>% filter(Type == 'City' | Type == 'Industry') %>% ggplot(aes(x=Name,y=Dose,fill=DoseType)) + 
  geom_boxplot() + xlab('Residential Areas (Pop. 10,000)') + ylab('Effective 1 year Dose (mSv)') + #facet_grid(Name~DoseType, labeller = label_wrap_gen(width=10)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "bold"),
        axis.text=element_text(size=26), 
        axis.title=element_text(size=28,face="bold"), 
        strip.text = element_text(size = 26) ,
        legend.title = element_text(size = 28, face='bold'),          
        legend.text = element_text(size = 26)) + scale_fill_discrete(name = "Dose type") + scale_y_log10(limits=c(1,NA),minor_breaks=c(5*10^seq(1,7,1))) +
  annotation_logticks(sides = "l", outside = FALSE)
print(k)

breaks <- c(5*10^seq(1,7,2))
