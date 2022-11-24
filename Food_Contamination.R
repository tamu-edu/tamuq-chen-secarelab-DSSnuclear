library(tidyverse)    # includes readr
library(lubridate)
library (readxl)

load("Data/Food_Contamination_Data.Rda")


r <- food_contam %>% filter(Contamination>0.5 & Food!= 'Flour Wheat' & Food!='Whole Wheat')%>% ggplot(aes(x=factor(Food), y=Contamination, fill=factor(Nuclide)))+ #facet_grid(.~Nuclide) +
  geom_boxplot() +  ylab("Food Contamination (Bq/kg)") + geom_hline(yintercept = 535,col='red') +
  xlab("Foods") + scale_y_log10(limits=c(NA,NA),minor_breaks = c(5*10^seq(1,7,1))) +  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "bold"),
        axis.text=element_text(size=28), 
        axis.title=element_text(size=32,face="bold"), 
        strip.text = element_text(size = 28) ,
        legend.title = element_text(size = 32),          
        legend.text = element_text(size = 28))  + 
  annotate("text",x='Lamb' ,y=900, label="535 Bq/kg", angle=0, size =9) +
  labs(fill = "Nuclides") + annotation_logticks(sides = "l", outside = FALSE)
print(r)

#Estimation foodstuff contamination median
foods <- unique(food_contam$Food)
nuclides <- unique(food_contam$Nuclide)
contam_medians <- data.frame(Nuclide=character(),Food=character(),Contamination_Median=double())
contam_medians1 <- contam_medians
for (i in 1:length(nuclides)) {
  for (j in 1:length(foods)) {
    contam_medians1[1,1] <- nuclides[i]
    contam_medians1[1,2] <- foods[j]
    contam_medians1[1,3] <- food_contam %>% filter(Contamination>=0.00001 & Food==foods[j] & Nuclide==nuclides[i]) %>% .$Contamination %>% median()
    contam_medians <- rbind(contam_medians,contam_medians1)
  }
  
}
rm(contam_medians1)
contam_medians <- contam_medians %>% filter(Nuclide!='Alpha')

#Guideline Import
guidelines <- read_excel('Guideline Level (after July sim).xlsx', sheet = 3) %>% gather(Nuclide,Contam_GL,-Foods)

#Food Restriction Metric Calculation
#Future Users!!! Please delete and redo this section!!
#Lumped FRM
foods <- foods[1:14] #To remove wheat
lumped_FRM <- data.frame(Nuclide=character(),Food=character(),FRM=double())
lumped_FRM1 <- lumped_FRM
for (k in 1:(length(nuclides)-1)) {#-1 to not include alpha
  for (l in 1:length(foods)) {
    lumped_FRM1[1,1] <- nuclides[k]
    lumped_FRM1[1,2] <- foods[l]
    lumped_GL <- guidelines[guidelines$Foods=='Lumped' & guidelines$Nuclide==nuclides[k],3]
    food_median <- contam_medians[contam_medians$Food==foods[l] & contam_medians$Nuclide==nuclides[k],3]
    lumped_FRM1[1,3] <- food_median/lumped_GL
    lumped_FRM <- rbind(lumped_FRM,lumped_FRM1)
  }
  
}
rm(lumped_FRM1,lumped_GL)
rm(i,j,k,l)

#Individual FRM
ind_FRM <- data.frame(Nuclide=character(),Food=character(),FRM=double())
ind_FRM1 <- ind_FRM 
rm(i,j,k,l)
for (k in 1:(length(nuclides)-1)) {#-1 to not include alpha
  for (l in 1:length(foods)) {
    ind_FRM1[1,1] <- nuclides[k]
    ind_FRM1[1,2] <- foods[l]
    ind_GL <- guidelines[guidelines$Foods==foods[l] & guidelines$Nuclide==nuclides[k],3]
    food_median <- contam_medians[contam_medians$Food==foods[l] & contam_medians$Nuclide==nuclides[k],3]
    ind_FRM1[1,3] <- food_median/ind_GL
    ind_FRM <- rbind(ind_FRM,ind_FRM1)
  }
  
}
rm(ind_FRM1,ind_GL)

#Plotting FRM
######
p <- lumped_FRM %>% ggplot(aes(x = reorder(Food, -FRM), y=FRM, fill=Nuclide)) + geom_bar(stat = "identity",position='dodge') 
print(p)

ind_FRM
