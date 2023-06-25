#Collective Dose

library(tidyverse)    # includes readr
library(lubridate)
directory <- 'C:\\Users\\arsha\\OneDrive - Texas A&M University\\Onedrive\\Research\\QGIS\\Computational Grid\\Grid + Imp Areas+ Pop'
thresholds <- data.frame(Threshold = c("Short Term", "Long Term"), Z = c(1000, 50))


pop_finder<- function(directory,NPP){ #Function to create a data frame with population for each cell
  file_location <- file.path(directory,NPP,'Imp_Grid_Pop_17.csv')
  originalfile <- read.csv(file_location) %>% filter(TOT_POP17>0) #File with cell number, name, type and population  
  cell_no <- unique(originalfile$Cell) # Eliminates duplicate cell numbers
  cell_population <- data.frame(cell_no,Population2017=0) #Empty dataframe with cell numbers
  for (i in 1:length(cell_no)) {
    cell_population[i,2] <- originalfile %>% filter(Cell==cell_no[i]) %>% .$TOT_POP17 %>% sum() #Filters to a specific cell number and adds all populations associated with it
    #cell_population[i,2] <- ceiling(cell_population[i,2]) #Rounds up to a whole number
  }
  return(cell_population)
}

#Following line needs the data frame containing overall dosage "dose_unmitigated" for important areas from Sec_Receptor comparison file. 
col_cities <- dose_unmitigated %>% filter(Dosage >(0.05)) %>%  mutate(CollectiveDose=0, Pop17=0) 
plants <- c('Bushehr','Barakah','Umm Huwayd') #Which plants are considered

col_cities <- col_cities %>% filter(Dosage >1 & Int_Time=='1year' & Pathway=='Total (1 year)') %>% filter(Type == 'City' | Type == 'Industry') #Keeps data of use 
colnames(col_cities)[5] <- 'IndDose' #Column name changed from Dosage to IndDose as this column contains individual dose

ind_dose <- col_cities #This makes a copy of the col_cities to make a new dataframe which stores the individual dose as 'Dose' column
ind_dose$Dose <- ind_dose$IndDose #The 'Dose' column contains individual dose
ind_dose$DoseType <- 'Individual'

col_cities <- col_cities %>% filter(IndDose >= 50)

for (i in 1:length(plants)) {
  cell_population <- pop_finder(directory,plants[i]) #Population in each cell
  for (j in 1:length(cell_population$cell_no)) {
    col_cities$CollectiveDose[col_cities$Cell==cell_population[j,1] & col_cities$NPP==plants[i]] <- #For every grid cell multiplies with corresponding population value by filtering to a unique 
               col_cities$IndDose[col_cities$Cell==cell_population[j,1]& col_cities$NPP==plants[i]]*cell_population[j,2] #grid cell and NPP
    
    col_cities$Pop17[col_cities$Cell==cell_population[j,1] & col_cities$NPP==plants[i]] <- cell_population[j,1] #Records the population associated with each grid cell
  }
}

collective_dose <- col_cities %>%  filter(CollectiveDose!=0) #This makes a copy of the col_cities to make a new dataframe which stores the collective dose as 'Dose' column for any area with individual dose 49.99
collective_dose$Dose <- collective_dose$CollectiveDose #The 'Dose' column contains collective dose
collective_dose$DoseType <-'Collective'

corrected_populations <- read.csv('C:\\Users\\arsha\\OneDrive - Texas A&M University\\Onedrive\\Research\\QGIS\\Computational Grid\\Corrected_City_populations.csv') %>% filter(Correct_Pop_17>0)
colnames(corrected_populations)[4] <- 'Umm Huwayd'


for (m in 1:length(collective_dose$Name)) {
  collective_dose$CollectiveDose[m] <- (collective_dose$CollectiveDose[m] * corrected_populations$Correct_Pop_17[corrected_populations$Name==collective_dose$Name[m]])/  corrected_populations[corrected_populations$Name==collective_dose$Name[m],collective_dose$NPP[m]]
}
  
final_dose <- rbind(ind_dose,collective_dose)

for (n in 1:length(final_dose$Name)) {
  final_dose$Name[n] <- str_c(final_dose$Name[n], '(',ceiling(corrected_populations[corrected_populations$Name==final_dose$Name[n],4]/10000),')')  
}

#Individual Dose
h <- final_dose %>% filter(Dose >1 & Int_Time=='1year' & Pathway=='Total (1 year)') %>% filter(Type == 'City' | Type == 'Industry') %>% ggplot(aes(x=Name,y=IndDose)) + 
  geom_boxplot(fill="skyblue1") + xlab('Residential Areas (Pop. 10,000)') + ylab('Eff. 1 year Individual Dose (mSv)') +# facet_grid(Name~Pathway,labeller = label_wrap_gen(width=10)) + scale_x_log10(labels = scales::comma)
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "bold"),
        axis.text=element_text(size=26), 
        axis.title=element_text(size=28,face="bold"), 
        strip.text = element_text(size = 26)) + scale_y_log10(limits=c(1,NA),minor_breaks=c(5*10^seq(1,7,1))) +
  annotate("text",x='Al Khor(5)' ,y=60, label="50 mSv", angle=0, size =9) + annotate("text", x='Al Khor(5)',y=1210, label="1000 mSv", angle=00, size =9)+
  annotation_logticks(sides = "l", outside = FALSE) + geom_hline(data = thresholds, aes(yintercept = Z),col=c('red','blue'))
print(h)

#Collective Dose
g <- final_dose %>% filter(Dose >1 & Int_Time=='1year' & Pathway=='Total (1 year)') %>% filter(Type == 'City' | Type == 'Industry') %>% ggplot(aes(x=Name,y=CollectiveDose)) + 
  geom_boxplot(fill="coral2") + xlab('Residential Areas (Pop. 10,000)') + ylab('Eff. 1 year Collective Dose (man.mSv)') +# facet_grid(Name~Pathway,labeller = label_wrap_gen(width=10)) + scale_x_log10(labels = scales::comma)
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "bold"),
        axis.text=element_text(size=26), 
        axis.title=element_text(size=28,face="bold"), 
        strip.text = element_text(size = 26)) + scale_y_log10(limits=c(1,NA),minor_breaks=c(5*10^seq(1,7,1))) +
  annotation_logticks(sides = "l", outside = FALSE)
print(g)

final_dose$DoseTypeNew <-  factor(final_dose$DoseType, levels= c('Individual','Collective'))
#Individual Dose vs Collective Dose Boxplot
k <- final_dose %>% filter(Dose >1 & Int_Time=='1year' & Pathway=='Total (1 year)') %>% filter(Type == 'City' | Type == 'Industry') %>% ggplot(aes(x=Name,y=Dose,fill=DoseTypeNew)) + 
  geom_boxplot() + xlab('Residential Areas (Pop. 10,000)') + ylab('Effective 1 year Dose (mSv or man.mSv)') + facet_wrap(~DoseTypeNew) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "bold"),
        axis.text=element_text(size=26), 
        axis.title=element_text(size=28,face="bold"), 
        strip.text = element_text(size = 26) ,
        legend.title = element_text(size = 28, face='bold'),          
        legend.text = element_text(size = 26)) + scale_fill_discrete(name = "Dose type") + scale_y_log10(limits=c(1,NA),minor_breaks=c(5*10^seq(1,7,1))) +
  annotation_logticks(sides = "l", outside = FALSE)
print(k)

breaks <- c(5*10^seq(1,7,2))
