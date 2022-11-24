#This code is made to work with all the data impacting chosen locations around Qatar
#Intensive release aka Disaster

library(tidyverse)    # includes readr
library(lubridate)
library(extrafont)
library(RColorBrewer)

#Importing Dose for different pathways and combining all into mega dosage called pathway_doses
#result_directory <- 'E:/JRodos/2017 Simulations'
version <- 'V6'
organ <- 'eff'
areas_covered <- 'important'
thresholds <- data.frame(Threshold = c("Short Term", "Long Term"), Z = c(1000, 50))


#dose_unmitigated <- rbind(all_pathway_import(result_directory,version,organ,'7days',areas_covered),all_pathway_import(result_directory,version,organ,'1year',areas_covered)) %>% mutate(Mitigation='None')
#dose_mitigated <- rbind(all_pathway_import(result_directory,'V10',organ,'7days',areas_covered),all_pathway_import(result_directory,'V10',organ,'1year',areas_covered)) %>% mutate(Mitigation='Yes')
load("Data/Sec_Receptor_Data_Unmitigated_ Case.Rda")
load("Data/Sec_Receptor_Data_Mitigated_Case.Rda")


#Plotting mitigated doses for same days as with accidents >1mSv in unmitigated doses
Plants <- unique(dose_unmitigated$NPP)
dose_mit <- dose_unmitigated[0,]
dose_mit1 <- dose_unmitigated[0,]

for (i in 1:length(Plants)) {
  imp_days <- dose_unmitigated %>% filter(Dosage>50 & NPP==Plants[i]) %>% .$Date %>% unique()
  dose_mit1 <- dose_mitigated %>% filter(NPP==Plants[i]) 
  dose_mit1 <- dose_mit1[dose_mit1$Date %in% imp_days,]
  dose_mit<- rbind(dose_mit,dose_mit1)
}

rm(dose_mit1)
dose_mitigated <- dose_mit

dose_combinded <- rbind(dose_unmitigated,dose_mitigated)
dose_combinded$Int_Time_New <- factor(dose_combinded$Int_Time, levels= c('7days','1year')) #Reorders the dataframe to put 7days first
dose_combinded$Mitigation<- factor(dose_combinded$Mitigation, levels= c('None','Yes'))
summer <- c('Jun', 'Jul', 'Aug')
autumn <- c('Sep', 'Oct', 'Nov')
winter <- c('Dec','Jan','Feb')
spring <- c('Mar','Apr','May')

dose_combinded$Season[dose_combinded$Month %in% summer] <- 'Summer'
dose_combinded$Season[dose_combinded$Month %in% autumn] <- 'Autumn'
dose_combinded$Season[dose_combinded$Month %in% winter] <- 'Winter'
dose_combinded$Season[dose_combinded$Month %in% spring] <- 'Spring'


dose_combinded$Pathway[dose_combinded$Pathway==c('Groundshine (7 days)')] <- 'Groundshine'
dose_combinded$Pathway[dose_combinded$Pathway==c('Groundshine (1 year)')] <- 'Groundshine'
dose_combinded$Pathway[dose_combinded$Pathway==c('Ingestion (1 year)')] <- 'Ingestion'
dose_combinded$Pathway[dose_combinded$Pathway==c('Ingestion (7 days)')] <- 'Ingestion'
dose_combinded$Pathway[dose_combinded$Pathway==c('Total (1 year)')] <- 'Total'
dose_combinded$Pathway[dose_combinded$Pathway==c('Total (7 days)')] <- 'Total'
dose_combinded$Pathway[dose_combinded$Pathway==c('Resuspension (1 year)')] <- 'Resuspension'
dose_combinded$Pathway[dose_combinded$Pathway==c('Resuspension (7 days)')] <- 'Resuspension'

rm(summer, autumn, winter, spring)

#Filtering dose to obtain different dataframes
timedep_pathways <- c('Groundshine','Ingestion','Total')
groundshine_pathways <- c('Groundshine (7 days)','Groundshine (1 year)')
depos_pathways <- c('Cloudshine','Inhalation','Skin')

#Recurring Pathway Dosage
ingestion <- dose_combinded %>% filter(Pathway=='Ingestion')
total <- dose_combinded %>% filter(Pathway=='Total')
total_no_ingestion <- total
total_no_ingestion$Dosage <- total$Dosage-ingestion$Dosage 
total_no_ingestion$Pathway <- 'Non-ingestion'
dose_combinded <- rbind(dose_combinded,total_no_ingestion)
r <- dose_combinded %>% filter(Dosage >=1) %>% filter(Pathway =='Total' | Pathway =='Non-ingestion' | Pathway=='Ingestion') %>% ggplot(aes(x= factor(Pathway), y= Dosage, fill=Int_Time_New)) +  
  geom_boxplot() +ylab("Effective Individual Dose (mSv)") +xlab("Pathways") + 
  scale_y_log10(limits=c(NA,10000),minor_breaks = c(5,50,500,5000),labels = scales::number_format(big.mark=',',accuracy = 1)) +  
  scale_fill_discrete(name = "Integration Time", labels = c("7 Days", "1 Year"))+  
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5, face = "bold"),
        axis.text=element_text(size=29), 
        axis.title=element_text(size=34,face="bold"), 
        strip.text = element_text(size = 29) ,
        legend.title = element_text(size = 34),          
        legend.text = element_text(size = 29),
        strip.text.x = element_blank(),
        strip.text.y =element_text(size=29)) + annotation_logticks(sides = "l", outside = FALSE) +  
  annotate("text",x='Non-ingestion' ,y=60, label="50 mSv", angle=0, size =9) + annotate("text", x='Non-ingestion',y=1210, label="1000 mSv", angle=00, size =9)+
  geom_hline(data = thresholds, aes(yintercept = Z),col=c('red','blue')) 
print(r)

#Dosage Variation with Month
f<- dose_combinded %>% filter(Dosage >=1 & Int_Time=='1year' & Pathway=='Total') %>% ggplot(aes(x= Dosage, fill=Pathway)) + 
  geom_density()+ xlab("Effective 1 year Individual Dose (mSv)") +
  ylab("Probability Density") +  scale_x_log10(limits=c(NA,NA),minor_breaks = NULL,labels = scales::number_format(big.mark=',',accuracy = 1)) +  
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5, face = "bold"), #scale_x_continuous(limits = c(NA, NA))+
        axis.text=element_text(size=29), 
        axis.title=element_text(size=32,face="bold"), 
        strip.text = element_text(size = 29) ,
        legend.title = element_text(size = 32),          
        legend.text = element_text(size = 29),
        legend.position = "none",
        strip.text.x = element_blank(),
        strip.text.y =element_text(size=29)) + geom_vline(xintercept = 50,color='blue') + annotate("text", x=53, y=0.7, label="50 mSv", angle=270, size =9) +
  geom_vline(xintercept = 1000,color='red') + annotate("text", x=1070, y=0.7, label="1000 mSv", angle=270, size =9) +
  facet_grid(Season~Pathway) 
print(f)


#Dosage variation with secondary receptors 
q <- dose_unmitigated %>% filter(Dosage >=1 & Int_Time=='1year')  %>% ggplot(aes(x=factor(Type), y=Dosage, fill=Pathway))+ #%>% filter(Type=='Industry'| Type=='Oil Field')
  geom_boxplot() +  ylab("Eff. One Year Ind. Dose (mSv)") +
  xlab("Secondary Receptor Categories") + scale_y_log10(limits=c(NA,10000),minor_breaks = c(5,50,500,5000),labels = scales::number_format(big.mark=',',accuracy = 1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "bold"),
        axis.text=element_text(size=29), 
        axis.title=element_text(size=31,face="bold"), 
        strip.text = element_text(size = 29) ,
        legend.title = element_text(size = 31),          
        legend.text = element_text(size = 29),
        strip.text.x = element_blank(),
        strip.text.y =element_text(size=29)) +
  scale_fill_discrete(name = "Exposure Pathways", labels = c("Cloudshine", "Groundshine", "Ingestion","Inhalation","Skin","Total")) + 
  annotation_logticks(sides = "l", outside = FALSE) +  geom_hline(data = thresholds, aes(yintercept = Z),col=c('red','blue')) +
  annotate("text",x='Transport Hub' ,y=60, label="50 mSv", angle=0, size =9) + annotate("text", x='Transport Hub',y=1210, label="1000 mSv", angle=00, size =9)
print(q)

#Dosage variation with Industry and Oil field receptors
m <- dose_unmitigated %>% filter(Dosage >=1 & Int_Time=='1year' & Pathway !='Non-ingestion') %>% filter(Type=='Industry'| Type=='Oil Field')%>% ggplot(aes(x=factor(Name), y=Dosage, fill=Pathway))+ 
  geom_boxplot() +  ylab("Eff. One Year Ind. Dose (mSv)") +
  xlab("Secondary Receptors") + scale_y_log10(limits=c(NA,10000),minor_breaks = c(5,50,500,5000),labels = scales::number_format(big.mark=',',accuracy = 1)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "bold"),
        axis.text=element_text(size=29), 
        axis.title=element_text(size=31,face="bold"), 
        strip.text = element_text(size = 29) ,
        legend.title = element_text(size = 31),          
        legend.text = element_text(size = 29),
        strip.text.x = element_blank(),
        strip.text.y =element_text(size=29)) +
        scale_fill_discrete(name = "Exposure Pathways", labels = c("Cloudshine", "Groundshine", "Ingestion","Inhalation","Skin","Total")) + 
  annotate("text",x='Shaheen Oil Field' ,y=60, label="50 mSv", angle=0, size =9) + annotate("text", x='Ras Laffan',y=1210, label="1000 mSv", angle=00, size =9) +
  annotation_logticks(sides = "l", outside = FALSE)  +  geom_hline(data = thresholds, aes(yintercept = Z),col=c('red','blue')) 
print(m)

#Comparing mitigation vs unmitigated dose (All doses)
o <- dose_combinded %>% filter(Dosage >=50 & Type!='Agriculture'& Int_Time=='1year') %>% filter( Pathway=='Ingestion') %>% ggplot(aes(x= factor(Type), y= Dosage, fill=Mitigation)) +  
  geom_boxplot() +ylab("One Year Ingestion Dose (mSv)") +xlab("Secondary Receptor Types") + 
  scale_y_log10(limits=c(NA,10000),minor_breaks = c(5,50,500,5000),labels = scales::number_format(big.mark=',',accuracy = 1)) +  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "bold"),
        axis.text=element_text(size=28), 
        axis.title=element_text(size=32,face="bold"), 
        strip.text = element_text(size = 28) ,
        legend.title = element_text(size = 32),          
        legend.text = element_text(size = 28),
        strip.text.x = element_blank(),
        strip.text.y =element_text(size=28)) + annotation_logticks(sides = "l", outside = FALSE) +  
  geom_hline(data = thresholds, aes(yintercept = Z),col=c('red','blue')) +
  annotate("text",x='Halul Island' ,y=60, label="50 mSv", angle=0, size =9) + annotate("text", x='Halul Island',y=1210, label="1000 mSv", angle=00, size =9)+
  scale_fill_brewer(palette="Set3",name = "Mitigation", labels = c("None", "Food Rest."))
print(o)

w <- dose_combinded %>% filter(Dosage >=50 & Type!='Agriculture'& Int_Time=='1year' ) %>% filter(Pathway=='Non-ingestion') %>% ggplot(aes(x= factor(Type), y= Dosage, fill=Mitigation)) +  
  geom_boxplot() +ylab("One Year Non-Ingestion Dose (mSv)") +xlab("Secondary Receptor Types") + 
  scale_y_log10(limits=c(NA,10000),minor_breaks = c(5,50,500,5000),labels = scales::number_format(big.mark=',',accuracy = 1)) +  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "bold"),
        axis.text=element_text(size=28), 
        axis.title=element_text(size=28,face="bold"), 
        strip.text = element_text(size = 28) ,
        legend.title = element_text(size = 32),          
        legend.text = element_text(size = 28),
        strip.text.x = element_blank(),
        strip.text.y =element_text(size=28)) + annotation_logticks(sides = "l", outside = FALSE) +  
  geom_hline(data = thresholds, aes(yintercept = Z),col=c('red','blue')) +
  annotate("text",x='Halul Island' ,y=60, label="50 mSv", angle=0, size =9) + annotate("text", x='Halul Island',y=1210, label="1000 mSv", angle=00, size =9)+
  scale_fill_brewer(palette="Set3",name = "Mitigation", labels = c("None", "Sheltering")) 
print(w)

w <- dose_combinded %>% filter(Dosage >=50 & Type!='Agriculture'& Int_Time=='1year' ) %>% filter(Pathway=='Total') %>% ggplot(aes(x= factor(Type), y= Dosage, fill=Mitigation)) +  
  geom_boxplot() +ylab("One Year Dose (mSv)") +xlab("Secondary Receptor Types") + 
  scale_y_log10(limits=c(NA,10000),minor_breaks = c(5,50,500,5000),labels = scales::number_format(big.mark=',',accuracy = 1)) +  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "bold"),
        axis.text=element_text(size=28), 
        axis.title=element_text(size=32,face="bold"), 
        strip.text = element_text(size = 28) ,
        legend.title = element_text(size = 32),          
        legend.text = element_text(size = 28),
        strip.text.x = element_blank(),
        strip.text.y =element_text(size=28)) + annotation_logticks(sides = "l", outside = FALSE) +  
  geom_hline(data = thresholds, aes(yintercept = Z),col=c('red','blue')) +
  annotate("text",x='Halul Island' ,y=60, label="50 mSv", angle=0, size =9) + annotate("text", x='Halul Island',y=1210, label="1000 mSv", angle=00, size =9)+
  scale_fill_brewer(palette="Set3",name = "Mitigation", labels = c("None", "Mitigation")) 
print(w)

Barakah <- dose_combinded %>% filter(Dosage >=1 & Type!='Agriculture' & NPP=='Barakah')

z <- Barakah[!Barakah$Name %in% c('Industrial Area',	'Rayyan',	'A- Structure',	'Al- Bunduq',	'Al Khalij Oil Field'), ] %>% 
  filter(Int_Time=='1year') %>% ggplot(aes(x= factor(Type), y= Dosage, fill=Mitigation)) +  
  geom_boxplot() +ylab("Eff. One Year Ind. Dose (mSv)") +xlab("Secondary Receptor Types") + 
  scale_y_log10(limits=c(NA,10000),minor_breaks = c(5,50,500,5000),labels = scales::number_format(big.mark=',',accuracy = 1)) +  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "bold"),
        axis.text=element_text(size=28), 
        axis.title=element_text(size=32,face="bold"), 
        strip.text = element_text(size = 28) ,
        legend.title = element_text(size = 32),          
        legend.text = element_text(size = 28),
        strip.text.x = element_blank(),
        strip.text.y =element_text(size=28)) + annotation_logticks(sides = "l", outside = FALSE) +  
  geom_hline(data = thresholds, aes(yintercept = Z),col=c('red','blue')) +
  annotate("text",x='Halul Island' ,y=60, label="50 mSv", angle=0, size =9) + annotate("text", x='Halul Island',y=1210, label="1000 mSv", angle=00, size =9)+
  scale_fill_brewer(palette="Set3",name = "Mitigation", labels = c("None", "Yes")) 
print(z)


#####
#Comparing mitigation vs unmitigated dose (Only those mitigated doses within 3 hours of release time compared to unmitigated compared)
dose_unmitigated$Date_time <- dose_unmitigated$release_year_time_char %>% ymd_hm()

dose_mitigated$Date_time <- dose_mitigated$release_year_time_char %>% ymd_hm()

pre_mit_times <- unique(dose_unmitigated$Date_time)
post_mit_times <- unique(dose_mitigated$Date_time)

for (i in 1:length(1:length(pre_mit_times))) {
  x[i] <- post_mit_times[i]>pre_mit_times[i]-21600 & post_mit_times[i]<pre_mit_times[i]+21600
}
x <- x[!is.na(x)]
pre_mit_times <- pre_mit_times[x]
post_mit_times <- post_mit_times[x]

for (j in 1:length(pre_mit_times)) {
  y <- dose_unmitigated %>% filter(Date_time==pre_mit_times[j])
  pre_mit <- rbind(y,pre_mit)
}

for (k in 1:length(post_mit_times)) {
  z <- dose_mitigated %>% filter(Date_time==post_mit_times[k])
  post_mit <- rbind(z,post_mit)
}


combo_mit <- rbind(pre_mit,post_mit)

ingestion <- combo_mit %>% filter(Pathway=='Ingestion')
total <- combo_mit %>% filter(Pathway=='Total (1 year)')
total_no_ingestion <- total
total_no_ingestion$Dosage <- total$Dosage-ingestion$Dosage 
total_no_ingestion$Pathway <- 'Non-ingestion'
combo_mit <- rbind(combo_mit,total_no_ingestion)


o <- combo_mit %>% filter(Dosage >=1 & Type!='Agriculture' & Int_Time=='1year' & Pathway=='Ingestion') %>% ggplot(aes(x= factor(Type), y= Dosage, fill=Mitigation)) +  
  geom_boxplot() +ylab("Eff. One Year Ind. Dose (mSv)") +xlab("Secondary Receptor Types") + 
  scale_y_log10(limits=c(NA,10000),minor_breaks = c(5,50,500,5000),labels = scales::number_format(big.mark=',',accuracy = 1)) +  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "bold"),
        axis.text=element_text(size=28), 
        axis.title=element_text(size=32,face="bold"), 
        strip.text = element_text(size = 28) ,
        legend.title = element_text(size = 32),          
        legend.text = element_text(size = 28),
        strip.text.x = element_blank(),
        strip.text.y =element_text(size=28)) + annotation_logticks(sides = "l", outside = FALSE) +  
  geom_hline(data = thresholds, aes(yintercept = Z),col=c('red','blue')) +
  annotate("text",x='Halul Island' ,y=60, label="50 mSv", angle=0, size =9) + annotate("text", x='Halul Island',y=1210, label="1000 mSv", angle=00, size =9)+
  scale_fill_brewer(palette="Set3",name = "Mitigation", labels = c("None", "Food Rest."))
print(o)



w <- combo_mit %>% filter(Dosage >=1 & Type!='Agriculture') %>% filter(Pathway=='Non-ingestion') %>% ggplot(aes(x= factor(Type), y= Dosage, fill=Mitigation)) +  
  geom_boxplot() +ylab("Eff. One Year Ind. Dose (mSv)") +xlab("Secondary Receptor Types") + 
  scale_y_log10(limits=c(NA,10000),minor_breaks = c(5,50,500,5000),labels = scales::number_format(big.mark=',',accuracy = 1)) +  
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "bold"),
        axis.text=element_text(size=28), 
        axis.title=element_text(size=32,face="bold"), 
        strip.text = element_text(size = 28) ,
        legend.title = element_text(size = 32),          
        legend.text = element_text(size = 28),
        strip.text.x = element_blank(),
        strip.text.y =element_text(size=28)) + annotation_logticks(sides = "l", outside = FALSE) +  
  geom_hline(data = thresholds, aes(yintercept = Z),col=c('red','blue')) +
  annotate("text",x='Halul Island' ,y=60, label="50 mSv", angle=0, size =9) + annotate("text", x='Halul Island',y=1210, label="1000 mSv", angle=00, size =9)+
  scale_fill_brewer(palette="Set3",name = "Mitigation", labels = c("None", "Sheltering")) 
print(w)


#####




#Media and Quartile comparison

percentile_unmit <- data.frame(Type=character(),Quartile25=double(), median=double(), quartile75=double())
percentile_mit <- data.frame(Type=character(),Quartile25=double(), median=double(), quartile75=double())
percentile_mit[0:8,1] <- unique(dose_unmitigated$Type)
percentile_unmit[0:8,1] <- unique(dose_unmitigated$Type)

for (i in 1:length(percentile_unmit$Type)) {
  x <- dose_unmitigated %>% filter(Type==percentile_unmit$Type[i] & Dosage>0)
  percentile_unmit[i,2:4] <- quantile(x$Dosage, probs = c(.25, .5, .75))
}

for (i in 1:length(percentile_mit$Type)) {
  y <- dose_mitigated %>% filter(Type==percentile_mit$Type[i] & Dosage>0)
  percentile_mit[i,2:4] <- quantile(y$Dosage, probs = c(.25, .5, .75))
}
