#Code to check sufficiency of sampling rate
#Checks whether current sampling rate is best possible. Does increase in sampling rate add any statistically significant information?

#########

library(tidyverse)    # includes readr 
library(lubridate) 
library(FSA)

load("Data/Sampling_Check_Data_S=2.Rda")


organ <- 'eff'
integration_time <- '1year' #This only applies to those pathways which do not relate to the presence of a cloud
areas_covered <- 'important'
#location_name <- c( 'Al Wakra', 'Dukhan','Ras Laffan', 'Al Khor' ,'Industrial Area', 'Rayyan', 'Doha', 'HIA', 'Dukhan Oil Field','Mesaieed', 
#                     'North Oil Field' , 'Shaheen Oil Field' ,'Al Khalij Oil Field' ,'Bul Hanine', 'Idd El Shargi', 'Halul Island' ,'Maydan Mahzam' ,
#                     'Al Rayyan Field', 'Hamad Port', 'Al Karkara', 'Ras Abu Fontas', 'Umm Al Houl', 'Dukhan Desalination')
location_name <- c( 'Al Wakra', 'Dukhan','Ras Laffan', 'Al Khor' ,'Industrial Area', 'Rayyan', 'Doha', 'HIA', 'Dukhan Oil Field','Mesaieed', 
                          'North Oil Field' , 'Shaheen Oil Field' ,'Al Khalij Oil Field' ,'Bul Hanine', 'Idd El Shargi', 'Halul Island' ,'Maydan Mahzam' ,
                          'Al Rayyan Field', 'Hamad Port', 'Al Karkara', 'A- Structure', 'Al- Bunduq', 'Ras Abu Fontas', 'Umm Al Houl', 'Dukhan Desalination')
conf_threshold <- 0.05 #0.01 for 99% and 0.05 for 95%

#########
#Data Import
#This one has to read all files cuz V1 doesnt have total w/o ingestion file

dose<- dose %>%  filter(Dosage>=1)

#Stat Test Functions
#######
#This function runs mann whitney test for a given location for two given samples for all three NPPs and records p-value, sample sizes, sample identity, NPP 
#and location name in a data frame & Paired=False gives similar to Mann Whitley U Test
stat_diff_mann <- function(dataset, sample1_id, sample2_id){ #This function filters the original full data set to obtain two samples and calculates Mann-Whitley p-value for all 3 NPPs
  plants <- c('Barakah','Bushehr','Umm Huwayd' )
  p_value <- data.frame(pvalue=double(),NPP=character(),Sample_one_size=integer(),Sample_two_size=integer())
  for (i in 1:length(plants)) {
    sample1<- dataset %>%  filter(NPP==plants[i] & Version==sample1_id)
    sample2<- dataset %>%  filter(NPP==plants[i] & Version==sample2_id)
    p_value[i,1] <- as.numeric(wilcox.test(sample1$Dosage,sample2$Dosage,paired=FALSE)[3])
    p_value[i,2] <- plants[i]
    p_value[i,3] <- length(sample1$Dosage)
    p_value[i,4] <- length(sample2$Dosage)
  }
  p_value$Sample_one_ID <- sample1_id
  p_value$Sample_two_ID <- sample2_id
  p_value$Difference[p_value$pvalue<conf_threshold] <- 'Yes'
  p_value$Difference[p_value$pvalue>=conf_threshold] <- 'No'
  return(p_value)
}
stat_diff_kruskal <- function(dataset){
  plants <- c('Barakah','Bushehr','Umm Huwayd' )
  p_value <- data.frame(pvalue=double(),NPP=character())
  for (i in 1:length(plants)) {
    filtered_dataset <- dataset %>% filter(NPP==plants[i])
    p_value[i,1] <- as.numeric(kruskal.test(Dosage ~ Version, data = filtered_dataset)[3])
    p_value[i,2] <- plants[i]
  }
  p_value$Difference[p_value$pvalue<conf_threshold] <- 'Yes'
  p_value$Difference[p_value$pvalue>=conf_threshold] <- 'No'
  return(p_value)
}
stat_diff_dunn <- function(dataset){
  plants <- c('Barakah','Bushehr','Umm Huwayd' )
  stat_data <- data.frame(Comparison=character(),Z=double(),P.unadj=double(),P.adj=double(),NPP=character())
  for (i in 1:length(plants)) { 
    #This extracts the  result of dunn test including which samples compared, and unadj as well as adj p-value
    #Extracts adj p-value as it adjusted for multiple comparisons as per method chosen
    filtered_dataset <- dataset %>% filter(NPP==plants[i])
    test_data <- dunnTest(Dosage ~ Version, data = filtered_dataset ,method="bonferroni")[["res"]] %>% mutate(NPP=plants[i])
    stat_data <- rbind(stat_data,test_data )
  }
  stat_data$Difference[stat_data$P.adj<conf_threshold] <- 'Yes'
  stat_data$Difference[stat_data$P.adj>=conf_threshold] <- 'No'
  return(stat_data)
}


#Conducting the Stat Tests
######
mann_test_result <- data.frame(pvalue=double(),NPP=character(),Sample_one_size=integer(),Sample_two_size=integer(),Sample_one_ID=character(),Sample_two_ID=character(), 
                               Difference=character(), Name=character(), Type=character(),SamplingRate=character())
kruskal_test_result <- data.frame(pvalue=double(),NPP=character(),Difference=character(), Name=character(), Type=character())
dunn_test_result <- data.frame(Comparison=character(),Z=double(),P.unadj=double(),P.adj=double(),NPP=character(),Name=character(), Type=character())

for (j in 1:length(location_name)) {
  filtered_doses<- dose %>% filter(Name==location_name[j]) #No Dose filtering &only location filtering
  #mann_test_result1 <- stat_diff_mann(filtered_doses,'V1','V4') %>% mutate(Name=location_name[j]) %>% mutate(Type=filtered_doses[1,3]) %>% mutate(SamplingRate='Daily vs Daily') 
  #mann_test_result <- rbind(mann_test_result, mann_test_result1) #For double sampling

  kruskal_test_result1 <- stat_diff_kruskal(filtered_doses) %>% mutate(Name=location_name[j]) %>% mutate(Type=filtered_doses[1,3])
  kruskal_test_result <- rbind(kruskal_test_result, kruskal_test_result1)
   
  dunn_test_result1 <- stat_diff_dunn(filtered_doses) %>% mutate(Name=location_name[j]) %>% mutate(Type=filtered_doses[1,3]) 
  dunn_test_result <- rbind(dunn_test_result, dunn_test_result1)
  
}
rm(mann_test_result1,kruskal_test_result1,dunn_test_result1)

dunn_result_summary <- data.frame(Comparison=character(),NPP=character(), Difference=character(), Percent=double())
dunn_result_summary1 <- dunn_result_summary
plants <- unique(dunn_test_result$NPP)
vers_comparisons <- unique(dunn_test_result$Comparison)
types <- unique(dunn_test_result$Type)
diff <- unique(dunn_test_result$Difference)
rm(i,j,k)

#Getting Summary of Dunn test result by plant & secondary receptor type
for (i in 1:length(plants)) {
  #filtered <- dunn_test_result %>% filter(NPP=plants[i])
  for (k in 1:length(vers_comparisons)) {
    filtered <- dunn_test_result %>% filter(NPP==plants[i] & Comparison==vers_comparisons[k])
    dunn_result_summary1[1,3] <- 'Yes'
    dunn_result_summary1[1,4] <- 100*length(filtered$Difference[filtered$Difference=='Yes'])/nrow(filtered)
    # dunn_result_summary1[2,3] <- 'No'
    # dunn_result_summary1[2,4] <- 100*length(filtered$Difference[filtered$Difference=='No'])/nrow(filtered)
    dunn_result_summary1[,1] <- filtered$Comparison[1]
    dunn_result_summary1[,2] <- filtered$NPP[1]
    #dunn_result_summary1[,3] <- filtered$Type[1]
    dunn_result_summary<- rbind(dunn_result_summary,dunn_result_summary1)
  }
}

rm(dunn_result_summary1)



#plotting the result
#######
p <- mann_test_result %>% ggplot(aes(x=factor(Name),y=Difference,size=0.5, shape=SamplingRate, color=NPP)) + geom_point() + facet_grid(.~NPP)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "bold")) +xlab("Terrestial Secondary Receptors") + ylab("Is there a Statistical Difference (Mann test)")
#print(p)

q <- kruskal_test_result %>% ggplot(aes(x=factor(Name),y=Difference,size=0.5, color=NPP)) + geom_point() + facet_grid(.~NPP)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "bold")) +xlab("Terrestial Secondary Receptors") + ylab("Is there a Statistical Difference (Kruskal Test)")
print(q)

r <- dunn_test_result %>% filter() %>% ggplot(aes(x=factor(Name),y=P.adj,size=0.5, shape=SamplingRate, color=NPP)) + geom_point() + facet_grid(~NPP)+ geom_hline(yintercept = 0.05,col='red')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "bold")) +xlab("Terrestial Secondary Receptors") + ylab("Is there a Statistical Difference (Dunn Test)")
print(r)

s <- dunn_test_result %>% ggplot(aes(x=factor(SamplingRate),y=P.adj, color=NPP)) + geom_boxplot() + facet_grid(~NPP)+ geom_hline(yintercept = 0.05,col='red')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "bold")) +xlab("Terrestial Secondary Receptors") + ylab("Is there a Statistical Difference (Dunn Test)")
print(s)
 
t <- dunn_result_summary %>% filter(Difference=='Yes') %>% ggplot(aes(x=factor(Type),y=Percent,size=0.5, shape=SamplingRate, color=NPP)) + geom_point() + facet_grid(~NPP)+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "bold")) +xlab("Terrestial Secondary Receptors") + ylab("Is there a Statistical Difference (Dunn Test)")
print(t)
