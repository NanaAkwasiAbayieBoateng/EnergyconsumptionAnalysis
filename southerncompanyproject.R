#==================================================================
# Install packages not already installed in a list
#==================================================================


list=c("tidyverse","stringr","forcats","ggmap","rvest","tm","SnowballC","dplyr","calibrate"
       ,"stringi","ggplot2","maps","httr","rsdmx","devtools","oec","OECD","plyr","dplyr","ggplot2","caret",
       "magrittr","babynames","acs","choroplethr","choroplethrMaps","broom","glmnet","Hmisc",'knitr',
       "ggcorrplot","GGally","ROCR","lattice","car","corrgram","ggcorrplot","sqldf")




list.of.packages <- list
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

if(!require(list)){
  install.packages("list")
  library(list)
}

sapply(list, require, character.only = TRUE)

data=read_csv("C:/Users/Gucci148/Documents/DataMiningscience/southerncompany/southerncompany.csv")


#==================================================================
#display all coumns of data with dplyr
#==================================================================

options(dplyr.width = Inf)


#==================================================================
# replace white spaces with underscores and convert the string to lowercase
#==================================================================

colnames(data)=gsub('([[:punct:]])|\\s+','_',colnames(data))


#==================================================================
# descriptive/summary statistics
#==================================================================

describe(data)

names(data)




knitr::kable(do.call(rbind,apply(data[,-1],2,summary)))

class((apply(data[,-1],2,summary)))

head(data)

glimpse(data)



names(data)

class(describe(data))


apply(data[,-1],2,describe)

knitr::kable(do.call(rbind,apply(data[,-1],2,describe)))

names(data[,-c(1,4,6)])

#==================================================================
# Histograms
#==================================================================

ggplot(data, aes(x =Electricity_Facility__kW__Hourly_ )) + geom_boxplot()




ggplot(data[,-1], aes(x =Electricity_Facility__kW__Hourly_ )) + geom_density()



boxplot(data[,2])

ggplot(data[,-1], aes(x =Gas_Facility__kW__Hourly_ )) + 
  geom_histogram(fill="black",col="black",alpha=0.2,binwidth=0.2) + 
  ggtitle("Gas_Facility__kW__Hourly_") +theme_minimal()


ggplot(data[,-1], aes(x =Heating_Gas__kW__Hourly_ )) + 
  geom_histogram(fill="black",col="black",alpha=0.2,binwidth=0.2) + 
  ggtitle("Heating_Gas__kW__Hourly_") +theme_minimal()

ggplot(data[,-1], aes(x =HVACFan_Fans_Electricity__kW__Hourly_)) + 
  geom_histogram(fill="black",col="black",alpha=0.2,binwidth=0.01) + 
  ggtitle("HVACFan_Fans_Electricity__kW__Hourly_") +theme_minimal()

ggplot(data[,-1], aes(x =Fans_Electricity__kW__Hourly_)) + 
  geom_histogram(fill="black",col="black",alpha=0.2,binwidth=0.01) + 
  ggtitle("Fans_Electricity__kW__Hourly_") +theme_minimal()


ggplot(data[,-1], aes(x =General_ExteriorLights_Electricity__kW__Hourly_)) + 
  geom_histogram(fill="black",col="black",alpha=0.2,binwidth=0.005) + 
  ggtitle("General_ExteriorLights_Electricity__kW__Hourly_") +theme_minimal()


ggplot(data[,-1], aes(x =Misc_InteriorEquipment_Electricity__kW__Hourly_)) + 
  geom_histogram(fill="black",col="black",alpha=0.2,binwidth=0.01) + 
  ggtitle("Misc_InteriorEquipment_Electricity__kW__Hourly_") +theme_minimal()

ggplot(data[,-1], aes(x =Electricity_Facility__kW__Hourly_ )) + 
  geom_histogram(fill="black",col="black",alpha=0.2,binwidth=0.05) + 
  ggtitle("Distribution of Electricity_Facility__kW__Hourly_") +theme_minimal()

ggplot(data[,-1], aes(x =Heating_Electricity__kW__Hourly_ )) + 
  geom_histogram(fill="black",col="black",alpha=0.2,binwidth=0.01) + 
  ggtitle("Heating_Electricity__kW__Hourly_") +theme_minimal()

ggplot(data[,-1], aes(x =Cooling_Electricity__kW__Hourly_ )) + 
  geom_histogram(fill="black",col="black",alpha=0.2,binwidth=0.01) + 
  ggtitle("Cooling_Electricity__kW__Hourly_") +theme_minimal()

ggplot(data[,-1], aes(x =Electricity_HVAC__kW__Hourly_ )) + 
  geom_histogram(fill="black",col="black",alpha=0.2,binwidth=0.05) + 
  ggtitle("Electricity_HVAC__kW__Hourly_") +theme_minimal()


ggplot(data[,-1], aes(x =General_InteriorLights_Electricity__kW__Hourly_)) + 
  geom_histogram(fill="black",col="black",alpha=0.2,binwidth=0.01) + 
  ggtitle("General_InteriorLights_Electricity__kW__Hourly_") +theme_minimal()


ggplot(data[,-1], aes(x =Appl_InteriorEquipment_Electricity__kW__Hourly_)) + 
  geom_histogram(fill="black",col="black",alpha=0.2,binwidth=0.01) + 
  ggtitle("Appl_InteriorEquipment_Electricity__kW__Hourly_") +theme_minimal()




ggplot(data[,-1], aes(x =Water_Heater_WaterSystems_Electricity__kW__Hourly_)) + 
  geom_histogram(fill="black",col="black",alpha=0.2,binwidth=0.01) + 
  ggtitle("Water_Heater_WaterSystems_Electricity__kW__Hourly_") +theme_minimal()


 #==================================================================
    # corellation statistics
#==================================================================





# Corrgram of the training dataset

corrgram(data[,-c(1,4,6)], order=NULL, lower.panel=panel.shade, upper.panel=NULL, text.panel=panel.txt,
         main="Corrgram of the  data")


    glimpse(data)
    
    
    pairs(data[,c(-1)])
    
    cor(data[,-1])
  
    # Correlation plot
    ggcorr(data[,-1], palette = "RdBu", label = TRUE)
    
    
    
    ggcorrplot(data[,-1])
    
    
    ggpairs(
      data[,-1], 
      upper = list(continuous = wrap(ggally_cor, size = 10)), 
      lower = list(continuous = 'smooth')
    )
    
  
    
    png(filename="C:/Users/Gucci148/Documents/DataMiningscience/southerncompany/ggpair.png")
    
    ggpairs(data[,-1])
    
    dev.off()

   ggpairs( data[,-c(1,4,6)])
    
    
    pairs(data[,-1], lower.panel=panel.smooth, upper.panel=panel.plot)
    
    # Compute a correlation matrix
    corrm <- round(cor(data[,-c(1,4,6)]), 1)
    # Compute a matrix of correlation p-values
    p.mat <- cor_pmat(data[,-1])
    require(ggcorrplot)
    # Visualize the correlation matrix
    # --------------------------------
    # method = "square" (default)
    ggcorrplot(corrm)
    
    # Reordering the correlation matrix
    # --------------------------------
    # using hierarchical clustering
    
    ggcorrplot(corrm, hc.order = TRUE, outline.col = "white")
    
    # Types of correlogram layout
    # --------------------------------
    # Get the lower triangle
    ggcorrplot(corrm, hc.order = TRUE, type = "lower",
               outline.col = "white")
    
    # Change colors and theme
    # --------------------------------
    # Argument colors
    ggcorrplot(corrm, hc.order = TRUE, type = "lower",
               outline.col = "white",
               ggtheme = ggplot2::theme_gray,
               colors = c("#6D9EC1", "white", "#E46726"))
    
    
    # Add correlation coefficients
    # --------------------------------
    # argument lab = TRUE
    
    
    png(filename="C:/Users/Gucci148/Documents/DataMiningscience/southerncompany/ggcorplot.pdf")
    
    ggcorrplot(corrm, hc.order = TRUE, type = "lower",
               lab = TRUE)
    
    dev.off()
   
    
    
 #==================================================================
    # box plot
#==================================================================   
    
    
    
    
    #==================================================================
    # scatterplot  matrix
    #==================================================================   
    
    
    
    # Run a scatterplot matrix on the entire dataset
    scatterplotMatrix(~Gas_Facility__kW__Hourly_ +
                        Heating_Gas__kW__Hourly_ +
                        HVACFan_Fans_Electricity__kW__Hourly_ +
                        Fans_Electricity__kW__Hourly_+
                        General_ExteriorLights_Electricity__kW__Hourly_	+ 
                        Misc_InteriorEquipment_Electricity__kW__Hourly_	+
                
                        Electricity_Facility__kW__Hourly_	+
                        Heating_Electricity__kW__Hourly_+
                        Cooling_Electricity__kW__Hourly_+
                        Electricity_HVAC__kW__Hourly_+
                        General_InteriorLights_Electricity__kW__Hourly_+
                        Appl_InteriorEquipment_Electricity__kW__Hourly_+
                        Water_Heater_WaterSystems_Electricity__kW__Hourly_
                      ,data=data, main="Simple Scatterplot Matrix")
    
    
    

    #==================================================================
    # Highest  and lowest  Consumption
    #==================================================================   
    
    
data[ which(data$Electricity_Facility__kW__Hourly_ ==max(data$Electricity_Facility__kW__Hourly_ ))  ,]
    
    
data%>%filter()%>%dplyr::summarise(max1=max(Electricity_Facility__kW__Hourly_ ))
    
data%>%dplyr::summarise(max1=max(Electricity_Facility__kW__Hourly_ ))
    
data %>% dplyr::slice(which.min(Electricity_Facility__kW__Hourly_ ))
    
data %>%dplyr::slice(which.max(Electricity_Facility__kW__Hourly_ ))
    
    
data%>% 
      filter(Electricity_Facility__kW__Hourly_  == min(Electricity_Facility__kW__Hourly_ )) %>% 
      filter(1:n() == 1)
    
    
filter(data, rank(Electricity_Facility__kW__Hourly_, ties.method="first")==1) 
    
filter(data, row_number(Electricity_Facility__kW__Hourly_) == 1)
    
    
data <- dplyr::arrange(data, decreasing(Electricity_Facility__kW__Hourly_))
  

names(data)  





#==================================================================
#Gas_Facility__kW__Hourly_
#==================================================================   




data %>% dplyr::slice(which.min(Gas_Facility__kW__Hourly_ ))

data %>%dplyr::slice(which.max(Gas_Facility__kW__Hourly_ ))

#==================================================================
#Electricity_Facility__kW__Hourly_
#==================================================================   




data %>% dplyr::slice(which.min(Electricity_Facility__kW__Hourly_ ))

data %>%dplyr::slice(which.max(Electricity_Facility__kW__Hourly_ ))


#==================================================================
#Heating_Gas__kW__Hourly_
#==================================================================   




data %>% dplyr::slice(which.min(Heating_Gas__kW__Hourly_ ))

data %>%dplyr::slice(which.max(Heating_Gas__kW__Hourly_ ))




#==================================================================
#Heating_Electricity__kW__Hourly_
#==================================================================   


data %>% dplyr::slice(which.min(Heating_Electricity__kW__Hourly_ ))

data %>%dplyr::slice(which.max(Heating_Electricity__kW__Hourly_ ))






#==================================================================
#HVACFan_Fans_Electricity__kW__Hourly_
#==================================================================   


data %>% dplyr::slice(which.min(HVACFan_Fans_Electricity__kW__Hourly_ ))

data %>%dplyr::slice(which.max(HVACFan_Fans_Electricity__kW__Hourly_ ))




#==================================================================
#Electricity_HVAC__kW__Hourly_
#==================================================================   


data %>% dplyr::slice(which.min(Electricity_HVAC__kW__Hourly_ ))

data %>%dplyr::slice(which.max(Electricity_HVAC__kW__Hourly_ ))




#==================================================================
#HVACFan_Fans_Electricity__kW__Hourly_
#==================================================================   


data %>% dplyr::slice(which.min(Fans_Electricity__kW__Hourly_ ))

data %>%dplyr::slice(which.max(Fans_Electricity__kW__Hourly_ ))


#==================================================================
#General_InteriorLights_Electricity__kW__Hourly_
#==================================================================   


data %>% dplyr::slice(which.min(General_InteriorLights_Electricity__kW__Hourly_ ))

data %>%dplyr::slice(which.max(General_InteriorLights_Electricity__kW__Hourly_ ))



#==================================================================
#General_ExteriorLights_Electricity__kW__Hourly_
#==================================================================   


data %>% dplyr::slice(which.min(General_ExteriorLights_Electricity__kW__Hourly_ ))

data %>%dplyr::slice(which.max(General_ExteriorLights_Electricity__kW__Hourly_ ))



#==================================================================
#Appl_InteriorEquipment_Electricity__kW__Hourly_
#==================================================================   


data %>% dplyr::slice(which.min(Appl_InteriorEquipment_Electricity__kW__Hourly_ ))

data %>%dplyr::slice(which.max(Appl_InteriorEquipment_Electricity__kW__Hourly_ ))


#==================================================================
#Misc_InteriorEquipment_Electricity__kW__Hourly_
#==================================================================   


data %>% dplyr::slice(which.min(Misc_InteriorEquipment_Electricity__kW__Hourly_ ))

data %>%dplyr::slice(which.max(Misc_InteriorEquipment_Electricity__kW__Hourly_ ))


#==================================================================
#Water_Heater_WaterSystems_Electricity__kW__Hourly_
#==================================================================   


data %>% dplyr::slice(which.min(Water_Heater_WaterSystems_Electricity__kW__Hourly_ ))

data %>%dplyr::slice(which.max(Water_Heater_WaterSystems_Electricity__kW__Hourly_))
     

#==================================================================
#Water_Heater_WaterSystems_Electricity__kW__Hourly_
#==================================================================   


data %>% dplyr::slice(which.min(Fans_Electricity__kW__Hourly_))

data %>%dplyr::slice(which.max(Fans_Electricity__kW__Hourly_))

names(data)