############################################################################################################
#Script to summarise the dataset of *RAW DATA* Lifewatch C-pod project (2016-2022) including only H porpoise detections
#The datatset contains multiple types of variables (spatio-temporal, technical, and primary)
#The primary variables are of interest as they describe the processed data
#****Version only including the final figures****
#Date: 02/08/2023
#Author: Patricia Navarro Gonzalez
#Steps:
##1-Import the libraries
##2. Import the data
##3. Data manipulation
##4. Deployments
##5. Lost minutes
##6. Plots
############################################################################################################

#clean the workspace
rm(list = ls(all.names = TRUE))


# 1. Import libraries -----------------------------------------------------
library(lwdataexplorer)#Library of the Lifewatch Project
library(dplyr)#pipes functions
library(lubridate)#year functon
library(ggplot2)#plots
library(stringr)#add italics to axes (bquote)
library(gridExtra)#for manipulation of maps


# 2. Import data ----------------------------------------------------------
#Raw data
cpod_raw<-getCpodData("2016-01-01","2023-01-01", processing = "Raw", quality=c("Hi","Mod"), by="1 week")#function to retrieve the data for the C-pod project (Cetacean Acoustic Hydrophone Network)

#cpod_raw <- read_csv("csv/BCPAN NBHF 2016-2022_1.csv")



# 3.  Data manipulation ----------------------------------------------------------------
##3.1. Filter for H. porpoise----
filter<-cpod_raw %>% 
  filter(species=="NBHF")


##3.2. Correct the time and create the total sums----
filter$time<-as_date(filter$time)
#extract the year from the date
#filter$year <- year(filter$time)

#capitalize column names
names(filter) <- toupper(names(filter))  


cpod_sum<-filter %>% 
  select(STATION, DEPLOYMENT_FK, YEAR, RECORDED, DPM, LOST_MINUTES, MOORING_TYPE) %>% 
  group_by(STATION, DEPLOYMENT_FK, MOORING_TYPE) %>% 
  filter(RECORDED>0)

#Number unique deployments
length(unique(cpod_sum$DEPLOYMENT_FK))#127

#Total recorded
sum_rec<-sum(cpod_sum$RECORDED)
sum(cpod_raw$RECORDED)
#Total DPMs
sum_DPM<-sum(cpod_sum$DPM)
#Total lost min
sum_lm<-sum(cpod_sum$LOST_MINUTES)

#pct of lost minutes vs RECORDED
pct_lm<-(sum_lm/sum_rec)*100
#pct of avergae lost minutes
avg_lm<-(cpod_sum$LOST_MINUTES/cpod_sum$RECORDED)*100
valid <- avg_lm[is.finite(avg_lm)]
avg_lm<-mean(valid)
#pct of mean surface lost minutes
avg_surf<-cpod_sum %>% 
  group_by(MOORING_TYPE) %>% 
  summarize(LOST_MINUTES=sum(LOST_MINUTES),
            RECORDED=sum(RECORDED),
            avg= mean((LOST_MINUTES/ RECORDED) * 100))


##3.3. Correct the station names
#Group the data by station to summarise it and arrange it alphabetically
grouped <- filter %>% 
  group_by(YEAR, STATION)%>% 
  summarise() 
grouped <-grouped %>% 
  group_by(YEAR)%>% 
  arrange(STATION)

# Define a correction dictionary to map misspelled station names to their correct versions
#Extract all the station names
STATION<-unique(grouped$STATION)
STATION
correction_dict <- c("AP-bpns-Belwind" = "bpns-Reefballs Belwind",
                     "AP-bpns-Birkenfels" = "bpns-Birkenfels",
                     "AP-bpns-Cpower" = "bpns-Reefballs-cpower", 
                     "AP_bpns-Grafton" = "bpns-Grafton", 
                     "bpns-oostdyck west" ="Oostdyck west")

#Correct misspelled STATION names
corrected <- filter %>%
  mutate(STATION = if_else(STATION %in% names(correction_dict), correction_dict[STATION], STATION)) %>% 
  mutate(STATION = if_else(str_starts(STATION, "bpns"), str_replace(STATION, "^bpns-", ""), STATION))


#Add a 4 letter code to the dataset for each of the STATIONs
corrected$code <- ifelse(corrected$STATION == 'Reefballs Belwind', 'ReB',
                         ifelse(corrected$STATION == 'Reefballs-cpower', 'ReC',
                                ifelse(corrected$STATION == 'Oostendebank Oost', 'OostO',
                                       ifelse(corrected$STATION == 'Oostdyck west', 'OostW',
                                              substr(corrected$STATION, 1, 4)))))




# 4. Summary of the deployments -------------------------------------------
dep<-corrected %>% 
  group_by(STATION, DEPLOYMENT_FK) %>% 
  select(STATION, DEPLOY_DATE_TIME, DEPLOYMENT_FK, VALID_DATA_UNTIL_DATETIME) %>% 
  distinct()

##summary
#Sumary of unique number of deployments per YEAR using their indv code
dep_summary <- corrected %>%
  group_by(STATION) %>%
  summarise(total_deployments = n_distinct(DEPLOYMENT_FK)) %>%
  arrange(total_deployments)  # Arrange STATIONs in ascending order of total deployments


dep <- merge(dep, dep_summary, by = "STATION", all.x = TRUE)

##Corrections for the timeline of data available
dep$DEPLOY_DATE_TIME<- as.POSIXct(dep$DEPLOY_DATE_TIME)
dep$VALID_DATA_UNTIL_DATETIME<- as.POSIXct(dep$VALID_DATA_UNTIL_DATETIME)








#5. Summary of the lost minutes -------------------------------------------
#Total of lost minutes 
mins<- corrected %>%
  group_by(code, MOORING_TYPE) %>% 
  #select(station, YEAR, LOST_MINUTES, RECORDED) %>% 
  select(LOST_MINUTES, RECORDED, code, MOORING_TYPE) %>% 
  summarise(lost_sum = sum(LOST_MINUTES),
            rec_sum = sum(RECORDED),
            prop= (lost_sum/ rec_sum)*100) #Proportion of lost minutes over total minutes

#mean pct of lost minutes
mean(mins$prop)

  
# Filter out stations with a proportion of 0
prop_filter <- mins %>%
  #group_by(YEAR) %>% 
  filter(prop > 0) %>% 
  arrange(desc(prop)) %>%
  ungroup()

str(prop_filter)
prop_filter$MOORING_TYPE<-as.factor(prop_filter$MOORING_TYPE)
#reorder the factors levels
prop_filter$MOORING_TYPE <- factor(prop_filter$MOORING_TYPE, levels = c("surface-buoy", "bottom-mooring"))
prop_filter$MOORING_TYPE <- recode(prop_filter$MOORING_TYPE,
                                   "surface-buoy" = "Surface mooring (2016-2018)",
                                   "bottom-mooring" = "Bottom mooring (2018-2022)")

##Table of the codes
table<-corrected %>% 
  distinct(STATION, code) %>% 
  arrange(STATION)
table<-table %>% 
  unique()

colnames(table)<-c("Station", "Code")

write.csv(table, "table_codes.csv")



#6. Plots -------------------------------------------

#customised theme for final figures
theme_rbook <- function(base_size = 17, base_family = "Helvetica", base_line_size = base_size/22, 
                        base_rect_size = base_size/22) {         
  theme( 
    axis.title = element_text(size = 21, family = "Helvetica"),                               
    axis.text= element_text(size = 19, family = "Helvetica"),                            
    plot.caption = element_text(size = 19, face = "italic", family = "Helvetica"),            
    panel.background = element_rect(fill="white"),                      
    axis.line = element_line(size = 1, colour = "black"),
    strip.background =element_rect(fill = "white"),
    panel.border = element_rect(colour = NA, fill=NA, size=0.5),
    strip.text = element_text(size=17, colour = "black", family = "Helvetica"),
    legend.key=element_blank(), 
    legend.title = element_text(size=19, family = "Helvetica"), 
    legend.text = element_text(size=19, family = "Helvetica"),
    #panel.grid.major.y = element_line(colour = "grey83"),
    #panel.grid.minor.y = element_line(colour = "grey83"),
    plot.background = element_blank()
  )
}


##6.1. Deployment timeline as a summary of data availability
#Using the period between deployment and recovery dates
# Open the pdf save function

setwd("C:/Users/arienne.calonge/OneDrive - VLIZ/Ari/C-POD paper/R/plots")
pdf(file = "~/data_availability.pdf",   # The directory you want to save the file in
    width =11.4 , # The width of the plot in inches
    height = 8) # The height of the plot in inches
#Size of an A4 : 8.3 x 11.7 

#Run to develop png file
#png(file = "data_availability.png",  # The directory you want to save the file in
#width = 1488, height = 688, units = "px")


p <- ggplot(dep) +
  geom_segment(aes(y = reorder(STATION, desc(total_deployments)), x = DEPLOY_DATE_TIME, xend = VALID_DATA_UNTIL_DATETIME, yend=reorder(STATION, desc(total_deployments))),
               linewidth = 8) +
  labs(x = "Year", y = "Station within the BPNS") +
  scale_x_datetime(limits = as.POSIXct(as.Date(c("2016-01-01", "2023-01-01"))), date_breaks = "1 year", date_labels = "%Y") +
  theme_rbook()

ggsave("~/data_availability.pdf")

dev.off()

ggsave("data_availability.tiff", dpi=300, width = 15.5, height = 7.1, device='tiff') 




##6.2. Lost mnutes as a percentage without years 
#(go back to point 5 and remove the years)
# Open the pdf save function
# pdf(file = "~/c-pod paper/lost minutes raw.pdf",   # The directory you want to save the file in
#     width =14 , # The width of the plot in inches
#     height = 8) # The height of the plot in inches
#Size of an A4 : 8.3 x 11.7 

#Run to develop png file
#png(file = "~/lost minutes raw.png",  # The directory you want to save the file in
#    width = 1488, height = 688, units = "px")

lost_graph<-ggplot(prop_filter, aes(x = reorder(factor(code), desc(prop)), y = prop, fill=factor(MOORING_TYPE))) +
  geom_bar(position ="stack", stat = "identity", width = 0.6)+ # Adjust width for space between bars and width of bars
  labs(x = "Station within the BPNS",
       y = "Total percentage of lost minutes") +
  scale_fill_grey()+
  facet_wrap(~ MOORING_TYPE, ncol=1, scales = "free_x") +#use the theme rbook with the increase font sizes and removal of grids
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "none")+
  theme_rbook()

#add a table of the codes
lost_table <- tableGrob(table, rows=NULL, theme = ttheme_minimal(base_size = 15, base_family = "Helvetica", 
                                                                 core=list(fg_params=list(hjust=1, x=0.95)),
                                                                 colhead=list(fg_params=list(hjust=1, x=0.95))))

grid.arrange(lost_graph,lost_table, ncol = 2, widths = c(6, 2))


ggsave("lost_minutes.jpeg", dpi=300, width = 15.5, height = 7.1, device='jpeg') 





