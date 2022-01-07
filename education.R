
library("tidyverse")

# Loaded from my computer

education <- read_csv("C:/Users/Vic/Desktop/R_WORK/PERSONALPROJECTS/Education/data_sets/primary/pupil-enrollment-2010-2015.csv") 


view(education)

summary(education)

#no of years collected
unique(education$Year)

unique(education$`Name of district`)
#they are 197 districts

'''
possible number of questions

1. What is the time range when the data was collected?
2. What is the total number of districts whose data was collected?
3. Find the total number of boys enrolled in all the five years?
4. Find the number of girls enrolled in p.1 in 2010, 2011, 2013, 2014, and 2015
5. How many boys were enrolled in p2  in 2010, 2011, 2013, 2014
6. which type of the school has the number of boys enrolled in p1?
7. which type of the school has the number of girls enrolled in p1

'''

#rename_district <- plyr::rename(education,  c("Lira"="LIRA",
#                                           "Moroto"= "MOROTO"))

#rename_district

district_to_caps <- toupper(education$`Name of district`)
district_to_caps

no_of_dist <- unique(district_to_caps)
no_of_dist
 #no of districts is 117


total_boys_allYears <- education %>% summarise(total_boys =sum(`Total Boys Enrollment`, na.rm =T)) 
total_boys_allYears
#They are 3818985 boys

 total_boys <- sum(education$`Total Boys Enrollment`, na.rm =T)
 total_boys
 #They are 3818985 boys
 

total_girls =sum(education$`Total Girls Enrollment`, na.rm =T)
total_girls
#They are 3783398 gals 

#bar graph of boys and gals
total_pupils <- c(total_boys , total_girls) #these are vector points, hence they have the same data type hence calling them with c() fn

total_pupils
label <- c("tota no of boys", "total no of girls")
pie(total_pupils ,labels =label,  main = 'Total boys against girls',  col = rainbow(length(total_pupils)))


gals_Allyrs_p1 <- education %>%  
  group_by(Year) %>%
  summarise(total_p1_gals= sum(`Total P1 Girls Enrollment`, na.rm = T)) 

gals_Allyrs_p1

gals_Allyrs_p1[gals_Allyrs_p1$Year %in% c("2010", "2012" ), ]

#p1 gals in 2010 and 2012 who enrolled
gals_10_12_p1 <- gals_Allyrs_p1[c(1,3), ]
gals_10_12_p1


# Find the number of girls enrolled in p.1 in 2010,
#na.rm =T ie not available .remove = True
gals_2010_p1 <- education %>%  
  group_by(Year) %>%
  summarise(total_p1_gals= sum(`Total P1 Girls Enrollment`, na.rm = T)) %>%
  filter(Year == 2010)
gals_2010_p1
 #There are 52019 girls who enrolled in  p1 in 2010




#5. How many boys were enrolled in p2  in 2010, 2011, 2013, 2014, 2015
p2_boys_allYears <- education %>%
  group_by(Year) %>%
  summarise(Total_Boy_p2_Count = sum(`Total P2 Boys Enrollment`, na.rm = T))
p2_boys_allYears
                  


#enrolled boys


highest_std <- education %>% 
  group_by(Year) %>%
  summarise(stds_per_year = sum(`Total Enrollment`, na.rm = T)) %>%

#bar graph to show stds _per_year vs years of enrollment
  ggplot(aes(x =Year, y =stds_per_year , fill = Year)) + 
  geom_bar(stat = "identity")

highest_std

#year that had the highest no of students enrolled
highest_std[highest_std$stds_per_year == max(highest_std), ]
# it is 2015   with this    2033539

#which year had the highest number  of gals enrolled.
highest_std_gals <- education %>% 
  group_by(Year) %>%
  summarise(gals_per_year = sum(`Total Girls Enrollment`, na.rm = T)) 
highest_std_gals

highest_std_gals[highest_std_gals$gals_per_year == max(highest_std_gals), ]
