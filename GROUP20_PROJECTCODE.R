

#=======================================================================================


install.packages("pacman")

pacman::p_load(
  tidyverse, 
  haven, 
  ggplot2,
  dplyr,
  gtsummary, # get summary statistics and tests
  janitor, # create pretty and customizable tables
  flextable, # let you create prettier tables
  skimr # gives you another way to get an overview of the data
)

# Load data
chs2019 <- read_sas("chs2019_public.sas7bdat")

#data cleaning 
chs2019NA <- chs2019 %>% 
  select(birthsex,imputed_neighpovgroup4_1418,bingenew,agegroup5,newrace6) %>% 
  drop_na()


# Change numerical category indicators to appropriate written labels matching data
chs2019clean <- chs2019NA %>% 
  select(birthsex, imputed_neighpovgroup4_1418, bingenew,agegroup5,newrace6) %>% 
  mutate(birthsex_chr = case_when(birthsex == 1 ~ 'Male',
                                  birthsex == 2 ~ 'Female')) %>%
  mutate(neighpovgroup_chr = case_when(imputed_neighpovgroup4_1418 == 1 ~ '0-<10% (low pov)',
                                       imputed_neighpovgroup4_1418 == 2 ~ '10-<20% (medium pov)',
                                       imputed_neighpovgroup4_1418 == 3 ~ '20-<30% (high pov)',
                                       imputed_neighpovgroup4_1418 == 4 ~ '30-<100% (very high pov)')) %>%
  mutate(bingenew_chr = case_when(bingenew == 1 ~ 'Binge drinking',
                                  bingenew == 2 ~ 'Non-binge drinking')) %>%
  mutate(agegroup5_chr = case_when(agegroup5 == 1 ~ '18-24',
                                   agegroup5 == 2 ~ '25-29',
                                   agegroup5 == 3 ~ '30-44',
                                   agegroup5 == 4 ~ '45-64',
                                   agegroup5 == 5 ~ '65+'))  %>%
  mutate(newrace6_chr = case_when(newrace6 == 1 ~ 'White Non-Hispanic',
                                   newrace6 == 2 ~ 'Black Non-Hispanic',
                                   newrace6 == 3 ~ 'Hispanic',
                                   newrace6 == 4 ~ 'Asian/PI Non-Hispanic',
                                   newrace6 == 5 ~ 'N.African/Middle Eastern, non-Hispanic',
                                   newrace6 == 6 ~ 'Other Non-Hispanic')) 

#------ Descriptive Statistic variables -------# 

# Calculate the percentage of answering "yes" or "no" to the question of whether
# have binge drank in the last 30 days by neighborhood poverty group 

chs2019clean %>% 
  group_by(neighpovgroup_chr)  %>% 
  count(pcp = factor(bingenew_chr)) %>% 
  mutate(pct = prop.table(n))


# Calculate the percentage of answering "yes" or "no" to the question of whether
# have binge drank in the last 30 days  by sex 

chs2019clean %>% 
  group_by(birthsex_chr)  %>% 
  count(mamm = factor(bingenew_chr)) %>% 
  mutate(pct = prop.table(n))

## Create a subset of data with needed categorical variables for creating summary statistics table using tbl_summary()
chs2019table <- chs2019clean %>% select(birthsex_chr, neighpovgroup_chr, bingenew_chr, agegroup5_chr, newrace6_chr)

### Table 1: create summary/descriptive analysis table
# use of list() to rename label names
chs2019table %>%   
  tbl_summary(label = list(bingenew_chr ~ 'Had binge drinking in the past 30 days?', 
                           neighpovgroup_chr ~ 'Neighborhood Poverty Groups',
                           birthsex_chr ~ 'Sex',
                           agegroup5_chr ~ 'Age',
                           newrace6_chr ~ 'Race'),
              type = list(bingenew_chr ~ "categorical"), ) %>%
  modify_caption("**Table 1. Univariate Descriptive Analysis of Binge Drinking, Neighborhood Poverty Groups, and Sex**") %>%
  as_gt() %>%
  gt::tab_source_note(gt::md("*Source: New York City Community Health Survey, 2019*"))


#---------- Analysis  -----------#

#First Graph to see the percentage of binge drinking stratified in different sex and in different
#neighborhoods 

chs2019percentage <- chs2019clean %>% 
  group_by(birthsex_chr, neighpovgroup_chr, bingenew_chr) %>% 
  summarise(N=n()) %>% # get # of binge drinking under each subgroup of sex and neighpov
  ungroup() %>% # ungroup the variables above for the next grouping
  group_by(birthsex_chr, neighpovgroup_chr) %>% 
  mutate(Total=sum(N),Percent=N/Total, # get total # of observations in each subgroup of the two variables to calculate binge drinking percentage
         Lab=paste0(paste0(round(100*Percent,2),'%')))

ggplot(chs2019percentage,aes(x=birthsex_chr,y=N,fill=bingenew_chr))+
  geom_bar(stat='identity', position = 'fill') + # stat = "identity" set y-axis as a specified variable in the data, set position = "fill" to make comparison of proportions easier
  facet_wrap(.~neighpovgroup_chr,scales = 'free') + # create multi-panel plots
  scale_y_continuous(labels = scales::percent) + # show % on y-axis
  geom_text(aes(label=Lab),position = position_fill(vjust = 0.5), size = 2.5) + # add percentage text to the plots
  labs(title = "Binge drinking status by neighborhood poverty level \n among men and women in NYC, 2019",
       subtitle = "Men in medium poverty neighborhoods have the \n highest rates of binge drinking.",
       y = "Percentage",
       x = "Sex",
       fill = "Binge Drinking Status", caption = "Source: New York City Community Health Survey, 2019")


#-------------------------------------#
#Second Graph: binge drinking in poverty neighbourhoods

chs2019pov <- chs2019clean %>% group_by(neighpovgroup_chr, bingenew_chr) %>% # group these two variables together to create summary
  summarise(N = n()) %>% # generate counts for the combination of these two variables using n()
  mutate(percentage = N/sum(N), 
         lab = paste0(round(100*percentage,2),'%'))# create a variable 'lab' to store text labels for the plots later

chs2019pov

chs2019pov %>% ggplot(aes(x = bingenew_chr, y = percentage, fill = bingenew_chr)) + # specify x values as the levels inbinge drinking, y is the percentage of each level in binge drinking
  geom_bar(stat = "identity") + # specifying stat="identity" tells R that you will manually provide the y values.
  # the default argument is stat="count" which will count the freq of each x value on the y-axis.
  facet_wrap(.~neighpovgroup_chr) + # get each plot of the percentage of binge drinking for different categories in neighpovgroup
  geom_text(aes(label=lab),vjust=1.5, size = 3) + # add labels of the percentages in the plot
  scale_y_continuous(labels = scales::percent) + # change the y-axis to percentage
  labs(x = "Binge Drinking", title = "Binge drinking status by neighborhood poverty level in adults (18+) \n in NYC, 2019", subtitle = "Adults in very high poverty neighborhoods had the lowest rates of binge drinking.", fill = "Binge Drinking Status", caption = "Source: New York City Community Health Survey, 2019") # change the y-axis to percentage


#---------#
#Third graph : Binge drinking in different sex (birthsex)

chs2019sex <- chs2019clean %>% group_by(birthsex_chr, bingenew_chr) %>% # group these two variables together to create summary
  summarise(N = n()) %>% # generate counts for the combination of these two variables using n()
  mutate(percentage = N/sum(N), 
         lab = paste0(round(100*percentage,2),'%'))# create a variable 'lab' to store text labels for the plots later

chs2019sex

chs2019sex %>% ggplot(aes(x = bingenew_chr, y = percentage, fill = bingenew_chr )) + # specify x values as the levels in binge drinking, y is the percentage of each level in binge drinking
  geom_bar(stat = "identity") + # specifying stat="identity" tells R that you will manually provide the y values.
  # the default argument is stat="count" which will count the freq of each x value on the y-axis.
  facet_wrap(.~birthsex_chr) + # get each plot of the percentage of binge drinking for different categories in birth sex
  geom_text(aes(label=lab),vjust=1.5, size = 3) + # add labels of the percentages in the plot
  scale_y_continuous(labels = scales::percent) + # change the y-axis to percentage
  labs(x = "Binge Drinking", title = "Binge drinking disparities by sex, NYC, 2019", subtitle = "Men have higher rates of binge drinking compared to women.", fill = "Binge Drinking Status", caption = "Source: New York City Community Health Survey, 2019") # change the y-axis to percentage




#------ ODDS RATIO ------ #

install.packages("mStats")

library(mStats)


#Mantel Haenszel Odds Ratio (works) (if x is birthsex, y is bingenew, and z is neighpov group)
table(chs2019clean$birthsex_chr, chs2019clean$bingenew_chr)
mhor(chs2019clean,
     birthsex_chr,
     bingenew_chr,
     strata = neighpovgroup_chr,
     
     digits = 2
)


#convert the neighpov to binary, followed the class 25 code 
#grouping the low and mid pov groups together and the high and very high pov group together
summary(chs2019$imputed_neighpovgroup4_1418)
chs2019$POVcat <-rep("",length(chs2019$imputed_neighpovgroup4_1418))
chs2019$POVcat[chs2019$imputed_neighpovgroup4_1418 <=2] <-"Poverty Level 1 and 2"
chs2019$POVcat[chs2019$imputed_neighpovgroup4_1418 >=3] <-"Poverty Level 3 and 4"
chs2019$POVcat[is.na(chs2019$imputed_neighpovgroup4_1418)] <- NA
chs2019$POVcat <-factor(chs2019$POVcat)
summary(chs2019$POVcat)

#converting the y variable into binary 
summary(chs2019$bingenew)
chs2019$BINGEcat <-rep("",length(chs2019$bingenew))
chs2019$BINGEcat[chs2019$bingenew ==1] <-"Yes"
chs2019$BINGEcat[chs2019$bingenew ==2] <-"No"
chs2019$BINGEcat[is.na(chs2019$bingenew)] <- NA
chs2019$BINGEcat <-factor(chs2019$BINGEcat)
summary(chs2019$BINGEcat)

#converting the z variable into binary 
summary(chs2019$birthsex)
chs2019$SEXcat <-rep("",length(chs2019$birthsex))
chs2019$SEXcat[chs2019$birthsex ==1] <-"Male"
chs2019$SEXcat[chs2019$birthsex ==2] <-"Female"
chs2019$SEXcat[is.na(chs2019$birthsex)] <- NA
chs2019$SEXcat <-factor(chs2019$SEXcat)
summary(chs2019$SEXcat)

#Mantel Haenszel Odds Ratio where x is neighpovgroup and y is bingenew and z is birthsex
mhor(chs2019,
     POVcat,
     BINGEcat,
     strata = SEXcat,
     
     
     digits = 2
)








