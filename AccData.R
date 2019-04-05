# Download the Accessions file from the OPM site
# https://www.opm.gov/Data/Files/546/f2677429-21d2-4f7f-9dca-a8c2c524df2f.zip 

# Unzip files and place in working directory

# install packages if necessary
################################################
# install.packages("tidyverse")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("readxl")
# install.packages("tibble")
# install.packages("gridExtra")
# library(gridExtra)

# links to tideverse resources
# help(tidyverse) 

# load libraries
################################################ 

library(tidyverse)  # combination of packages
library(ggplot2)    # grammar of graphics 
# library(gridExtra)  # misc functions for ''grid graphics'
# library(grid)
# library(graphics)
# library(lattice)
# library(dplyr)      # dplyr provides a grammar (verbs) for data manipulation
# library(readxl)     # alternatively, install.packages("readxl")
# library(tibble)     # modern reimagining of the data.frame

# get & set working directory 
getwd()
setwd("/Users/ptsag/SIOP2019/FedScope Accessions Cube (FY 2011 - FY 2017)") # create a folder for your files (On a Mac)
# Windows PC, for example "C:/Users/NAME/Documents/" or  "C:/Temp/"

# Next, load data in a dataframe
################################################################

# accessions <- readr::() # get tiddy
joiners <- read_csv("ACCDATA_FY2011-2017.txt", col_names = T)

# let's view the dataframe acc
head(joiners) # notice salary and length of service are text (character) #help(cols)

# transform text fields to numeric
joiners$SALARY <- as.numeric(as.character(joiners$SALARY)) #help(as.numeric)
joiners$LOS <- as.numeric(as.character(joiners$LOS))

# let's review the dataframe to check SALARY and LOS
summary(joiners) # max of length of service is 65 years

# let's pull up the first 20 rows
head(joiners,20) 

# basic graphs
############################

graphics.off() # clear workspace

# bar chart of work schedules 

barWORKSCH <- ggplot(data = joiners) + geom_bar(mapping = aes(x = joiners$WORKSCH))
barWORKSCH <- barWORKSCH + scale_y_continuous(labels = scales::comma)
barWORKSCH
  # from the DTwrksch.text file, we see F = Full-time Nonseasonal

barACC <- ggplot(data = joiners) + geom_bar(mapping = aes(x = joiners$ACC))
barACC <- barACC + scale_y_continuous(labels = scales::comma)
barACC
  # DTacc.txt file, we find descriptions for accession types
    ### AA,Transfer In - Individual Transfer
    ### AB,Transfer In - Mass Transfer
    ### AC,New Hire - Competitive Service Appointment
    ### AD,New Hire - Excepted Service Appointment
    ### AE,New Hire - Senior Executive Service (SES) Appt

bar_ACCxWORKSCH <- ggplot(joiners, aes(joiners$ACC))
bar_ACCxWORKSCH + geom_bar(aes(fill = joiners$WORKSCH))

# frequency tables for work schedule and accession type
crosstab <- table(joiners$WORKSCH,joiners$ACC); crosstab # print table 

hist(joiners$LOS, xlim=c(0,70)) 
hist(joiners$SALARY) # review histogram, axes, any outliers? number of classes?
s
# add filter for work schedule and accesstion type
joiners_f <- joiners[joiners$WORKSCH == "F" & joiners$ACC %in% c("AC", "AD"), ] # subset to 1.2m rows (was 1.6m)

# bar chart of accession types
##############################

barACC_f <- ggplot(data = joiners_f) + geom_bar(mapping = aes(x = joiners_f$ACC))
barACC_f <- barACC_f + scale_y_continuous(labels = scales::comma)
barACC_f

summary(joiners_f$SALARY)

# create bow & whisker for salary by accession type
bw_SAL <- ggplot(data = joiners_f, mapping = aes(x = joiners_f$ACC, y = joiners_f$SALARY)) + 
  geom_boxplot() +
  coord_flip()
bw_SAL <- bw_SAL + scale_y_continuous(labels = scales::comma)
bw_SAL


# books on R and data visualization
################################################

# R for data science: https://r4ds.had.co.nz/index.html
# Fundamentals of data visualization: https://serialmentor.com/dataviz/ 
# The R graph gallery: https://www.r-graph-gallery.com 




