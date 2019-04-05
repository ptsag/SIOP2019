# install packages if necessary
################################################

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("readxl")
install.packages("tibble")
install.packages("gridExtra")

# links to tideverse resources
# help(tidyverse) 

# load libraries
################################################ 

library(tidyverse)  # combination of packages
library(ggplot2)    # grammar of graphics 
library(dplyr)      # dplyr provides a grammar (verbs) for data manipulation
library(readxl)     # alternatively, install.packages("readxl")
library(tibble)     # modern reimagining of the data.frame
library(gridExtra)  # misc functions for ''grid graphics'

# get & set working directory 
################################################

getwd()
setwd("/Users/ptsag/SIOP2019/") # create a folder for your files (On a Mac)
# Windows PC, for example "C:/Users/NAME/Documents/" or  "C:/Temp/"

# Download the HCAAF file from the OPM site
# https://www.opm.gov/fevs/reports/data-reports/index-results-by-agency/hcaaf-results-by-agency/2017/2017-hcaaf-report.xls
# Open file in xls, and save a copy as xlsx format in the working directory
# Main page: https://www.opm.gov/fevs/reports/data-reports/

# Human Capital Assessment and Accountability Framework (HCAAF)
# The HCAAF consists of 39 items that are grouped into four indices: 
### Leadership and Knowledge Management
### Results-Oriented Performance Culture
### Talent Management
### Job Satisfaction

# Next, load data from the four worksheets into dataframes
# Each worksheet in the xls file corresponds to an HCAAF index
################################################################

ldrpKM <- read_xlsx("2017-hcaaf-report.xlsx", sheet="HCAAF - Ldrshp & Knowledge Mgmt", 
                    range = "B2:G92", col_names = TRUE, 
                    col_types = c("text","numeric","numeric", "numeric", "numeric", "numeric"))

# you may have noticed warning messages for NA values in xls file
warnings() # we can ignore the warnings

# let's view the dataframe
View(ldrpKM) # warnings are due to NA (missing/bad values)

# Let's continue loading the data from three remaining sheets into dataframes

resOPC <- read_xlsx("2017-hcaaf-report.xlsx", sheet="HCAAF - Results Oriented Perf C", 
                    range = "B2:G92", col_names = TRUE,
                    col_types = c("text","numeric","numeric", "numeric", "numeric", "numeric"))

talMgt <- read_xlsx("2017-hcaaf-report.xlsx", sheet="HCAAF - Talent Management", 
                    range = "B2:G92", col_names = TRUE,
                    col_types = c("text","numeric","numeric", "numeric", "numeric", "numeric"))

jobSat <- read_xlsx("2017-hcaaf-report.xlsx", sheet="HCAAF - Job Satisfaction", 
                    range = "B2:G92", col_names = TRUE,
                    col_types = c("text","numeric","numeric", "numeric", "numeric", "numeric"))


# basic data exploration 
########################

data.class(ldrpKM)
str(ldrpKM)
head(ldrpKM,10)

summary(ldrpKM)
summary(resOPC)
summary(talMgt)
summary(jobSat)

# basic graphs
# help("hist"); help(ggplot)

hist(ldrpKM$'2013', xlim=c(0,100)) 
hist(ldrpKM$'2017', xlim=c(0,100))

ggplot(ldrpKM, aes( ldrpKM$'2013', ldrpKM$'2017')) + 
  xlim(0, 100) + ylim(0, 100) +
  geom_point() + 
  geom_smooth(method = loess, se = F) 
  # loess = curve fitting (method = loess, se = F) 
  # for linear change geom_smooth(method = lm, se = T) 

# more data management 
# add columns to indicate index name & abbreviation # help("add_column")

ldrpKM <- add_column(ldrpKM, index="Leadership & Knowledge Mgmt", index_abbr="Ldshp & KM")
resOPC <- add_column(resOPC, index="Results Oriented Perf Culture", index_abbr="Rslt OPC")
talMgt <- add_column(talMgt, index="Talent Management", index_abbr="Talent Mgmt")
jobSat <- add_column(jobSat, index="Job Satisfaction", index_abbr="Job Sat")

# combine dataframes into new dataframe called 'hcaaf' # help("bind_rows")
hcaaf <- bind_rows (ldrpKM, resOPC, talMgt, jobSat)
hcaaf 

# rename X__1 to agency # help("rename")  
colnames(hcaaf)

# For Windows PCs, replace X__1 with ...1
hcaaf <- hcaaf %>% rename(agency = X__1) # %>% is a pipe operation, to chain 2+ methods without parantheses 

colnames(hcaaf)

data.class(hcaaf)

hcaaf

# more basic graphs
hist (hcaaf$'2017')
summary (hcaaf$'2017')

# filter government wide results and combine to one dataframe # help("filter") 
hcaaf_gvt <- filter(hcaaf, agency=="Governmentwide")
hcaaf_gvt

### even more data management 
#################################

# sort by 2017 index result
hcaaf_gvt_sort <- hcaaf_gvt %>% arrange(desc(hcaaf_gvt$'2017'))
hcaaf_gvt_sort

### create new variables for ggplot
x = hcaaf_gvt$index_abbr
value1 = hcaaf_gvt$'2013'
value2 = hcaaf_gvt$'2017'
diff = value2 - value1
diff_ave = mean(value2 - value1)
diff; diff_ave # show data
hcaaf_gvt # show table

### Charts 
### Lollipop 
################

# first iteration
i1 <- ggplot(hcaaf_gvt) +
  geom_segment( aes(x=x, xend=x, y=value1, yend=value2 ), color="grey") +
  geom_point( aes(x=x, y=value1), color="grey", size=3 ) +
  geom_point( aes(x=x, y=value2), color="black", size=3 ) +
  coord_flip() + # flips chart horizontally 
  xlab("") +
  ylab("HCAAF Index Results - Governmentwide - 2013-2017")
i1 # show graph

# second interation
i2 <- i1 +  theme_light() +
  theme(legend.position = "bottom", panel.border = element_blank())
i2 # show graph

# third interation
i3 <- i2 + labs(title = "HCAAF Index Results - Governmentwide",
           subtitle = "A lollipop chart is a hybrid of the bar chart and Cleveland dot plot",
           caption = "Source: OPM") 
i3 # show graph

grid.arrange(i1, i2, i3, nrow=1)
# and click zoom to see plots

# data visualization improvements 
#################################

# first attempt - full scale 0-100
i4a <- ggplot(hcaaf_gvt) +
  geom_segment( aes(x=reorder(hcaaf_gvt$index, -hcaaf_gvt$'2017' ), xend=hcaaf_gvt$index, y=hcaaf_gvt$'2013', yend=hcaaf_gvt$'2017' )) +
  geom_point( aes(x=hcaaf_gvt$index, y=hcaaf_gvt$'2013' ), color="grey", size=3 ) +
  geom_point( aes(x=hcaaf_gvt$index, y=hcaaf_gvt$'2017' ), color="black", size=3) +
  coord_flip() +  
  theme_light() +
  theme(legend.position = "bottom", panel.border = element_blank()) +
  xlab("") +
  ylab("") +
  ylim(0, 100) +
  labs(title = "HCAAF Index Results - Governmentwide",
       subtitle = "A lollipop chart is a hybrid of the bar chart and Cleveland dot plot",
       caption = "Source: OPM") 
i4a # show graph

# second attempt
i4b <- ggplot(hcaaf_gvt) +
  geom_segment( aes(x=reorder(hcaaf_gvt$index, -hcaaf_gvt$'2017'), xend=hcaaf_gvt$index, y=hcaaf_gvt$'2013', yend=hcaaf_gvt$'2017' )) +
  geom_point( aes(x=hcaaf_gvt$index, y=hcaaf_gvt$'2013' ), color="grey", size=5 ) +
  geom_point( aes(x=hcaaf_gvt$index, y=hcaaf_gvt$'2017' ), color="black", size=5 ) +
  coord_flip() +  
  theme_minimal() +
  theme(legend.position = "bottom", panel.border = element_blank()) +
  xlab("") +
  ylab("") +
  ylim(50, 70) +
  labs(title = "HCAAF Index Results - Governmentwide",
        subtitle = "A lollipop chart is a hybrid of the bar chart and Cleveland dot plot",
        caption = "Source: OPM") 
i4b # show graph and click zoom to see plots

# ggplot2 themes: https://ggplot2.tidyverse.org/reference/ggtheme.html

### Combined agency type and index 
#######################################

hcaaf_agencyType_1vl <- filter(hcaaf, agency=="Very Large Agencies Overall")
hcaaf_agencyType_2lg <- filter(hcaaf, agency=="Large Agencies Overall")
hcaaf_agencyType_3md <- filter(hcaaf, agency=="Medium Agencies Overall")
hcaaf_agencyType_4sm <- filter(hcaaf, agency=="Small Agencies Overall")
hcaaf_agencyType_5vs <- filter(hcaaf, agency=="Very Small Agencies Overall")

# help("bind_rows")
hcaaf_agency_type <- bind_rows (hcaaf_agencyType_1vl, hcaaf_agencyType_2lg, hcaaf_agencyType_3md, hcaaf_agencyType_4sm, hcaaf_agencyType_5vs)
hcaaf_agency_type

####################################
### Lollipop 2 #####################
### Small multiples ################
####################################

# At the heart of quantitative reasoning is a single question: Compared to what? -Edward Tufte

x = paste(hcaaf_agency_type$index)
value1 = hcaaf_agency_type$'2013'
value2 = hcaaf_agency_type$'2017'

diff = value2 - value1
diff_ave = mean(value2 - value1)
diff; diff_ave

# good
i5a <- ggplot(hcaaf_agency_type) +
  geom_segment(aes(x=reorder(hcaaf_agency_type$index, -hcaaf_agency_type$'2017'), xend=x, y=value1, yend=value2 ), color="grey") +
  geom_point( aes(x=x, y=value1), color="grey", size=2 ) +
  geom_point( aes(x=x, y=value2), color="black", size=2 ) +
  coord_flip()+
  theme_light() +
  ylim(0, 100) +
  labs(title = "HCAAF Index Results - Governmentwide - 2013-2017",
       subtitle = "Small panels. Multple panels. Small multiples visually enforce comparisons...",
       caption = "Source: OPM") +
  theme_minimal() +
  xlab("") +
  ylab("") +
  facet_wrap(~ hcaaf_agency_type$agency, ncol=1)
i5a # show graph and click zoom to see plots

# better, sorted in by agency size
i5b <- ggplot(hcaaf_agency_type) +
  geom_segment(aes(x=reorder(hcaaf_agency_type$index, -hcaaf_agency_type$'2017'), xend=x, y=value1, yend=value2 ), color="grey") +
  geom_point( aes(x=x, y=value1), color="grey", size=2 ) +
  geom_point( aes(x=x, y=value2), color="black", size=2 ) +
  coord_flip()+
  theme_light() +
  ylim(0, 100) +
  labs(title = "HCAAF Index Results - Governmentwide - 2013-2017",
       subtitle = "Small panels. Multple panels. Small multiples visually enforce comparisons...",
       caption = "Source: OPM") +
  theme_minimal() +
  xlab("") +
  ylab("") +
  facet_wrap(~ hcaaf_agency_type$agency, ncol=1)

hcaaf_agency_type$agency <- factor(hcaaf_agency_type$agency, 
                                levels=c("Very Large Agencies Overall", 
                                  "Large Agencies Overall", 
                                  "Medium Agencies Overall", 
                                  "Small Agencies Overall", 
                                  "Very Small Agencies Overall"))
                 
i5b # show graph and click zoom to see plots

# the end... or just the beginning 

# books on R and data visualization
################################################

# R for data science: https://r4ds.had.co.nz/index.html
# Fundamentals of data visualization: https://serialmentor.com/dataviz/ 
# The R graph gallery: https://www.r-graph-gallery.com 




