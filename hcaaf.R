install.packages("tidyverse")
install.packages("gridExtra")

# help(tidyverse) review library resources, eg tibble

library(tidyverse)
library(dplyr)    # dplyr provides a grammar (verbs) for data manipulation
library(readxl)   # alternatively, install.packages("readxl")
library(tibble)   # modern reimagining of the data.frame
library(gridExtra)

getwd()

setwd("/Users/ptsag/SIOP2019/")

# Open file in xls, save as xlsx, load data from four sheets into dataframes

ldrpKM <- read_xlsx("2017-hcaaf-report.xlsx", sheet="HCAAF - Ldrshp & Knowledge Mgmt", 
                    range = "B2:G92", col_names = TRUE, 
                    col_types = c("text","numeric","numeric", "numeric", "numeric", "numeric"))

resOPC <- read_xlsx("2017-hcaaf-report.xlsx", sheet="HCAAF - Results Oriented Perf C", 
                    range = "B2:G92", col_names = TRUE,
                    col_types = c("text","numeric","numeric", "numeric", "numeric", "numeric"))

talMgt <- read_xlsx("2017-hcaaf-report.xlsx", sheet="HCAAF - Talent Management", 
                    range = "B2:G92", col_names = TRUE,
                    col_types = c("text","numeric","numeric", "numeric", "numeric", "numeric"))

jobSat <- read_xlsx("2017-hcaaf-report.xlsx", sheet="HCAAF - Job Satisfaction", 
                    range = "B2:G92", col_names = TRUE,
                    col_types = c("text","numeric","numeric", "numeric", "numeric", "numeric"))

# data exploration 
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
########################

hist(ldrpKM$'2013', xlim=c(0,100)) 
hist(ldrpKM$'2017', xlim=c(0,100))

ggplot(ldrpKM, aes( ldrpKM$'2013', ldrpKM$'2017')) + 
  xlim(0, 100) + ylim(0, 100) +
  geom_point() + 
  geom_smooth(method = loess, se = F) # loess-local polynomial regression fitting; for linear change method=lm & se=T


# more data management
# help("add_column")
########################

ldrpKM <- add_column(ldrpKM, index="Leadership & Knowledge Mgmt", index_abbr="Ldshp & KM")
resOPC <- add_column(resOPC, index="Results Oriented Perf Culture", index_abbr="Rslt OPC")
talMgt <- add_column(talMgt, index="Talent Management", index_abbr="Talent Mgmt")
jobSat <- add_column(jobSat, index="Job Satisfaction", index_abbr="Job Sat")

# help("bind_rows")
hcaaf <- bind_rows (ldrpKM, resOPC, talMgt, jobSat)
hcaaf

# help("rename") rename X__1 to agency
colnames(hcaaf)
hcaaf <- hcaaf %>% rename(agency = X__1) # %>% is a pipe operation, to chain 2+ methods without parantheses 
colnames(hcaaf)
hcaaf

hist (hcaaf$'2017')
summary(hcaaf$'2017')

# help("filter") filter government wide results and combine to one dataframe
hcaaf_gvt <- filter(hcaaf, agency=="Governmentwide")
hcaaf_gvt

### even more data management ###
#################################

# sort by 2013+2017 index result
hcaaf_gvt_sort <- hcaaf_gvt %>% arrange(desc(hcaaf_gvt$'2017'))
hcaaf_gvt_sort

x = hcaaf_gvt$index_abbr
value1 = hcaaf_gvt$'2013'
value2 = hcaaf_gvt$'2017'
diff = hcaaf_gvt$'2017'-hcaaf_gvt$'2013'
diff_ave = mean(hcaaf_gvt$'2017' - hcaaf_gvt$'2013')
diff; diff_ave
hcaaf_gvt

### Lollipop ###
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
i3

grid.arrange(i1, i2, i3, nrow=1)

# improvements 

i4 <- ggplot(hcaaf_gvt) +
  geom_segment( aes(x=reorder(hcaaf_gvt$index, -hcaaf_gvt$'2017'), xend=hcaaf_gvt$index, y=hcaaf_gvt$'2013', yend=hcaaf_gvt$'2017' )) +
  geom_point( aes(x=hcaaf_gvt$index, y=hcaaf_gvt$'2013' ), color="grey", size=3 ) +
  geom_point( aes(x=hcaaf_gvt$index, y=hcaaf_gvt$'2017' ), color="black", size=6) +
  coord_flip() +  
  theme_minimal() +
  theme(legend.position = "bottom", panel.border = element_blank()) +
  xlab("") +
  ylab("") +
  ylim(50, 70) +
  labs(title = "HCAAF Index Results - Governmentwide",
        subtitle = "A lollipop chart is a hybrid of the bar chart and Cleveland dot plot",
        caption = "Source: OPM") 
i4 # show graph

# ggplot2 themes: https://ggplot2.tidyverse.org/reference/ggtheme.html

# improvements (original)
i4 <- ggplot(hcaaf_gvt) +
  geom_segment( aes(x=reorder(hcaaf_gvt$index, -hcaaf_gvt$'2017' ), xend=hcaaf_gvt$index, y=hcaaf_gvt$'2013', yend=hcaaf_gvt$'2017' )) +
  geom_point( aes(x=hcaaf_gvt$index, y=hcaaf_gvt$'2013' ), color="grey", size=3 ) +
  geom_point( aes(x=hcaaf_gvt$index, y=hcaaf_gvt$'2017' ), color="black", size=6) +
  coord_flip() +  
  theme_light() +
  theme(legend.position = "bottom", panel.border = element_blank()) +
  xlab("") +
  ylab("") +
  ylim(50, 70) +
  labs(title = "HCAAF Index Results - Governmentwide",
       subtitle = "A lollipop chart is a hybrid of the bar chart and Cleveland dot plot",
       caption = "Source: OPM") 
i4 # show graph


####################################
### Lollipop 2 #####################
### Combined agency type and index #
####################################

hcaaf_agencyType_1vl <- filter(hcaaf, agency=="Very Large Agencies Overall")
hcaaf_agencyType_2lg <- filter(hcaaf, agency=="Large Agencies Overall")
hcaaf_agencyType_3md <- filter(hcaaf, agency=="Medium Agencies Overall")
hcaaf_agencyType_4sm <- filter(hcaaf, agency=="Small Agencies Overall")
hcaaf_agencyType_5vs <- filter(hcaaf, agency=="Very Small Agencies Overall")

# help("bind_rows")
hcaaf_agency_type <- bind_rows (hcaaf_agencyType_1vl, hcaaf_agencyType_2lg, hcaaf_agencyType_3md, hcaaf_agencyType_4sm, hcaaf_agencyType_5vs)
hcaaf_agency_type


### Lollipop 3 ######
### Small multiples #
#####################

# At the heart of quantitative reasoning is a single question: Compared to what? -Edward Tufte

x = paste(hcaaf_agency_type$index)
value1 = hcaaf_agency_type$'2013'
value2 = hcaaf_agency_type$'2017'
diff = hcaaf_agency_type$'2017'-hcaaf_agency_type$'2013'
diff_ave = mean(hcaaf_agency_type$'2017'-hcaaf_agency_type$'2013')
diff; diff_ave

# good
i5 <- ggplot(hcaaf_agency_type) +
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

# dev
i5 <- ggplot(hcaaf_agency_type) +
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

hcaaf_agency_type$agency <- factor(hcaaf_agency_type$agency, levels=c("Very Large Agencies Overall", 
                                                                 "Large Agencies Overall", 
                                                                 "Medium Agencies Overall", 
                                                                 "Small Agencies Overall", 
                                                                 "Very Small Agencies Overall"))

                 
i5

### filter from HCAAF
########################

hcaaf_gvt_sort <- hcaaf %>% 
  filter(hcaaf$agency=="Governmentwide") %>%
  arrange(desc(hcaaf_gvt$'2017'))
hcaaf_gvt_sort


### gg themes ###
#################
install.packages('ggthemes')
suppressMessages(library(ggthemes))


# bar chart

data_agency <- 
  hcaaf_agencyType %>%
  group_by(hcaaf_gvt_agencyType$index) %>%
  summarise(hcaaf_gvt_agencyType$'2017'=mean(hcaaf_gvt_agencyType$'2017'))

ggplot(data=hcaaf_gvt_agencyType, 
       aes(x=hcaaf_gvt_agencyType$index, group_by(hcaaf_gvt_agencyType$agency, y=hcaaf_gvt_agencyType$'2017')) +
  geom_bar(stat="identity")

hcaaf$17 <- factor(data$hcaaf17, levels = data$agency[order(-data$hcaaf17)])

ggplot(data=hcaaf$17, aes(x=hcaaf$agency, y=hcaaf$'2017')) +
  geom_bar(stat="identity") +
  theme_tufte()



