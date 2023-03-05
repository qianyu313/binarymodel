# libraries needed
library(tidyverse)  # most variable creation here uses tidyverse 
library(tidyselect) # used to select variables in FP_EVENTS.R
library(haven)      # used for Haven labeled DHS variables
library(labelled)   # used for Haven labeled variable creation
library(expss)    # for creating tables with Haven labeled data
library(xlsx)     # for exporting to excel
library(naniar)   # to use replace_with_na function
library(here)       # to get R project path


library(dplyr)


children <- readRDS("~/Library/Caches/rdhs/datasets/ZMKR71DT.rds")
births <- readRDS("~/Library/Caches/rdhs/datasets/ZMBR71DT.rds")
individual <- readRDS("~/Library/Caches/rdhs/datasets/ZMIR71DT.rds")
household <- readRDS("~/Library/Caches/rdhs/datasets/ZMHR71DT.rds")



####################
#####DHS_RH_ANC#####
####################

IRdata <- individual %>%
  mutate(wt = v005/1000000)

# period and age of child
# choose reference period, last 2 years (24 months) or last 5 years (60 months)
# Using a period of the last 2 years will not match final report but would provide more recent information.
# period = 24
IRdata <- IRdata %>%
  mutate(period = 60)

# age of child. If b19_01 is not available in the data use v008 - b3_01
if ("TRUE" %in% (!("b19_01" %in% names(IRdata))))
  IRdata [[paste("b19_01")]] <- NA
if ("TRUE" %in% all(is.na(IRdata $b19_01)))
{ b19_included <- 0} else { b19_included <- 1}

if (b19_included==1) {
  IRdata <- IRdata %>%
    mutate(age = b19_01)
} else {
  IRdata <- IRdata %>%
    mutate(age = v008 - b3_01)
}

# b19_01 is identical before and after 36-48


# //Number of ANC visits in 4 categories that match the table in the final report
IRdata <- IRdata %>%
  mutate(rh_anc_numvs =
           case_when(
             m14_1 == 0 ~ 0 ,
             m14_1 == 1 ~ 1 ,
             m14_1  %in% c(2,3)   ~ 2 ,
             m14_1>=4 & m14_1<=90  ~ 3 ,
             m14_1>90  ~ 9 ,
             age>=period ~ 99 )) %>%
  replace_with_na(replace = list(rh_anc_numvs = c(99))) %>%
  set_value_labels(rh_anc_numvs = c(none = 0, "1" = 1, "2-3"=2, "4+"=3, "don't know/missing"=9  )) %>%
  set_variable_labels(rh_anc_numvs = "Number of ANC visits")

# //4+ ANC visits  
IRdata <- IRdata %>%
  mutate(rh_anc_4vs =
           case_when(
             rh_anc_numvs==3 ~ 1,
             rh_anc_numvs %in% c(0,1,2,9)   ~ 0 )) %>%
  set_value_labels(rh_anc_4vs = c("Yes" = 1, "No"=0)) %>%
  set_variable_labels(rh_anc_4vs = "Attended 4+ ANC visits")



table(IRdata$m14_1)
# 0    1    2    3    4    5    6    7    8    9   10   11   12   98 
# 89  100  441 1935 2363 1383  739  174   57   14    6    1    3   67 
table(IRdata$rh_anc_4vs)
# 0    1 
# 2632 4740 
sum(is.na(IRdata$rh_anc_4vs))
#6311
length(IRdata$rh_anc_4vs)
#13683

####################
##compare w/indi####
####################


#####individual########


individual <- readRDS("~/Library/Caches/rdhs/datasets/ZMIR71DT.rds")
indi.raw.dat.tmp<-individual
dim(indi.raw.dat.tmp)
#13683

# B19 current age of child in months (months since birth for dead children)?
U5<-indi.raw.dat.tmp %>%filter( b19_01<=60)
dim(U5)# 7428 

c(sum(is.na( U5$m14_1)),dim(U5)[1]-sum(is.na( U5$m14_1)))

table(U5$m14_1)
# 
# 0    1    2    3    4    5    6    7    8    9   10   11   12   98 
# 89  100  441 1935 2363 1383  739  174   57   14    6    1    3   67 


Antenatal4<-  indi.raw.dat.tmp %>%
  filter( b19_01<=60)%>%
  dplyr::select(c(cluster="v001", region="v024", weight="v005", strata="v025",
                  Antenatal="m14_1")) %>%
  mutate(Antenatal = ifelse(Antenatal>3 &  Antenatal<98,1,0)) 

table(Antenatal4$Antenatal)

# 0    1 
# 2632 4740 


sum(is.na(Antenatal4$Antenatal))
#56
length(Antenatal4$Antenatal)
#7428










#####births########

births <- readRDS("~/Library/Caches/rdhs/datasets/ZMBR71DT.rds")

raw.dat.tmp<-births
dim(raw.dat.tmp)#38446
# B19 current age of child in months (months since birth for dead children) ..not..
## cbind(births$v008(date of interview)-births$b3(date of birth),births$b19)

U5<-raw.dat.tmp %>%filter( b19<=60)
dim(U5)#10133
sum(is.na( U5$m14))#2761


# U5M<-raw.dat.tmp %>%filter( b19<=60)%>%filter( b5==0)
# dim(U5M)
# c(sum(is.na( U5M$m14)),dim(U5M)[1]-sum(is.na( U5M$m14)))


table(U5$m14)
# 0    1    2    3    4    5    6    7    8    9   10   11   12   98 
# 89  100  441 1935 2363 1383  739  174   57   14    6    1    3   67 

Antenatal4<-  raw.dat.tmp %>%
  # filter( b19<=60)%>%
  dplyr::select(c(cluster="v001", region="v024", weight="v005", strata="v025",
                  Antenatal="m14")) %>%
  mutate(Antenatal = ifelse(Antenatal>3 &  Antenatal<98,1,0))
table(Antenatal4$Antenatal)
# 0    1 
# 2632 4740 
sum(is.na(Antenatal4$Antenatal))
#2761
length(Antenatal4$Antenatal)
#10133





