###final project
###arm queries


#set wd
setwd("C:/Users/nicktinsley/OneDrive/Syracuse/R code/IST 707/data/finalProject")

#load libraries
library(arules)
library(arulesViz)
library(plyr)
library(dplyr)
library(funModeling)


#load data
red <- read.csv2("red.csv")
white <- read.csv2("white.csv")

View(red[red$quality > 5,])

hist(red$quality)
hist(white$quality)

##quality 5 - 7 mid
##quality < 5 low
##quality > 7 high

##convert char to factor
red <- red %>% mutate_if(is.character, funs(as.factor))
str(red)

white <- white %>% mutate_if(is.character, funs(as.factor))
str(white)


#discretize quality
red$quality_disc <- cut(red$quality, breaks = c(0,4,6,Inf),
                        labels=c("low","mid","high"))

table(red$quality, red$quality_disc)

white$quality_disc <- cut(white$quality, breaks = c(0,4,6,Inf),
                        labels=c("low","mid","high"))

table(white$quality, white$quality_disc)

#qith


#try apriori on red without fully discretizing the rest of the factors,, see if any intersesting rules pop up
## all these rules apply to red set
rules <- apriori(red, parameter=list(supp=0.001, conf=0.9, maxlen=4))
rules <- sort(rules, decreasing = TRUE, by = "confidence")
inspect(rules[1:50])

#removing quality, it creates unnecessary rules
red <- red %>% select(-quality)
white <- white %>% select(-quality)

#see what if right hand is high quality
rules_highQ <- apriori(data=red, parameter = list(supp=0.001, conf=0.08,minlen=2),
                       appearance = list(default="rhs",lhs="quality_disc=high"),
                       control=list(verbose=T))
rules_highQ <- sort(rules_highQ, decreasing = T, by = "lift")
inspect(rules_highQ)

#mid 
rules_midQ <- apriori(data=red, parameter = list(supp=0.001, conf=0.08,minlen=2),
                       appearance = list(default="rhs",lhs="quality_disc=mid"),
                       control=list(verbose=T))
rules_midQ <- sort(rules_midQ, decreasing = T, by = "lift")
inspect(rules_midQ)

#low
rules_lowQ <- apriori(data=red, parameter = list(supp=0.001, conf=0.08,minlen=2),
                      appearance = list(default="rhs",lhs="quality_disc=low"),
                      control=list(verbose=T))
rules_lowQ <- sort(rules_lowQ, decreasing = T, by = "lift")
inspect(rules_lowQ)

arulesViz::ruleExplorer(rules_highQ, sidebarWidth = 2, graphHeight = "600px")

##now white
rules_wht <- apriori(white, parameter=list(supp=0.001, conf=0.9, maxlen=4))
rules_wht <- sort(rules_wht, decreasing = TRUE, by ="confidence")
inspect(rules_wht[1:50])


#rules for 5's, 6's and 7's see if there is any diff between these qualities
#what are the attribs of 8's and 9's
#red and white varients of grape, 
#

red$source <- "red"
white$source <- "white"

red_white <- rbind(red,white)
red_white <- red_white %>% select(-source)

## build rules for full data set
rules_redwht <- apriori(red_white, parameter=list(supp=0.001, conf=0.9, maxlen=4))
rules_redwht <- sort(rules_redwht, decreasing = T, by="confidence")
inspect(rules_redwht[1:50])


rules_rdwht_mid <- apriori(data=red_white, parameter = list(supp=0.001, conf=0.9,maxlen=4),
                           appearance = list(default="lhs",rhs="quality_disc=mid"),
                           control=list(verbose=T))
rules_rdwht_mid <- sort(rules_rdwht_mid, decreasing = T, by="lift")
inspect(rules_rdwht_mid[1:10])

rules_rdwht_high <- apriori(data=red_white, parameter = list(supp=0.001, conf=0.9,maxlen=4),
                            appearance = list(default="lhs",rhs="quality_disc=high"),
                            control=list(verbose=T))
rules_rdwht_high <- sort(rules_rdwht_high, decreasing = T, by="lift")
inspect(rules_rdwht_high[1:10])






