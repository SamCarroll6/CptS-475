library(tidyr)
library(dplyr)

# Read the csv files containing the data
DemoGraphics = read.csv("~/Desktop/DS_ClassProject_Fall2018_data/Demographics.csv")
C_S = read.csv("~/Desktop/DS_ClassProject_Fall2018_data/Card_Swipes.csv")

# Split data into individual data frames, each corresponding
# to a specific term date.
new = by(DemoGraphics, DemoGraphics[,"record_term"], function(x) x)

T20123 = new[[1]]
T20131 = new[[2]]
T20132 = new[[3]]
T20133 = new[[4]]
T20141 = new[[5]]
T20142 = new[[6]]
T20143 = new[[7]]
T20151 = new[[8]]
T20152 = new[[9]]
T20153 = new[[10]]

# For easier manipulation I'm splitting TRANDATE section into 
# two columns, one for Date and the other for Time. 
T20123 = T20123 %>% separate(TRANDATE, c("DATE", "TIME"), " ")
T20131 = T20131 %>% separate(TRANDATE, c("DATE", "TIME"), " ")
T20132 = T20132 %>% separate(TRANDATE, c("DATE", "TIME"), " ")
T20133 = T20133 %>% separate(TRANDATE, c("DATE", "TIME"), " ")
T20141 = T20141 %>% separate(TRANDATE, c("DATE", "TIME"), " ")
T20142 = T20142 %>% separate(TRANDATE, c("DATE", "TIME"), " ")
T20143 = T20143 %>% separate(TRANDATE, c("DATE", "TIME"), " ")
T20151 = T20151 %>% separate(TRANDATE, c("DATE", "TIME"), " ")
T20152 = T20152 %>% separate(TRANDATE, c("DATE", "TIME"), " ")
T20153 = T20153 %>% separate(TRANDATE, c("DATE", "TIME"), " ")

# Just for 20123 data so far, returns the ID# of top 25% most
# frequent visitors and bottom 75% visitors.
Tab20123 = data.frame(table(T20123$PATRONID.x))
n = 25
TabH20123 = Tab20123[Tab20123$Freq > quantile(Tab20123$Freq, prob=1-n/100),]
n = 75
TabL20123 = Tab20123[Tab20123$Freq > quantile(Tab20123$Freq, prob=1-n/100),]
  
# Make barplots that show how frequently each acaddemic level
# attends the gym
barplot(table(T20123$academic_level), main = "20123")
barplot(table(T20131$academic_level), main = "20131")
barplot(table(T20133$academic_level), main = "20133")
barplot(table(T20141$academic_level), main = "20141")
barplot(table(T20143$academic_level), main = "20143")
barplot(table(T20151$academic_level), main = "20151")
barplot(table(T20153$academic_level), main = "20153")

# Match time periods that occur during 05:00-05:59
sum(grepl('^05:', T20123$TIME))
