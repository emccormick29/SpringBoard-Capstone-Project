
##INSTALLING THE NECESSRARY PACKAGES
install.packages("mice")
library (dplyr)
library (ggplot)
library(mice)

##CLEANING THE DATA
#merge the cleaned happy moments with the demogrpahic information of the responder by the wid (worker ID)
cleaneddata <- data.frame (cleaned_hm)
demodata <-data.frame (demographic)
merged <-merge(cleaneddata, demodata, by = "wid")

#INVESTIGATING NA's
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(merged,2,pMiss)
apply(merged,1,pMiss)
merged <- select(merged,wid:num_sentence, predicted_category:parenthood)

#now that I've removed the ground_truth_category, I can now use the na.omit clause and only remove
#a handful of my obersvations
merged <- na.omit(merged)
str(merged)

#Convering the age values to all numbers--> this solves the problem of 39.0 vs. 39
merged<-filter(merged, age != "prefer not to say")
merged <-filter(merged, age != 'čá')
#All of our data points are double-digits, so only take first two characters in string to ensure we treat 20.0 and 20 as the 
#same age
merged$age <- substr(merged$age, start=1, stop = 2)
#Need to convert age to a numeric value rather than a character value
merged$age <- as.numeric(merged$age)

#Imputing the NA values (if I had to do this, which I didn't)
imputed <-mice(merged, m = 5, method = 'pmm')
imputed2 <- complete(imputed, "long", inc = TRUE)
str(imputed)

##GENERAL UNDERSTANDING OF DATA
table (merged$gender)
table(merged$marital)
table(merged$parenthood)
table(merged$age)
min(merged$age)
max(merged$age)
str(merged)
summary(merged)


##VISUALIZING THE DATA

#Two ways to plot all of the responses
ggplot(merged, aes(fill= predicted_category, x= predicted_category)) + geom_bar(stat= 'count') + ggtitle ("Count of Happines Observations by Category")

ggplot(merged, aes(x = predicted_category, fill = predicted_category)) +
geom_bar(position = "dodge") + labs (title = 'Happiest Moments by Category', x= 'Category of Derived Happines', y = 'Count of Moments')

#histogram of age
hist(merged$age, main = "Age Buckets of Observations", xlab = 'Age', xlim = c(0,100), breaks = 10)

# comparing male to female answers
ggplot(merged, aes(fill=gender, x=predicted_category)) + 
geom_bar(position="dodge", stat="count") + ggtitle ("Comparing Happiness Drivers across Genders")

# comparing parents vs. non-parents
ggplot(merged, aes(fill=parenthood, x=predicted_category)) + 
geom_bar (position= 'dodge', stat = 'count') + ggtitle ("Comparing Happiness Drivers across Parenthood Status")

# comparing married vs. non-married
ggplot(merged, aes(fill=marital, x=predicted_category)) + 
geom_bar (position= 'dodge', stat = 'count')

#
ggplot(merged, aes(fill=age, x=predicted_category)) + 
geom_bar (position= 'dodge', stat = 'count')

#comparing across ages
  agebucket1020  <- filter(merged, age<20 & age>=10)
ggplot(agebucket1020, aes(fill=predicted_category, x=predicted_category)) + 
  geom_bar (position= 'dodge', stat = 'count')

agebucket2030  <- filter(merged, age<30 & age>=20)
ggplot(agebucket2030, aes(fill=predicted_category, x=predicted_category)) + 
  geom_bar (position= 'dodge', stat = 'count') + ggtitle('Happiness Observations for Ages 20-30')

agebucket3040  <- filter(merged, age<40 & age>=30)
ggplot(agebucket3040, aes(fill=predicted_category, x=predicted_category)) + 
  geom_bar (position= 'dodge', stat = 'count') + ggtitle('Happiness Observations for Ages 30-40')

agebucket4050  <- filter(merged, age<50 & age>=40)
ggplot(agebucket4050, aes(fill=predicted_category, x=predicted_category)) + 
  geom_bar (position= 'dodge', stat = 'count')

agebucket5060  <- filter(merged, age<60 & age>=50)
ggplot(agebucket5060, aes(fill=predicted_category, x=predicted_category)) + 
  geom_bar (position= 'dodge', stat = 'count')

agebucket6070  <- filter(merged, age<70 & age>=60)
ggplot(agebucket6070, aes(fill=predicted_category, x=predicted_category)) + 
  geom_bar (position= 'dodge', stat = 'count')

agebucket7080  <- filter(merged, age<80 & age>=70)
ggplot(agebucket7080, aes(fill=predicted_category, x=predicted_category)) + 
  geom_bar (position= 'dodge', stat = 'count')

agebucket8090  <- filter(merged, age<90 & age>=80)
ggplot(agebucket8090, aes(fill=predicted_category, x=predicted_category)) + 
  geom_bar (position= 'dodge', stat = 'count')

#LOGISTIC REGRESSION
#Use lgm to predict happiness category based on age and gender
merged$predicted_category <- factor(merged$predicted_category, levels=c("affection", "enjoy_the_moment", "achievement", "nature", "bonding", "excericse", "leisure"))

levels(merged$gender)
merged$gender <- droplevels(merged$gender)
merged$predicted_category <- droplevels(merged$predicted_category)
merged$gender <- factor(merged$gender, levels = c("m", "f", "o"))
merged$parenthood <- as.factor(merged$parenthood)

merged$parenthood <-droplevels(merged$parenthood)

PredictedHappiness <- glm(predicted_category~gender + age + parenthood + marital,
                  data=merged, family = "binomial")

summary(PredictedHappiness)


