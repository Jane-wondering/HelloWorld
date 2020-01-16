library(readxl)
library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)
library(stringr)
library(moderndive)



View(Raw_data)    

#Inspect the dataset
glimpse(Raw_data)
summary(Raw_data)
Raw_data %>% ggplot(aes(x=Age,y=`Health Score`)) +
  geom_point(aes(color=factor(`Sex (Male=1)`)))  
#There're some oblivious errors and missing values


#Clean the data
Clean_data <- Raw_data %>%
  select(-`Observation Number`) %>%
  rename(Sex = `Sex (Male=1)`, #Rename the columns
         Employee_Id = `Employee Id`,
         Num_vis_hpt = `Hospital Visit This Quarter (1=Yes)`,
         Health_score = `Health Score` ) %>%
  mutate(Salary = gsub('//$','',Salary)) %>%
  mutate(Age = as.integer(Age), #Convert the data type
         Sex = as.factor(Sex),
         Race = as.factor(Race),
         Salary = round(as.numeric(Salary)))
         
# Make sure there is no pattern in NAs
ind1 <- which(is.na(Clean_data$Race))
Clean_data[ind1, ]
ind2 <- which(is.na(Clean_data$Sex))
Clean_data[ind2, ]

# Edit the obvious errors in 'Age'
ind3 <- which(Clean_data$Age > 100)
Clean_data$Age[ind3] <- Clean_data$Age[ind3] - 100
ind4 <- which(Clean_data$Age < 14)
Clean_data$Age[ind4] <- NA
summary(Clean_data$Age)

# Make sure there's no pattern in 10.0 Health Score
ind5 <- which(Clean_data$Health_score == 10)
Clean_data[ind5,]
#Remove these data
Clean_data$Health_score[ind5] <- NA
summary(Clean_data$Health_score)

# Remove the NAs
Clean_data <- na.omit(Clean_data)

# Inspect the clean data
str(Clean_data)
summary(Clean_data)

# EDA
boxplot(Clean_data$Age)
boxplot(Clean_data$Salary)
hist(Clean_data$Quarter)
hist(Clean_data$Health_score)

#Q1

# Demographic Characteristics
# Number of employees over time
Clean_data %>% 
  group_by(Quarter) %>%
  summarize(num = n()) %>%
  ggplot(aes(x=Quarter, y=num)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks=c(1:12)) +
  labs(y='Number of employees',
       title='Number of employees over time',
       subtitle='Q1 - Q12')

# Number of male/ female employees over time
Clean_data %>% 
  group_by(Quarter,Sex) %>%
  summarize(num = n()) %>%
  ggplot(aes(x=Quarter, y=num,color=Sex)) +
  geom_point() +
  geom_line(aes(color=Sex)) +
  scale_x_continuous(breaks=c(1:12)) +
  labs(y='Number of employees',
       title='Number of male/ female employees over time',
       subtitle='Q1 - Q12, red -> Female, blue -> Male')


# Discover the distribution of Age over time
Clean_data %>% ggplot(aes(x=factor(Quarter),y=Age)) +
  geom_boxplot() +
  labs(x='Quarter',
       title='Demographic charactersitics of Age')

Clean_data %>% 
  group_by(factor(Quarter)) %>%
  ggplot(aes(Age,group=Quarter,color=Quarter)) +
  geom_density() +
  facet_grid(.~Sex) +
  scale_x_continuous(14:75) +
  labs(x='Age',y='density of age',
       title='Distribution of age',
       subtitle = 'group by sex')

# Discover the distribution of Race over time
Clean_data %>% ggplot(aes(x=as.numeric(Race))) +
  facet_wrap(~Quarter) +
  geom_histogram(binwidth=0.5) +
  scale_x_continuous(1,2,3)

Clean_data %>% 
  ggplot(aes(x=as.numeric(Race),group=Quarter,color=factor(Quarter))) +
  scale_x_continuous(1,2,3) +
  geom_density()

Clean_data %>% 
  group_by(Quarter,Race) %>%
  summarize(num = n()) %>%
  ggplot(aes(x=Quarter, y=num,color=Race)) +
  geom_point() +
  geom_line(aes(color=Race)) +
  scale_x_continuous(breaks=c(1:12)) +
  labs(y='Number of employees',
       title='Number of employees of different races over time',
       subtitle='Q1 - Q12')



#Q2

# Correlation between variables

Clean_data %>% ggplot(aes(x=Age,y=Health_score)) +
  geom_point(aes(color=Sex),alpha=0.5) +
  geom_smooth(method = 'lm', se = F) +
  labs(title = 'Scatterplots of Health Score versus Age',
       subtitle = 'with colors reprenting Sex')

Clean_data %>% ggplot(aes(x=Sex,y=Health_score)) +
  geom_boxplot() +
  labs(title = 'Boxplots of Health Score versus Sex')

Clean_data %>% ggplot(aes(x=Race,y=Health_score)) +
  geom_boxplot() +
  labs(title = 'Boxplots of Health Score versus Race')

Clean_data %>% ggplot(aes(x=factor(Num_vis_hpt),y=Health_score)) +
  geom_boxplot() +
  labs(title = 'Boxplots of Health Score versus Hospital Visits This Quarter')

Clean_data %>% ggplot(aes(x=factor(Quarter),y=Health_score)) +
  geom_boxplot() +
  labs(title = 'Boxplots of Health Score versus Quarter')

Clean_data %>% 
  group_by(Quarter) %>%
  summarize(mean_hs = mean(Health_score)) %>%
  ggplot(aes(x=factor(Quarter),y=mean_hs)) +
  geom_point() +
  labs(title = 'Mean Health Score by quarter')

Clean_data %>% ggplot(aes(x=Salary,y=Health_score)) +
  geom_point(aes(color=Sex),alpha=0.5) +
  geom_smooth(method = 'lm', se = F) +
  labs(title = 'Scatterplots of Health Score versus Salary',
       subtitle = 'with colors reprenting Sex')

Clean_data %>% 
  group_by(Quarter) %>%
  summarize(mean_hs = mean(Health_score)) 

# Run regression

# Convert data type for regression
reg_data <- Clean_data %>%
  mutate (Sex = as.numeric(Sex),
          Race = as.numeric(Race))

# Regress 'Health score' with all other factors
model_1 <- lm(Health_score ~ Quarter+Sex+Age+Race+Num_vis_hpt+Salary, 
              data = reg_data)
get_regression_table(model_1)

# Regress 'Salary' with relative factors
model_2 <- lm(Salary ~ Quarter+Sex+Age+Race,
              data = reg_data)
get_regression_table(model_2)

# Regress 'Health Score' with significant factors
model_3 <- lm(Health_score ~ Sex+Age+Race+Num_vis_hpt, 
              data = reg_data)
get_regression_table(model_3)


# Q3

# Simply remove health score = 10
remove_10_data <- Raw_data %>%
  select(Quarter,`Health Score`) %>%
  rename(Health_score = `Health Score` ) 

ind6 <- which(remove_10_data$Health_score == 10)
remove_10_data$Health_score[ind5] <- NA
summary(remove_10_data$Health_score)

remove_10_data <- na.omit(remove_10_data)

# Mean health score per quarter
remove_10_data %>% 
  group_by(Quarter) %>%
  summarize(mean_hs = mean(Health_score)) 
