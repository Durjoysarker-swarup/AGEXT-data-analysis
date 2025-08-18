#first export the data form the local drive to R


library(tidyverse)
library(readxl)
library(Hmisc)      # rcorr for r and p-values
library(ggcorrplot) # nice corr heatmap
library(GGally)     # ggpairs
library(car)        # leveneTest, vif
library(broom)      # tidy() for model outputs

#cleaning the names
install.packages("janitor")
library(janitor)    # clean_names()
student_data
clean_names(student_data)
head(student_data)
typeof(student_data$`Family Size`)

#now let's make it integer value of the data, maybe don't required if I save the file in csv format.
stu_int <- student_data %>% 
  mutate(across(where(is.double), as.integer))
print(stu_int)
#there was an misconception like I think double isnot an numeric value, my fault. so no need to convert the data. I will do the analysis with the student_data

summary(student_data)
#To see is there any missing value or not.
colSums(is.na(student_data))

#descriptive of income
student_data %>% summarise(
  n = n(), #n is used here to count the number of row is given here
  mean_income = mean(`Annual Income`,na.rm= TRUE),
  sd_income = sd(`Annual Income`,na.rm= TRUE),
  max = max(`Annual Income`,na.rm= TRUE),
  min = min(`Annual Income`,na.rm= TRUE)
)



#calculating the pearson correlation and p value
library(Hmisc)
res <- rcorr(as.matrix(student_data), type="pearson")
r_mat <- res$r
p_mat <- res$P

#make this into tidy format for the analysis
r <- r_mat["Annual Income", ]
p <- p_mat["Annual Income", ]

corr_income <- tibble(
  variable = colnames(r_mat),
  coorelation = r,
  p_value = p) %>% filter(variable != "Annual Income") #here it is used to make the the age variable disappear

#plot of coorelation
install.packages("ggcorrplot")
library(ggcorrplot)
corr_all <- cor(student_data, use="pairwise.complete.obs")
ggcorrplot(corr_all, lab = TRUE, show.diag = FALSE)


#adding more plot for the explination
ggplot(student_data, aes(`Access to Info Sources`,`Annual Income`)) +
  geom_point() +
  geom_smooth(method="lm", se=TRUE) +
  labs(x="Access to information", y="Annual income", title="Info Access vs Income")


#age categories adding in the student data
student_data <- student_data %>% 
  mutate(age_cat = ntile(`Age`, 3)) %>% 
  mutate(age_cat= recode(age_cat,
                            `1` = "Young", 
                            `2` = "Middle", 
                            `3` = "Old"
                            ))
#income categories adding in the student data
student_data <- student_data %>% 
  mutate(income_cat = ntile(`Annual Income`, 3)) %>% 
  mutate(income_cat= recode(income_cat,
                         `1` = "Low", 
                         `2` = "Medium", 
                         `3` = "High"
  ))
student_data %>% count(age_cat)
student_data %>% count(income_cat)


#table for just analysis
New_table <-  student_data %>% group_by(income_cat) %>%
  summarise(
    mean_age = mean(`Age`, na.rm=TRUE),
    mean_education = mean(`Education`, na.rm=TRUE),
    mean_experience = mean(experience, na.rm=TRUE),
    mean_access = mean(`Access to Info Sources`, na.rm=TRUE),
    mean_knowledge = mean(`Knowledge`, na.rm=TRUE),
    mean_family = mean(`Family Size`, na.rm=TRUE)
  )



#histogram plot
ggplot(student_data, aes(`Annual Income`)) +
  geom_histogram(binwidth = 20, boundary = 0, color = "black", fill = "grey80") +
  labs(title = "Histogram of Annual Income",
       x = "Annual income",
       y = "Count") +
  theme_minimal()



#Regression model building
model <- lm(`Annual Income` ~ `Education` + `Access to Info Sources`+ `Knowledge` + `Age` + `Family Size` + `Farm Size` + `experience`, data = student_data)
summary(model)

# check multicollinearity
car::vif(model)
#this VIF is good but the model is-not good because of the RMSE and the r-squred and the adjusted r-squred. the model is very bad





