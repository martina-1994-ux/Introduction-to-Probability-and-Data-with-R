library(ggplot2)
library(dplyr)

load("brfss2013.RData")

# Query the relevant variables
# sleptim1: How Much Time Do You Sleep
# sex: Respondents Sex
# qlhlth2: How Many Days Full Of Energy In Past 30 Days

q1 <- select(brfss2013, qlhlth2 , sex , sleptim1) %>% 
  filter(!is.na(qlhlth2), !is.na(sex), sleptim1 <= 12 )

# Present summary statics of continuous variables and count of categorical data

q1 %>% group_by(qlhlth2) %>%    summary(count=n())

# Plot relevant variables
# Use scatter plot to obeserve relationship between two continuous variables. Add linear trend line with confidence interval to see relationship via line along with the spread of the results. Facet by gender to observe differences between the sexes.

ggplot(data = q1, aes(x = sleptim1, y = qlhlth2 ))+
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)   +
  scale_x_continuous(limits = c(4,10), breaks = 4:10) +
  facet_grid(. ~  sex) +
  xlab("sleptim1 = How much time do you sleep?") +
  ylab ("qlhlth2: How Many Days Full Of Energy In Past 30 Days")

# Survey results indicate that those who sleep between 6 and 8 hours make up the majority of the respondents. There appears to be a positive linear relationship between the x and y variables with a wide dispersion between the number of energetic days reported at each recorded level of sleep (in hours.) The dispersion is greater in male respondents. 

# # Query the relevant variables
# # lsatisfy: Satisfaction With Life ( categorical)
# # educa: Education Level
# # sex: Respondents Sex

q2 <- select(brfss2013, lsatisfy , sex, educa) %>% 
  filter(!is.na(lsatisfy), !is.na(sex), !is.na(educa))

# Present totals of variables being analyzed

q2 %>% group_by(lsatisfy) %>%    summarise(count=n())

q2 %>% group_by(educa) %>%   summarise(count=n())

q2 %>% group_by(sex) %>%   summarise(count=n())


# # Plot relevant variables
# # Use count plot to obeserve relationship between two catergorical variables. Facet by gender to observe differences between the sexes.


ggplot(data = q2, aes(x = lsatisfy , y = educa ))+
  geom_count () +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_grid(. ~  sex) +
  xlab("lsatisfy: Satisfaction With Life") +
  ylab ("educa: Education Leve")

# Both genders are beahaving simlilary in the observation that satisfation levles are greatest (Satisfied, Very Satisfied) for those that have at least completed highschool or the equivalent.  Further, most respondents in this survey have completed at least high school, which may indicate there is some sort of systematic bias in the survey ( those with phones may be more likely to have completed high school and therefore may be overall more satisfied with life)

# Finally, it is interesting to note the make up of the outlier cases. The handful of cases that reported never attending any education report either being satisfied or very satisfied with life. This result along with the reported cases of disastifaction with life (or very dissatisfie) across those who have complete high school or better indicate that eductation levels are not perfectly correlated with life sat

# Query the relevant variables
# # genhlth: General Health
# # income2: Income Level

q3 <- select(brfss2013, genhlth ,income2) %>% 
  filter(!is.na(genhlth), !is.na(income2))

# Present totals of variables being analayzed

q3 %>% group_by(genhlth) %>%    summarise(count=n())

q3 %>% group_by(income2) %>%   summarise(count=n())



ggplot(data = q3, aes(x = genhlth , y = income2 ))+
  geom_count () +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("genhlth: General Health ") +
  ylab ("income2: Income Level")


# More than 50 percent of the surveyed population reported an income level greater than 35,000 USD. It appears that there is a postive relationship between earning more income and those who reported health levels of at least good.

# Further, when we look at reported numbers of very good and exceleent respondents, the number is trending up as we move up the income scale. This provides some evidence that more research could help identify possible causaul relationships.  

# Finally, short of more robust analysis to identify causation, I believe this survey would benefit from further segementation of those who earn more than 75,000 to see how even higher earners fare in terms of general health relative to income levels. The positions that pay even high salaries may in fact come with more work time and stress levels that could correlate to lower reported health levels.







