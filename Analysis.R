## -- INPUT -- ##
# Loading the necessary packages for the analysis
library(tidyverse)
library(mediation)
library(interactions)
library(ggplot2)

# Loading in the data
df = read_csv("cleaned_data.csv")

# Checking the summary, to make sure everything was saved correctly
summary(df)
table(df$dv_dummy)

## --- ANALYSIS --- ##
### H0: There is a significant difference in the likelihood of donating between the money-as-time-worked framing and simply asking for money conditions. ###
# Making sure the dv_manipulation is a binomial variable where 1 means that the respondent donated some amount of money and 0 means that they did not
df = df %>%
  mutate(dv_dummy = ifelse(dv_manipulation > 0, 1, 0))

model = glm(dv_dummy ~ iv, data = df, family = "binomial")
summary(model) # --> The results of the analysis show that there is no difference in the likelihood of donating between the different groups

### H1: Money-as-time-worked framing increases donations (compared to conventional money framing). ###
model2 = lm(dv_manipulation ~ iv, data = df)
summary(model2) # --> The results show that there is a significant difference between the amount donated between the different conditions, p-value < 0.05 (= 0.000447).
                # --> The 'Intercept' shows that for the condition that simply asks for money is estimated to be 31.06 euro.
                # --> Whilst the 'iv' condition shows that the respondents within the money-as-time-worked condition donate 70.27 euro more, so a total of 101.33 euro.

### H2: This effect is mediated by self-representativeness. (That is, the belief that the donation would reflect the donor’s self) ###
model3 = lm(self_representativeness ~ iv, data = df)
summary(model3) # --> The results show that there is a significant difference between the perception of self representativeness between the different conditions, p-value < 0.05 (0.00258).
                # --> The 'Intercept' shows that for the condition that simply asks for money is estimated to be 4.3036.
                # --> Whilst the 'iv' condition shows that the respondents within the money-as-time-worked condition feel more self representativeness by 0.6930, so a total of 4.9966.

### H3: This effect is mediated by control. (That is, the belief that the donor has control over how the donation is used) ###
model4 = lm(control ~ iv, data = df)
summary(model4) # --> The results show that there is a significant difference between the perception of control between the different conditions, p-value < 0.05 (2.73e-11).
                # --> The 'Intercept' shows that for the condition that simply asks for money is estimated to be 2.4620.
                # --> Whilst the 'iv' condition shows that the respondents within the money-as-time-worked condition feel more control by 1.7658, so a total of 4.2278

### However, this mediation takes place simultaneously thus we will have to analyze this with the help of Hayes (2022) ###
# This enables us to use process macro by Andrew F. Hayes (2022)
source("../../process.R")

set.seed(3451947) # Setting the seed for replicability for the bootstrapping
process(data = df, y = "dv_manipulation", x = "iv", m = c("self_representativeness", "control"), model = 4)

# --> The results confirm the results from the previous analyse, since it shows that the different 'iv' conditions are significantly (p-value < 0.05, self representativeness = 0.0026, control = 0.0000) for both mediators.
# --> The results of the simultaneous mediating analysis (including the iv, self representativeness, and control) show that only the iv has a significant effect on the amount of money donated, namely a increase of 44.7856 euro's, with a p-value < 0.05 (0.0396).
# --> This means that self representativeness and control do not influence the amount of money donated, both have a p-value > 0.05 (0.1168 and 0.1038 respectively)
# --> However, the bootstrapping did show a significant effect of self representativeness and control on the amount that was donated, since the lowerbound and upperbound of the bootstrap does not include a 0. Thus, still is certainly something to keep in mind.

### H4: This effect is moderated by short-term vs. long-term impact. (That is, whether the charity is focused on making a s/t vs l/t impact.) ###
model5 = lm(dv_manipulation ~ iv + long_or_short + iv * long_or_short, data = df)
summary(model5) # --> The results show that if the condition is combined with a long or short term question the amount donated is not significantly influenced, p-value > 0.05 (0.77861).

model6 = lm(control ~ iv + long_or_short + iv * long_or_short, data = df)
summary(model6) # --> The results show that if the condition is combined with a long or short term question the perceived control is not significantly influenced, p-value > 0.05 (0.624)

### However, again, the moderated mediation takes place simultaneously thus we will have to anlyze this too with the help of Hayes (2022) ###
set.seed(1573920)
process(data = df, y = "dv_manipulation", x = "iv", m = "control", w = "long_or_short", model = 8)

# --> I have to explain the results still

### H5: This effect is moderated by trust of the charity. (That is, how much the donor trusts this specific charity.) ###
model7 = lm(dv_manipulation ~ iv + trust + iv * trust, data = df)
summary(model7) # --> The results show that if the condition is combined with perceived trust the amount donated is not significantly influenced, p-value > 0.05 (0.414)

model8 = lm(self_representativeness ~ iv + trust + iv * trust, data = df)
summary(model8) # --> The results show that if the conditions is combined with perceived trust the perceived self representativeness is not significantly influenced, p-value > 0.05 (0.937055).

### However, again, the moderated mediation takes place simultaneously thus we will have to anlyze this too with the help of Hayes (2022) ###
set.seed(56789) # Setting the seed for replicability for the bootstrapping
process(data = df, y = "dv_manipulation", x = "iv", m = "self_representativeness", w = "trust", model = 5)

# --> I have to explain the results still


## --- VISUALIZAITON --- ##
df %>% 
  group_by(iv) %>% 
  summarize(mean_dv = mean(dv_manipulation)) %>%
  mutate(iv = ifelse(iv == 0, "Money", "Money-as-time-worked")) %>% 
  ggplot(aes(x = iv, y = mean_dv)) +
  geom_col() +
  labs(x = "Donation Frames", y = "Mean Donation", title = "Mean Donation by Donation Frame") +
  theme_bw()

df %>% 
  group_by(randomized_group) %>% 
  summarize(mean_dv = mean(dv_manipulation)) %>% 
  ggplot(aes(x = randomized_group, y = mean_dv)) + 
  geom_col() +
  labs(x = "Conditions", y = "Mean Donation", title = "Mean Donation by Condition") +
  theme_bw()



##### TRYING SOMETHING ELSE FROM YOUTUBE ##### --> https://www.youtube.com/watch?v=p2XbncjiA6k and https://www.youtube.com/watch?v=MIIF-ICF52Y
# Testing the significance of all the different paths
c_path = lm(dv_manipulation ~ iv, data = df)
summary(c_path)

a_path = lm(self_representativeness ~ iv, data = df)
summary(a_path)

b_path = lm(dv_manipulation ~ self_representativeness + iv, data = df)
summary(b_path)

# Complete mediation results:
set.seed(78992850)

results_boot = mediate(a_path, b_path, sims = 5000, treat = "iv", mediator = "self_representativeness", boot = TRUE)
summary(results_boot)



##### Moderated mediation from youtube ##### --> https://www.youtube.com/watch?v=If0ap-Yonbc and https://www.youtube.com/watch?v=eYGwF7pRozI&t=8s
# This enables us to use process macro by Andrew F. Hayes (2022)
source("../../process.R")

## --> with self-representativeness as a mediator
# Testing if the c-path is significant
result_c_rep = lm(dv_manipulation ~ iv + trust + iv * trust, data = df)
summary(result_c_rep)

# Testing the moderated mediation
set.seed(56789)
process(data = df, y = "dv_manipulation", x = "iv", m = "self_representativeness", w = "trust", model = 8)

## --> with control as a moderator
# Testing if the c-path is significant
result_c_con = lm(dv_manipulation ~ iv + long_or_short + iv * long_or_short, data = df)
summary(result_c_con)

# testing the moderated mediation 
set.seed(56789)
process(data = df, y = "dv_manipulation", x = "iv", m = "control", w = "long_or_short", model = 8)