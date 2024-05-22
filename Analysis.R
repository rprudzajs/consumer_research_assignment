## -- INPUT -- ##
# Loading the necessary packages for the analysis
library(tidyverse)
library(mediation)
library(interactions)

# Loading in the data
df = read_csv("cleaned_data.csv")

# Checking the summary, to make sure everything was saved correctly
summary(df)

## --- ANALYSIS --- ##
# FRIST TRY FROM CHATGPT AND THE MODULES
# Mediator Models
mediation_rep = lm(self_representativeness ~ iv + age + gender, data = df)
summary(mediation_rep)

mediation_con = lm(control ~ iv + age + gender, data = df)
summary(mediation_con)

# Fit Outcome Model
full_model_rep = lm(dv_manipulation ~ iv + self_representativeness, data = df)
full_model_con = lm(dv_manipulation ~ iv + control, data = df)

full_model_test = lm(dv_manipulation ~ iv + self_representativeness + control, data = df)

# Bootstrapping Mediation Analysis
mediation_results_rep = mediate(mediation_rep, full_model_rep, treat = "iv", mediator = "self_representativeness", boot = TRUE, sims = 500)
mediation_results_con = mediate(mediation_con, full_model_con, treat = "iv", mediator = "control", boot = TRUE, sims = 500)

summary(mediation_results_rep)
summary(mediation_results_con)

mediation_results_rep2 = mediate(mediation_rep, full_model_test, treat = "iv", mediator = "self_representativeness", boot = TRUE, sims = 500)
mediation_results_con2 = mediate(mediation_con, full_model_test, treat = "iv", mediator = "control", boot = TRUE, sims = 500)

summary(mediation_results_rep2)
summary(mediation_results_con2)



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

## --> with control as a mediator
result_c_con = lm(dv_manipulation ~ iv + long_or_short + iv * long_or_short, data = df)
summary(result_c_con)

# testing the moderated mediation 
set.seed(56789)
process(data = df, y = "dv_manipulation", x = "iv", m = "control", w = "long_or_short", model = 8)
