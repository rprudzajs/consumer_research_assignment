## -- INPUT -- ##
# Loading the necessary packages for the analysis
library(interactions)
library(tidyverse)
library(ggplot2)
library(psych)
library(car)

# Loading in the data
df = read_csv("cleaned_data.csv")

# Checking the summary, to make sure everything was saved correctly
summary(df)

## --- ANALYSIS --- ##
# Getting some summary statistics to report
describe(df)

# Correlation matrix between different variables
cor_matrix = df %>% 
  select(trust, self_representativeness, control) %>% 
  corr.test()
print(cor_matrix)

# Exploring the correlation between self-representativeness and trust
ggplot(data = df, aes(x = self_representativeness, y = trust)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Self-Representativeness", y = "Trust", title = "Checking for multicollinearity")

# Calculating the VIF
VIF = lm(self_representativeness ~ trust + control, data = df) # adding control because you need more than 2 variables
vif(VIF)

## --- DIRECT EFFECT -- ##
### H1: Compared to money, money-as-time-worked framing has a larger positive effect on monetary donation. ###
model1 = lm(dv_manipulation ~ iv, data = df)
summary(model1) 

## --- PARALLEL MEDIATION --- ##
### H2: The relationship between donation framing (money vs money-as-time-worked) and monetary donations is positively mediated by a higher level of self-representativeness. ###
model2 = lm(self_representativeness ~ iv, data = df)
summary(model2) 

### H3: The relationship between donation framing (money vs money-as-time-worked) and monetary donations is positively mediated by a higher level of control over the donation. ###
model3 = lm(control ~ iv, data = df)
summary(model3)

### However, this mediation takes place simultaneously thus we will have to analyze this with the help of Andrew F. Hayes (2022). ###
# This enables us to use process macro by Andrew F. Hayes (2022).
source("../../process.R") # --> This only works on my computer since this uses a specific path

set.seed(3451947) # Setting the seed for replicability for the bootstrapping
process(data = df, 
        y = "dv_manipulation", 
        x = "iv", 
        m = c("self_representativeness", "control"), 
        model = 4)

## --- MODERATED MEDIATION --- ##
### H4: The mediated effect of control over the donation is more positively moderated by immediate impact framing compared to long-term impact framing. ###
model4 = lm(control ~ iv + long_or_short + iv * long_or_short, data = df)
summary(model4) 

### H5: The relationship between donation framing (money vs money-as-time-worked) and monetary donations is more positively moderated by immediate impact framing than long-term impact framing. ###
model5 = lm(dv_manipulation ~ iv + long_or_short + iv * long_or_short, data = df)
summary(model5)

### However, again, the moderated mediation takes place simultaneously thus we will have to analyze this too with the help of Andrew F. Hayes (2022). ###
set.seed(1573920)
process(data = df, 
        y = "dv_manipulation", 
        x = "iv", m = "control", 
        w = "long_or_short", 
        model = 8)

### H6: The relationship between donation framing (money vs money-as-time-worked) and monetary donations is positively moderated by a higher level of trust of the charity. ###
model6 = lm(dv_manipulation ~ iv + trust + iv * trust, data = df)
summary(model6)

### However, again, the moderated mediation takes place simultaneously thus we will have to analyze this too with the help of Andrew F. Hayes (2022). ###
set.seed(56789) # Setting the seed for replicability for the bootstrapping
process(data = df, 
        y = "dv_manipulation", 
        x = "iv", m = "self_representativeness", 
        w = "trust", 
        model = 5)

# Using Johnson-Neyman to determine where trust has a significant effect on the direct relationship between Donation Framing and Monetary Donation
JN_model = lm(dv_manipulation ~ iv * trust, data = df)
trust_range = range(df$trust, na.rm = TRUE)

# This is also used for figure 6: Johson-Neyman Intervals regarding Trust on the direct relationship
johnson_neyman(model = JN_model, 
               pred = "iv", 
               modx = "trust", 
               plot = TRUE,
               mod.range = trust_range)

## --- VISUALIZAITON --- ##
# Figure 2: Average Monetary Donation by Donation Frame
df %>% 
  group_by(iv) %>% 
  summarize(mean_dv = mean(dv_manipulation)) %>%
  mutate(iv = ifelse(iv == 0, "Money", "Money-as-time-worked")) %>% 
  ggplot(aes(x = iv, y = mean_dv)) +
  geom_col() +
  labs(x = "Donation Frames", y = "Mean Donation", title = "Mean Donation by Donation Frame") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 30, face = "bold"),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20))

# Figure 3: Average Self-Representativeness by Donation Frame
df %>% 
  group_by(iv) %>% 
  summarize(mean_rep = mean(self_representativeness)) %>%
  mutate(iv = ifelse(iv == 0, "Money", "Money-as-time-worked")) %>% 
  ggplot(aes(x = iv, y = mean_rep)) +
  geom_col() +
  labs(x = "Donation Frames", y = "Mean Self-Representativeness", title = "Mean Self-Representativenss by Donation Frame") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 30, face = "bold"),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20))

# Figure 4: Average Control by Donation Frame
df %>% 
  group_by(iv) %>% 
  summarize(mean_con = mean(control)) %>%
  mutate(iv = ifelse(iv == 0, "Money", "Money-as-time-worked")) %>% 
  ggplot(aes(x = iv, y = mean_con)) +
  geom_col() +
  labs(x = "Donation Frames", y = "Mean Control", title = "Mean Control by Donation Frame") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 30, face = "bold"),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20))

# Figure 5: Average Monetary Donation by different levels of Framing of Impact
df %>% 
  group_by(randomized_group) %>% 
  summarize(mean_dv = mean(dv_manipulation)) %>% 
  ggplot(aes(x = randomized_group, y = mean_dv)) + 
  geom_col() +
  labs(x = "Levels of Framing of Impact", y = "Mean Donation", title = "Mean Donation by levels of Framing of Impact") +
  theme_bw() +
  theme(
    plot.title = element_text(size = 30, face = "bold"),
    axis.title.x = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    axis.text.x = element_text(size = 18),
    axis.text.y = element_text(size = 18))
