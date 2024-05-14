## --- INPUT --- ##
# Loading the necessary packages for the analysis
library(tidyverse)
library(openxlsx)

# Loading in the data
df = read.xlsx("CR24 Spring class survey raw data.xlsx")

# Looking at the data
View(df)
summary(df)

# Checking for duplicate column names
duplicates <- names(df)[duplicated(names(df))]
print(duplicates)

## -- TRANSFORMATION --- ##
# Making unique column names, otherwise tidyverse will throw an error
names(df) = make.names(names(df), unique = TRUE)

# Deleting unnecessary columns since they are not informative
df = df %>% 
  select(-Start.Date, -End.Date, -Response.Type, -Progress, -Finished, -Recorded.Date)

# Combining all the different DV manipulation
df = df %>% 
  mutate(dv_manipulation = coalesce(Imagine.that.UNICEF.was.asking.you.to.donate.some.of.your.money..You.can.really.help.needy.children.by.donating.your.money..Anything.that.you.donate.would.be.used.to.make.a.long.term.impact..It.would.not.help.children.within.a.week.of.you.donating.it..but.would.be.used.for.programs.that.can.make.a.big.difference.over.time..How.much.money.would.you.donate., 
                                    Imagine.that.UNICEF.was.asking.you.to.donate.some.of.your.money..You.can.really.help.needy.children.by.donating.your.money..Anything.that.you.donate.would.be.used.to.make.an.immediate.impact..It.would.help.children.within.a.week.of.you.donating.it..How.much.money.would.you.donate., 
                                    Imagine.that.UNICEF.was.asking.you.to.donate.some.of.your.time..Here.is.how.it.works..Instead.of.volunteering.for.UNICEF.by.personally.helping.children.in.the.developing.world..you.could.help.just.as.much.by.donating.your.work..Specifically..you.could.donate.your.salary.from.some.amount.of.time.that.you.spend.working.at.a.job..In.this.way..you.can.really.help.needy.children.by.donating.your.work..Anything.that.you.donate.would.be.used.to.make.a.long.term.impact..It.would.not.help.children.within.a.week.of.you.donating.it..but.would.be.used.for.programs.that.can.make.a.big.difference.over.time..How.many.hours.of.your.work.would.you.donate.,
                                    Imagine.that.UNICEF.was.asking.you.to.donate.some.of.your.time..Here.is.how.it.works..Instead.of.volunteering.for.UNICEF.by.personally.helping.children.in.the.developing.world..you.could.help.just.as.much.by.donating.your.work..Specifically..you.could.donate.your.salary.from.some.amount.of.time.that.you.spend.working.at.a.job..In.this.way..you.can.really.help.needy.children.by.donating.your.work..Anything.that.you.donate.would.be.used.to.make.an.immediate.impact..It.would.help.children.within.a.week.of.you.donating.it..How.many.hours.of.your.work.would.you.donate.)) %>% 
  select(-Imagine.that.UNICEF.was.asking.you.to.donate.some.of.your.money..You.can.really.help.needy.children.by.donating.your.money..Anything.that.you.donate.would.be.used.to.make.a.long.term.impact..It.would.not.help.children.within.a.week.of.you.donating.it..but.would.be.used.for.programs.that.can.make.a.big.difference.over.time..How.much.money.would.you.donate., 
         -Imagine.that.UNICEF.was.asking.you.to.donate.some.of.your.money..You.can.really.help.needy.children.by.donating.your.money..Anything.that.you.donate.would.be.used.to.make.an.immediate.impact..It.would.help.children.within.a.week.of.you.donating.it..How.much.money.would.you.donate., 
         -Imagine.that.UNICEF.was.asking.you.to.donate.some.of.your.time..Here.is.how.it.works..Instead.of.volunteering.for.UNICEF.by.personally.helping.children.in.the.developing.world..you.could.help.just.as.much.by.donating.your.work..Specifically..you.could.donate.your.salary.from.some.amount.of.time.that.you.spend.working.at.a.job..In.this.way..you.can.really.help.needy.children.by.donating.your.work..Anything.that.you.donate.would.be.used.to.make.a.long.term.impact..It.would.not.help.children.within.a.week.of.you.donating.it..but.would.be.used.for.programs.that.can.make.a.big.difference.over.time..How.many.hours.of.your.work.would.you.donate., 
         -Imagine.that.UNICEF.was.asking.you.to.donate.some.of.your.time..Here.is.how.it.works..Instead.of.volunteering.for.UNICEF.by.personally.helping.children.in.the.developing.world..you.could.help.just.as.much.by.donating.your.work..Specifically..you.could.donate.your.salary.from.some.amount.of.time.that.you.spend.working.at.a.job..In.this.way..you.can.really.help.needy.children.by.donating.your.work..Anything.that.you.donate.would.be.used.to.make.an.immediate.impact..It.would.help.children.within.a.week.of.you.donating.it..How.many.hours.of.your.work.would.you.donate.)

# Combining all the question regarding personal significance (Rep1)
df = df %>% 
  mutate(rep1 = coalesce(Please.indicate.to.what.extent.you.agree.with.the.following.statements.regarding.donating.your.time.to.UNICEF..Donating.my.time.to.UNICEF.would.feel.personally.significant.to.me., 
                         Please.indicate.to.what.extent.you.agree.with.the.following.statements.regarding.donating.your.time.to.UNICEF..Donating.my.time.to.UNICEF.would.feel.personally.significant.to.me..1,
                         Please.indicate.to.what.extent.you.agree.with.the.following.statements.regarding.donating.your.money.to.UNICEF..Donating.my.money.to.UNICEF.would.feel.personally.significant.to.me.,
                         Please.indicate.to.what.extent.you.agree.with.the.following.statements.regarding.donating.your.money.to.UNICEF..Donating.my.money.to.UNICEF.would.feel.personally.significant.to.me..1)) %>%
  select(-Please.indicate.to.what.extent.you.agree.with.the.following.statements.regarding.donating.your.time.to.UNICEF..Donating.my.time.to.UNICEF.would.feel.personally.significant.to.me.,
         -Please.indicate.to.what.extent.you.agree.with.the.following.statements.regarding.donating.your.time.to.UNICEF..Donating.my.time.to.UNICEF.would.feel.personally.significant.to.me..1,
         -Please.indicate.to.what.extent.you.agree.with.the.following.statements.regarding.donating.your.money.to.UNICEF..Donating.my.money.to.UNICEF.would.feel.personally.significant.to.me.,
         -Please.indicate.to.what.extent.you.agree.with.the.following.statements.regarding.donating.your.money.to.UNICEF..Donating.my.money.to.UNICEF.would.feel.personally.significant.to.me..1)

# Combining all the question regarding values, beliefs, or identities (Rep2)
df = df %>% 
  mutate(rep2 = coalesce(Donating.my.time.to.UNICEF.would.resonate.with.my.values..beliefs..or.identities.,
                         Donating.my.time.to.UNICEF.would.resonate.with.my.values..beliefs..or.identities..1,
                         Donating.my.money.to.UNICEF.would.resonate.with.my.values..beliefs..or.identities.,
                         Donating.my.money.to.UNICEF.would.resonate.with.my.values..beliefs..or.identities..1)) %>%
  select(-Donating.my.time.to.UNICEF.would.resonate.with.my.values..beliefs..or.identities.,
         -Donating.my.time.to.UNICEF.would.resonate.with.my.values..beliefs..or.identities..1,
         -Donating.my.money.to.UNICEF.would.resonate.with.my.values..beliefs..or.identities.,
         -Donating.my.money.to.UNICEF.would.resonate.with.my.values..beliefs..or.identities..1)

# Combining all the question regarding who the respondent is as a person (Rep3)
df = df %>% 
  mutate(rep3 = coalesce(Donating.my.time.to.UNICEF.would.reflect.who.I.am.as.a.person.,
                         Donating.my.time.to.UNICEF.would.reflect.who.I.am.as.a.person..1,
                         Donating.my.money.to.UNICEF.would.reflect.who.I.am.as.a.person.,
                         Donating.my.money.to.UNICEF.would.reflect.who.I.am.as.a.person..1)) %>%
  select(-Donating.my.time.to.UNICEF.would.reflect.who.I.am.as.a.person.,
         -Donating.my.time.to.UNICEF.would.reflect.who.I.am.as.a.person..1,
         -Donating.my.money.to.UNICEF.would.reflect.who.I.am.as.a.person.,
         -Donating.my.money.to.UNICEF.would.reflect.who.I.am.as.a.person..1)

# Combining all the questions regarding the control about their donation (Control1)
df = df %>% 
  mutate(control1 = coalesce(Please.indicate.to.what.extent.you.agree.with.the.following.statements.regarding.donating.your.time.to.UNICEF..I.would.have.control.over.the.way.my.time.would.be.used.,
                             Please.indicate.to.what.extent.you.agree.with.the.following.statements.regarding.donating.your.time.to.UNICEF..I.would.have.control.over.the.way.my.time.would.be.used..1,
                             Please.indicate.to.what.extent.you.agree.with.the.following.statements.regarding.donating.your.time.to.UNICEF..I.would.have.control.over.the.way.my.money.would.be.used.,
                             Please.indicate.to.what.extent.you.agree.with.the.following.statements.regarding.donating.your.time.to.UNICEF..I.would.have.control.over.the.way.my.money.would.be.used..1)) %>%
  select(-Please.indicate.to.what.extent.you.agree.with.the.following.statements.regarding.donating.your.time.to.UNICEF..I.would.have.control.over.the.way.my.time.would.be.used.,
         -Please.indicate.to.what.extent.you.agree.with.the.following.statements.regarding.donating.your.time.to.UNICEF..I.would.have.control.over.the.way.my.time.would.be.used..1,
         -Please.indicate.to.what.extent.you.agree.with.the.following.statements.regarding.donating.your.time.to.UNICEF..I.would.have.control.over.the.way.my.money.would.be.used.,
         -Please.indicate.to.what.extent.you.agree.with.the.following.statements.regarding.donating.your.time.to.UNICEF..I.would.have.control.over.the.way.my.money.would.be.used..1)

# Combining all the questions regarding the ability to shape their donation (Control2)
df = df %>% 
  mutate(control2 = coalesce(I.would.have.the.ability.to.shape.how.my.time.would.be.utilized.,
                             I.would.have.the.ability.to.shape.how.my.time.would.be.utilized..1,
                             I.would.have.the.ability.to.shape.how.my.money.would.be.utilized.,
                             I.would.have.the.ability.to.shape.how.my.money.would.be.utilized..1)) %>%
  select(-I.would.have.the.ability.to.shape.how.my.time.would.be.utilized.,
         -I.would.have.the.ability.to.shape.how.my.time.would.be.utilized..1,
         -I.would.have.the.ability.to.shape.how.my.money.would.be.utilized.,
         -I.would.have.the.ability.to.shape.how.my.money.would.be.utilized..1)

# Combining all the questions regarding their personal involvement (Control3)
df = df %>% 
  mutate(control3 = coalesce(I.would.be.involved.in.how.my.time.would.be.used.,
                             I.would.be.involved.in.how.my.time.would.be.used..1,
                             I.would.be.involved.in.how.my.money.would.be.used.,
                             I.would.be.involved.in.how.my.money.would.be.used..1)) %>%
  select(-I.would.be.involved.in.how.my.time.would.be.used.,
         -I.would.be.involved.in.how.my.time.would.be.used..1,
         -I.would.be.involved.in.how.my.money.would.be.used.,
         -I.would.be.involved.in.how.my.money.would.be.used..1)

# Renaming the columns, so they are easier to understand
df = df %>% 
  rename(duration = Duration..in.seconds.,
         hourly_wage = How.much.money.do.you.make.per.hour.of.work.at.your.job.,
         trust = How.much.do.you.trust.UNICEF.,
         age = What.is.your.age.,
         gender = What.gender.do.you.identify.with.,
         randomized_group = FL_5...Block.Randomizer...Display.Order)

# Making sure the variable is all a single measure, we decided on using money
df = df %>%
  mutate(dv_manipulation = if_else(randomized_group == 'MoneyasTimeWorkedImmediate' | randomized_group == 'MoneyasTimeLongTerm', dv_manipulation * hourly_wage, 
                                   dv_manipulation))

# Removing all the rows with missing values --> There was a respondent who did not answer the second control question
df = df %>% 
  na.omit()

# Creating a new variable for the self_representativeness and control
df = df %>%
  group_by(duration, hourly_wage, trust, age, gender, randomized_group, dv_manipulation) %>%
  mutate(self_representativeness = ((rep1 + rep2 + rep3) / 3),
         control = ((control1 + control2 + control3) / 3))

## --- ANALYSIS --- ##
install.packages("mediation")
library(mediation)

# Specify the mediation model
med_model <- lm(cbind(self_representativeness, control ) ~ randomized_group, data = df)
out_model <- lm(dv_manipulation ~ randomized_group + self_representativeness + control, data = df)

# Combine the models into a mediation object
med_obj <- mediate(med_model, out_model, treat = "randomized_group", mediator = c("mediator1", "mediator2"))
summary(med_obj)

# Bootstrap for indirect effect
med_boot <- mediate(med_model, out_model, treat = "randomized_group", mediator = "mediator", sims = 1000)
summary(med_boot)







