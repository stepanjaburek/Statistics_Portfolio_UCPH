###### Portfolio 1: Binary outcomes and hierarchical models #######
## 
## Authors: Štěpán Jabůrek & Flynn Schirott
##
##
##

#------------------------------------------------------------------------------
# Setup
library(tidyverse)
library(devtools)
library(RiPraksis)
library(flexplot)
library(stargazer)
library(lme4)
library(alpaca)
library(ggeffects)
library(purrr)

#---------------------------------------------------
# Data
load("df.rda")

#------------------------------------------------------------------------------
# rename variables

#rename judges
df <- df %>%
  rename_with(
    ~ gsub("NS_judge_([0-9]+)_20", "Prop_Nazi_Judge_Appointed_\\1", .),
    matches("NS_judge_[0-9]+_20")
  )

# rename judges before nazis
df <- df %>%
  rename_with(
    ~ gsub("NS_judge_([0-9]+)_19", "Prop_Before_Nazi_Judge_Appointed_\\1", .),
    matches("NS_judge_[0-9]+_19")
  )

# rename judges using education as the proxy
df <- df %>%
  rename_with(
    ~ gsub("NS_judge_([0-9]+)_2", "Prop_Nazi_Judge_Educated_\\1", .),
    matches("NS_judge_[0-9]+_2")
  )

# rename judges educated before nazis
df <- df %>%
  rename_with(
    ~ gsub("NS_judge_([0-9]+)_5", "Prop_Before_Nazi_Judge_Educated_\\1", .),
    matches("NS_judge_[0-9]+_5")
  )

# ------------------------------------------
# Renaming prosecutors

# prosecutors
df <- df %>%
  rename_with(
    ~ gsub("NS_prosecutor_([0-9]+)_20", "Prop_Nazi_Prosecutor_Appointed_\\1", .),
    matches("NS_prosecutor_[0-9]+_20")
  )

# prosecutors before nazis
df <- df %>%
  rename_with(
    ~ gsub("NS_prosecutor_([0-9]+)_19", "Prop_Before_Nazi_Prosecutor_Appointed_\\1", .),
    matches("NS_prosecutor_[0-9]+_19")
  )

# prosecutors education proxy
df <- df %>%
  rename_with(
    ~ gsub("NS_prosecutor_([0-9]+)_2", "Prop_Nazi_Prosecutor_Educated_\\1", .),
    matches("NS_prosecutor_[0-9]+_2")
  )

# prosecutors educated before nazis
df <- df %>%
  rename_with(
    ~ gsub("NS_prosecutor_([0-9]+)_5", "Prop_Before_Nazi_Prosecutor_Educated_\\1", .),
    matches("NS_prosecutor_[0-9]+_5")
  )


#-------------------------------------------------------------------
# Flexplot histograms with titles using built in layered ggplot syntax
# titles describe what we plot

flexplot(Prop_Nazi_Judge_Appointed_1~1|state, df) + 
  labs(y = "Number of Courts", x = "Proportion of Nazi Appointed Judges")

flexplot(Prop_Nazi_Judge_Educated_1~1, df) + 
  labs(y = "Number of Courts", x = "Proportion of Nazi Educated Judges")

flexplot(Prop_Nazi_Prosecutor_Appointed_1~1, df) + 
  labs(y = "Number of Courts", x = "Proportion of Nazi Appointed Prosecutors")

flexplot(Prop_Before_Nazi_Prosecutor_Educated_1~1, df) + 
  labs(y = "Number of Courts", x = "Proportion of Nazi Educated Prosecutors")

flexplot(Months2~1, df) + 
  labs(title = "Length of Sentence Imposed (in Months)")


# ------------------------------

# numeric variable of the conviction
df$convicted_num <- ifelse(df$convicted == TRUE, 1,0)

# plot distribution of convictions
flexplot(convicted_num~1, df) +
  labs(title = "Baseline Rate of Conviction")

sum(df$convicted)/length(df$convicted)


# plot the number of crimes
crimes <- df %>% 
  reframe(
  Denunciation = Denunciation,
  Euthanasia = Euthanasia,
  FinalPhase = FinalPhase,
  ExterminationEinsatzgruppen = ExterminationEinsatzgruppen,
  ExterminationCamps = ExterminationCamps,
  ExterminationOther = ExterminationOther,
  Judicial = Judicial,
  NSDetainment = NSDetainment,
  NSOther = NSOther,
  WarCrimes = WarCrimes
)

# Calculate the sums of the columns
crime_sums <- colSums(crimes)

crime_sums_df <- data.frame(
  Crime = names(crime_sums),
  Count = as.numeric(crime_sums)
)

crime_sums_df$Crime <- fct_reorder(crime_sums_df$Crime, crime_sums_df$Count)

# Create the bar plot with sorted bars
ggplot(crime_sums_df, aes(x = Crime, y = Count)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Charges per Type of Crime") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#----------------------------------------------------------------
#  data for a descriptive statistics table of vars used in Table 3 (Kern and Vanbergs 2024)

df_desc <-
  df %>%
select(convicted,
       Prop_Nazi_Judge_Appointed_1,
       Prop_Before_Nazi_Judge_Appointed_1,
       Prop_Nazi_Prosecutor_Appointed_1,
       Prop_Before_Nazi_Prosecutor_Appointed_1,
       FirstSessionYear,
       state,
       crime) %>%
  as.data.frame()

stargazer(df_desc,
          summary = T,
          header = F,
          iqr = T, median = T, min.max = T,
          type = "latex",
          title = "Descriptive statitics")

df$convicted<-as.numeric(df$convicted)
cor.test(df$convicted, df$Prop_Nazi_Judge_Educated_1)



# -------------------- Question 3 -------------------------------------
# Question 3
# bivariate flexplots

flexplot(convicted_num~Prop_Nazi_Judge_Appointed_1, df, method = "logistic") + 
  labs(subtitle = "Logistic Bivariate Relationship",
       y = "Conviction Probability", x = "Proportion of Nazi Appointed Judges")

flexplot(convicted_num~Prop_Nazi_Judge_Educated_1, df, method = "logistic") + 
  labs(subtitle = "Logistic Bivariate Relationship",
       y = "Conviction Probability", x ="Proportion of Nazi Educated Judges")


# evolution over time flexplots
df$year_grouped <- ifelse(df$FirstSessionYear <= 1955, "Early 50´s",
                          ifelse(df$FirstSessionYear >1955 & df$FirstSessionYear <=1960, "Late 50´s",
                                 ifelse(df$FirstSessionYear >1960, "Early 60´s", "")))

df$FirstSessionYear<-as.numeric(df$FirstSessionYear)

flexplot(convicted_num~FirstSessionYear, df, method = "logistic") + 
  labs(subtitle = "Logistic Bivariate Relationship",
       y = "Conviction Probability", x = "Year") +
  scale_x_continuous(breaks = seq(1952,1964,by = 2))

flexplot(Prop_Nazi_Judge_Educated_1~FirstSessionYear, df, method = "loess") + 
  labs(subtitle = "LOESS Bivariate Relationship",
       y = "Proportion of Nazi Educated Judges", x = "Year") +
  scale_x_continuous(breaks = seq(1952,1964,by = 2))

flexplot(Prop_Nazi_Judge_Appointed_1~FirstSessionYear, df, method = "loess") + 
  labs(subtitle = "LOESS Bivariate Relationship",
       y = "Proportion of Nazi Appointed Judges", x = "Year") +
  scale_x_continuous(breaks = seq(1952,1964,by = 2))

flexplot(convicted_num~Prop_Nazi_Judge_Educated_1 + year_grouped, df, method = "logistic") + 
  labs(subtitle = "Logistic Bivariate Relationship",
       y = "Conviction Probability", x ="Proportion of Nazi Educated Judges")


flexplot(convicted_num~Prop_Nazi_Judge_Appointed_1 + year_grouped, df, method = "logistic") + 
  labs(subtitle = "Logistic Bivariate Relationship",
       y = "Conviction Probability", x ="Proportion of Nazi Appointed Judges")

 # variation within states
table(df$convicted, df$state)

# variation within courts
table(df$convicted, df$Court)

 # variation within years
table(df$convicted, df$FirstSessionYear)

flexplot(convicted_num~ Prop_Nazi_Judge_Appointed_1,
         df, method = "logistic", bins = 7)
?flexplot


# -------------------- Question 4 -------------------------------------
# Question 4
# Statistical replication (or rather reproduction as Ken Benoit would insist :D)

df$convicted<-as.factor(df$convicted)
?feglm
df$convicted

# Probit 2FE reproduction of reduced model in table 3 (Kern and Vanberg 2024)
model_alpaca <- alpaca::feglm(convicted ~ Prop_Nazi_Judge_Appointed_1 + Prop_Nazi_Prosecutor_Appointed_1
                        | FirstSessionYear + state + Denunciation + Euthanasia + FinalPhase + ExterminationEinsatzgruppen +
                          ExterminationCamps + ExterminationOther + Judicial + NSDetainment +
                         NSOther + WarCrimes,
                      data = df,
                      binomial(link = "probit"))
summary(model_alpaca)

# Probit 2FE reproduction of full model in table 3 (Kern and Vanberg 2024)
model_alpaca2 <- alpaca::feglm(convicted ~ Prop_Nazi_Judge_Appointed_1 + Prop_Nazi_Prosecutor_Appointed_1 +
                                Prop_Before_Nazi_Judge_Appointed_1 + Prop_Before_Nazi_Prosecutor_Appointed_1
                              | FirstSessionYear + state + Denunciation + Euthanasia + FinalPhase + ExterminationEinsatzgruppen +
                                ExterminationCamps + ExterminationOther + Judicial + NSDetainment +
                                NSOther + WarCrimes,
                              data = df,
                              binomial(link = "probit"))
summary(model_alpaca2)

summary(df$Prop_Before_Nazi_Judge_Appointed_1)
logoddsratio <- -2.5589      * 0.1556 # The coefficient multiplied by a typical change, here we go for 5% increase in public opinion)
logoddsratio # change in log odds

cumulative_change_odds <- (exp(logoddsratio)-1)*100 # to odds ratio and to a cumulative percentage change
cumulative_change_odds

library(texreg)
texreg(list(model_alpaca, model_alpaca2), file = "model_results.tex",  include.fixef = FALSE)

# -------------------------------------------
# logit models
# -------------------
# Logit 2FE reproduction of reduced model in table 3 (Kern and Vanberg 2024)

# create year_centered variable
df<- df %>%
  mutate(year_centered = FirstSessionYear - mean(FirstSessionYear))

model_alpaca <- alpaca::feglm(convicted ~ Prop_Nazi_Judge_Appointed_1 + Prop_Nazi_Prosecutor_Appointed_1 + year_centered +I(year_centered^2)
                              | state + Denunciation + Euthanasia + FinalPhase + ExterminationEinsatzgruppen +
                                ExterminationCamps + ExterminationOther + Judicial + NSDetainment +
                                NSOther + WarCrimes,
                              data = df,
                              binomial(link = "logit"))
summary(model_alpaca)

# Logit 2FE reproduction of full model in table 3 (Kern and Vanberg 2024)

model_alpaca2 <- alpaca::feglm(convicted ~ Prop_Nazi_Judge_Appointed_1 + Prop_Nazi_Prosecutor_Appointed_1 +
                                 Prop_Before_Nazi_Judge_Appointed_1 + Prop_Before_Nazi_Prosecutor_Appointed_1
                               | FirstSessionYear + state + Denunciation + Euthanasia + FinalPhase + ExterminationEinsatzgruppen +
                                 ExterminationCamps + ExterminationOther + Judicial + NSDetainment +
                                 NSOther + WarCrimes,
                               data = df,
                               binomial(link = "logit"))
summary(model_alpaca2)

texreg(list(model_alpaca, model_alpaca2), file = "model_results_logit.tex",  include.fixef = FALSE)



# -----------------------------------------------------------------
# Question 5

# cumulative odds changes and marginal effects
# logit versions

summary(df$Prop_Nazi_Judge_Appointed_1)
logoddsratio <- -4.18 * 0.25   # moving from a court with 25% of nazi tainted judges to 50%
logoddsratio

marginal_effect <- exp(logoddsratio)
marginal_effect

cumulative_change_odds <- (exp(logoddsratio)-1)*100
cumulative_change_odds

# Probit marginal effect of probability

?pnorm
pnorm(-2.5589 * 0.25)  # Approx. probability at 25%
pnorm(-2.5589 * 0.50)  # Approx. probability at 50%

#-----------------------------
# full scenario 1
# bavaria, 1958, war crimes
# same model only using glm instead of alpaca for it to work with ggeffects

twowayFE <- glm(convicted ~ Prop_Nazi_Judge_Appointed_1 + Prop_Nazi_Prosecutor_Appointed_1 +
                  Denunciation + Euthanasia + FinalPhase + ExterminationEinsatzgruppen + ExterminationCamps +
                  ExterminationOther + Judicial + NSDetainment  + NSOther + WarCrimes
                + factor(FirstSessionYear) + state,
                data = df,
                binomial(link = "probit"))
summary(twowayFE)

scenarios <-
  data.frame(
    Prop_Nazi_Judge_Appointed_1 = c(0,0.3134  ),
  Prop_Nazi_Prosecutor_Appointed_1 = median(df$Prop_Nazi_Prosecutor_Appointed_1), #median
    WarCrimes = 1,
  Denunciation  = 0,
  Euthanasia= 0,
  FinalPhase = 0,
  ExterminationEinsatzgruppen = 0,
    ExterminationCamps = 0,
  ExterminationOther = 0,
  Judicial = 0,
  NSDetainment = 0,
    NSOther= 0,
    state = "Bayern",
  FirstSessionYear = median(df$FirstSessionYear)
  )

scenarios <- scenarios %>%
  mutate(
    preds = predict(twowayFE, scenarios, type = "response")
  )

scenarios
?predict


# Predict across the range of Nazi judge appointments
predictions <- ggpredict(twowayFE,
                         terms = "Prop_Nazi_Judge_Appointed_1 [0, 0.3134  ]",
                         condition = c(
                           Prop_Nazi_Prosecutor_Appointed_1 = median(df$Prop_Nazi_Prosecutor_Appointed_1),
                           WarCrimes = 1,
                           Denunciation = 0,
                           Euthanasia = 0,
                           FinalPhase = 0,
                           ExterminationEinsatzgruppen = 0,
                           ExterminationCamps = 0,
                           ExterminationOther = 0,
                           Judicial = 0,
                           NSDetainment = 0,
                           NSOther = 0,
                           state = "Bayern",
                           FirstSessionYear = median(df$FirstSessionYear)))

# View results
print(predictions)

# Plot
plot(predictions) +
  labs(subtitle = "Minimum to Median shift. While standing War Crimes charges in Bavaria 1958",
       y = "Conviction Probability", x = "Proportion of Nazi Appointed Judges")

#----------------------
#scenario 2
# NRW, 1958, judicial crime, interquartile range
summary(df$Prop_Nazi_Judge_Appointed_1)

scenarios <-
  data.frame(
    Prop_Nazi_Judge_Appointed_1 = c(0.274,0.4186  ),
    Prop_Nazi_Prosecutor_Appointed_1 = median(df$Prop_Nazi_Prosecutor_Appointed_1), #median
    WarCrimes = 0,
    Denunciation  = 0,
    Euthanasia= 0,
    FinalPhase = 0,
    ExterminationEinsatzgruppen = 0,
    ExterminationCamps = 0,
    ExterminationOther = 0,
    Judicial = 1,
    NSDetainment = 0,
    NSOther= 0,
    state = "Nordrhein_Westfalen",
    FirstSessionYear = median(df$FirstSessionYear)
  )

scenarios <- scenarios %>%
  mutate(
    preds = predict(twowayFE, scenarios, type = "response")
  )

scenarios

# Predict across the range of Nazi judge appointments
predictions <- ggpredict(twowayFE,
                         terms = "Prop_Nazi_Judge_Appointed_1 [0.274, 0.4186  ]",
                         condition = c(
                           Prop_Nazi_Prosecutor_Appointed_1 = median(df$Prop_Nazi_Prosecutor_Appointed_1),
                           WarCrimes = 0,
                           Denunciation = 0,
                           Euthanasia = 0,
                           FinalPhase = 0,
                           ExterminationEinsatzgruppen = 0,
                           ExterminationCamps = 0,
                           ExterminationOther = 0,
                           Judicial = 1,
                           NSDetainment = 0,
                           NSOther = 0,
                           state = "Nordrhein_Westfalen",
                           FirstSessionYear = median(df$FirstSessionYear)))

# View results
print(predictions)

plot(predictions) +
  labs(subtitle = "Q1 to Q3 shift. While standing Judicial charges in Nordrhein Westfalen",
       y = "Conviction Probability", x = "Proportion of Nazi Appointed Judges")



#------------------------------------------------------
# Question 6

# Same replication models, but using the GLM function and manual fixed effects
# because alpaca doesnt show model metrics and doesnt export to stargazer :(

twowayFE <- glm(convicted ~ Prop_Nazi_Judge_Appointed_1 + Prop_Nazi_Prosecutor_Appointed_1 +
                   Denunciation + Euthanasia + FinalPhase + ExterminationEinsatzgruppen + ExterminationCamps +
                   ExterminationOther + Judicial + NSDetainment  + NSOther + WarCrimes
                 + factor(FirstSessionYear) + factor(state),
                 data = df,
                 binomial(link = "probit"))
summary(twowayFE) # same model only using glm instead of alpaca - alpaca doesnt show model statistics

twowayFE2 <- glm(convicted ~ Prop_Nazi_Judge_Appointed_1 + Prop_Nazi_Prosecutor_Appointed_1 +
                    Prop_Before_Nazi_Judge_Appointed_1 + Prop_Before_Nazi_Prosecutor_Appointed_1 +
                  Denunciation + Euthanasia + FinalPhase + ExterminationEinsatzgruppen + ExterminationCamps +
                   ExterminationOther + Judicial + NSDetainment  + NSOther + WarCrimes
                 + factor(FirstSessionYear) + factor(state),
                 data = df,
                 binomial(link = "probit"))
summary(twowayFE2)# same model only using glm instead of alpaca - alpaca doesnt show model statistics


#----------------------------------------
# OUR ALTERNATIVE MODELS

# court fixed effects reduced
twowayFE_court <- glm(convicted ~ Prop_Nazi_Judge_Appointed_1 + Prop_Nazi_Prosecutor_Appointed_1 +
                   Denunciation + Euthanasia + FinalPhase + ExterminationEinsatzgruppen + ExterminationCamps +
                   ExterminationOther + Judicial + NSDetainment  + NSOther + WarCrimes
                 +  factor(FirstSessionYear) + factor(Court),
                 data = df,
                 binomial(link = "probit"))
summary(twowayFE_court) # ignore the warning, we know that some courts dont have variation for mthe alpaca models

# court fixed effects full
twowayFE2_court <- glm(convicted ~ Prop_Nazi_Judge_Appointed_1 + Prop_Nazi_Prosecutor_Appointed_1 +
                    Prop_Before_Nazi_Judge_Appointed_1 + Prop_Before_Nazi_Prosecutor_Appointed_1 +
                    Denunciation + Euthanasia + FinalPhase + ExterminationEinsatzgruppen + ExterminationCamps +
                    ExterminationOther + Judicial + NSDetainment  + NSOther + WarCrimes
                 +  factor(FirstSessionYear) + factor(Court),
                  data = df,
                  binomial(link = "probit"))
summary(twowayFE2_court)


# state random effects reduced
multilevel <- glmer(convicted ~ Prop_Nazi_Judge_Appointed_1 + Prop_Nazi_Prosecutor_Appointed_1 +
                        Denunciation + Euthanasia + FinalPhase + ExterminationEinsatzgruppen +
                       ExterminationCamps + ExterminationOther + Judicial + NSDetainment +
                       NSOther + WarCrimes +   factor(FirstSessionYear) + (1|state),
                     data = df,
                     binomial(link = "probit"),
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(multilevel)

# state random effects full
multilevel2 <- glmer(convicted ~ Prop_Nazi_Judge_Appointed_1 + Prop_Nazi_Prosecutor_Appointed_1 +
                        Prop_Before_Nazi_Judge_Appointed_1 + Prop_Before_Nazi_Prosecutor_Appointed_1 +
                        Denunciation + Euthanasia + FinalPhase + ExterminationEinsatzgruppen +
                        ExterminationCamps + ExterminationOther + Judicial + NSDetainment +
                        NSOther + WarCrimes +  factor(FirstSessionYear) + (1|state),
                      data = df,
                      binomial(link = "probit"),
                     control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))

summary(multilevel2)

#--------------------
# court random effects reduced
multilevel_court <- glmer(convicted ~ Prop_Nazi_Judge_Appointed_1 + Prop_Nazi_Prosecutor_Appointed_1 +
                      Denunciation + Euthanasia + FinalPhase + ExterminationEinsatzgruppen +
                      ExterminationCamps + ExterminationOther + Judicial + NSDetainment +
                      NSOther + WarCrimes + factor(FirstSessionYear) + (1|Court),
                    data = df,
                    binomial(link = "probit"),
                    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(multilevel_court)

# court random effects full
multilevel2_court <- glmer(convicted ~ Prop_Nazi_Judge_Appointed_1 + Prop_Nazi_Prosecutor_Appointed_1 +
                       Prop_Before_Nazi_Judge_Appointed_1 + Prop_Before_Nazi_Prosecutor_Appointed_1 +
                       Denunciation + Euthanasia + FinalPhase + ExterminationEinsatzgruppen +
                       ExterminationCamps + ExterminationOther + Judicial + NSDetainment +
                       NSOther + WarCrimes +  factor(FirstSessionYear) + (1|Court),
                     data = df,
                     binomial(link = "probit"),
                     control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(multilevel2_court)


# court and state random effects
multilevel_both <- glmer(convicted ~ Prop_Nazi_Judge_Appointed_1 + Prop_Nazi_Prosecutor_Appointed_1 +
                            Denunciation + Euthanasia + FinalPhase + ExterminationEinsatzgruppen +
                            ExterminationCamps + ExterminationOther + Judicial + NSDetainment +
                            NSOther + WarCrimes + factor(FirstSessionYear) + (1|state) + (1|Court),
                          data = df,
                          binomial(link = "probit"),
                         control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5)))
summary(multilevel_both)


#-------------------

# reduced models AIC and BIC
small_models <- data.frame( # use the bics for the fixed effect probits later in overleaf
  Model = c("2FE","2FE Court ",  "Multilevel", "Multilevel Court"),
  AIC = c(AIC(twowayFE), AIC(twowayFE_court), AIC(multilevel),AIC(multilevel_court)),
  BIC = c(BIC(twowayFE), BIC(twowayFE_court), BIC(multilevel),BIC(multilevel_court))
) #
print(small_models)


# full models AIC and BIC
full_models <- data.frame(
  Model = c("2FE Full","2FE Court Full",  "Multilevel Full", "Multilevel Court Full"),
  AIC = c(AIC(twowayFE2), AIC(twowayFE2_court), AIC(multilevel2),AIC(multilevel2_court)),
  BIC = c(BIC(twowayFE2), BIC(twowayFE2_court), BIC(multilevel2),BIC(multilevel2_court))
)
print(full_models)



#--- export reduced models and omit fixed effects
stargazer::stargazer(twowayFE,twowayFE_court,multilevel,multilevel_court,
                     type="latex",
                     omit =c("factor\\(Court\\)", "state", "FirstSessionYear","Denunciation","Euthanasia",
                             "FinalPhase","ExterminationEinsatzgruppen",
                             "ExterminationCamps","ExterminationOther","Judicial",
                             "NSDetainment","NSOther", "WarCrimes"),
                     out= "small_models.tex")

# export full models and omit fixed effects
stargazer::stargazer(twowayFE2,twowayFE2_court,multilevel2,multilevel2_court,
                     type="latex",
                     omit =c("factor\\(Court\\)", "state", "FirstSessionYear","Denunciation","Euthanasia",
                             "FinalPhase","ExterminationEinsatzgruppen",
                             "ExterminationCamps","ExterminationOther","Judicial",
                             "NSDetainment","NSOther", "WarCrimes"),
                     out= "full_models.tex")



#-----------------------------
# predicting for brier score

de <- df %>%
  mutate(
    preds1 = predict(twowayFE,
                     newdata = df,
                     type = "response"),
    preds2 = predict(twowayFE2,
                     newdata = df,
                     type = "response"),
    preds3 = predict(twowayFE_court,
                     newdata = df,
                     type = "response"),
    preds4 = predict(twowayFE2_court,
                     newdata = df,
                     type = "response"),
    preds5 = predict(multilevel,
                     newdata = df,
                     type = "response"),
    preds6 = predict(multilevel2,
                     newdata = df,
                     type = "response"),
    preds7 = predict(multilevel_court,
                     newdata = df,
                     type = "response"),
    preds8 = predict(multilevel2_court,
                     newdata = df,
                     type = "response")
  )




# calculating brier score

brier <- de %>%
  mutate(diff1 = preds1 - convicted_num,
         diff1 = diff1^2,
         diff2 = preds2 - convicted_num,
         diff2 = diff2^2,

         diff3 = preds3 - convicted_num,
         diff3 = diff3^2,
         diff4 = preds4 - convicted_num,
         diff4 = diff4^2,

         diff5 = preds5 - convicted_num,
         diff5 = diff5^2,
         diff6 = preds6 - convicted_num,
         diff6 = diff6^2,

         diff7 = preds7 - convicted_num,
         diff7 = diff7^2,
         diff8 = preds8 - convicted_num,
         diff8 = diff8^2
         ) %>%
  reframe(Brier_twowayFE = mean(diff1, na.rm = T),
          Brier_twowayFE2 = mean(diff2, na.rm = T),

          Brier_twowayFE_court = mean(diff3, na.rm = T),
          Brier_twowayFE2_court = mean(diff4, na.rm = T),

          Brier_multilevel = mean(diff5, na.rm = T),
          Brier_multilevel2 = mean(diff6, na.rm = T),

          Brier_multilevel_court = mean(diff7, na.rm = T),
          Brier_multilevel2_court = mean(diff8, na.rm = T))


brier


brier_small <- brier %>% # set of reduced models
  select(1,3,5,7)

brier_full <- brier %>% # set of full models
    select(2,4,6,8)



stargazer(brier_small,
          summary = F,
          header = T,
          type = "latex", 
          out = "Brier_small.tex")

stargazer(brier_full,
          summary = F,
          header = T,
          type = "latex",
          out = "Brier_full.tex")




# separation plot for multilevel courts
tab <- 
  de %>% 
  dplyr::select(convicted, preds7) %>%
  arrange(sample(nrow(brier))) %>%
  arrange(preds7) %>%
  mutate(index = 1:nrow(de))


#Plot
tab %>%
  ggplot() +
  geom_segment(aes(x = index,
                   xend = index,
                   y = 0,
                   yend = 1,
                   color = as.factor(convicted)),
               alpha = 0.6) +
  geom_line(data = tab,
            aes(y = preds7,
                x = index))

# separation plot for 2FE state
tab <- 
  de %>% 
  dplyr::select(convicted, preds1) %>%
  arrange(sample(nrow(brier))) %>%
  arrange(preds1) %>%
  mutate(index = 1:nrow(de))


#Plot
tab %>%
  ggplot() +
  geom_segment(aes(x = index,
                   xend = index,
                   y = 0,
                   yend = 1,
                   color = as.factor(convicted)),
               alpha = 0.6) +
  geom_line(data = tab,
            aes(y = preds1,
                x = index))

