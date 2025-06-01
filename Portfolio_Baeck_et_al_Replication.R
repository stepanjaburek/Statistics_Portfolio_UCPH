###### Portfolio 2: Count outcomes and hierarchical models #######
## 
## Authors: Štěpán Jabůrek & Flynn Schirott
##
##
##


# ==================================== #
# -------------  SETUP --------------- 
# ==================================== # 

# Load data 
load("~/Desktop/Copenhagen/Beyond Linear Regression/Assignments/Assignment 2/Data Analysis/df-1.rda")

# Load packages
library(dplyr)
library(ggplot2)
library(stargazer)
library(xtable)
library(labelled)
library(lme4)
library(tidyr)
library(patchwork)
library(purrr)
library(modelsummary)
library(tibble)
library(DHARMa)
library(ggeffects)
library(marginaleffects)

# Save labeled dataset in a seperate one, so we can work with unlabeled data
df_label <- df
# Inspect variables with labels
var_label(df_label)

# Since data has labels attached, and this is sometimes confusing to R, we create a dataset without labels. 
df<- remove_labels(df)
summary(df)



# ==================================== #
# -----------  QUESTION 2 ------------ 
# ==================================== # 

# =============== #
#  ---- 2 a) ----
# =============== #

# describe its distribution 
summary(df$nreforms)
 

# HISTOGRAM BY POLICY ACROSS COUNTRIES #

# Create a policy factor variable to plot by policy type
df <- df %>%
  mutate(policy_lab = haven::as_factor(policy_f))
levels(df$policy_lab) <- c("economy", "social")

# Plot distribution
df %>%
  ggplot(aes(x = nreforms, fill = policy_lab)) +
  geom_histogram(
    binwidth = 1,              
    alpha = 0.8,
    position = "identity"
  ) +
  xlab("Number of reforms by cabinet") +
  ylab("Number of cabinets") +
  theme_gray() +
  theme(
    legend.title = element_blank(),
    legend.position = "top"
  )

# Save plot
ggsave("~/Desktop/Copenhagen/Beyond Linear Regression/Assignments/Assignment 2/Data Analysis/Question 2/reform_count.pdf", width = 6, height = 4)

# FURTHER DESCRIPTIVE STATS #

# Number of observations
nrow(df) # 150 cabinet-policy type combinations. Since there is only 2 types, there are 75 cabinets in the data. 

# Number of no events (cabinets who did not pass reforms)
sum(df$nreforms==0) # Only 3 cabinets did not implement a single reform

# Number of extreme events (high number of reform)
quantile(df$nreforms, 0.95) # 5% of cabinets passed more than 29 reforms in their terms. 

max(df$nreforms) # The most 'reformistic' cabinet passed 37 reforms in their term.


# =============== #
#  ---- 2 b) ----
# =============== #
# Estimate an intercept-only poisson model for the governments’ productivity 
mod1.pois <- glm(nreforms ~ 1,
                 # set probability distribution 
                 family = "poisson",
                 df)

summary(mod1.pois) 

# Take exponent of value
exp(2.54) # 12.68

# Display as a table in overleaf #
stargazer(mod1.pois,
          type = "latex",
          out = "~/Desktop/Copenhagen/Beyond Linear Regression/Assignments/Assignment 2/Data Analysis/Question 2/q2_poisson.tex",
          float = FALSE,
          title = "",
          dep.var.labels = c("Number of Reforms"),
          column.labels = c("Raw Count"),
          column.separate = c(1),
          covariate.labels = c("Intercept"),
          omit.stat = c("ll", "aic"),
          font.size = "footnotesize",
          notes.label = "Significance levels:",
          notes.align = "l")


# =============== #
#  ---- 2 c) ----
# =============== #

# Dirty test #
# Mean
mean(df$nreforms)
# Variance
var(df$nreforms) # Variance in the outcome is way higher than the mean

# According to Gelman and Hill (2007)
yhat_1 <- predict(mod1.pois, type = "response")
z_1 <- (df$nreforms - yhat_1) / sqrt(yhat_1)

n_1 <- nrow(df)
k_1 <- length(coef(mod1.pois))  

# Overdispersion ratio
cat("Overdispersion ratio is", sum(z_1^2) / (n_1 - k_1), "\n") # 5.94

# p-value of overdispersion vs. Chi² distribution
cat("p-value of overdispersion test is", 1 - pchisq(sum(z_1^2), df = n_1 - k_1), "\n")

# Create a function for all upcoming overdispersion calculations #
overdispersion <- function(model, observed) {
  yhat <- predict(model, type = "response")
  z <- (observed - yhat) / sqrt(yhat)
  n <- length(observed)
  k <- length(coef(model)) 
  ratio <- sum(z^2) / (n - k)
  p <- 1 - pchisq(sum(z^2), df = n - k)
  
  cat("Overdispersion ratio is", round(ratio, 2), "\n")
  cat("p-value of overdispersion test is", signif(p, 3), "\n")
}

# Check if it is consistent with manual calculation above
overdispersion(mod1.pois, df$nreforms) # 5.94 -> Yes, consistent


# =============== #
#  ---- 2 d) ----
# =============== #
# To avoid counting a cabinets passed reforms in a year twice, we have to count social and economic policy reforms together by cabinet.
df_policy <- df %>%
  group_by(cabinet) %>%
  summarise(all_reforms = sum(nreforms),
            cab_duration_y = first(cab_duration_y),
            reforms_year = all_reforms/cab_duration_y)

# Calculate average number of reforms per year
round(mean(df_policy$reforms_year, na.rm = T), 2) # 10.29 reforms per year

# VISUALIZE REFORMS BY YEAR #
df_policy %>%
  ggplot(aes(x = reforms_year)) +
  geom_histogram(
    bins = 30,              
    fill = "#1f77b4",       
    color = "white",        
    alpha = 0.7            
  ) +
  labs(
    x = "Average Number of Reforms per Year",
    y = "Number of Cabinets"
  ) +
  theme_grey()

ggsave("~/Desktop/Copenhagen/Beyond Linear Regression/Assignments/Assignment 2/Data Analysis/Question 2/reforms_per_year.pdf", width = 6, height = 4)


# =============== #
#  ---- 2 e) ----
# =============== #

# PLOT INTERCEPT-ONLY MODEL WITH OFFSET #
mod2.pois <- glm(nreforms ~ 1,
                 # set probability distribution 
                 family = "poisson",
                 offset = log(cab_duration_y),
                 df)

summary(mod2.pois)

# Calculate overdispersion
overdispersion(mod2.pois, df$nreforms) # 2.85


# POISSON MODELS WITH COVARIATES (NO OFFSET) #
mod3.pois <- glmer(nreforms ~ 
                     alt_rm_cp4y + # minister alternation                  
                     alt_swsw_coalition_cp4y + # Coalition alternation
                     mv_index_value + 
                     # Covariates
                     cab_av_inflationChange +
                     cab_av_realgdpgrChange +
                     cab_av_unemp_crisis_d + 
                     n_cab_parties + 
                     policy + 
                     rm_distance_swswCoailtion + 
                     cab_duration_y +
                     (1 | country_f), # random intercept (country)
                     family = poisson, 
                     data = df)

# Calculate overdispersion
overdispersion(mod3.pois, df$nreforms) # 2.24

# Predicted vs. observed values #
# Predict outcomes
df$preds_pois3 <- predict(mod3.pois, 
                          newdata = df,
                          type = "response",
                          allow.new.levels = TRUE)


# Create data frame in long format so legend can be created in a next step
df_long3 <- df %>%
  select(nreforms, preds_pois3) %>%
  pivot_longer(cols = everything(), names_to = "type", values_to = "reforms")

# Plot
plot3 <- ggplot(df_long3, aes(x = reforms, fill = type)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 25) +
  scale_fill_manual(values = c("nreforms" = "black", "preds_pois3" = "#1f77b4"),
                    labels = c("Observed", "Predicted"),
                    name = "") +
  ggtitle("Raw Count Model (No Offset)") +
  ylab("Number of Cabinets") +
  theme_grey() +
  theme(
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# WITH OFFSET #
mod4.pois <- glmer(nreforms ~ 
                    alt_rm_cp4y + # minister alternation                  
                    alt_swsw_coalition_cp4y + # coalition alternation
                    mv_index_value + 
                    # Covariates
                    cab_av_inflationChange +
                    cab_av_realgdpgrChange +
                    cab_av_unemp_crisis_d + 
                    n_cab_parties + 
                    policy + 
                    rm_distance_swswCoailtion +
                    # Set offset
                    offset(log(cab_duration_y)) +
                    (1 | country_f), # random intercept (country)
                    family = poisson, 
                    data = df)

# Calculate overdispersion
overdispersion(mod4.pois, df$nreforms) # 1.9

# Predicted vs. observed values #
# Predict outcomes
df$preds_pois4 <- predict(mod4.pois, 
                          newdata = df,
                          type = "response",
                          allow.new.levels = TRUE)


# Create data frame in long format so legend can be created in a next step
df_long4 <- df %>%
  select(nreforms, preds_pois4) %>%
  pivot_longer(cols = everything(), names_to = "type", values_to = "reforms")

# Plot
plot4 <- ggplot(df_long4, aes(x = reforms, fill = type)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 25) +
  scale_fill_manual(values = c("nreforms" = "black", "preds_pois4" = "#1f77b4"),
                    labels = c("Observed", "Predicted"),
                    name = "") +
  ggtitle("Rate Model (Offset)") +
  theme_grey() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )


# Dummy plot to have one central x-label #
x_label_plot <- ggplot() +
  theme_void() +
  annotate("text", x = 0.5, y = 0.5, 
           label = "Number of Reforms per Cabinet", 
           size = 4.5, hjust = 0.5)
plot3 <- plot3 +
  coord_cartesian(ylim = c(0, 30))  # Set y-axis range 

plot4 <- plot4 +
  coord_cartesian(ylim = c(0, 30))  # Match the same range

# Combine plots and dummy label row #
final_combined <- (plot3 | plot4) / x_label_plot +
  plot_layout(heights = c(10, 1), guides = "collect") &
  theme(legend.position = "bottom")

# Save 
ggsave("~/Desktop/Copenhagen/Beyond Linear Regression/Assignments/Assignment 2/Data Analysis/Question 2/hist_combined.pdf", final_combined, width = 6, height = 4)


# =============== #
#  ---- 2 f) ----
# =============== #

# HISTOGRAM BY POLICY & COUNTRY #

df %>%
  ggplot(aes(x = nreforms, fill = policy_lab)) +
  geom_histogram(
    binwidth = 1,             
    alpha = 0.8,
    position = "identity"
  ) +
  facet_wrap(~ country) +
  labs(
    x = "Number of reforms by cabinet",
    y = "Number of cabinets"
  ) +
  theme_gray() +
  theme(
    legend.title = element_blank(),
    legend.position = "top"
  )

# Save plot
ggsave("~/Desktop/Copenhagen/Beyond Linear Regression/Assignments/Assignment 2/Data Analysis/Question 2/reforms_by_country.pdf", width = 8, height = 6)


# ==================================== #
# -----------  QUESTION 3 ------------ 
# ==================================== # 

# =============== #
#  ---- 3 a) ----
# =============== #

# (1) Ministerial alternation #
summary(df$alt_rm_cp4y)

df %>%
  ggplot(aes(x = alt_rm_cp4y, fill = policy_lab)) +
  geom_histogram(
    binwidth = 0.1,              
    alpha = 0.8,                
    position = "identity"
  ) +
  xlab("Responsible Minister Alternation (4-Year Window)") +
  ylab("Number of Cabinets") +
  theme_gray() +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
  )

# save plot 
ggsave("~/Desktop/Copenhagen/Beyond Linear Regression/Assignments/Assignment 2/Data Analysis/Question 3/minister_alt.pdf", width = 6, height = 4)

# (2) coalition alternation #
summary(df$alt_swsw_coalition_cp4y)

# Plot
df %>%
  ggplot(aes(x = alt_swsw_coalition_cp4y, fill = policy_lab)) +
  geom_histogram(
    binwidth = 0.1,            
    alpha = 0.8,
    position = "identity"
  ) +
  xlab("Coalition Alternation (4-Year Window)") +
  ylab("Number of Cabinets") +
  theme_gray() +
  theme(
    legend.title = element_blank(),
    legend.position = "top"
  )

# Save the plot
ggsave("~/Desktop/Copenhagen/Beyond Linear Regression/Assignments/Assignment 2/Data Analysis/Question 3/coalition_alt.pdf", width = 6, height = 4)


# =============== #
#  ---- 3 b) ----
# =============== #

# Legislative Policing Index
df %>%
  ggplot(aes(x = mv_index_value, fill = country)) +
  geom_histogram(
    binwidth = 0.1,              
    alpha = 0.8,
    position = "identity"        
  ) +
  xlab("Legislative Policing Index") +
  ylab("Number of Cabinets") +
  theme_gray() +
  theme(
    legend.title = element_blank(),
    legend.position = "top"
  )

# Save the plot
ggsave("~/Desktop/Copenhagen/Beyond Linear Regression/Assignments/Assignment 2/Data Analysis/Question 3/mv_index_value.pdf", width = 6, height = 4)


# =============== #
#  ---- 3 c) ----
# =============== #

# (1) Ministerial alternation #
# Set a seed so jitter remains constant
set.seed(123)

# Plot
df %>%
  ggplot +
  #Scatterplot with jittering
  geom_jitter(aes(
    y = nreforms,
    x = alt_rm_cp4y
  ),
  alpha = 0.4) +
  #Local regression
  geom_smooth(aes(
    y = nreforms,
    x = alt_rm_cp4y
  )
  ) +
  # Linear regression 
  geom_smooth(aes(
    y = nreforms,
    x = alt_rm_cp4y
  ),
  method = "lm",
  color = "grey30",
  linetype = "dashed",
  se = FALSE
  ) +
  ylab("Number of Reforms") +
  xlab("Ministerial Alternation")

# Save plot
ggsave("~/Desktop/Copenhagen/Beyond Linear Regression/Assignments/Assignment 2/Data Analysis/Question 3/biv_stats_minalt.pdf", width = 6, height = 4)

# (2) Coalition alternation #
# Set a seed so jitter remains constant
set.seed(123)

df %>%
  ggplot +
  # Scatterplot with jittering
  geom_jitter(aes(
    y = nreforms,
    x = alt_swsw_coalition_cp4y
  ),
  alpha = 0.4) +
  #Local regression
  geom_smooth(aes(
    y = nreforms,
    x = alt_swsw_coalition_cp4y
  )
  ) +
  # Linear regression 
  geom_smooth(aes(
    y = nreforms,
    x = alt_swsw_coalition_cp4y
  ),
  method = "lm",
  color = "grey30",
  linetype = "dashed",
  se = FALSE
  ) +
  ylab("Number of Reforms") +
  xlab("Coalition alternation")

# Save plot
ggsave("~/Desktop/Copenhagen/Beyond Linear Regression/Assignments/Assignment 2/Data Analysis/Question 3/biv_stats_colalt.pdf", width = 6, height = 4)

# (3) Legislative Policing Index #
# Set a seed so jitter remains constant
set.seed(123)

df %>%
  ggplot +
  # Scatterplot with jittering
  geom_jitter(aes(
    y = nreforms,
    x = mv_index_value
  ),
  alpha = 0.4) +
  # Local regression
  geom_smooth(aes(
    y = nreforms,
    x = mv_index_value
  )
  ) +
  # Linear regression 
  geom_smooth(aes(
    y = nreforms,
    x = mv_index_value
  ),
  method = "lm",
  color = "grey30",
  linetype = "dashed",
  se = FALSE
  ) +
  ylab("Number of Reforms") +
  xlab("Legislative Policing Index")

# Save plot
ggsave("~/Desktop/Copenhagen/Beyond Linear Regression/Assignments/Assignment 2/Data Analysis/Question 3/biv_stats_legindex.pdf", width = 6, height = 4)

# (4) Correlations #
# Define nice labels 
var_labels <- c(
  alt_rm_cp4y = "Ministerial Alternation",
  alt_swsw_coalition_cp4y = "Coalition Alternation",
  mv_index_value = "Legislative Policing"
)

# Create correlation table
cor_table <- names(var_labels) %>%
  map_df(~{
    test <- cor.test(df$nreforms, df[[.x]], use = "complete.obs")
    r <- round(test$estimate, 2)
    p <- test$p.value
    
    # significance stars
    stars <- case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      p < 0.1   ~ ".",
      TRUE      ~ ""
    )
    
    tibble(
      Variable = var_labels[[.x]],
      Correlation = paste0(r, stars)
    )
  })

# LaTeX output for Overleaf
stargazer(cor_table,
          type = "latex",
          summary = FALSE,
          rownames = FALSE,
          title = "Correlation with Reform Output",
          notes = c("* p < 0.05; ** p < 0.01; *** p < 0.001"),
          notes.align = "l"
)


# =============== #
#  ---- 3 d) ----
# =============== #

# Get an overview of leg. policing
summary(df$mv_index_value)
hist(df$mv_index_value)

# Dichotomous classifications of leg. policing 
df <- df %>%
  mutate(
    # Cutoff at 0
    policing_scale = if_else(mv_index_value >= 0, "Strong (0+)", "Weak (<0)"),
    # Cutoff at the median
    policing_median = if_else(
      mv_index_value >= median(mv_index_value, na.rm = TRUE),
      "Strong (Median+)",
      "Weak (Median-)"
    )
  )

# (1) Scale-based plot
p_scale <- df %>%
  ggplot(aes(x = alt_rm_cp4y, y = nreforms)) +
  geom_jitter(alpha = 0.3, width = 0.1, height = 0.1) +
  geom_smooth(method = "loess", se = TRUE, color = "blue", fill = "grey70", alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "grey30", linetype = "dashed") +
  facet_wrap(~ policing_scale) +
  labs(
    title = "Scale-Based Grouping",
    x = "Ministerial Alternation",
    y = "Number of Reforms"
  ) +
  theme_gray()

# (2) Median-based plot
p_median <- df %>%
  ggplot(aes(x = alt_rm_cp4y, y = nreforms)) +
  geom_jitter(alpha = 0.3, width = 0.1, height = 0.1) +
  geom_smooth(method = "loess", se = TRUE, color = "blue", fill = "grey70", alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "grey30", linetype = "dashed") +
  facet_wrap(~ policing_median) +
  labs(
    title = "Median-Based Grouping",
    x = "Ministerial Alternation",
    y = "Number of Reforms"
  ) +
  theme_gray()

# Combine both plots into one
combined_plot <- p_scale + p_median

# Save
ggsave("~/Desktop/Copenhagen/Beyond Linear Regression/Assignments/Assignment 2/Data Analysis/Question 3/interaction.pdf", combined_plot, width = 9, height = 4)



# Correlations by both classifications #
# Create function to do so
cor_summary <- function(data, group_var) {
  data %>%
    group_split(!!sym(group_var)) %>%
    map_df(~{
      g <- unique(.x[[group_var]])
      test <- cor.test(.x$nreforms, .x$alt_rm_cp4y, use = "complete.obs")
      r <- round(test$estimate, 2)
      p <- test$p.value
      
      stars <- case_when(
        p < 0.001 ~ "***",
        p < 0.01  ~ "**",
        p < 0.05  ~ "*",
        p < 0.1   ~ ".",
        TRUE      ~ ""
      )
      
      tibble(
        Group = paste0(g, " (", group_var, ")"),
        Correlation = paste0(r, stars)
      )
    })
}

# Combine both 
cor_combined <- bind_rows(
  cor_summary(df, "policing_scale"),
  cor_summary(df, "policing_median")
)

# Create stargazer table and output as latex code 
stargazer(cor_combined,
          type = "latex",
          summary = FALSE,
          rownames = FALSE,
          title = "Correlation between Ministerial Alternation and Reform Output by Leg. Oversight",
          notes = c("* p < 0.05; ** p < 0.01; *** p < 0.001"),
          notes.align = "l"
)

# =============== #
#  ---- 3 f) ----
# =============== #

# Save variables as an object
vars <- c("nreforms", "alt_rm_cp4y", "alt_swsw_coalition_cp4y",
          "mv_index_value", "n_cab_parties", "rm_distance_swswCoailtion",
          "cab_av_realgdpgrChange", "cab_av_unemp_crisis_d",
          "cab_av_inflationChange", "cab_duration_y")

# Calculation
descriptive_stats <- data.frame(
  Variable = vars,
  Mean     = sapply(df[vars], function(x) round(mean(x, na.rm = TRUE), 2)),
  SD       = sapply(df[vars], function(x) round(sd(x, na.rm = TRUE), 2)),
  Min      = sapply(df[vars], function(x) round(min(x, na.rm = TRUE), 2)),
  `25%`    = sapply(df[vars], function(x) round(quantile(x, 0.25, na.rm = TRUE), 2)),
  Median   = sapply(df[vars], function(x) round(median(x, na.rm = TRUE), 2)),
  `75%`    = sapply(df[vars], function(x) round(quantile(x, 0.75, na.rm = TRUE), 2)),
  Max      = sapply(df[vars], function(x) round(max(x, na.rm = TRUE), 2)),
  N        = sapply(df[vars], function(x) sum(!is.na(x)))
)

# Create Latex table
xt <- xtable(descriptive_stats,
             caption = "Descriptive Statistics",
             label = "tab:descstats",
             align = c("l", "l", rep("r", 8)))  

# Output as .tex file
print(xt,
      file = "~/Desktop/Copenhagen/Beyond Linear Regression/Assignments/Assignment 2/Data Analysis/Question 3/descriptive_stats.tex",  
      include.rownames = FALSE,
      caption.placement = "top",
      sanitize.text.function = identity)


# ==================================== #
# -----------  QUESTION 4 ------------ 
# ==================================== #

# Model 1
nbin.1 <- glmer.nb(nreforms ~
                     # Main predictors
                     alt_rm_cp4y + # minister alternation
                     alt_swsw_coalition_cp4y + # Coalition alternation
                     # Controls
                     n_cab_parties +
                     rm_distance_swswCoailtion +
                     cab_av_realgdpgrChange +
                     cab_av_unemp_crisis_d + 
                     cab_av_inflationChange +
                     cab_duration_y +
                     policy + # Automatically treated as a factor
                     (1 | country_f),
                   data = df)


nbin.2 <- glmer.nb(nreforms ~
                     # Main predictors
                     alt_rm_cp4y*mv_index_value +  # interaction of min alt and leg polic. 
                     alt_swsw_coalition_cp4y + # Coalition alternation
                     # Controls
                     n_cab_parties +
                     rm_distance_swswCoailtion +
                     cab_av_realgdpgrChange +
                     cab_av_unemp_crisis_d + 
                     cab_av_inflationChange +
                     cab_duration_y +
                     policy +
                     (1 | country_f),
                   data = df)


nbin.3 <- glmer.nb(nreforms ~
                     # Main predictors
                     alt_rm_cp4y +  #  minister alternation
                     alt_swsw_coalition_cp4y*mv_index_value + # interaction of cab alt and leg polic.
                     # Controls
                     n_cab_parties +
                     rm_distance_swswCoailtion +
                     cab_av_realgdpgrChange +
                     cab_av_unemp_crisis_d + 
                     cab_av_inflationChange +
                     cab_duration_y +
                     policy +
                     (1 | country_f),
                   data = df)

# Get a first idea of results
stargazer(nbin.1, nbin.2, nbin.3, 
          type = "text")


# Create a nice table for overleaf #
# relabel models
models <- list(
  "Model 1" = nbin.1,
  "Model 2" = nbin.2,
  "Model 3" = nbin.3
)

# Assign approrpiate variable names identical to paper
custom_labels <- c(
  "alt_rm_cp4y" = "Minister Alternation (RMA)",
  "alt_swsw_coalition_cp4y" = "Coalition Alternation (CA)",
  "mv_index_value" = "Legislative policing index",
  "alt_rm_cp4y:mv_index_value" = "RMA × Leg. policing",
  "alt_swsw_coalition_cp4y:mv_index_value" = "CA × Leg. policing",
  "n_cab_parties" = "Number of coalition parties",
  "rm_distance_swswCoailtion" = "Resp. minister-coalition distance",
  "cab_av_realgdpgrChange" = "Average Δ in GDP growth",
  "cab_av_unemp_crisis_d" = "Unemployment crisis (dummy)",
  "cab_av_inflationChange" = "Average Δ in inflation rate",
  "cab_duration_y" = "Cabinet duration (years)",
  "policysocial" = "Social (vs. economic) policy",
  "SD (Intercept country_f)" = "Random intercept (country level)"
)

# Extract Log-Likelihood & AIC 
loglik_1 <- round(as.numeric(logLik(nbin.1)), 2)
loglik_2 <- round(as.numeric(logLik(nbin.2)), 2)
loglik_3 <- round(as.numeric(logLik(nbin.3)), 2)

aic_1 <- round(AIC(nbin.1), 0)
aic_2 <- round(AIC(nbin.2), 0)
aic_3 <- round(AIC(nbin.3), 0)

# Define GOF-Values manually
extra_gof <- tribble(
  ~term,               ~`Model 1`, ~`Model 2`, ~`Model 3`,
  "Log Likelihood",    loglik_1,   loglik_2,   loglik_3,
  "AIC",               aic_1,      aic_2,      aic_3
)

# Create table and save as .tex
modelsummary(
  models,
  exponentiate = TRUE, # exponentiate as in paper
  coef_rename = custom_labels,
  stars = TRUE,
  title = "Exponentiated Multilevel Mixed-Effects Negative Binomial Models of Reform Output",
  add_rows = extra_gof,
  gof_map = NULL,
  output = "~/Desktop/Copenhagen/Beyond Linear Regression/Assignments/Assignment 2/Data Analysis/Question 4/models_table.tex",
  float = FALSE
)         

# ==================================== #
# -----------  QUESTION 5 ------------ 
# ==================================== # 

# =============== #
#  ---- 5 a) ----
# =============== #

# Calculate overdispersion
overdispersion(nbin.3, df$nreforms) # 2.30


# =============== #
#  ---- 5 b) ----
# =============== #

# Predicted vs. observed values #
# Predict outcomes
df$preds.repl.3 <- predict(nbin.3, 
                          newdata = df,
                          type = "response",
                          allow.new.levels = TRUE)


# Create data frame in long format so legend can be created in a next step
df_long_repl3 <- df %>%
  select(nreforms, preds.repl.3) %>%
  pivot_longer(cols = everything(), names_to = "type", values_to = "reforms")

# Plot
ggplot(df_long_repl3, aes(x = reforms, fill = type)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 25) +
  scale_fill_manual(
    values = c("nreforms" = "black", "preds.repl.3" = "#1f77b4"),
    labels = c("Observed", "Predicted"),
    name = ""
  ) +
  ylab("Number of Cabinets") +
  xlab("Number of Reforms per Cabinet")
  theme_grey() +
  theme(
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )
  
# Save
ggsave("~/Desktop/Copenhagen/Beyond Linear Regression/Assignments/Assignment 2/Data Analysis/Question 5/Task b/repl3_preds.pdf", width = 6, height = 4)


# Dharma Tests #
# Simulate residuals for model 3 
sim3 <- simulateResiduals(fittedModel = nbin.3, n = 1000) # Set to 1,000 times 

# (1) Overall diognostics
pdf("~/Desktop/Copenhagen/Beyond Linear Regression/Assignments/Assignment 2/Data Analysis/Question 5/Task b/dharma_mod3_overview.pdf", width = 7, height = 4)
plot(sim3, rank = FALSE)
dev.off()

# (2) Residuls vs. actual observations 
pdf("~/Desktop/Copenhagen/Beyond Linear Regression/Assignments/Assignment 2/Data Analysis/Question 5/Task b/dharma_mod3_resid_reforms.pdf", width = 7, height = 4)
plotResiduals(sim3, df$nreforms, rank = FALSE) # Get actual count on x-axis
dev.off()


# =============== #
#  ---- 5 c) ----
# =============== #

# (1) Cabinet offset
nbin.3.cab <- glmer.nb(nreforms ~
                     # Main predictors
                     alt_rm_cp4y +  #  minister alternation
                     alt_swsw_coalition_cp4y * mv_index_value + # interaction of cab alt and leg polic.
                     # Controls
                     n_cab_parties +
                     rm_distance_swswCoailtion +
                     cab_av_realgdpgrChange +
                     cab_av_unemp_crisis_d + 
                     cab_av_inflationChange +
                     policy +
                     offset(log(cab_duration_y)) +
                     (1 | country_f),
                   data = df)

summary(nbin.3.cab)

# (2) Quasi poisson 
# (2.1) no offset
nbin.3.quasipois.noff <- glm(
  nreforms ~ 
    alt_rm_cp4y +
    alt_swsw_coalition_cp4y * mv_index_value +
    n_cab_parties +
    rm_distance_swswCoailtion +
    cab_av_realgdpgrChange +
    cab_av_unemp_crisis_d +
    cab_av_inflationChange +
    cab_duration_y +
    policy,
  family = "quasipoisson",
  data = df
)

summary(nbin.3.quasipois.noff)

# (2.2) with offset
nbin.3.quasipois.off <- glm(
  nreforms ~ 
    alt_rm_cp4y +
    alt_swsw_coalition_cp4y * mv_index_value +
    n_cab_parties +
    rm_distance_swswCoailtion +
    cab_av_realgdpgrChange +
    cab_av_unemp_crisis_d +
    cab_av_inflationChange +
    policy,
  offset = log(cab_duration_y),
  family = "quasipoisson",
  data = df
)

summary(nbin.3.quasipois.off)

# (3) Cabinet random effects 
df$cabinet_f <- as.factor(df$cabinet)  # convert to factor

nbin.3.cab.random <- glmer.nb(
  nreforms ~ 
    alt_rm_cp4y +
    alt_swsw_coalition_cp4y * mv_index_value +
    n_cab_parties +
    rm_distance_swswCoailtion +
    cab_av_realgdpgrChange +
    cab_av_unemp_crisis_d +
    cab_av_inflationChange +
    cab_duration_y +
    policy +
    (1 | country_f) +        # country-level intercept
    (1 | cabinet_f),         # cabinet-level intercept
  data = df
)

summary(nbin.3.cab.random)


# Summarize regressions in one table #
# Give models meaningful names
models_named <- list(
  "NB (Offset)" = nbin.3.cab,
  "Quasi-Poisson" = nbin.3.quasipois.noff,
  "Quasi-Poisson (Offset)" = nbin.3.quasipois.off,
  "NB (Random Cabinet)" = nbin.3.cab.random
)

# variable names in line with the paper
custom_labels <- c(
  "alt_rm_cp4y" = "Minister Alternation (RMA)",
  "alt_swsw_coalition_cp4y" = "Coalition Alternation (CA)",
  "mv_index_value" = "Legislative policing index",
  "alt_swsw_coalition_cp4y:mv_index_value" = "CA × Leg. policing",
  "n_cab_parties" = "Number of coalition parties",
  "rm_distance_swswCoailtion" = "Resp. minister-coalition distance",
  "cab_av_realgdpgrChange" = "Average Δ in GDP growth",
  "cab_av_unemp_crisis_d" = "Unemployment crisis (dummy)",
  "cab_av_inflationChange" = "Average Δ in inflation rate",
  "cab_duration_y" = "Cabinet duration (years)",
  "policy" = "Social (vs. economic) policy",
  "SD (Intercept country_f)" = "Random intercept (country level)",
  "SD (Intercept cabinet_f)" = "Random intercept (cabinet level)"
)

# Add a row: Is there an offset or not? 
offset_row <- tibble(
  term = "Cabinet Duration (offset)",
  `Model 1` = "Yes",
  `Model 2` = "No",
  `Model 3` = "Yes",
  `Model 4` = "No"
)

# Number of observations as a character
obs_row <- tibble(
  term = "Num. Obs.",
  `Model 1` = as.character(nobs(nbin.3.cab)),
  `Model 2` = as.character(nobs(nbin.3.quasipois.noff)),
  `Model 3` = as.character(nobs(nbin.3.quasipois.off)),
  `Model 4` = as.character(nobs(nbin.3.cab.random))
)

# Log-Likelihood & AIC as character 
extra_gof <- tibble(
  term = c("Log-Likelihood", "AIC"),
  `Model 1` = c(as.character(round(logLik(nbin.3.cab), 0)), as.character(round(AIC(nbin.3.cab), 0))),
  `Model 2` = c("—", "—"),
  `Model 3` = c("—", "—"),
  `Model 4` = c(as.character(round(logLik(nbin.3.cab.random), 0)), as.character(round(AIC(nbin.3.cab.random), 0)))
)

# Merge stats into one object 
add_rows_final <- bind_rows(offset_row, obs_row, extra_gof)

# Create a table and save it as .tex 
modelsummary(
  models_named,
  exponentiate = TRUE,
  coef_rename = custom_labels,
  add_rows = add_rows_final,
  stars = TRUE,
  missing = "—",
  output = "~/Desktop/Copenhagen/Beyond Linear Regression/Assignments/Assignment 2/Data Analysis/Question 5/Task c/alternative_models.tex"
)

# =============== #
#  ---- 5 d) ----
# =============== #

# Calculate overdispersions
# NB (Offset)
overdispersion(nbin.3.cab, df$nreforms) # 1.91

# Quasi-Poisson
overdispersion(nbin.3.quasipois.noff, df$nreforms) # 2.54

# Quasi-Poisson (Offset)
overdispersion(nbin.3.quasipois.off, df$nreforms) # 2.2

# NB (Random Cabinet)
overdispersion(nbin.3.cab.random, df$nreforms) # 1



# Dharma Diagnostics #
# Set working directory
setwd("~/Desktop/Copenhagen/Beyond Linear Regression/Assignments/Assignment 2/Data Analysis/Question 5/Task d")

# (1) NB (Offset) 
# Run simulation
sim.nb.offset <- simulateResiduals(fittedModel = nbin.3.cab, n = 1000)  

# Overall diagnostics plot
pdf("dharma_nb_offset_overview.pdf", width = 7, height = 4)
plot(sim.nb.offset)
dev.off()

# Residuls vs. actual observations 
pdf("dharma_nb_offset_resid_reforms.pdf", width = 7, height = 4)
plotResiduals(sim.nb.offset, df$nreforms, rank = FALSE)
dev.off()


# (2) Quasi-Poisson (No offset)
# Residual plot
# Calculate residuals & fitted values 
df_resid <- data.frame(
  fitted = fitted(nbin.3.quasipois.noff),
  resid = resid(nbin.3.quasipois.noff, type = "pearson")
)

# Residual plot
ggplot(df_resid, aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  theme_grey() +
  labs(
    x = "Fitted values",
    y = "Pearson residuals"
  )

# Save
ggsave("performance_QP_noffset.pdf", width = 6, height = 4)

# (3) Quasi-Poisson (Offset) 
# Residual plot
# Calculate residuals und fitted values 
df_resid_offset <- data.frame(
  fitted_offset = fitted(nbin.3.quasipois.off),
  resid_offset = resid(nbin.3.quasipois.off, type = "pearson")
)

# Residual plot
ggplot(df_resid_offset, aes(x = fitted_offset, y = resid_offset)) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  theme_gray() +
  labs(
    x = "Fitted values",
    y = "Pearson residuals"
  )

# Save
ggsave("performance_QP_offset.pdf", width = 6, height = 4)


# (4) NB (Random Cabinet) 
# Run simulation
sim.nb.rand.cab <- simulateResiduals(fittedModel = nbin.3.cab.random, n = 1000)  

# Overall diagnostics plot
pdf("dharma_nb_random_overview.pdf", width = 7, height = 4)
plot(sim.nb.rand.cab)
dev.off()

# Residuls vs. actual observations 
pdf("dharma_nb_random_resid_reforms.pdf", width = 7, height = 4)
plotResiduals(sim.nb.rand.cab, df$nreforms, rank = FALSE)
dev.off()



# ==================================== #
# -----------  QUESTION 6 ------------ 
# ==================================== # 

# =============== #
#  ---- 6 a) ----
# =============== #
summary(df$alt_rm_cp4y)

# Extract fixed effect
coef_typ <- fixef(nbin.1)["alt_rm_cp4y"]
# Calculate marginal effect 
eff1 <- exp(coef_typ) - 1
eff1

# IQR
iqr <- IQR(df$alt_rm_cp4y, na.rm = T) # 1.074
iqr 

# Effect of RMA on outcome: Typical change #
# Calculate it 
eff2 <- 1 - (exp(coef_typ * iqr))
eff2 # -0.1252605

# ============= #
# Set Scenarios #
# ============= #

# Inspect variables of interest
summary(df$alt_rm_cp4y)
summary(df$n_cab_parties)
summary(df$cab_duration_y)
summary(df$cab_av_realgdpgrChange)
summary(df$cab_av_inflationChange)
summary(df$cab_av_unemp_crisis_d)

# (1) Minimum to Median RMA; All variables set to mean
eff_A <- ggpredict(nbin.1, terms = list("alt_rm_cp4y" = seq(0, 0.8, by = 0.01)))
# (2) Minimum to Median RMA; Small coaltion (minimum = 2)
eff_B <- ggpredict(nbin.1, terms = list("alt_rm_cp4y" = seq(0, 0.8, by = 0.01)), condition = c(n_cab_parties = 2))
# (3) IQR RMA; Long serving cabinet (3rd Qu)
eff_C <- ggpredict(nbin.1, terms = list("alt_rm_cp4y" = seq(0.3, 1.4, by = 0.01)), condition = c(cab_duration_y = 4))
# (4) IQR RMA; Good Economy (3rd Qu in GDP; 1st Qu. Inflation; No unemployment crisis ( = 0)
eff_D <- ggpredict(nbin.1, terms = list("alt_rm_cp4y" = seq(0.3, 1.4, by = 0.01)), 
                   condition = c(
                     cab_av_realgdpgrChange = 3, 
                     cab_av_inflationChange = 1.5, # Moderate Inflation
                     cab_av_unemp_crisis_d = 0 # No Unemployment crisis
                   ))

# ================== #
# First Differences  #
# ================== #
# Calculate start and end values 
pred_A_start <- head(eff_A$predicted, 1)
pred_A_end   <- tail(eff_A$predicted, 1)

pred_B_start <- head(eff_B$predicted, 1)
pred_B_end   <- tail(eff_B$predicted, 1)

pred_C_start <- head(eff_C$predicted, 1)
pred_C_end   <- tail(eff_C$predicted, 1)

pred_D_start <- head(eff_D$predicted, 1)
pred_D_end   <- tail(eff_D$predicted, 1)

# Calculate First Differences
fd_A <- pred_A_end - pred_A_start
fd_B <- pred_B_end - pred_B_start
fd_C <- pred_C_end - pred_C_start
fd_D <- pred_D_end - pred_D_start

# Create table
fd_table <- data.frame(
  Scenario = c(
    "Typical Case (Min → Median)",
    "Small Coalition (Min → Median)",
    "Long Cabinet Duration (IQR)",
    "Good Economy (IQR)"
  ),
  Start_Value = round(c(pred_A_start, pred_B_start, pred_C_start, pred_D_start), 2),
  End_Value   = round(c(pred_A_end,   pred_B_end,   pred_C_end,   pred_D_end),   2),
  First_Difference = round(c(fd_A, fd_B, fd_C, fd_D), 2)
)

# Set saving directory
out_path <- "~/Desktop/Copenhagen/Beyond Linear Regression/Assignments/Assignment 2/Data Analysis/Question 6/Task a/first_differences_table.tex"

# Export
print(
  xtable(fd_table,
         caption = "Predicted Reform Counts and First Differences by Scenario",
         label = "tab:full_first_differences"),
  include.rownames = FALSE,
  file = out_path
)


# ============ #
# Create Plots #
# ============ #
plot_A <- ggplot(eff_A, aes(x = x, y = predicted)) +
  geom_line(color = "black", size = 1.1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "gray70", alpha = 0.3) +
  ylim(5, 15) +
  labs(
    x = "Minister Alternation",
    y = "Predicted Number of Reforms"
  ) +
  theme_gray()

plot_B <- ggplot(eff_B, aes(x = x, y = predicted)) +
  geom_line(color = "black", size = 1.1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "gray70", alpha = 0.3) +
  ylim(5, 15) +
  labs(
    x = "Minister Alternation",
    y = "Predicted Number of Reforms"
  ) +
  theme_gray()

plot_C <- ggplot(eff_C, aes(x = x, y = predicted)) +
  geom_line(color = "black", size = 1.1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "gray70", alpha = 0.3) +
  ylim(10, 30) +
  labs(
    x = "Minister Alternation",
    y = "Predicted Number of Reforms"
  ) +
  theme_gray()

plot_D <- ggplot(eff_D, aes(x = x, y = predicted)) +
  geom_line(color = "black", size = 1.1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "gray70", alpha = 0.3) +
  ylim(5, 15) +
  labs(
    x = "Minister Alternation",
    y = "Predicted Number of Reforms"
  ) +
  theme_gray()

# Save 
save_path <- "~/Desktop/Copenhagen/Beyond Linear Regression/Assignments/Assignment 2/Data Analysis/Question 6/Task a/"

ggsave(filename = paste0(save_path, "plot_A_typical.pdf"), plot = plot_A, width = 7, height = 5)
ggsave(filename = paste0(save_path, "plot_B_small_coalition.pdf"), plot = plot_B, width = 7, height = 5)
ggsave(filename = paste0(save_path, "plot_C_long_duration.pdf"), plot = plot_C, width = 7, height = 5)
ggsave(filename = paste0(save_path, "plot_D_good_economy.pdf"), plot = plot_D, width = 7, height = 5)



# =============== #
#  ---- 6 b) ----
# =============== #
# Follow Berry et al. (2012): test marginal effect of X (minister alternation) at different levels of Z (legislative policing)

# (1) Test interaction term directly 
summary(nbin.2)$coefficients["alt_rm_cp4y:mv_index_value", ]

# (2) Estimate conditional marginal effects of RMA given min and max of legislative policing 
cond_mfx_b <- slopes(nbin.2,
                        variables = "alt_rm_cp4y",
                        newdata = datagrid(mv_index_value = c(min(df$mv_index_value, na.rm = TRUE),
                                                              max(df$mv_index_value, na.rm = TRUE)))
)

# Create table
cond_mfx_b_table <- cond_mfx_b %>%
  select(mv_index_value, estimate, std.error, conf.low, conf.high, p.value) %>%
  mutate(across(where(is.numeric), round, digits = 3)) %>%
  rename(
    `Legislative Policing` = mv_index_value,
    `Marginal Effect` = estimate,
    `Std. Error` = std.error,
    `95% CI Lower` = conf.low,
    `95% CI Upper` = conf.high,
    `p-value` = p.value
  )

# Export as latex
xtable_6b <- xtable(cond_mfx_b_table,
                    caption = "Conditional Marginal Effects of Minister Alternation Given Legislative Policing",
                    label = "tab:mfx_alt_given_policing",
                    align = c("l", "c", "c", "c", "c", "c", "c")
)

# Save as .tex 
print(xtable_6b, include.rownames = FALSE, type = "latex",
      file = "~/Desktop/Copenhagen/Beyond Linear Regression/Assignments/Assignment 2/Data Analysis/Question 6/Task b/cond_marg_b.tex")

# ============== #
# ---- 6 c) ----
# ============== #

# Calculate marginal effects
mfx_c <- slopes(nbin.2,
                variables = "mv_index_value",
                newdata = datagrid(alt_rm_cp4y = c(min(df$alt_rm_cp4y, na.rm = TRUE),
                                                      max(df$alt_rm_cp4y, na.rm = TRUE)))
)


# Create table
mfx_table <- mfx_c %>%
  select(alt_rm_cp4y, estimate, std.error, conf.low, conf.high, p.value) %>%
  mutate(across(where(is.numeric), round, digits = 3)) %>%
  rename(
    `Minister Turnover` = alt_rm_cp4y,
    `Marginal Effect` = estimate,
    `Std. Error` = std.error,
    `95% CI Lower` = conf.low,
    `95% CI Upper` = conf.high,
    `p-value` = p.value
  )

# Create latex output
xtable_out <- xtable(mfx_table,
                     caption = "Conditional Marginal Effects of Legislative Policing Given Ministerial Turnover",
                     label = "tab:mfx_policing_given_turnover",
                     align = c("l", "c", "c", "c", "c", "c", "c")
)

# Safe as .tex
print(xtable_out, include.rownames = FALSE, type = "latex",
      file = "~/Desktop/Copenhagen/Beyond Linear Regression/Assignments/Assignment 2/Data Analysis/Question 6/Task c/cond_marg_c.tex")

# =============== #
#  ---- 6 e) ----
# =============== #

# Plot 1: -1.9 (Minimum) #

# Predict
eff_1 <- ggpredict(nbin.2,
                   terms = "alt_rm_cp4y [0:4 by=0.05]",
                   condition = c(mv_index_value = -1.9))

# Plot
plot_min <- ggplot(eff_1, aes(x = x, y = predicted)) +
  geom_line(color = "black", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "gray60", alpha = 0.3) +
  labs(
    x = "Minister Alternation",
    y = "Predicted Number of Reforms"
  ) +
  theme_gray() +
  theme(legend.position = "none")

# Save 
ggsave("~/Desktop/Copenhagen/Beyond Linear Regression/Assignments/Assignment 2/Data Analysis/Question 6/Task e/fig_minimum_e.pdf", plot = plot_min, width = 5, height = 4)


# Plot 2: Policing = 0.46 (Median) #

# Predict
eff_2 <- ggpredict(nbin.2,
                   terms = "alt_rm_cp4y [0:4 by=0.05]",
                   condition = c(mv_index_value = 0.46))

# Plot
plot_med <- ggplot(eff_2, aes(x = x, y = predicted)) +
  geom_line(color = "black", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "gray60", alpha = 0.3) +
  labs(
    x = "Minister Alternation",
    y = "Predicted Number of Reforms"
  ) +
  theme_gray() +
  theme(legend.position = "none")

# Save
ggsave("~/Desktop/Copenhagen/Beyond Linear Regression/Assignments/Assignment 2/Data Analysis/Question 6/Task e/fig_median_e.pdf",  plot = plot_med, width = 5, height = 4)


# Plot 3: Policing = 0.66 (3rd Quartile) #

# Predict
eff_3 <- ggpredict(nbin.2,
                   terms = "alt_rm_cp4y [0:4 by=0.05]",
                   condition = c(mv_index_value = 0.66))

# Plot
plot_q3 <- ggplot(eff_3, aes(x = x, y = predicted)) +
  geom_line(color = "black", size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "gray60", alpha = 0.3) +
  labs(
    x = "Minister Alternation",
    y = "Predicted Number of Reforms"
  ) +
  theme_gray() +
  theme(legend.position = "none")

# Save
ggsave("~/Desktop/Copenhagen/Beyond Linear Regression/Assignments/Assignment 2/Data Analysis/Question 6/Task e/fig_q3.pdf", plot = plot_q3, width = 5, height = 4)

