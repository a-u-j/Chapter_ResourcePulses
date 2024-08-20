install.packages("mgcViz")
library(mgcViz)
library(mgcv)
library(effects)
library(performance)
library(car)
library(ggplot2)
library(mvabund)
library(dplyr)
library(tidyverse)
library(lme4)
library(DHARMa)
library(sjPlot)
library(cowplot)
library(cosinor)

options(scipen = 20)

data_raw <- read.csv("~/GitHub/collar_data_preparation/output/2-cleaned_collar_data.csv")
coast <- read.csv("~/GitHub/collar_data_preparation/output/covariates/coast_gps.csv")
coast <- select(coast,"rownum", "coast_dist", "coast_yn")
tide <- read.csv("~/GitHub/collar_data_preparation/output/covariates/tidal_gps_2023.csv")
tide <- select(tide, rownum, tideheight_m,tide_cat)

data <- data_raw %>%
  left_join(tide, by = "rownum") %>%
  left_join(coast, by = "rownum")

data_2023 <- data %>% subset(year == "2023") # Only relevant if looking at tide

##### (Can ignore) Filtering data for closer inspection #####
filtered_data <- data %>%
  mutate(coast_dist_km = coast_dist*0.001) %>%
  subset(id == "UOF1801") %>%
  filter(day <= 25, month == 5) %>%
  subset(year == "2023")

# Create the plot
ggplot(filtered_data, aes(x = hour)) +
  geom_point(aes(y = coast_dist, color = "Coast Distance"), size = 0.5) + 
  geom_smooth(aes(y = coast_dist, color = "Coast Distance"), method = "loess", se = FALSE, linewidth = 0.5) +
  
  geom_line(aes(y = tideheight_m * 500, color = "Tide Height")) +  # Scale tideheight_m
  scale_y_continuous(
    name = "Coast Distance (m)",
    sec.axis = sec_axis(~./500, name = "Tide Height (m)")  # Reverse the scaling for the secondary axis
  ) +
  scale_color_manual(
    name = "Legend",
    values = c("Coast Distance" = "blue", "Tide Height" = "red")
  ) +
  labs(x = "Hour") +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(color = "red"),  # Secondary y-axis title color
    axis.text.y.right = element_text(color = "red")    # Secondary y-axis text color
  ) +
  facet_wrap(~day, nrow = 5, ncol = 5)



##### (Can ignore) ggPlot of coast presences by ID in 2023 ####
# Aggregate the counts of ID for each month to get average counts of coast binary
data_summary <- data_clean %>%
  group_by(id, month, coast_yn) %>%
  summarize(count = n(), .groups = 'drop')

# Create a bar plot
ggplot(data_summary, aes(x = as.factor(month), y = count, fill = coast_yn)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Count of Coast Presences by ID in 2023",
       x = "Month",
       y = "Count") +
  scale_x_discrete(labels = month.abb) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~id, scales = "free_y") +
  theme_cowplot(12)



##### Cleaning data for analysis ####
data_clean_2023 <- data_2023 %>%
  mutate(coast_yn = as.factor(coast_yn),
         tide_cat = as.factor(tide_cat),
         month = as.factor(month),
         id = as.factor(id))

data_clean <- data %>%
  mutate(coast_yn = as.factor(coast_yn),
         tide_cat = as.factor(tide_cat),
         month = as.factor(month),
         id = as.factor(id))

##### Statistical test of difference

data_receding <- data_clean %>% 
  filter(tide_cat == "receding") %>% 
  pull(coast_dist)
data_rising <- data_clean %>% 
  filter(tide_cat == "rising") %>% 
  pull(coast_dist)

# Check normality
ks_test_result <- ks.test(data_receding, data_rising)
wilcox_test_result <- wilcox.test(data_receding, data_rising)



###### MODEL 1 <- TIDE, 2023 ####

model <- glmer(coast_yn ~ tide_cat + (1 | id), data = data_clean_2023, family = binomial())
summary(model)

vif(model)
m <- simulateResiduals(model, plot = TRUE)
residuals(simulationOutput, plot = T)

plotQQunif(m)
plotResiduals(m)
plotResiduals(m, form = data_clean$month)
testDispersion(m)

chisq.test(table(data_clean$tide_cat, data_clean$month))



##### MODEL 2 <- ONLY MONTH ####

model2 <- glmer(coast_yn ~ month + (1 | id), data = data_clean, 
                family = binomial())

summary(model2)

vif(model2)
m2 <- simulateResiduals(model2, plot = TRUE)

plotQQunif(m2)
plotResiduals(m2)
plotResiduals(m2, form = data_clean$month)
testDispersion(m2)

#Performance
check_overdispersion(model2)
check_convergence(model2)

# Plotting model:
#sjPlot:
sjPlot::plot_model(model2, sort.est = FALSE,
                   show.values = TRUE, show.p = TRUE, value.offset = .3,
                   title = "Presence on coast as function of month",
                   vline.color = "grey",
                   group.terms = c(1,2,2,2,3,3,3,4,4,4,1)
                   ) + theme_classic()


sjPlot::tab_model(model2, show.re.var=TRUE,
                  dv.labels = "Presence on coast as function of month")


# Further visualising

hours <- seq(0, 23, by = 1)
cos_hour <- cos(2 * pi * hours / 24)
sin_hour <- sin(2 * pi * hours / 24)
month <- factor(rep(1, length(hours)), levels = levels(data_clean$month))

newdata <- data.frame(month = month, cos_hour = cos_hour, sin_hour = sin_hour)
predicted_probs <- predict(model2, newdata = newdata, type = "response", re.form = NA)
plot(hours, predicted_probs, type = "l", xlab = "Hour of Day", ylab = "Predicted Probability of Coastal Presence")


##### MODEL 3 <- TIDE, Coast Dist - didn't use ######
model3 <- lmer(coast_dist ~ tide_cat + (1 | id), data = data_clean_2023)
summary(model3)




##### MODEL 4 <- Time of Day

gam_model <- gam(coast_yn ~ s(hour),
                   family = binomial, 
                   data = data_clean)

#gam_model_check <- simulateResiduals(gam_model, plot = TRUE)
#check_overdispersion(gam_model)

# Generate predictions of probabilities for each hour of the day
hours_seq <- data.frame(hour = seq(0, 23, length.out = 100))
linear_pred <- predict(gam_model, newdata = hours_seq, type = "link")
probabilities <- plogis(linear_pred)
plot_data <- data.frame(hour = hours_seq$hour, probability = probabilities)

ggplot(plot_data, aes(x = hour, y = probability)) +
  geom_line(color = "blue") +
  labs(x = "Hour of the Day", y = "Probability of Being on Coast",
       title = "Probability of Being on the Coast by Hour of the Day") +
  theme_minimal()

