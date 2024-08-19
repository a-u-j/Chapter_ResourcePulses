install.packages("mvabund")
install.packages("lme4")
install.packages("DHARMa")
install.packages("car")
install.packages("sjPlot")
install.packages("performance")
install.packages("cowplot")
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

data_raw <- read.csv("~/GitHub/collar_data_preparation/output/2-cleaned_collar_data.csv")
coast <- read.csv("~/GitHub/collar_data_preparation/output/covariates/coast_gps.csv")
coast <- select(coast,"rownum", "coast_dist", "coast_yn")
tide <- read.csv("~/GitHub/collar_data_preparation/output/covariates/tidal_gps_2023.csv")
tide <- select(tide, rownum, tideheight_m,tide_cat)

#data <- left_join(data_raw,tide, by = "rownum")
data <- left_join(data_raw, coast, by = "rownum")
#data <- data %>% subset(year == "2023")

##### Filtering data for close inspection #####
filtered_data <- data %>%
  mutate(coast_dist_km = coast_dist*0.001) %>%
  subset(id == "UOF1801") %>%
  filter(day <= 25, month == 5)

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


###### Filtering data for binary variables ######
# Remove rows with NA values in tide_cat or coast_yn, and filter for coast_yn = "Y"
data_clean <- data %>%
  filter(!is.na(tide_cat) & coast_yn == "Y")
######

##### ggPlot of coast presences by ID in 2023 ####
# Aggregate the counts of each combination of tide_cat for each month
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

######

####### Analysis #######

# data_clean <- data %>%
#   filter(!is.na(tide_cat) & !is.na(coast_yn))

data_clean <- data %>%
  mutate(coast_yn = as.factor(coast_yn),
         #tide_cat = as.factor(tide_cat),
         month = as.factor(month),
         id = as.factor(id))

###### MODEL 1 #####
model <- glmer(coast_yn ~ tide_cat + month + (1 | id), data = data_clean, family = binomial())
summary(model)

vif(model)
m <- simulateResiduals(model, plot = TRUE)
residuals(simulationOutput, plot = T)

plotQQunif(m)
plotResiduals(m)
plotResiduals(m, form = data_clean$month)
testDispersion(m)

chisq.test(table(data_clean$tide_cat, data_clean$month))
######

##### MODEL 2

model2 <- glmer(coast_yn ~ month + (1 | id), data = data_clean, 
                family = binomial())

summary(model2)

vif(model2)
m2 <- simulateResiduals(model2, plot = TRUE)

plotQQunif(m2)
plotResiduals(m2)
plotResiduals(m2, form = data_clean$month)
testDispersion(m2)

######

##### MODEL 3 ######
model3 <- lmer(coast_dist ~ tide_cat + month + (1 | id), data = data)
summary(model3)

m3 <- simulateResiduals(model3, plot = TRUE)
plot(m3)
######

# Plotting model:

#sjPlot:

sjPlot::plot_model(model2, sort.est = FALSE,
                   show.values = TRUE, show.p = TRUE, value.offset = .3,
                   title = "Presence on coast as function of month",
                   vline.color = "grey")


sjPlot::tab_model(model2, show.re.var=TRUE,
                  dv.labels = "Presence on coast as function of month")
                  

#Performance

check_overdispersion(model2)
check_convergence(model2)


######


states <- read.csv()
