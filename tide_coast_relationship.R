install.packages("mvabund")
library(ggplot2)
library(mvabund)

data <- read.csv("~/GitHub/collar_data_preparation/output/3-collar_coast_tide.csv")

data$coast <- as.factor(data$coast)
data <- data %>% subset(year == "2023") %>%
  na.omit()
data$coast_numeric <- as.numeric(data$coast) - 1

m <- glm(coast_numeric ~ tideheight_m + Hour, family = "binomial", data = data)
m_many <- manyglm(coast_numeric ~ tideheight_m + Hour, family = "binomial", data = data)

m_int <- glm(coast_numeric ~ tideheight_m*Hour, family = "binomial", data = data)
m_many_int <- manyglm(coast_numeric ~ tideheight_m*Hour, family = "binomial", data = data)
plot(m_many_int)
# No fan shape, so mean-variance assumption reasonable for data. Residuals have a random component

anova(m_int, test = "Chisq")
