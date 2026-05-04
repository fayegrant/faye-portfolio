# Install once if needed
install.packages("tidyverse")

# Load libraries
library(tidyverse)

# Import data turtle_reviews.csv
turtle <- read.csv("turtle_reviews.csv", header = TRUE)

# View data
head(turtle)
str(turtle)
summary(turtle)

# Remove columns
turtle <- turtle %>%
  select(language,platform)

turtle <- turtle %>%
  rename(
    salary = remuneration..k..,
    spending = spending_score..1.100.
  )

turtle %>%
  group_by(loyalty_points) %>%
  summarise(count = n())


names(turtle)

#plot histogram
ggplot(turtle, aes(x = loyalty_points))+
geom_histogram(fill = "blue")

#hiostogram for loyalty points and age DOESNT WORK
ggplot(turtle) +
  geom_histogram(aes(x = loyalty_points, fill = "Loyalty Points"),
                 bins = 30, alpha = 0.5) +
  geom_histogram(aes(x = age, fill = "Age"),
                 bins = 30, alpha = 0.5) +
  scale_fill_manual(values = c("Loyalty Points" = "skyblue",
                               "Age" = "orange")) +
  
  # scattergraph age 
  theme_minimal()

ggplot(turtle, aes(x = age, y = loyalty_points)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "green", se = FALSE) +
  theme_minimal()

#line of best fit 
ggplot(turtle, aes(x = age, y = loyalty_points)) +
  geom_point() +
  geom_smooth(method = "lm")color = "darkgreen", se = FALSE) +
  theme_minimal()



#plot scattergraph
ggplot(turtle, aes(x = remuneration..k.., y = loyalty_points)) +
  geom_point(colour = "blue")

#line of best fit 
ggplot(turtle, aes(x = remuneration..k.., y = loyalty_points)) +
  geom_point() +
  geom_smooth(method = "lm")color = "darkgreen", se = FALSE) +
  theme_minimal()

#colour amended scattergraph 
ggplot(turtle, aes(x = remuneration..k.., y = loyalty_points)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "darkgreen", se = FALSE) +
  theme_minimal()

#plot boxplot
ggplot(turtle, aes(x = factor(spending_score..1.100.), y = loyalty_points)) +
  geom_boxplot()

#boxplot with colour
ggplot(turtle, aes(x = factor(spending_score..1.100.), y = loyalty_points)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen", alpha = 0.7) +
  theme_minimal()


# Install once if needed
install.packages("tidyverse")

# Load libraries
library(tidyverse)

# Load required libraries
library(tidyverse)

# Load your dataset
data <- read.csv("turtle_reviews.csv")

# View structure
str(data)
summary(data)

# Remove missing values
data <- na.omit(data)

# Ensure numeric variables are correct
data$age <- as.numeric(data$age)
data$salary <- as.numeric(data$salary)
data$spending_score <- as.numeric(data$spending_score)
data$loyalty_points <- as.numeric(data$loyalty_points)

# Correlation matrix
cor(data[, c("age", "remuneration..k..", "spending_score..1.100.", "loyalty_points")])

# Create model
mlr_model <- lm(loyalty_points ~ age + remuneration..k.. + spending_score..1.100., data = data)

# View model summary
summary(mlr_model)

# Diagnostic plots
par(mfrow = c(2, 2))
plot(mlr_model)
