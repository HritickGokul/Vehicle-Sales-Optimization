library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
install.packages("leaps")
library(leaps)
install.packages("pROC")
library(pROC)
install.packages("glmnet")
library(glmnet)
library(ggplot2)
install.packages("cluster")
install.packages("MASS")
library(cluster)
library(cluster)  # For cluster analysis
library(corrplot)  # For correlation plot
library(car)  # For VIF calculation
library(MASS) 

laptop_data <- read.csv("/Users/vaibhav/Desktop/Intermediate_Analytics/project/laptopPrice.csv")
laptop_data
str(laptop_data)
exchange_rate <- 0.013
laptop_data$Price_USD <- laptop_data$Price * exchange_rate
#Q1
ggplot(laptop_data, aes(x = brand, y = Price_USD)) +
  geom_boxplot() +
  labs(title = "Price Distribution of Laptops Among Brands",
       x = "Brand",
       y = "Price (in USD)") +
  theme_minimal()
#Q2
spec_counts <- laptop_data %>%
  group_by(ram_gb, processor_name, graphic_card_gb) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
spec_counts
most_sold_spec <- spec_counts[1, ]
cat("Most sold specification:\n")
cat("RAM:", most_sold_spec$ram_gb, "GB\n")
cat("Processor:", most_sold_spec$processor_name, "\n")
cat("GPU:", most_sold_spec$graphic_card_gb, "GB\n")

#Q3
summary_price <- aggregate(Price ~ brand, data = laptop_data, FUN = summary)
summary_price
boxplot(Price ~ brand, data = laptop_data, main = "Price Distribution by Brand", 
        xlab = "Brand", ylab = "Price", col = "lightblue")


anova_result <- aov(Price ~ brand, data = laptop_data)
summary(anova_result)

tukey_result <- TukeyHSD(anova_result)
print(tukey_result)


#Q4
laptop_data$rating_numeric <- as.numeric(gsub(" stars", "", laptop_data$rating))


lm_model <- lm(Price ~ rating_numeric, data = laptop_data)

summary(lm_model)



#adjusted r square
predictors <- c("ram_gb", "ssd", "hdd", "graphic_card_gb", "rating","Price_USD" )

lm_model <- lm(Price_USD ~ ., data = laptop_data[, predictors])

adjusted_r_squared <- summary(lm_model)$adj.r.squared

summary(lm_model)

print(paste("Adjusted R-squared:", adjusted_r_squared))


