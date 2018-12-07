#Julian David Realpe Campaña, jdrealpec@gmail.com

library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(scales)

#A) Reading data and assembling the data frame 
data <- read_csv("screening_exercise_orders_v201810.csv")
data_f <- data %>% select(customer_id, gender, date)
data_count = count(data_f, customer_id)
data_f = data_f %>% group_by(customer_id) %>%  arrange(desc(date)) %>% slice(1)

data_f = merge(data_f, data_count, by = c("customer_id"))
names(data_f)[3]=paste("most_recent_order_date")
names(data_f)[4]=paste("order_count")
data_f <- data_f[order(data_f$customer_id),]
head(data_f, n=10)

#B) Creating the weeks column and plotting counts order per week
data_f$most_recent_order_date <- as.Date(data_f$most_recent_order_date)
data_f$weeks <- as.Date(cut(data_f[,"most_recent_order_date"], breaks="week"))
agg <- data_f %>% group_by(weeks) %>% summarise(agg=sum(order_count))
ggplot(agg, aes(as.Date(weeks), agg)) + geom_point() + scale_x_date() + ylab("order count by Week") + xlab("Week") + geom_line()

#C) Calculating the mean for each gender
data %>% group_by(gender) %>% summarise(mean = sum(value))
data %>% group_by(gender) %>% summarise(mean = mean(value))

#The difference between genders is ~4.5% in mean order value. Considering the total value and the money difference ~$100.000 
# between them, it is in fact a sustancial amount of money that is certaninly significant.

# Confusion matrix, accuracy, sensitivity and specificity
table(data$predicted_gender, data$gender)

acc = (3349+5249)/(3349+3410+1463+5249)
sen = 5249/(3410+5249)
spe = 3349/(3349+1463)

#The accuracy of the predctions is ~64% which isn't great in general terms. However, considering
# the potential money that the model could generate in terms of accurate marketing strategies for
# each gender that could generate a greater profit than the actual base line.


#I always love to pre-visualize the data before starting to create models and predictions. I believe
# the model I create in my mind is the first model I could make and this always is helpful when looking
# for trends, predictions and patterns in data.
                                                                                                                                                                  