library(ggplot2)
library(dplyr)
library(tidyr)


data <- read.csv("/Users/abhavbhanot/Downloads/ai4i2020.csv")  

head(data)

data("/Users/abhavbhanot/Downloads/ai4i2020.csv")
ggplot(data, aes(x= Type, y = Air.temperature..K.)) +
  geom_boxplot() +
  labs(title = "Air Temperature by Product Quality Variants",
       x = "Product Quality",
       y = "Air Temperature")

# a) Air temperature vs. product quality variant
ggplot(data, aes(x = Air.temperature..K., fill = Type)) + geom_bar(alpha = 3.5) +
  labs(title = "Air Temperature Distribution Across Product Quality Variants",
       x = "Air Temperature (K)", y = "Products")

# b) Process temperature vs. rotational speed
ggplot(data, aes(x = Process.temperature..K., y = Rotational.speed..rpm.)) +
  geom_point() +
  labs(title = "Process Temperature vs Rotational Speed",
       x = "Process Temperature",
       y = "Rotational Speed")

#c
ggplot(data, aes(x = Rotational.speed..rpm., y = Torque..Nm.)) +
  geom_line() +
  labs(title = "Torque vs Rotational Speed",
       x = "Rotational Speed",
       y = "Torque")

#d
ggplot(data, aes(x = Type, y = Tool.wear..min.)) +
geom_boxplot() +
geom_vline(xintercept = c(1, 2, 3), linetype = "dashed") +
labs(title = "Distribution of Tool Wear by Product Quality Variants",
     x = "Product Quality",
     y = "Tool Wear")

#e
table(data$Machine.failure)

success_count <- sum(data$Machine.failure == 0)
failure_count <- sum(data$Machine.failure != 0)

table_data <- data.frame(Status = c("Success", "Failure"), Count = c(success_count, failure_count))

print(table_data)

ggplot(data.frame(Status = c("Success", "Failure"), Count = c(success_count, failure_count)), aes(x = Status, y = Count)) +
   geom_bar(stat = "identity", fill = "steelblue") +
   xlab("Status") +
   ylab("Count") +
   ggtitle("Number of Successes and Failures")


#f
 failure_data <- data %>% filter(Machine.failure == 1)


 ggplot(failure_data, aes(x =  Air.temperature..K., y = Process.temperature..K.)) +
   geom_point() +
   geom_smooth(method = "lm", se = FALSE, color = "red") +
   labs(x = "Air Temperature", y = "Process Temperature", title = "Correlation between Air and Process Temperature during Machine Failures") +
   theme_minimal()


#g

 power_data <- data %>%
  mutate(power = ifelse(Machine.failure == 0, Torque..Nm. * Rotational.speed..rpm., NA)) %>%
  mutate(power = ifelse(Machine.failure == 1, Torque..Nm. * Rotational.speed..rpm., power))

ggplot(power_data, aes(x = factor(Machine.failure), y = power, fill = factor(Machine.failure))) +
  geom_violin() +
  scale_fill_manual(values = c("green", "red")) +
  labs(x = "Machine Failure", y = "Power (torque * rotational speed)", title = "Distribution of Power for Successful and Failed Processes") +
  theme_minimal()

#i How does tool wear correlate with torque for each product quality variant?

ggplot(data, aes(x =Torque..Nm., y =Tool.wear..min., color =Type)) +
  geom_point(alpha = 0.5) +
  labs(title = "Tool Wear vs Torque for Each Product Quality Variant",
       x = "Torque",
       y = "Tool Wear")


# j

failure_data <- data[data$Machine.failure == 1, ]

ggplot(failure_data, aes(x = Tool.wear..min.)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Distribution of Tool Wear at the Time of Machine Failure",
       x = "Tool Wear (minutes)",
       y = "Frequency")

#h


failure_modes_sum <- colSums(data[, c("TWF", "HDF", "PWF", "OSF", "RNF")],
                             na.rm = TRUE)


barplot(failure_modes_sum, 
        xlab = "Failure Mode", 
        ylab = "Occurrence", 
        main = "Occurrence of Machine Failure with Different Failure Modes")
