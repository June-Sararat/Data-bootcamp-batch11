#Load the package 
library(readr)
library(tidyverse)
library(dplyr)


#read a csv file
m_data <- read_csv("Memory Test on Drugged Islanders Data.csv")

#View the rows and columns of the data 
dim(M_Data)

#Check the structure of the data
glimpse(M_Data)

#get a summary of the data
summary(M_Data)

#data cleaning
data <- na.omit(M_Data)

# Calculate mean memory score difference by Drug and Dosage
mean_data <- m_data %>%
  group_by(Drug, Dosage) %>%
  summarise(mean_diff = mean(Diff))

# Prepare data for plotting
drugs <- unique(m_data$Drug)
x <- c(1, 2, 3) # Dosages

# Create plot
plot(x, mean_data$mean_diff[mean_data$Drug == drugs [1]], #first drug
     type = "b",
     ylim = c (-5,25),
     ylab = "Average Difference in Memory Score",
     xlab = "Dosage",
     main = "Interaction Plot: Drug x Dosage")

# Add lines for each drug
for (i in 1:length(drugs)) {
  lines(x, mean_data$mean_diff[mean_data$Drug == drugs[i]],
        type = "b",
        col = i, # Use different colors
        lty = i  # Use different line types
        )
}

# Add legend
legend("topleft",
       legend = drugs,
       col = 1:length(drugs),
       lty = 1:length(drugs),
       cex = 0.8)

m_data$Happy_Sad_group <- as.factor(m_data$Happy_Sad_group)
m_data$Drug <- as.factor(m_data$Drug)
m_data$Dosage <- as.factor(m_data$Dosage)


plot(Diff~Happy_Sad_group, data = m_data)
plot(Diff~Drug, data = m_data)
plot(Diff~Dosage, data = m_data)




