# Load the package
library(readr)
library(dplyr)
library(agricolae)


# Read a csv file
mt <- read_csv("Memory Test on Drugged Islanders Data.csv")

# Check the structure of the data
glimpse(mt)

# Check the data for the older age group
mt %>% 
  select(age, Happy_Sad_group) %>%
  arrange(-age) %>%
  head(1)

# Check the number of participants in each group
happy_A_1 <- mt [(mt$Happy_Sad_group == "H") &
                   (mt$Drug == "A") &
                   (mt$Dosage == 1), ]
count(happy_A_1)


happy_A_2 <- mt [(mt$Happy_Sad_group == "H") &
                   (mt$Drug == "A") &
                   (mt$Dosage == 2), ]
count(happy_A_2)


happy_A_3 <- mt [(mt$Happy_Sad_group == "H") &
                   (mt$Drug == "A") &
                   (mt$Dosage == 3), ]
count(happy_A_3)


sad_A_1 <- mt [(mt$Happy_Sad_group == "S") &
                 (mt$Drug == "A") &
                 (mt$Dosage == 1), ]
count(sad_A_1)

sad_A_2 <- mt [(mt$Happy_Sad_group == "S") &
                 (mt$Drug == "A") &
                 (mt$Dosage == 2), ]
count(sad_A_2)

sad_A_3 <- mt [(mt$Happy_Sad_group == "S") &
                 (mt$Drug == "A") &
                 (mt$Dosage == 3), ]
count(sad_A_3)


happy_S_1 <- mt [(mt$Happy_Sad_group == "H") &
                   (mt$Drug == "S") &
                   (mt$Dosage == 1), ]
count(happy_S_1)


happy_S_2 <- mt [(mt$Happy_Sad_group == "H") &
                   (mt$Drug == "S") &
                   (mt$Dosage == 2), ]
count(happy_S_2)


happy_S_3 <- mt [(mt$Happy_Sad_group == "H") &
                   (mt$Drug == "S") &
                   (mt$Dosage == 3), ]
count(happy_S_3)


sad_S_1 <- mt [(mt$Happy_Sad_group == "S") &
                 (mt$Drug == "S") &
                 (mt$Dosage == 1), ]
count(sad_S_1)


sad_S_2 <- mt [(mt$Happy_Sad_group == "S") &
                 (mt$Drug == "S") &
                 (mt$Dosage == 2), ]
count(sad_S_2)


sad_S_3 <- mt [(mt$Happy_Sad_group == "S") &
                 (mt$Drug == "S") &
                 (mt$Dosage == 3), ]
count(sad_S_3)


happy_T_1 <- mt [(mt$Happy_Sad_group == "H") &
                   (mt$Drug == "T") &
                   (mt$Dosage == 1), ]
count(happy_T_1)


happy_T_2 <- mt [(mt$Happy_Sad_group == "H") &
                   (mt$Drug == "T") &
                   (mt$Dosage == 2), ]
count(happy_T_2)


happy_T_3 <- mt [(mt$Happy_Sad_group == "H") &
                   (mt$Drug == "T") &
                   (mt$Dosage == 3), ]
count(happy_T_3)

sad_T_1 <- mt [(mt$Happy_Sad_group == "S") &
                 (mt$Drug == "T") &
                 (mt$Dosage == 1), ]
count(sad_T_1)

sad_T_2 <- mt [(mt$Happy_Sad_group == "S") &
                 (mt$Drug == "T") &
                 (mt$Dosage == 2), ]
count(sad_T_2)

sad_T_3 <- mt [(mt$Happy_Sad_group == "S") &
                 (mt$Drug == "T") &
                 (mt$Dosage == 3), ]
count(sad_T_3)

# Change data types (Character -> factor)
mt$Happy_Sad_group <- as.factor(mt$Happy_Sad_group)
mt$Drug <- as.factor(mt$Drug)
mt$Dosage <- as.factor(mt$Dosage)

# Create boxplot
plot(Diff~Happy_Sad_group, data = mt)
plot(Diff~Drug, data = mt)
plot(Diff~Dosage, data = mt)

# Calculate mean memory score difference by Drug and Dosage
mean_data <- mt %>%
  group_by(Drug, Dosage) %>%
  summarise(mean_diff = mean(Diff))
mean_data

# Prepare data for plotting
drugs <- unique(mt$Drug)
x <- c(1, 2, 3) # Dosages

# Create plot
plot(x, mean_data$mean_diff[mean_data$Drug == drugs [1]],  
# First drug
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

# ANOVA
mt_anova <- aov( data = mt, Diff ~ Happy_Sad_group + Dosage + Drug)

summary(mt_anova)


# PostHoc Comparison
# Tukey Honest Significant Difference (HSD)
TukeyHSD(mt_anova)

HS_HSD <- HSD.test(y= mt_anova, trt = "Happy_Sad_group")
HS_HSD


Dosage_HSD <- HSD.test(y= mt_anova, trt = "Dosage")
Dosage_HSD


Drug_HSD <- HSD.test(y= mt_anova, trt = "Drug")
Drug_HSD
