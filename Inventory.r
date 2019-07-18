#read in csv file
Inventoryfile <- read.csv("C:/Users/sarah/Documents/R/Regression.Inv.csv",
                      header = T)

attach(Inventoryfile)  #attach header to each column
InvDF <- data.frame(Inventoryfile) #put it into a data frame

summary(InvDF) #examine summary stats

str(InvDF) #examine data type

#Use multilinear regression to examine impact on sales
lm1 <- lm(formula = Sales ~ OH + Traffic)
summary(lm1)

library(ggplot2)

ggplot(InvDF, aes(x=Traffic, y=Sales))+ theme_classic() +
  geom_point(color="skyblue") + xlab("Traffic") +
  ylab("Sales") + ggtitle(label = "Sales by Traffic") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = c(20000, 40000, 60000), labels = c("20,000", "40,000", "60,000")) +
  scale_x_continuous(breaks = c(2000, 4000, 6000, 8000), labels = c("2,000", "4,000", "6,000", "8,000"))