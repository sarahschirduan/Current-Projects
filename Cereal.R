# Read in file
cerealFile <- read.csv("Cereal.csv", header = TRUE)

#Look at variable types
str(cerealFile)

#Check for number of unique values within these variables
length(unique(cerealFile$protein))
length(unique(cerealFile$fat))
length(unique(cerealFile$vitamins))
length(unique(cerealFile$weight))
length(unique(cerealFile$cup))

#Change to factor variables
cerealFile$shelf <- as.factor(cerealFile$shelf)
cerealFile$ftrprotein <- as.factor(cerealFile$protein)
cerealFile$ftrfat <- as.factor(cerealFile$fat)
cerealFile$ftrvitamins <- as.factor(cerealFile$vitamins)
cerealFile$ftrweight <- as.factor(cerealFile$weight)
cerealFile$ftrcup <- as.factor(cerealFile$cup)

#Check for missing values
table(is.na(cerealFile))

#Initial plot attempt
#Chart relationship between Manufacturer & Sugars
sugarplot <- ggplot(cerealFile, aes(x=mfr, y=sugars)) +
geom_point(color="blue") + theme_classic() + xlab("Manufacturer")+
  ylab("Sugars")

print(sugarplot)
  
#Final plot with 2 y variables
#Chart relationship between Manufacturer & Fat
cols <- c("Fat"="seagreen","Sugars"="skyblue")
ggplot(cerealFile, aes(x=mfr)) + geom_point(aes(y=fat, colour='Fat'), size=5) + 
  geom_point(aes(y=sugars, colour = 'Sugars'), shape = 18, size=3.5) +
  xlab("Manufacturer") + theme_classic() + 
  scale_colour_manual(name = "Nutrition", values=cols) + ylab(' ') + theme_classic()

library(dplyr)

#Table of Sugar & Fat by Cereal Manufacturer
AvgFat.SugarsTbl <- cerealFile %>% group_by(mfr) %>% summarise(Avg.Fat=prettyNum(round(mean(fat))), Avg.Sug=prettyNum(round(mean(sugars))))
print(AvgFat.SugarsTbl)

#Vitamin Graph 
ShelfByVitamins <- ggplot(cerealFile, aes(shelf)) + geom_bar(aes(fill = ftrvitamins)) +
        ylab(' ') + theme_classic() + xlab('Shelf') + labs(fill = "Vitamin Level") +
  ggtitle("Vitamin Level by Shelf") + theme(plot.title = element_text(hjust = 0.5))

print(ShelfByVitamins)

#Protein Graph
ShelfByProtein <- ggplot(cerealFile, aes(shelf)) + geom_bar(aes(fill = ftrprotein)) +
  theme_classic() + xlab('Shelf') + labs(fill = "Protein") + ylab(' ') +
  ggtitle("Protein Level by Shelf") + theme(plot.title = element_text(hjust = 0.5))

print(ShelfByProtein)

Potassium.FiberGraph <- ggplot(cerealFile, aes(x = potass, y = fiber)) + 
  geom_smooth(aes(color=shelf)) + xlab('Potassium') + ylab('Fiber') + theme_classic() +
 ggtitle("Fiber & Potassium by Shelf") + theme(plot.title = element_text(hjust = 0.5))

print(Potassium.FiberGraph)

#Table of Potassium & Fiber by Shelf
AvgPotass.FiberTbl <- cerealFile %>% group_by(shelf) %>% summarise(Avg.Potass=prettyNum(round(mean(potass))), Avg.Fiber=prettyNum(round(mean(fiber))))
print(AvgPotass.FiberTbl)

#Regular variables
summary(lm(cerealFile$rating ~ cerealFile$fiber + cerealFile$protein + 
             cerealFile$sugars + cerealFile$protein + cerealFile$fat + 
             cerealFile$vitamins + cerealFile$weight + cerealFile$cups))

#Factor variables
summary(lm(cerealFile$rating ~ cerealFile$fiber + cerealFile$sugars +
             cerealFile$ftrprotein + cerealFile$ftrfat + 
             cerealFile$ftrvitamins + cerealFile$ftrweight + cerealFile$ftrcup))

#Factor variables no weight or cups
summary(lm(cerealFile$rating ~ cerealFile$fiber + cerealFile$sugars +
             cerealFile$ftrprotein + cerealFile$ftrfat + cerealFile$ftrvitamins))

#Factor variables with calories, potass
summary(lm(cerealFile$rating ~ cerealFile$fiber + cerealFile$sugars +
             cerealFile$ftrprotein + cerealFile$ftrfat + cerealFile$ftrvitamins +
             cerealFile$calories + cerealFile$potass))

#Final
summary(lm(cerealFile$rating ~ cerealFile$fiber + cerealFile$sugars +
             cerealFile$ftrprotein + cerealFile$ftrfat + cerealFile$ftrvitamins))

