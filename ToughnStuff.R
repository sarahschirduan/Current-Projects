#read in csv file
Toughfile <- read.csv("C:/Users/sarah/Documents/R/ToughnStuff.csv",
                          header = T)

attach(Toughfile)  #attach header to each column

library(ggplot2)  #we'll use this later
library(dplyr)    #we'll use this later

GrMar <- round((Gross.profit/Revenue)*100, 2)  #create a column with GM

Var.Pl.Rev <- (Revenue - Planned.revenue) / Planned.revenue  #create a column with variation of Rev to Planned Rev

DummyGoal <- c(rep(0, 24666))  #create a Dummy variable

for (i in 1:24666) {            #all rows of Var. Pl. Rev column
  if(Var.Pl.Rev[i] >= -0.05)    #if var. is within -5%
  {DummyGoal[i]<- 1 }           #make i in Dummy Variable a 1
}

Toughfile <- cbind(Toughfile, GrMar, DummyGoal)  #binds file + new GM column + DummyGoal column

head(Toughfile)

RevbyYearPlot <- ggplot(Toughfile, aes(x=Year, y=Revenue))+ 
  geom_bar(stat = "identity", width = 0.5, fill="skyblue") + 
  ggtitle(label = "Revenue by Year")

RevbyYearPlot + theme_bw() + scale_y_continuous(labels=c("0","$500k","$1M", "$1.5M")) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5))

RevSummary <- Toughfile %>% group_by(Year) %>% summarise(Rev=prettyNum(sum(Revenue), big.mark = ","), Min.Rev=prettyNum(min(Revenue), big.mark = ","), Max.Rev=prettyNum(max(Revenue), big.mark = ","), GM=prettyNum(round(mean(GrMar), 2)))
print(RevSummary)

RevbyP.LinePlot <- ggplot(Toughfile, aes(x=Product.line, y=Revenue))+ 
  geom_bar(stat = "identity", width = 0.5, fill="skyblue") + xlab("Product Line") +
  ggtitle(label = "Revenue by Product Line")

RevbyP.LinePlot + theme_bw() + theme(axis.text.x = element_text(angle=30, hjust = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) + scale_y_continuous(labels=c("0","$500k","$1M", "$1.5M", "$2M"))

GMbyPL <- ggplot(Toughfile, aes(x=Product.line, y=GrMar)) + 
  stat_summary(fun.y="mean", geom="bar", fill="skyblue") + 
  ggtitle("GM by Product Line") + xlab("Product Line")

GMbyPL + theme_bw() + theme(axis.text.x = element_text(angle=30, hjust = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) + scale_y_continuous(breaks = c(0,10,20,30,40,50,60))

RevbyPLineTbl <- Toughfile %>% group_by(Product.line,Year) %>% summarise(Sum.Rev=prettyNum(sum(Revenue), big.mark = ","), GM=prettyNum(round(mean(GrMar), 2)))
print(RevbyPLineTbl)

VarRevPersAccessTbl <- Toughfile %>% filter(Product.line=="Personal Accessories") %>% group_by(Product.type) %>% summarise(Var.Rev=prettyNum(sum(Revenue[Year==2007])-sum(Revenue[Year==2006]), big.mark = ","),                                                                                                                      Var.GM.06.07=mean(GrMar[Year==2007])-mean(GrMar[Year==2006]), Avg.GM=mean(GrMar))
print(VarRevPersAccessTbl)

VarRevCampingTbl <- Toughfile %>% filter(Product.line=="Camping Equipment") %>% group_by(Product.type) %>% summarise(Var.Rev=prettyNum(sum(Revenue[Year==2007])-sum(Revenue[Year==2006]), big.mark = ","), 
                                                                                             Var.GM.06.07=mean(GrMar[Year==2007])-mean(GrMar[Year==2006]), Avg.GM=mean(GrMar))
print(VarRevCampingTbl)

RevbyCountryPlot <- ggplot(Toughfile, aes(x=reorder(Retailer.country, Revenue), y=Revenue))+ 
  geom_bar(stat = "identity", width = 0.5, fill="skyblue") + xlab("Country") +
  ggtitle(label = "Revenue by Country")

RevbyCountryPlot + theme_bw() + theme(axis.text.x = element_text(angle=90, hjust = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) + scale_y_continuous(labels=
                                                                       c("0","$2M","$4M", "$6M", "$8M")) + coord_flip()

GMtblbyCountry <- Toughfile %>% group_by(Retailer.country) %>% summarise(GM=prettyNum(round(mean(GrMar), 2)))
print(GMtblbyCountry , n=21)  #output shows that GM is fairly level across all countries

round((sum(Revenue[Year==2007])-sum(Revenue[Year==2006]))/sum(Revenue[Year==2006])*100, 2) #avg sales variation

RevbyCountrytbl <- Toughfile %>% group_by(Retailer.country) %>% summarise(Var.Percent=((sum(Revenue[Year==2007])-sum(Revenue[Year==2006]))/sum(Revenue[Year==2006]))*100, Var.Dollar=prettyNum(sum(Revenue[Year==2007])-sum(Revenue[Year==2006]), big.mark = ","))
print(RevbyCountrytbl, n=21)  #sales var % & var $ from 2006 to 2007 by Country

RevbyMethod <- ggplot(Toughfile, aes(x=reorder(Order.method.type, Revenue), y=Revenue))+ 
  geom_bar(stat = "identity", width = 0.5, fill="skyblue") + xlab("Order Method") +
  ggtitle(label = "Revenue by Order Method") + scale_y_continuous(labels = c("0", "$1M", "$2M", "$3M", "$4M"))

RevbyMethod + theme_bw() + theme(axis.text.x = element_text(angle=90, hjust = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5)) + coord_flip() 

GMtblbyOrderM <- Toughfile %>% group_by(Order.method.type) %>% summarise(GM=round(sum(as.numeric(Gross.profit))/sum(as.numeric(Revenue))*100,2))
print(GMtblbyOrderM)  #output shows average GM by Order Method Type

summary(glm(formula = DummyGoal ~ Revenue + as.factor(Retailer.country) + 
              as.factor(Product.line) + as.factor(Year), family = "binomial", data = Toughfile))

summary(lm(Revenue ~ as.factor(Order.method.type) + as.factor(Product.type) 
           + as.factor(Retailer.country) + as.factor(Year)))
