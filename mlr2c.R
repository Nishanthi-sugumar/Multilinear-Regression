computerdata<- read.csv(file.choose())
View(computerdata)
class(computerdata)

library(plyr)
Computer_data1 <- computerdata
Computer_data1$cd <- as.numeric(revalue(Computer_data1$cd,c("yes"=1, "no"=0)))
Computer_data1$multi <- as.numeric(revalue(Computer_data1$multi,c("yes"=1, "no"=0)))
Computer_data1$premium <- as.numeric(revalue(Computer_data1$premium,c("yes"=1, "no"=0)))
View(Computer_data1)
class(Computer_data1)
attach(Computer_data1)
summary(Computer_data1)

plot(speed, price)


plot(hd, price)


plot(ram, price)

plot(screen, price)
plot(cd,price)
plot(multi,price)
plot(premium,price)
plot(ads,price)
plot(trend,price)
windows()
# 7. Find the correlation between Output (Profit) & inputs (R.D Spend, Administration, Marketing, State) - SCATTER DIAGRAM
pairs(Computer_data1)


# 8. Correlation coefficient - Strength & Direction of correlation
cor(Computer_data1)

Model.Computer_data1 <- lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend)
summary(Model.Computer_data1)

panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  
  if(missing(cex.cor)) cex <- 0.4/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex)
}
pairs(Computer_data1, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")


### Partial Correlation matrix - Pure correlation between the variables
# install.packages("corpcor")
library(corpcor)
cor2pcor(cor(Startups))

library(mvinfluence)

library(car)


influence.measures(Model.Computer_data1)

influenceIndexPlot(Model.Computer_data1, id.n=3) # Index Plots of the influence measures



influencePlot(Model.Computer_data1, id.n=3) # A user friendly representation of the above

Model.computer_dataLog <- lm(price~log(speed)+log(hd)+log(ram)+log(screen)+
                               log(cd)+log(multi)+log(premium)+log(ads)+log(trend)
                             ,data=Computer_data1[-c(1441,1701),])

summary(Model.computer_dataLog) #Adjusted R2 Value = 0.9591  

confint(Model.computer_dataLog,level=0.95)

predict(Model.computer_dataLog,interval="predict")

Model.Computer_data2<-lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend,
                         data=Computer_data1[-c(1441,1701),])
summary(Model.Computer_data2)

Model.Computer_exp<-lm(log(price)~speed+hd+ram+screen+cd+multi+premium+ads+trend,
                       data=Computer_data1[-c(1441,1701),])
summary(Model.Computer_exp) 
#quad model
Model.Computer_Quad <- lm(price~speed+I(speed^2)+hd+I(hd^2)+ram+I(ram^2)+screen+I(screen^2)+
                            +cd+I(cd^2)+multi+I(multi^2)+premium+I(premium^2)
                          +ads+I(ads^2)+trend+I(trend^2),data=Computer_data1[-c(1441,1701),])
summary(Model.Computer_Quad) 
confint(Model.Computer_Quad,level=0.95)
predict(Model.Computer_Quad,interval="predict")

# Poly Modal
Model.Computer_Poly <- lm(price~speed+I(speed^2)+I(speed^3)+
                            hd+I(hd^2)+I(hd^3)+
                            ram+I(ram^2)+I(ram^3)+
                            screen+I(screen^2)+I(screen^3)+
                            cd+I(cd^2)+I(cd^3)+
                            multi+I(multi^2)+I(multi^3)+
                            premium+I(premium^2)+I(premium^3)+
                            ads+I(ads^2)+I(ads^3)+
                            trend+I(trend^2)+I(trend^3),data=Computer_data1[-c(1441,1701),])
summary(Model.Computer_Poly) #Adjusted R Square Value is 0.813

# Final Model
FinalModel<-lm(price~speed+I(speed^2)+I(speed^3)+
                 hd+I(hd^2)+I(hd^3)+
                 ram+I(ram^2)+I(ram^3)+
                 screen+I(screen^2)+I(screen^3)+
                 cd+I(cd^2)+I(cd^3)+
                 multi+I(multi^2)+I(multi^3)+
                 premium+I(premium^2)+I(premium^3)+
                 ads+I(ads^2)+I(ads^3)+
                 trend+I(trend^2)+I(trend^3),data=Computer_data1[-c(1441,1701),])

summary(FinalModel) #Adjusted R2 Value = 0.813 



Profit_Predict <- predict(FinalModel)
View(Profit_Predict)

finplot <- Computer_data1[-c(1441,1701),]
View(finplot)

plot1 <- cbind(finplot$price, Profit_Predict)
pairs(plot1)

Final <- cbind(speed,hd,ram,screen,cd,multi,premium,ads,trend,price,Profit_Predict)
pairs(Final)
View(Final)

# Evaluate model LINE assumptions
plot(FinalModel)

library("MASS")
stepAIC(Model.Computer_dataLog) 





















