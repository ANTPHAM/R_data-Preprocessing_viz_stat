data('mtcars')
head(mtcars)
str(mtcars)
help(mtcars)
## data type conversion to factors is as follows:
to.factors<- function(df, variables){
    for (variable in variables) {
      df[[variable]] <- as.factor(df[[variable]])
    }
    return(df)
}
## perform data type transformation

categorical.vars<- c("cyl", "vs", "am", "gear", "carb")
mtcars<- to.factors(mtcars, categorical.vars)
# verify transformation
str(mtcars)

#Exploratory Data Analysis
# pairs plot observing relationships between variables, which shows the relationship between each pair of attributes in the dataset:
pairs(mtcars, panel = panel.smooth,main = "Pairs plot for mtcars data set")

# mpg of cars
# mpg of cars
dotchart(mtcars$mpg, labels=row.names(mtcars),
         cex=0.7, pch=16,
         main="Miles per Gallon (mpg) of Cars",
         xlab = "Miles per Gallon (mpg)")

head(mtcars[order(mtcars$mpg),], 2)
tail(mtcars[order(mtcars$mpg),], 2)

# cylinder counts/cex lab = taill caractere xlab et ylab/ces axis=taill chiffre axe verti/...
barplot(table(mtcars$cyl),
        col="lightblue",
        main="Car Cylinder Counts Distribution",
        xlab="Number of Cylinders", ylab="Total Cars",
        cex.main = 0.8, cex.axis=0.6,
        cex.names=0.6, cex.lab=0.8)
# gear counts
barplot(table(mtcars$gear),
        col="lightblue",
        main="Car Gear Counts Distribution",
        xlab="Number of Gears", ylab="Total Cars",
        cex.main = 0.8, cex.axis=0.6,
        cex.names=0.6, cex.lab=0.8)

#we relabel the factor variable levels from 0 and 1 to Automatic and Manual first, and then we plot the chart
# transmission counts(O devient auto 1 devient manual)
mtcars$am<- factor(mtcars$am,labels=c('Automatic','Manual'))
head(mtcars,1)
barplot(table(mtcars$am),
        col="lightblue",
        main="Car Transmission Type",
        xlab="Number of Gears", ylab="Total Cars",
        cex.main = 0.8, cex.axis=0.6,
        cex.names=0.6, cex.lab=0.8)

# visualizing cars distribution by cylinders and transmission
counts<- table(mtcars$am, mtcars$cyl)# tableau croise dynamique
counts
barplot(counts, main="Car Distribution by Cylinders and Transmission",
        xlab="Number of Cylinders", ylab="Total Cars",
        col=c("steelblue","lightblue"),
        legend=rownames(counts), beside=TRUE,
        args.legend=list(x="top", title="Transmission Type",cex=0.8),
        cex.main = 0.8, cex.axis=0.6,
        cex.names=0.6, cex.lab=0.8)
#we observe car distributions by cylinders as well as gears.
counts<- table(mtcars$gear, mtcars$cyl)
counts
barplot(counts, main="Car Distribution by Cylinders and Gears",
        xlab="Number of Cylinders", ylab="Total Cars",
        col=c("darkblue", "steelblue","lightblue"),
        legend=rownames(counts), beside=TRUE,
        args.legend=list(x="top", title="Gears", cex=0.8),
        cex.main = 0.8, cex.axis=0.6,
        cex.names=0.6, cex.lab=0.8)

#create a grouped dot plot showing the miles per gallon of various cars that are grouped by number of cylinders using the following code snippet:
# visualizing car mpg distribution by cylinder
# add a color column within the data frame for plotting
mtcars<- within(mtcars, {
    color<- ifelse(cyl == 4, "coral", ifelse(cyl == 6,
                                             "cadetblue", "darkolivegreen"))
  })
head(mtcars,1)
dotchart(mtcars$mpg, labels=row.names(mtcars),
         groups=mtcars$cyl,
         color=mtcars$color,
         cex=0.7, pch=16,
         main="Miles per Gallon (mpg) of Cars\nby Cylinders",
         xlab = "Miles per Gallon (mpg)")
# remove the color column within the data frame after plotting
mtcars<- within(mtcars, rm("color"))

# use ggplot2, to plot some boxplots that display the relationship of mpg with some other car attributes
# load visualization dependencies
library(ggplot2)
theme<- theme_set(theme_minimal())
# Car MPGs by number of cylinders visualization
ggplot(mtcars,
       mapping=aes_string(y = "mpg", x = "cyl")) + # attention ne pas mettre le + au debut de la ligne
xlab("Number of Cylinders") +
ylab("Miles per Gallon (mpg)") +
ggtitle("Distribution of Miles per Gallon (mpg)\nby number of cylinders") +
geom_boxplot(outlier.colour = NULL,
aes_string(colour="cyl", fill="cyl"), alpha=0.8) +
stat_summary(geom = "crossbar",
width=0.70,
fatten=0.5,
color="white",
fun.data = function(x) {
     return(c(y=median(x),
     ymin=median(x),
     ymax=median(x)))
               }
  ) +
stat_summary(fun.data = function(x) {
    return(c(y = median(x)*1.03,
    label = round(median(x),2)))
  },
geom = "text",
fun.y = mean,
colour = "white")

# insights into average\median mpg of cars by cylinder
# to observe that the median values are the same as the ones that we observed in the plot, and the mean values are also quite similar to the median:
aggregate(list(mpg=mtcars$mpg),
          list(cylinders=mtcars$cyl),
          FUN=function(mpg) {
            c(avg=mean(mpg),
              median=median(mpg)
            )
          }
)

# Car MPGs by Transmission type visualization
ggplot(mtcars,
       mapping=aes_string(y = "mpg", x = "am")) +
  xlab("Transmission Type") +
  ylab("Miles per Gallon (mpg)") +
  ggtitle("Distribution of Miles per Gallon (mpg)\nby
          transmission type") +
  geom_boxplot(outlier.colour = NULL,
               aes_string(colour="am",
                          fill="am"), alpha=0.8) +
  stat_summary(geom = "crossbar",
               width=0.7,
               fatten=0.5,
               color="white",
               fun.data = function(x) {
                 return(c(y=median(x),
                          ymin=median(x),
                          ymax=median(x)))
               }
  ) +
  stat_summary(fun.data = function(x) {
    return(c(y = median(x)*1.03,
             label = round(median(x),2)))
  },
  geom = "text",
  fun.y = mean,
  colour = "white")
#to check 
aggregate(list(mpg=mtcars$mpg),
          list(transmissions=mtcars$am),
          FUN=function(mpg) {
            c(avg=mean(mpg),
              median=median(mpg)
            )
          }
)

# Statistical inference/after using visualizations and aggregations to see that the average miles per
#gallon were significantly different for cars with automatic and manual transmission.
#use a statistical test to prove this.
#start off with a hypothesis (H0) that the difference in mpg means forautomatic and manual transmission cars is zero
# view data distribution
ggplot(mtcars, aes(x=mpg)) +
  geom_density(colour="steelblue",
               fill="lightblue", alpha=0.8) +
  expand_limits(x = 0, y = 0)

#perform the t-test 
t.test(mpg ~ am, data = mtcars)
# visualzing t-test results
aggr<- aggregate(list(mpg=mtcars$mpg),
                 list(transmission=mtcars$am),
                 FUN=function(mpg){c(avg=mean(mpg))})
ggplot(mtcars, aes(x=mpg)) +
  geom_density(aes(group=am, colour=am, fill=am),
               alpha=0.6) +
  geom_vline(data=aggr, aes(xintercept=mpg, color=transmission),
             linetype="dashed", size=1)
str(mtcars)
tail(mtcars,15)
#Statistical modeling with regression

#building some regression models to try and predict car miles per gallon (mpg) values that are based on the other attributes of the car.
# prepare datasets/ beaucause of a far smaller number of samples in our dataset, we will train our model
#on almost all the samples and try to predict the mpg value for one sample
car.to.predict<- mtcars[15, ]# the 15 th row
car.to.predict
training.data<- mtcars[-15, ]# all without the 15th row.
training.data

#build initial model
initial_model<- lm(mpg ~ ., data = training.data)
summary(initial_model)# 78.32% of variation in our response variable (mpg) is explained by our input variables
#try to build a series of regression models and select the best model
#from them on the basis of an evaluation metric called Akaike Information Criterion(AIC)

# best model selection/ regression multiple 
best_model<- step(initial_model, direction = "both")# the best model choisi : Step:  AIC=60.51/mpg ~ wt + qsec + am
summary(best_model)
#Adjusted R-squared:  0.8179 is better than initial_model

# MPG of car to predict
print(data.frame(car.to.predict=data.matrix(
  list(rownames(car.to.predict),
       car.to.predict[,"mpg"])
)), row.names = FALSE)

# predict with intial_ model
predict(initial_model, car.to.predict)# mpg predited =16.0991 
# predict with the best_ model
predict(best_model, car.to.predict)# # mpg predited =  11.30242 much closer to its true mpg value (10.4), as compared to the predicted mpg value (16.1)

# best model diagnostics
par(mfrow = c(2, 2))
plot(best_model)
# The points in the Residuals vs. Fitted plot seem to be randomly scattered on the plot, and they verify homoscedasticity, which indicates the variance of error is uniform across all x values
#The Normal Q-Q plot consists of the points that mostly fall on the line, indicating that the residuals are almost normally distributed
#The Scale-Location plot consists of points that are scattered in a constant band pattern, indicating constant variance
#There are also some distinct points of interest (outliers or leverage points) in the plots, which we shall discuss next

#some specific data points with the names of cars mentioned in the preceding plots. 
#These points are often known as outliers, and they can be separated into two types of points:
#influence and leverage points.
#Influential points:if removed from the dataset, they change the parameter estimates of the regression model by a significant amount and cause a
#notable change in the computation results, based on changing the position of the regression line.
influential<- dfbetas(best_model)
tail(sort(influential[,4]),4)# top 4

#Leverage points are data points that always have high or extreme values of the
#independent variables such that they might have a greater ability to move the
#regression line, based on its position as compared to the other data points. These
#points can also be influential if they fall outside the general pattern of the other data
#points, thus, greatly affecting the position of the regression line
leverage<- hatvalues(best_model)
tail(sort(leverage),4)


