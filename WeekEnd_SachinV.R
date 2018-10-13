# Look for the source File location and set the working dir

getwd()
setwd("G:/Gynan/Assignment")
list.files(pattern = "MBA")

# Create the dataframe by reading the required file

Mba.StartSal <- read.csv("MBA Starting Salaries Data.csv" , header = TRUE)
View(Mba.StartSal)

#Get Cetral Tendency values to understad the distribution of data
##function to get mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmode(Mba.StartSal$age)
mean(Mba.StartSal$age)
median(Mba.StartSal$age)

#Histogram of the Age 
hist(Mba.StartSal$age ,
     col=heat.colors(10) ,main="Hist of Ages" ,xlab = "No of students" ,
     ylab = "Age "  ,axes = TRUE ,plot = TRUE  )

hist(Mba.StartSal$age ,density = 50 ,angle = 45 ,
     col="green" ,border = "red" ,main="Histogram of Ages" ,xlab = "Age" ,
     ylab = "No of Students " ,axes = TRUE ,plot = TRUE ,las=1)

summary(Mba.StartSal$age)
# Mode < Median < Mean for AGE distribution is not a normal distribution

################Gmat Total: Mean, Median and Mode#############################
Gmat.Score <- Mba.StartSal$gmat_tot
getmode(Gmat.Score)
mean(Gmat.Score)
median(Gmat.Score)

  cat("Mean ="  , mean(Gmat.Score) ,
      "\nMedian =" ,median(Gmat.Score) ,
      "\nMode =",getmode(Gmat.Score),
  if(mean(Gmat.Score) > median(Gmat.Score) && median(Gmat.Score) > getmode(Gmat.Score) ) {
    print("\nThis is a left skewed distribution")
    } else if(mean(Gmat.Score) < median(Gmat.Score) && median(Gmat.Score) < getmode(Gmat.Score) )
    {
      print("\nThis is a right skewed distribution")
    }
  else if (mean(Gmat.Score) == median(Gmat.Score) && median(Gmat.Score) == getmode(Gmat.Score) ) {
    print("\nThis is a normal distribution")
  }
  )
  
  ################Draw the hist & FD of the salary##############
  ## removing unwated data or un answered data
  Mba.Sal <-Mba.StartSal$salary[!(Mba.StartSal$salary %in% c(0 ,999,998))]
  # check if there are unwanted data
  unique(Mba.Sal)
  print (Mba.StartSal$salary[(  Mba.Sal %in% c(0 ,999,998))])
 # Histogram of sals
  hist(Mba.Sal ,col='yellow',border = 'red',angle=60,density=30,main = 'Beak bown of MBA Salary',
       xlab = 'Salary',ylab = 'No Of Students')
#Frequency distribution we need to know Range ,Class,Class width
  range.Sal<-max(Mba.Sal)-min(Mba.Sal)
  range.Sal  ## we have the range
  length(Mba.Sal) ## no of observations
  number.Classes <-round(1+(3.3*(log(length(Mba.Sal)))),0)
  Class.width <-(range.Sal/number.Classes) ##width of the class
  Class.width

  lowerClass.Boundary <- round((min(Mba.Sal)/Class.width),0)-1 ##boundary
  HigestClass.Boundary <-lowerClass.Boundary+number.Classes +1
  lowerClass.Boundary
  HigestClass.Boundary
## Histogram of FD   
  hist(Mba.Sal ,breaks = (Class.width)*(lowerClass.Boundary:HigestClass.Boundary) ,density = 25,angle = 135,col = 'orange',
       border = 'brown',xlab = 'Salary' ,ylab = 'Students',
       xlim=c(min(Mba.Sal),max(Mba.Sal)),
       main = 'Frequency Dist of Salary')
###It is a single mode class and right tail extremes
  
####Are highest salaries drawn by the highest 'satis' (7) students
  
  Strat.Mba <- data.frame(Mba.StartSal$salary,Mba.StartSal$satis)
  View(Strat.Mba)
  
  New.Set.Mba<-subset(Strat.Mba,(!Strat.Mba$Mba.StartSal.salary %in%c(998,999,0)))
View(New.Set.Mba)

plot((New.Set.Mba$Mba.StartSal.salary),
     (New.Set.Mba$Mba.StartSal.satis),type="o",col="red",
     main = "Salary vs Satisfaction ",xlab="Salary",ylab = "Strat"
     ,lty=3,pch='*')
##lets breakdown to satisfaction 7 vs non satisfaction 7
Start.Mba.seven <-subset(New.Set.Mba,(New.Set.Mba$Mba.StartSal.satis=='7'))
Strat.Mba.Notseven <-subset(New.Set.Mba,(!New.Set.Mba$Mba.StartSal.satis=='7'))
View(Strat.Mba.Notseven)
View(Start.Mba.seven)

plot(Start.Mba.seven$Mba.StartSal.salary,col='blue',xlim = c(1,90),
     ylim = c(70000,230000),xlab='Studets',ylab = 'Salary',lty=1,
    main = 'Level of Satisfaction 7 vs non Satisfaction 7'
     )
points(Strat.Mba.Notseven$Mba.StartSal.salary ,col='red',pch=20)


##from the plot we can say that studets with satisfaction less than 7 are paid more 
