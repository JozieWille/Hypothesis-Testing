# Hypthosis Testing - Taxi Example


#Tips given to taxi drivers form a substantial part of their income.
# They also serve as a loose metric of service quality that can be useful for taxi companies.
# An analysis of a large dataset shows that the tips given (as a percentage of taxi fares) 
# are normally distributed with mean 18.7% and standard deviation 5.8%. 
# Furthermore, tips received for different trips are independent from each other. 
-----------------------------------------------------------------------------------
  # Q1.) A taxi driver expects to do 2000 trips next year. For approximately how many
  # of these trips will he receive a tip over 20%? 
  
  pnorm(0.20, 0.187, 0.058, lower.tail = FALSE) # = 0.411325

2000 * 0.411325

# 820 trips
-----------------------------------------------------------------------------------
  # Q2.) A taxi driver just received a 15% tip. S/he plans to do five more trips for 
  # the day. What is the chance that all five trips will receive tips over 15%?
  
  sum(dbinom(5:5, 5, 0.738))

# or 

1 - pnorm(0.15, 0.187, 0.058)
dbinom(5, 5, 0.738))
# ANSWER 0.21927
-----------------------------------------------------------------------------------
  # Q3.) A taxi driver just received a 15% tip. S/he plans to do five more trips for the day. What is the chance 
  # that at least three out of the five trips will receive tips over 15%?
  1 - pnorm(0.15, 0.187, 0.058)
sum(dbinom(3:5, 5, 0.7382407))

# ANSWER 0.8836952
-----------------------------------------------------------------------------------
  # Q4.) The taxi driver is scheduled to do three trips with fares $20, $30, and $40, respectively. 
  # What is the expected tip amount the taxi driver will receive for all three trips?
  
  mu = (20 + 30 + 40) * 0.187 # mean

# ANSWER = 16.83

-----------------------------------------------------------------------------------
  # Q5.) The taxi driver is scheduled to do three trips with fares $20, $30, and $40, respectively. 
  # What is the standard deviation of the tip amount the taxi driver will receive for all three trips?
  
  sigma = sqrt(20^2 + 30^2 + 40^2) *0.058 # standard deviation

# answer = 3.123396
-----------------------------------------------------------------------------------
  # Q6.) The taxi driver is scheduled to do three trips with fares $20, $30, and $40, respectively. What is the probability 
  # the taxi driver will receive more than $15 in tips for all three trips combined?
  
  # Z - N (mu, sigma)
  # P(Z>15)
  pnorm(15, mu, sigma, lower.tail = FALSE)

# ANSWER 0.72
-----------------------------------------------------------------------------------
  # Q7.) The ride-sharing service Uber experienced explosive growth in the last few years.
  # It belongs to the so-called “gig” economy where workers have considerable flexibility over their work schedule. 
  # A recent study looked at the pay of Uber drivers in three major cities in the US. The file uber.csvDownload uber.csv contains the hourly pay for 
  # 1000 randomly sampled drivers in the three major cities. The file has five columns, which are briefly explained below:
  # DriverID: A unique ID for each driver.
  # City: The city in which the driver works.
  # Gender: The gender of the driver.
  # Trips: The number of trips completed by the driver.
  # Hourly Pay: The hourly pay earned by the driver, net of cost (fuel, etc.).
  
  # Produce a histogram for each of the relevant sample data.
# Which of the following sample data has a histogram that resembles the normal distribution the least?

uber = read.csv ("uber.csv")
head(uber)

# subset of only hourly pay in new york
hist(uber$Hourly.Pay[uber$City == "New York"])

# subset of only hourly pay in chicago
hist(uber$Hourly.Pay[uber$City == "Chicago"])

# subset of only hourly pay for male drivers
hist(uber$Hourly.Pay[uber$Gender == "Male"])

# subset of only hourly pay for female drivers
hist(uber$Hourly.Pay[uber$Gender == "Female"])

# subset of number of trips of male drivers
hist(uber$Trips[uber$Gender == "Male"])   # this one is LEAST normally distrubuted
-----------------------------------------------------------------------------------
  # Q8.) Compute the average and standard deviation of the hourly pay for each city. Which city has the lowest average hourly pay? 
  # Which city has the highest standard deviation for hourly pay?
  
  tapply(uber$Hourly.Pay, uber$City, FUN = mean)
# or longer way:

NewYork <- subset(uber, City == 'New York', select = c("City", "Hourly.Pay"))
NYsd = sd(NewYork$Hourly.Pay) #1.959777
Nymean = mean(NewYork$Hourly.Pay) # 7.882752

Boston <- subset(uber, City == 'Boston', select = c("City", "Hourly.Pay"))
Bostonsd = sd(Boston$Hourly.Pay) # 2.0297
Bostonmean = mean(Boston$Hourly.Pay) # 7.7649

Chicago <- subset(uber, City == 'Chicago', select = c("City", "Hourly.Pay", "Gender"))
Chisd = sd(Chicago$Hourly.Pay) # 1.9985
Chimean = mean(Chicago$Hourly.Pay) # 8.0711

# ANSWER: LOWEST AVERAGE PAY = BOSTON HIGHEST SD = BOSTON
-----------------------------------------------------------------------------------
  # Q9.) Suppose you want to build a 95% confidence interval on the hourly pay for a 
  # male driver in Chicago. What is the margin of error for the confidence interval?
  
  data1 = uber$Hourly.Pay[uber$City == "Chicago" & uber$Gender == "Male"]

x_bar = mean(data1)
s = sd(data1)
n = length(data1)
qt(0.975, df = n-1)* s / sqrt (n)

# ANSWER : 0.2792341
-----------------------------------------------------------------------------------
  # Q10.) In 2018, the US minimum hourly pay is $7.25. 
  # Is there evidence that an average Uber driver earns more than the minimum hourly pay at
  # the 5% significance level?  
  # Pick the choice that contains the correct conclusion and interpretation.
  
  xbar = mean(uber$Hourly.Pay)
s = sd(uber$Hourly.Pay)
n = nrow(uber)
t = (xbar - 7.25) / (s/sqrt(n))

pvalue = pt(t, df = n -1, lower.tail = FALSE)
pvalue

# P-Value = 1.312714e-23 NEAR ZERO
# STRONG evidence that alternative hypothesis is supported
# reject null hypothesis and conclude that average uber driver earns more than the minimum pay
-----------------------------------------------------------------------------------
  # Q11.) Which of the following statements is true?
  # There are more female drivers than male drivers.
  # Female drivers earn a higher hourly pay than male drivers.
  # The average number of trips for female drivers is higher than the average number of trips for male drivers.
  # The proportion of female drivers is the highest in Chicago.
  
drivergendermean = table(uber$Gender)
driverpaymean = tapply(uber$Hourly.Pay, uber$Gender, FUN = mean)
drivertripmean = tapply(uber$Trips, uber$Gender, FUN = mean)
driverfemalehigh = table(uber$Gender, uber$City)


# answer = The proportion of female drivers is the highest in Chicago.

