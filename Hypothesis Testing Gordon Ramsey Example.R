# Gordon Ramsey Hypothesis Testing
# Consider the Gordon Ramsay’s restaurant example. The data is collected
# in the Excel file, “Gordon Ramsay.xls”. Do diners spend on average more than 
# $40 on appetizers?t test because we dont have population SD

library(readxl) 
data = read_excel("Gordon_Ramsay.xlsx", range = "B5:C205")

# Null Hypothesis: H0 = x_bar <= $40 # equals sign always in Null never in alternative
# Alternative Hypothesis: Ha = x_bar > $40 

x_bar = mean(data$'App. Spend')
s = sd(data$'App. Spend')
n = nrow(data)

# $40 average on appetizers
t = (x_bar - 40) / (s / sqrt(n))

# t = 2.9009

# P value with upper tail probability
pvalue = pt(t, df = n - 1, lower.tail = F)

# pvalue = 0.002069481, reject null hypothesis, test is significant, because p is less than .005 
# therefore there is evidence for alternative hypothesis