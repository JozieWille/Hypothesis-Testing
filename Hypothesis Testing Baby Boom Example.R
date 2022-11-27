# Hypothesis Testing Baby Boom Example


# BABY BOOM
# Some sources claim that in a 30 day period 9 months after the event, Toronto 
# observed an average of 343 births per day. Historically, the mean was only 340 births/day 
# with a standard deviation of 18 births/day. So the 343 births per day translate into a 
# 1% higher birth rate than normal. Would you conclude that there is a baby boom?
-----------------------------------------------------------------------------------------------------------------------
  # q1.) (Mu = birth rate) 
  # H0 = Mu < 340
  # Ha =  Mu > 340 
  
  # x_bar = 343 
  # n = 30 # n = 30 days
  # sd = 18 # population standard deviation
  
  ztest = ( 343 - 340 ) / ( 18 / sqrt(30))
  pvalue = pnorm (ztest, lower.tail = FALSE )
  pvalue
  # ztest =  0.9128709
  # pvalue = 0.1806552
  
  # Conclusion = No baby boom