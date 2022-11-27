# Hypothesis Testing Loyalty Program Example


# Loyalty Program
# A variety of stores offer loyalty programs. Participating shoppers swipe a bar-coded tag at the register 
# when checking out and receive discounts on certain purchases. Stores benefit by gleaning information about shopping habits
# and hope to encourage shoppers to spend more. A typical Saturday morning shopper who does not participate in this program 
# spends $120 on her order. In a sample of 80 shoppers participating in the loyalty program, each shopper spent $130 on average
# during a recent Saturday, with standard deviation $40. Is there statistical proof that the shoppers participating in the loyalty 
# program spend more on average than typical shoppers?
------------------------------------------------------------------------------------------------------------------
  # Q1.) State the null and alternative hypotheses.
  
  # H0 = M <= 120
  # Ha = M > 120
  
  ztest = ( 130 - 120 ) / ( 40 / sqrt(80))
  pvalue = pnorm (ztest, lower.tail = FALSE )
  pvalue
  
  # Q2.) Do the data supply enough evidence to reject the null hypothesis if α = 0.05? 
  
  n = 80
  
  t = ( 130 - 120 ) / ( 40 / sqrt(80))
  
  pvalue = pt (t, df = n-1, lower.tail = F)
  pt
  # OR---------
  pvalue = pt (2.222049, df = 79, lower.tail = F)
  pt
  
  # pvalue = 0.01457033, yes we have enough evidence to reject the null hypothesis
  ------------------------------------------------------------------------------------------------------------------
    # Q3.) Describe the type I and type II errors?
    
    # Type 1 error: probability = 0.05
    
    # Type 2 error: null hypothesis is false, but no evidence of alternative hypothesis 
    ------------------------------------------------------------------------------------------------------------------
    # QUESTION
    # 10 alternative hypothesis are all false. with a 5% cutoff, the type 1 error (incorrectly reject null hypothesis) is 5%.
    # assume further that the tests are independent. what is the probability that at least 1 null hypothesis is rejected?
    
    # probability of rejecting null hypothesis = 0.05 
    pbinom(0, 10, 0.05, lower.tail = FALSE, log.p = FALSE)
  
  # q = 0 since probability is 1, 1 - 1 = 0
  
  # OR -----------
  1 - dbinom(0, 10, 0.05)
  
  # answer = 0.4012
  -------------------------------------------------------------------------------------------------------------------
  