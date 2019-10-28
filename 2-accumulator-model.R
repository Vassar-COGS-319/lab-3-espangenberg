# implement the model by filling in the function below
# the model should return a data frame with two columns: correct and rt
# the correct column should be TRUE or FALSE, and rt should contain the
# number of steps it took to reach the criterion.

# note that the function takes four arguments:
# samples is the number of samples to draw from the model
# rate.1 is the evidence accumulation rate for the correct response (default value is 40)
# rate.2 is the evidence accumulation rate for the incorrect response (default value is 40)
# criterion is the threshold for a response (default value is 3)

# one oddity: note that higher values for rate.1 and rate.2 will actually produce slower RTs.
# this is because the rate parameter is controlling the rate of decay of the exponential distribution,
# so faster rates mean that less evidence is likely to accumulate on each step. we could make
# these parameters more intuitive by taking 1/rate.1 and 1/rate.2 as the values to rexp().

#instead of a single value moving to two boundaries, we have two values racing to a single
#boundary. rising at different rates. if both hit, the one with >

accumulator.model <- function(samples, rate.1=40, rate.2=40, criterion=3){
  #moving some the stuff below into the for loop
   rt <- rep(0, samples)
   accuracy <- logical()
  # evidence.1 <- 0
  # evidence.2 <- 0
  for (i in 1: samples){
    evidence.1 <- 0
    evidence.2 <- 0
    while ((evidence.1<criterion) && (evidence.2<criterion)) {
      #need to individualize trial to each evidence
      #trial <- rexp(1, rate = rate.1)
      evidence.1 = rexp(1, rate = rate.1) + evidence.1
      evidence.2 = rexp(1, rate = rate.2) + evidence.2
      rt[i] = rt[i] +1
    }
    #need to rewrite this or
    #turns out it needs to be if one is greater than other not or
    if (evidence.1 > evidence.2){
    #((evidence.1>criterion)||(evidence.2>criterion)){
      accuracy[i] <- TRUE
    }
    else {
      accuracy[i] <- FALSE
    }
  }
  
  output <- data.frame(
    correct = accuracy,
    rt = rt
  )
  
  return(output)
}


# rate <-rexp(1)
# if (rate==abs(rate)){
#   true.eviaccu <- true.eviaccu + rate}
#   else{
#     false.eviaccu <- false.eviaccu + rate

# test the model ####

# if the model is working correctly, then the line below should generate a data frame with 
# 1000 samples and about half of the samples should be correct. the average rt will probably
# be around 112, but might vary from that by a bit.

initial.test <- accumulator.model(1000)
sum(initial.test$correct) / length(initial.test$correct) # should be close to 0.5
mean(initial.test$rt) # should be about 112

# visualize the RT distributions ####

# we can use dplyr to filter the data and visualize the correct and incorrect RT distributions

library(dplyr)

correct.data <- initial.test %>% filter(correct==TRUE)
incorrect.data <- initial.test %>% filter(correct==FALSE)

hist(correct.data$rt)
hist(incorrect.data$rt)
