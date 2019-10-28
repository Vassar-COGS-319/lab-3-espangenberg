# model selection ####

#look at properties of model predictions and formulate how you could tell which did which

# suppose we have data from an experiment like this:
# mean RT correct = 250ms
# mean RT incorrect = 246ms
# accuracy = 0.80

# try to fit this data with both models by adjusting the parameters of the model
# HINT: you can speed up your parameter search by using a small number of samples
# initially, and then increasing the samples as you get closer to a viable set
# of parameters.
# 2nd HINT: Don't adjust the sdrw parameter of the random.walk.model or the criterion
# paramter of the accumulator model.

# You don't need to get a perfect match. Just get in the ballpark. 

#we are using the same pseudo-randomly generated number sequence
set.seed(12604)

#very slightly altered the found parameters from joshcode
rw.model.result <- random.walk.model(1000, drift=0.0125, sdrw=0.3, criterion = 4.8)
rw.model.result %>% group_by(correct) %>% summarize(mean.rt = mean(rt))
mean(rw.model.result$correct)


acc.model.result <- accumulator.model(1000, rate.1 = 84.5, rate.2 = 91, criterion=3)
acc.model.result %>% group_by(correct) %>% summarize(mean.rt = mean(rt))
mean(acc.model.result$correct)


# Can both models do a reasonable job of accounting for the mean RT and accuracy? Report the
# results of your efforts:

#yes! The average results of the RW is false - 226, true - 243, accuracy = 0.795
#Average results of the acc is false - 258, true - 250, accuracy = 0.801
#these are reasonable and very close predictions compared to what was asked.

# Using the parameters that you found above, plot histograms of the distribution of RTs
# predicted by each model. Based on these distributions, what kind of information could
# we use to evaluate which model is a better descriptor of the data for the experiment?
# Describe briefly how you might make this evaluation.

#following graph code taken from Josh
layout(matrix(1:4, nrow=2, byrow=T))
hist((rw.model.result %>% filter(correct==TRUE))$rt, breaks=seq(0,2000,100), main="RW Model, correct", xlab="RT")
hist((rw.model.result %>% filter(correct==FALSE))$rt, breaks=seq(0,2000,100), main="RW Model, incorrect", xlab="RT")
hist((acc.model.result %>% filter(correct==TRUE))$rt, breaks=seq(0,2000,10), main="ACC Model, correct", xlab="RT")
hist((acc.model.result %>% filter(correct==FALSE))$rt, breaks=seq(0,2000,10), main="ACC Model, incorrect", xlab="RT")

#We can immediately see that the ACC model clusters the data tightly while the RW model
# has a far more spread out distribution for both correct and incorrect RTs
# This indicates to us that variability of the data in the RW model is much higher.
# I would look at the data for the experiment to see if it is highly variable or not.
# as pointed out, perhaps the standard deviations of each model's data could quantitatively
# show us this.