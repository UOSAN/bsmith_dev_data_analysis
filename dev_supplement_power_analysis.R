#devtools::install_github("dbaranger/InteractionPoweR")

library(InteractionPoweR)

test_power<-power_interaction(
  n.iter=2000,
  alpha = 0.05,             # alpha, for the power analysis
  N = 1050,                  # sample size
  r.x1x2.y = .1,           # interaction effect to test (correlation between x1*x2 and y)
  r.x1.y = .1,              # correlation between x1 and y
  r.x2.y = .1,              # correlation between x2 and y
  r.x1.x2 = .1,              # correlation between x1 and x2
  k.x1 = 2, #treatment
  k.x2 = 40, #cognitive ability
  k.y = 40,
  adjust.correlations = TRUE,
  
)
test_power

#for our main variable of interest, let's go 90% power at a p<0.01
test_power_aim1<-power_interaction(
  n.iter=2000,
  alpha = 0.01,             # alpha, for the power analysis
  N = 1500,                  # sample size
  r.x1x2.y = .1,           # interaction effect to test (correlation between x1*x2 and y)
  r.x1.y = .1,              # correlation between x1 and y
  r.x2.y = .1,              # correlation between x2 and y
  r.x1.x2 = .1,              # correlation between x1 and x2
  k.x1 = 2, #treatment
  k.x2 = 40, #cognitive ability
  k.y = 40,
  adjust.correlations = TRUE
  
)
print(test_power_aim1)

#so that's probably a good effect size for the first set.
#for the second set...

#assume moderators are independent...
#then you're only going to need the same level of power...
#BUT...you'll need a higher alpha.
#assume bonferonni correction, but let's just go for 80% power
test_power<-power_interaction(
  n.iter=2000,
  alpha = 0.05/27,             # alpha, for the power analysis
  N = 1500,                  # sample size
  r.x1x2.y = .1,           # interaction effect to test (correlation between x1*x2 and y)
  r.x1.y = .1,              # correlation between x1 and y
  r.x2.y = .1,              # correlation between x2 and y
  r.x1.x2 = .1,              # correlation between x1 and x2
  k.x1 = 2, #treatment
  k.x2 = 40, #cognitive ability
  k.y = 40,
  adjust.correlations = TRUE
  
)
print(test_power)


#assume moderators are independent...
#then you're only going to need the same level of power...
#BUT...you'll need a higher alpha.
#assume bonferonni correction, but let's just go for 80% power
test_power_aim2<-power_interaction(
  n.iter=2000,
  alpha = 0.05/27,             # alpha, for the power analysis
  N = 1500,                  # sample size
  r.x1x2.y = .1,           # interaction effect to test (correlation between x1*x2 and y)
  r.x1.y = .1,              # correlation between x1 and y
  r.x2.y = .1,              # correlation between x2 and y
  r.x1.x2 = .1,              # correlation between x1 and x2
  k.x1 = 2, #treatment
  k.x2 = 40, #cognitive ability
  k.y = 40,
  adjust.correlations = TRUE
  
)
print(test_power_aim2)