## 6.7 Creating a New Column Based on Some Condition ####
dat1.df %>%
  mutate(
    EdadDic = case_when(Edad <= 17 ~ "17 años o menos",
                          Edad <= 18 ~ "18 años o más",
                        Edad > 18 ~ "18 años o más",
                          TRUE ~ "all other results")
  )

# 7 Strings and Dates ####

# 8 Probability ####

## Normal distribution function ####
dnorm # Normal density
pnorm # Normal distribution function
qnorm # Normal quantile function
rnorm # Normal random variates

## Common discrete distributions ####
binom # Binomial n = number of trials; p = probability of success fon one trial
geom # Geometric p = probability of success for one trial
hyper # Hypergeometrica m number w balls in urn; n = number of b balls in urn; k = number of balls drawn from urn
nbinom # Negative binomial size = number of succesul trials; either prob = probability of succcessful trial or mu
pois # Poisson lambda = mean

## Common continuos distributions ####
beta # shape1; shape2
cauchy # location; scale
chisq # df = degrees of freedom
exp # rate
f # df1 and df2 = degrees of freedom
gamma # rate or scale
lnorm # meanlog = mean on logarithmic scale; sdlog = standard deviation on
logis # location; scale
norm # mean; sd = standard deviation
t # (TDist) t df = degrees of freedom
unif # min = lower limit; max = upper limit
weibull # shape; scale
wilcox # m = number of observations in first sample; n = number of observations in second

?Normal
?TDist
