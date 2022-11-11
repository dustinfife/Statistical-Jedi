require(tidyverse)
set.seed(12122122)
carpool = fifer::make.data(cor=NULL, means=c(19, 24), sds=c(2.75, 2.75), c(5, 6), c("Time","Route"), c("Route A", "Route B"), digits=c(1))

  ### summarize data
summary_stats = carpool %>%
  group_by(Route) %>%
  summarize(driving_time = round(mean(Time), digits=1), stdev = round(sd(Time), digits=1), n = length(Time))
summary_stats
difference.mean = diff(summary_stats$driving_time)
difference.sd = with(summary_stats, sqrt(((n[1]-1)*stdev[1]^2 + (n[2]-1)*stdev[2]^2)/(n[1] + n[2] - 2)))

  ### estimate prior distribution
prior.mean = 15
prior.sd = 1.5
prior.diff = 2


  ### aggregate means
n = nrow(carpool)
dmean = summary_stats$driving_time[1]
dsd = summary_stats$stdev[1]
posterior.mean = ((1/prior.sd^2)/(n/dsd^2 + 1/prior.sd^2))*prior.mean + ((n/dsd^2)/(n/dsd^2 + 1/prior.sd^2))*dmean
posterior.mean
posterior.diff = ((1/prior.sd^2)/(n/difference.sd^2 + 1/difference.sd^2))*prior.diff + ((n/difference.sd^2)/(n/dsd^2 + 1/prior.sd^2))*difference.mean

  ### aggregate standard deviations
posterior.sd = sqrt(prior.sd^2 + (dsd^2)/n)
posterior.sd.diff = sqrt(prior.sd^2 + (difference.sd^2)/n)

  ### create densities
x = seq(from=-7, to=15, length.out=100)
prior.density = dnorm(x, prior.diff, prior.sd)
data.density = dnorm(x, difference.mean, difference.sd)
posterior.density = dnorm(x, posterior.diff, posterior.sd.diff)
d = data.frame(density = c(prior.density, data.density, posterior.density), estimate = rep(c("Prior", "Data", "Posterior"), each=100), minutes=x)
d$estimate = factor(d$estimate, c("Prior", "Data", "Posterior"))
difference.plot = ggplot(d, aes(x=minutes, y=density, group=estimate, color=estimate)) + geom_line() + theme_minimal()


x = seq(from=7, to=28, length.out=100)
prior.density = dnorm(x, prior.mean, prior.sd)
data.density = dnorm(x, dmean, dsd)
posterior.density = dnorm(x, posterior.mean, posterior.sd)
d = data.frame(density = c(prior.density, data.density, posterior.density), estimate = rep(c("Prior", "Data", "Posterior"), each=100), minutes=x)
means.plots = ggplot(d, aes(x=minutes, y=density, group=estimate, color=estimate)) + geom_line() + theme_minimal()

save(summary_stats, carpool, difference.mean, difference.sd, prior.mean, prior.sd, prior.diff,
     dmean, dsd, posterior.diff, posterior.sd, posterior.sd.diff, posterior.mean,
     difference.plot, means.plots,
     file="data/carpool.rda")



posterior.mean -2*posterior.sd


