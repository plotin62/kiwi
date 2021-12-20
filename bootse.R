library(joineRML)

d <- read.csv("pg_350.csv")

d <- d[which(d$pg_paid_price != 0 
  # & d$pg_paid_price < 500
  & d$content_type != '' 
  & d$trip_type != 'multicity'
  & d$trip_type != 'nomad' 
  & d$test_string != ''
  ),]

ct.arm <- d[which(d$test_string == 'control'
  & d$content_type == 'nonvi'
  # & d$trip_type == 'roundtrip'
  ),]$pg_paid_price
tr.arm <- d[which(d$test_string == 'treatment'
    & d$content_type == 'nonvi'
  # & pg_paid$trip_type == 'roundtrip'
  ),]$pg_paid_price



# outliers
outbox(na.omit(d$pg_paid_price),mbox=F,gval=NA)


# the boxplot rule that is based in part on a robust measure of skewness, 
# called the medcouple, which was introduced by Brys, Hubert, and Struyf (2004)
adjboxout(d$pg_paid_price)


# confidence interval for the 20% trimmed mean
trimci(na.omit(d$pg_paid_price), tr=.2, alpha=0.05)
# confidence interval for the population Winsorized mean
winci(na.omit(d$pg_paid_price),tr=0.2,alpha=0.05)

onesampb(na.omit(d$pg_paid_price),est=onestep,alpha=0.1,nboot=1000,tr=0.1)

# 0.95 confidence interval using the percentile bootstrap method.
# the bootstrap trimmed means
trimpb(na.omit(d$pg_paid_price)
  ,tr=0.2,alpha=0.05,nboot=2000,WIN=F,plotit=T,win=0.1,pop=1)


akp.effect(ct.arm ,tr.arm,tr=0.2)

ks(ct.arm ,tr.arm,w=F,sig=T,alpha=0.05)
shifthd(ct.arm ,tr.arm,nboot=200)