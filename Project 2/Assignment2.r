#Mathew Witek
#Partners: Michael Gleyzer, and Harishkartik Kurmaran Pillai

NOAA<-read.csv("~/Desktop/NOAA+GISS.csv")
NOAA.mat<-as.matrix(NOAA)

my.smooth.forKS<-function(X,Y){

  #Given x inputs and y outputs, this line fits a cubic function, and stores the polynomial fit in the variable smsp.strcv.
  smsp.strcv<-smooth.spline(X,Y)

  #Calculates the residuals
  smspcv.resid<-(Y)-approx(smsp.strcv$x,smsp.strcv$y,X)$y

  #Calculates the standard deviation of the residuals.
  sd.resid<-sqrt(sum(smspcv.resid^2)/(length(X)-smsp.strcv$df))  
  stud.resid<-smspcv.resid/sd.resid

  #Kolmogorov-Smirnov Test between residual/empirical distribution and the normal distrbution.
  #Then stores the outcome in ks.str
  ks.str<-ks.test(stud.resid,pnorm)

  #Stores the test stat in variable D
  D<-ks.str$statistic

  #P-value of the test is stored in Pval.
  Pval<-ks.str$p.value

  #Linearly interpolates x coordinates of the smooth spline and the y coordinates. 
  #Then stores the y coordinates of the outcome in variable my.smooth.
  my.smooth<-approx(smsp.strcv$x,smsp.strcv$y,X)$y

  #Creates a list which holds the KS test statistic, vector of residuals, and the linear interpolation of the smooth spline.
  list(D=D,raw.resid=smspcv.resid,sd.resid=sd.resid,smooth=my.smooth)
  
}

my.KS.pboot.normal<-function(X,Y,nboot=10000,mydf=NULL){

  #Function my.smooth.forks(X,Y) is called and stores the result in str0.
  str0<-my.smooth.forKS(X,Y)

  #A null Ddist object is created.
  Ddist<-NULL

  #base.smooth is set to store the smooth variable of the list created by the my.smooth.forks(X,Y) function.
  base.smooth<-str0$smooth

  #base.sd is set to store the residual vector of the list which was created by the my.smooth.fork(X,Y) function.
  base.sd<-str0$sd.resid

  #Stores the KS test statistic in D0
  D0<-str0$D
  n1<-length(X)

  #loop iterates 10000 times
  for(i in 1:nboot){

    #Produces n1 samples from the normal distribution
    #paramters: mean = 0, standard deviation = base.sd
    bres<-rnorm(n1,0,base.sd)

    #adds bres to base.smooth vector
    Yboot<-((base.smooth+bres))

    #Calls my.smooth.forKS function with input vector X, new Y vector, and Yboot.
    bstr0<-my.smooth.forKS(X,Yboot)

    #Creates data frame that stores the KS test statistics.
    Ddist<-c(Ddist,bstr0$D)

  }

  #Gets the p-value by adding up all test stats which are greater than D0, and then divides that sum by 10,000.
  Pval<-sum(Ddist>D0)/nboot

  #my.KS.pboot.normal returns the p-value.
  Pval
 
}

my.bootstrap.ci<-function(vec0,nboot=10000,alpha=0.05){

    #extracts sample size of the original data.
    n0<-length(vec0)

    #extracts mean of the original data.
    mean0<-mean(vec0)

    #extracts standard deviation of the original data.
    sd0<-sqrt(var(vec0))

    #sets bootvec to null.
    bootvec<-NULL

    #for loop which is cycled through 10000 times.
    for(i in 1:nboot){

        #sets vecb to a sample taken from vec0 with replacement, hence replace = T.
        vecb<-sample(vec0,replace=T)

        #sets meanb to the mean of vecb.
        meanb<-mean(vecb)

        #sets sdb to the sqrt of the variance of vecb.
        sdb<-sqrt(var(vecb))

        while(sdb == 0){ 

        #We need to keep resampling until the stdev no longer equals 0.
        vecb<-sample(vec0,replace=T)    
        meanb<-mean(vecb)    
        sdb<-sqrt(var(vecb))        
     
        }   

        #c is a function that combines its arguments and forms a vector.
        #So this line sets bootvec to a vector containing bootvec, and (meanb-mean0)/(sqrt(var(vecb))/sqrt(n0)).
        bootvec<-c(bootvec,(meanb-mean0)/(sdb/sqrt(n0)))

    }

    #quantile function produces sample quantiles according to the provided probabilities.
    #So lq is set to a sample quantile of bootvec, with alpha/2.
    #I assume that lq stands for lower quantile.
    lq<-quantile(bootvec,alpha/2)

    #quantile function produces sample quantiles according to the provided probabilities.
    #So uq is set to a sample quantile of bootvec, with 1-alpha/2.
    #I assume that uq stands for upper quantile.
    uq<-quantile(bootvec,1-alpha/2)

    #These below two lines calculate the lower and upper bound of the confidence interval.
    #Lower Bound
    LB<-mean0-(sd0/sqrt(n0))*uq

    #Upper Bound
    UB<-mean0-(sd0/sqrt(n0))*lq

    #Calculates the Normal Lower Bound of the CI.
    NLB<-mean0-(sd0/sqrt(n0))*qt(1-alpha/2,n0-1)

    #Calculates the Normal Upper Bound of the CI.
    NUB<-mean0+(sd0/sqrt(n0))*qt(1-alpha/2,n0-1)

    #Lists the Confidence Interval, and the Normal Confidence Interval.
    list(boostrap.confidence.interval=c(LB,UB),normal.confidence.interval=c(NLB,NUB))

}

lognormal.simfunc<-function(mu.val=3,n=30,nsim=1000){

    #create coverage indicator vectors for bootstrap and normal

    #Creates cvec.boot object
    cvec.boot<-NULL

    #Creates cvec.norm object
    cvec.norm<-NULL
    
    #Computes the mean of the lognormal distr.
    mulnorm<-(exp(mu.val+1/2))
    
    #Iterates the loop 1000 times
    for(i in 1:nsim){

        if((i/10)==floor(i/10)){

            #Prints i anytime that i divisible by 10 (a way to follow the simulation).
            print(i)
            
        }
        
    #vec.sample contains the lognormal distribution parameters, mean = mu.val, and n = sample size.
    vec.sample<-rlnorm(n,mu.val)
        
    #Create confidence interval and normal confidence interval given vec.sample.
        boot.list<-my.bootstrap.ci(vec.sample)
        boot.conf<-boot.list$bootstrap.confidence.interval
        norm.conf<-boot.list$normal.confidence.interval
     
    #WHAT ARE WE DOING BELOW???
    #Combine vector that combines the values of the boot.conf vector which are less than the mean and greater than the mean.
    cvec.boot<-c(cvec.boot,(boot.conf[1]<mulnorm)*(boot.conf[2]>mulnorm))

    #Combine vector that combines the values of the norm.conf vector which are less than the mean and greater than the mean.
    cvec.norm<-c(cvec.norm,(norm.conf[1]<mulnorm)*(norm.conf[2]>mulnorm))

    }

#Returns boot.coverage and norm.coverage
#boot.coverage = sum of all values in the cvec.boot vector / the number of simulations.
#norm.coverage = sum of all values in the cvec.norm vector / the number of simulations.
#Ultimately is the mean of the simulated data in both cases.  
list(boot.coverage=(sum(cvec.boot)/nsim),norm.coverage=(sum(cvec.norm)/nsim))

}   

#Simulations for aplpha = 0.1
lognormal.simfunc(3,3,1000)
lognormal.simfunc(3,10,1000)
lognormal.simfunc(3,30,1000)
lognormal.simfunc(3,100,1000)

#Simulations for alpha = 0.05
lognormal.simfunc(3,3,1000)
lognormal.simfunc(3,10,1000)
lognormal.simfunc(3,30,1000)
lognormal.simfunc(3,100,1000)