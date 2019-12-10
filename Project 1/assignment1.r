#Group: Mathew Witek, Michael Gleyzer, and Harishkartik Kumaran Pillai

NOAA<-read.csv("~/Desktop/NOAA+GISS.csv")
NOAA.mat<-as.matrix(NOAA)

#Implementation of bootstrap as applied to linear regression.
my.smooth.for.boot<-function(X, Y){ 

#Fits a linear function for vector of input values x and output values y
  smsp.strcv<-smooth.spline(X,Y)

#calculates residuals by subtracting the y coordinates of the linear interpolation from the y coordinates
  smspcv.resid<-(Y-approx(smsp.strcv$x,smsp.strcv$y,X)$y)  

#calculates the standard deviation of the residuals
    sd.resid<-sqrt(sum(smspcv.resid^2)/(length(Y)-smsp.strcv$df))

#this line standarizes the residuals
  stud.resid<-smspcv.resid/sd.resid

#store the y coordiantes of the linear interpolation in the variable my.smooth
  my.smooth<-approx(smsp.strcv$x,smsp.strcv$y,X)$y

#creates a list which stores vectors of the residuals, s.d. and y interpolation values
  list(raw.resid=smspcv.resid,sd.resid=sd.resid,smooth=my.smooth)
  
}

my.boot.smooth<-function(X,Y,nboot=1000,confidence=0.95){

  #Creates a matrix of 1 row by 1 column
  par(mfrow=c(1,1))  

  #Stores the return value of the function my.smooth.for.boot in the variable str0.
  str0<-my.smooth.for.boot(X,Y)

  #Initializes smooth.dist as a null object
  smooth.dist<-NULL

  #stores y coordinates of linear interpolation in the varibale base.smooth
  base.smooth<-str0$smooth  

  #store the standard deviation of the residuals in the variable base.sd
  base.sd<-str0$sd.resid  

  #store the residuals in the variable base.resid
  base.resid<-str0$raw.resid

  #Stores length of base.smooth vector in n1
  n1<-length(base.smooth)

  #loops 1000 times
  for(i in 1:nboot){

    #This line stores the results of the sample with replacement from the vector holding the residuals in the bres variable.
    bres<-sample(base.resid,length(base.resid),replace=T)

    #add to the vector containing the y values from the linear interpolation
    Yboot.dat<-((base.smooth+bres))  

    #print out vector
    #print(boot.dat) 

    #store the result of my.smooth.for.boot in the variable bstr0
    bstr0<-my.smooth.for.boot(X,Yboot.dat)

    #store the y values of the Linear Interpolation in boot.smooth
    boot.smooth<-bstr0$smooth  

    #create data frame holding the differences between the bootstrapped itnerpolated y values and the interpolated y values from the intial data
    #Data frame to have 1000 columns
    smooth.dist<-rbind(smooth.dist,boot.smooth-base.smooth)
    
    }  

  #this line assigns the length of the data frame smooht.dist's first row
  n1<-length(smooth.dist[1,])

  #calculates alpha = 0.05
  alpha<-1-confidence

  #Initializes LB as a null object
  LB<-NULL  

  #Initialize UB as a null object
  UB<-NULL

  #this for loop iterates through all columns of smooth.dist object/data
  for(i in 1:n1){  

    #sorts all the distances in a given column of smooth.dist in increasing order
    s1<-sort(smooth.dist[,i])

    #finds the length of s1 and stores it in the n2 variable
    n2<-length(s1)

    #assign v1 the vector (1/n2, 2/n2 ... 1)
    v1<-c(1:n2)/n2

    #Assigns bvec the results of linearly interpolating v1 and s1 in the bounds of alpha = (0.025, 0.975) and Creates a list where each entry contains two y coordinates(for LB and UB).
    bvec<-approx(v1,s1,c(alpha/2,1-alpha/2))$y    
    LB<-c(LB,base.smooth[i]-bvec[2])    
    UB<-c(UB,base.smooth[i]-bvec[1])
    
    }

  #plots lower bond, smooth fit, and upper bound
  plot(rep(X,4),c(LB,base.smooth,UB,Y),xlab="X",ylab="Y",type="n")

  #plots the points of X,Y
  points(X,Y)  
  o1<-order(X)  
  lines(X[o1],LB[o1],col=2)  
  lines(X[o1],UB[o1],col=2)
  smooth <- smooth.spline(X,Y,df = 2)

  #lines(smooth.spline(X,Y),col = 1)
  lines(X[o1],base.smooth[o1],col=1)
 
  lines(smooth,col=3)
  
  }  

my.smooth.for.boot(NOAA[[3]] , NOAA[[2]])
my.boot.smooth(NOAA[[3]] , NOAA[[2]],nboot =1000 , confidence= 0.95)
