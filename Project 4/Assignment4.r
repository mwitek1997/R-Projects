library(ISLR)

auto.mat<-as.matrix(Auto[,-9])
  y.auto<-auto.mat[,1]
  x.auto<-auto.mat[,-1]
  dim(x.auto)
  dim(y.auto)

  matrix.2ndorder.make<-function(x, only.quad=F){
    x0<-x
    dimn<-dimnames(x)[[2]] #extract the names of the variables
    num.col<-length(x[1,]) #how many columns
    for(i in 1:num.col){# if we are doing all 2nd order
      if(!only.quad){
        for(j in i:num.col){
          x0<-cbind(x0,x[,i]*x[,j])
          dimn<-c(dimn,paste(dimn[i],dimn[j],sep=""))#create interaction dimnames
          }}
          else{#in here only if doing only squared terms
            x0<-cbind(x0,x[,i]*x[,i])
            dimn<-c(dimn,paste(dimn[i],"2",sep="")) #squared dimmension names
            }}
        dimnames(x0)[[2]]<-dimn
        x0
        }
 
  x.auto2<-matrix.2ndorder.make(x.auto,F)
  x.auto2a<-matrix.2ndorder.make(x.auto,T)
  dim(x.auto2)
  dim(x.auto2a)
 
  v1<-sort(sample(c(1:392),200))
  x.auto2.train<-x.auto[v1,]
  x.auto2.train<-x.auto2[v1,]
  x.auto2.test<-x.auto2[-v1,]
  x.auto2a.train<-x.auto2a[v1,]
  x.auto2a.test<-x.auto2a[-v1,]
  y.auto.train<-y.auto[v1]
  y.auto.test<-y.auto[-v1]

my.cp.extract.lars1<-function(str,matrix.train=x.auto2.train,matrix.test=x.auto.2.test,y.train=y.train){  
    
    #creates a boolean vector I1. This takes every value of that structure when Cp = the minimum Cp is True, otherwise the entry is set to be false.
    I1<-(str$Cp==min(str$Cp))       

    #takes the true values of I1 and creates a vector out of them, which is then stored in s1.
    s1<-c(1:length(I1))[I1]

    #creates training matrix of the data.
    xmat.train<-matrix.train
    ymat.train<-y.train  

    #predicts the values from lars regression and gets the training data fit.
    yp0<-predict(str,xmat.train,s1)$fit

    #calculates the training data residuals.
    resid<-ymat.train-yp0
    xmat.pred<-matrix.test

    #predicts the values from lars regression on the prediction data.
    yp1<-predict(str,xmat.pred,s1)$fit


    #stores the length of the prediction vector yp1 in npred.
    npred<-length(yp1)


    #takes the sample with replacement of the training data residuals.
    resp<-sample(resid,npred,replace=T)


    #returns the resampled residuals, the predicted values from the predicted data, along with the beta values of the selected predictors.
    list(ypredict0=yp1,resid=resp,beta=str$beta[I1,])
  }

my.cp.extract.leaps1<-function(str,matrix.train=x.auto2a.train,matrix.test=x.auto2a.test,y.train=y.train){
    
    #creates a boolean vector I1. This then takes every value of that structure when Cp = the minimum Cp is True, otherwise the entry is set to false.
    I1<-(str$Cp==min(str$Cp))

    #creates a matrix of the true and false values.
    which1<-str$which[I1,]

    #creates a matrix of the training data containing the selected predictors. These predictor variables will be columns in the created matrix.
    xmat.train<-(matrix.train)[,which1]
    ymat.train<-y.train

    #constructs a least fit line of the training data.
    ls.train<-lsfit(xmat.train,ymat.train)

    #the coeffciients of the training data regression are stored in the coef0.
    coef0<-ls.train$coef

    #creates a vector which2 that contains all the true values of the matrix which1.
    which2<-c(1:length(which1))[which1]

    #stores which1 in coef1.
    coef1<-which1

    #replaces true values with the regression coefficeints and the false values are set to 0.
    coef1[which1]<-coef0
    resid<-ls.train$resid

    #creates a prediction matrix with true predictor values as the columns.
    xmat.pred<-(matrix.test)[,which1]

    #takes beta values from the training set of least squares fit and multiplies it by the prediction matrix of the predictor data and adds the intercepts.
    yp1<-xmat.pred%*%coef0[-1]+coef0[1]  

    #the length of yp1 is then stored in npred.
    npred<-length(yp1)

    #resamples the residuals npred times and stores this in resp.
    resp<-sample(resid,npred,replace=T)

    #returns residuals, the preidction from test data, and the beta coefficients.
    list(ypredict0=yp1,resid=resp,beta=coef1)
  }

#initializing a function named my.boot.xy.conf
my.boot.xy.conf<-function(mat.train=x.auto2a.train,mat.test=x.auto2a.test,y.train=y.auto.train,y.test=y.auto.test,xstring="lars",brep=10000,pred.int=T,alpha=.05){

#specialized version of bootstrap
    if(xstring=="lars"){
        library(lars)
        func0<-lars
        reduce.function<-my.cp.extract.lars1
    }

    if(xstring=="leaps"){
        library(leaps)
        func0<-leaps
        reduce.function<-my.cp.extract.leaps1
    }

#initializing ypredmat, residmat, and betamat to null values.
ypredmat<-NULL
residmat<-NULL
betamat<-NULL

#setting n1 to the length of the first column of mat.train.
n1<-length(mat.train[,1])

#setting out0 to the value of reduce.function
#the reduce function in r combines the given parameters from the left.
out0<-reduce.function(func0(mat.train,y.train),mat.train,mat.test,y.train)

#combines all values of the ypredict0 column from the dataframe out0, returns them as a vector and stores that vector in ypred0.
ypred0<-c(out0$ypredict0)

#combines all values of the resid column from the dataframe out0, returns them as a vector and stores that vector in resid0.
resid0<-c(out0$resid)

#combines all values of the beta column from the dataframe out0, returns them as a vector and stores that vector in beta0.
beta0<-c(out0$beta)

#the rbind function combines the rows of ypredmat and ypred0. This is then stored in ypredmat.
ypredmat<-rbind(ypredmat,ypred0)

#the rbind function combines the rows of residmat and resid0. This is then stored in residmat.
residmat<-rbind(residmat,resid0)

#the rbind function combines the rows of betamat and beta0. This is then storeed in betamat.
betamat<-rbind(betamat,beta0)

    #This loop will iterate until i reaches the value of brep, which is defined as 10000 in the fucntion parameters above.
    for(i in 1:brep){

        if((i/200)==floor(i/200)){
            
            #if i/200 is equal to the floor value of i/200, which means the largest number not equal to and smaller than (i/200), then
            #combine i an brep into list, and return those values.
            print(c(i,brep))

        }
        
    #takes a sample of n1 of size n1, with replacement, and then sorts it. This is then stored in v1.
    v1<-sort(sample(n1,n1,replace=T))

    #sets m1 to the first row of mat.train
    m1<-(mat.train)[v1,]

    #sets y1 to the vector y.train.
    y1<-(y.train)[v1]

    #setting out1 to the value of reduce.function
    #the reduce function in r combines the given parameters from the left.
    out1<-reduce.function(func0(m1,y1),m1,mat.test,y1)

    #combines all the values of the predict0 column from the dataframe out1, returns them as a vector and stores that vector in ypred1.
    ypred1<-c(out1$ypredict0)

    #combines all the values of the resid column from the dataframe out1, returns them as a vector and stores that vector in resid1.
    resid1<-c(out1$resid)

    #combines all the values of the beta column from the dataframe out1, returns them as a vector and stores that vector in beta1.
    beta1<-c(out1$beta)

    #the rbind function combines the rows of ypredmat and ypred1. This is then stored in ypredmat.
    ypredmat<-rbind(ypredmat,ypred1)

    #the rbind function combines the rows of residmat and resid1. This is then stored in residmat.
    residmat<-rbind(residmat,resid1)

    #the rbind function combines the rows of betamat and beta1. This is then stored in betamat.
    betamat<-rbind(betamat,beta1)

    }

    #applies the mean function on the column of ypredmat matrix, and then stores this mean value in bagged.pred
    bagged.pred<-apply(ypredmat,2,mean)

    #applies the mean function on the column of betamat matrix, and then stores this mean value in bagged.beta
    bagged.beta<-apply(betamat,2,mean)

    #a function named quant.boot is being declared, with a parameter of x
    quant.boot<-function(x){

    #quantile function being called on the x parameter (which is a vector), and the vector created by the c function combining alpha/2 and 1-alpha/2
    quantile(x,c(alpha/2,1-alpha/2))
    
    }

    #if pred.int parameter is set to True in my.boot.xy.conf function's parameters...
    if(pred.int){

        #store the length of the first column of residmat
        n1<-length(residmat[,1])

        #takes a sample of the n1 vector of size n1, without replacement, and is stored in v9.
        v9<-sample(n1,n1,replace=F)

        #the paste function concatenates vectors after converting them to characters, thats the case for vectors xstring ("Prediction Interval") and alpha ("alpha"), and stores this in main1
        main1<-paste("Prediction interval",xstring,"alpha=",alpha)

        #applies the quant.boot function to on the column of both the ypredmat matrix and the residmat[v9,] matrix, and then stores the result in qboot.
        qboot<-apply(ypredmat+residmat[v9,],2,quant.boot)

    }else{

        main1=paste("Confidence interval,",xstring,"alpha=",alpha)
        qboot<-apply(ypredmat,2,quant.boot)

    }

    #sets y0 to y.test.
    y0<-y.test

    #Here bagged.pred is replicated 5 times; and y0, ypred0, bagged.pred, qboot[1,0], and qboot[2,1] are combined and then plotted with a y-axis label of "Data and intervals"
    #and a x-axis label of "Bagged prediction". With type "n" -> no plotting, and an overall title of main1.
    plot(rep(bagged.pred,5),c(y0,ypred0,bagged.pred,qboot[1,],qboot[2,]),xlab="Bagged prediction",ylab="Data and intervals",type="n",main=main1)

    #Draws points at the specified coordinates -> (bagged.pred,y0)
    points(bagged.pred,y0)

    #Joins the points bagged.pred and y0 with a line segment
    #lines(bagged.pred,y0)

    #puts bagged.pred in order and stores that vector in o1.
    o1<-order(bagged.pred)

    #plots the original predictions line
    lines(bagged.pred[o1],ypred0[o1],col=2)

    #plots lower bound of the bagged predictions
    lines(bagged.pred[o1],smooth(qboot[1,o1]),col=3)

    #plots upper bound of the bagged predictions
    lines(bagged.pred[o1],smooth(qboot[2,o1]),col=3)

#We take the mean of the bagged.beta, and the mean of the original beta. We then return these values to compare them.
baggedbetamean<-mean(bagged.beta)
originalbetamean<-mean(beta0)
print(baggedbetamean)
print(originalbetamean)

#this list function lists back bpred, ypred0, type, bagged.beta, orig.beta, and pred.int
list(bpred=bagged.pred,ypred0=ypred0,type=xstring,bagged.beta=bagged.beta,orig.beta=beta0,pred.int=pred.int)


}

#Running with lars.
my.boot.xy.conf(mat.train=x.auto2a.train,mat.test=x.auto2a.test,y.train=y.auto.train,y.test=y.auto.test,xstring="lars",brep=10000,pred.int=T,alpha=.05)

#Running with leaps.
my.boot.xy.conf(mat.train=x.auto2a.train,mat.test=x.auto2a.test,y.train=y.auto.train,y.test=y.auto.test,xstring="leaps",brep=10000,pred.int=T,alpha=.05)