#Group: Mathew Witek -> Partners: Michael Gleyzer and Harishkartik Kumaran Pillai

#initializing a function with name ridge.trace which takes in parameters x, y, and kvec.
ridge.trace<-function(x,y,kvec){

    #The scale function returns the correct measure of center for a chosen value.
    #Finds the measure of center for x.
    x1<-scale(x)

    #Finds the measure of center for y.
    y1<-scale(y)

    #remove unnecessary scale differences due to #measurement units
    #Sets beta.mat to a null value.
    beta.mat<-NULL

    #Sets kmat to a null value.
    kmat<-NULL

        #This for loop iterates for every value in kvec.
        for(k in kvec){

            #Returns the coefficent column's values by calling ridge.regression on the measure of centers x1 and y1.
            beta<-ridge.regression(x1,y1,k)$coef

            #recreate actual coefficients
            #cbind is a column bind function
            #Combines beta.mat with the beta dataset multipled by the 3rd inner value of x1.
            beta.mat<-cbind(beta.mat,beta*attributes(x1)[[3]])

            #k is replicated length(beta) times, and is then combined with kmat.
            #This is then set to kmat.
            kmat<-cbind(kmat,rep(k,length(beta)))
            
            }
            
            #concatenates kmat and beta.mat, then plots the results.
            plot(c(kmat),c(beta.mat))
            
}

#initializing a function with name ridge.regression which takes in parameters x, y, and k.
ridge.regression<-function(x,y,k){

    #Stores the value of the x parameter into the variable xmat.
    xmat<-x

    #Stores the lenght of the first row of matrix x into the variable n1.
    n1<-length(x[1,])

    #modify x and y to include k matrix in (x’x)-1
    #The rbind fucntion combines xmat, along with matrix k multiplied by the diagonal matrix.
    #This is then stored in xmat.
    xmat<-rbind(xmat,k*diag(n1))

    #The rep function replicates 0, n1 times.
    #The c function then combines y with the replicated 0's.
    #The resulting vector is then stored in yvec.
    yvec<-c(y,rep(0,n1))

    #Finds the least square estimate given xmat and yvec.
    #int=F states that an intercept term should not be used.
    lsfit(xmat,yvec,int=F)
    
}

#Given function in class to calculate PRESS.
PRESS<-function(x,y){

    resid<-lsfit(x,y)$resid
    stud.resid<-resid/(1-hat(x))
    press<-sum(stud.res^2)
    press
    
}

#Code for Least Square Regression.
leastsq.regression<-function(x,y){

    ls.str<-lsfit(x,y)
    PRESS(x,y)
    ls.str$press<-PRESS
    ls.str

}