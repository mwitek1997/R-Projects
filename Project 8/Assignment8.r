#v1is the provided vector used test our fdr function. 
v1<-(c((1e-5*runif(100)),runif(900)))

#set new_v1 to have a NULL value.
new_v1 <- NULL
#set follow_up to have a NULL value.
follow_up <- NULL

#defined FDR as a function which takes in v1 and Ind as parameters, as Q already equals 0.05.
FDR <- function(v1 , Q=0.05 , Ind) {

#if statement is implemented inorder to test assuming independent(Ind = True) and not independent(Ind = False)

#Independent (Ind==True)
  if (Ind == TRUE) {
  #sorts the v1 vector and stores the sorted vector in sorted_v1
  sorted_v1 <- sort(v1)
  #set i to the value of 1:length(sorted_v1), which is just the length of the sorted vector.
  i <- (1:length(sorted_v1))
  #plots i(x-axis) vs. sorted_v1(y-axis).
  plot(i, sorted_v1,main="Ind=True")
  lines(i, Q*c(1:length(sorted_v1))/length(sorted_v1))
  Array <- Q*c(1:length(sorted_v1))/length(sorted_v1)
  #forloop which is itterated over the lenght of v1.
  for (x in 1: length(v1)) {
    #if the xth value in the v1 vectotr is less than the xth value in the Array vector
    #then store that x into the xth value of follow_up, and store the xth value of v1 into new_array.
    if (v1[x] < Array[x]) {
      follow_up[x] <- x
      new_array <- v1[x]
    }
  }
  #finds the maximum value of new_array and stores it in p_val.
  p_val<- max(new_array)
  #returns p_val
  print(p_val)
  #retunrs the follow_up vecrtor
  print(follow_up)
  }
  
  #Not Independent (Ind==False)
  else {
    #sorts the v1 vector and stores the sorted vector in sorted_v1
    sorted_v1 <- sort(v1)
    #set i to the value of 1:length(sorted_v1), which is just the length of the sorted vector.
    i <- (1:length(sorted_v1))
    #plots i(x-axis) vs. sorted_v1(y-axis).
    plot(i, sorted_v1,main="Ind=False")
    lines(i, Q*c(1:length(sorted_v1))/(length(sorted_v1)*(sum(1/i))))
    Array2 <- Q*c(1:length(sorted_v1))/(length(sorted_v1)*(sum(1/i)))
    #forloop which is itterated over the lenght of v1.
    for (x in 1: length(v1)) {
     #if the xth value in the v1 vectotr is less than the xth value in the Array2 vector
     #then store that x into the xth value of follow_up, and store the xth value of v1 into new_array.
      if (v1[x] < Array2[x]) {
        follow_up[x] <- x
        new_array <- v1[x]
      }
    }
    #finds the maximum value of new_array and stores it in p_val.
    p_val<- max(new_array)
    #returns p_val
    print(p_val)
    #returns follow_up
    print(follow_up)
  } 
  
}  

#Ind=True
FDR(v1, Q=0.05, Ind = TRUE)
#Ind=False
FDR(v1, Q=0.05, Ind = FALSE)
