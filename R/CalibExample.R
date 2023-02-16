# S Optimization Example
?DEoptim
f <- function (x) {
  CNmodelnew=CNmodel(CNmodeldf =CNmodeldf,CNavg = x)
  NSE(CNmodelnew$Qmm,CNmodelnew$Qpred)  
}
optimize(f, c(35,99), tol = 0.0001,maximum = TRUE)$maximum

# But if we want to optimize multiple parameters? Someone must have done 
# this before?!?!
?DEoptim

f <- function (x) {
  CNopt=x[1]
  IaOpt=x[2]
  CNmodelnew=CNmodel(CNmodeldf =CNmodeldf,CNavg = CNopt,IaFrac = IaOpt)
  1-NSE(CNmodelnew$Qmm,CNmodelnew$Qpred)  
}

lower <- c(35,.01)
upper <- c(99,.25)
