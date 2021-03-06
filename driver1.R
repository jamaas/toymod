rm(list=ls(all=TRUE))

detach("package:toymod", unload=TRUE)
library(toymod)

## library(doParallel)
## cl <-makeCluster(10)
## registerDoParallel(cl)

cl <- doMPI::startMPIcluster(count=14)
doMPI::registerDoMPI(cl)

fun1.params <- list(var11=10, var12=150, var13=365)
fun2.params <- list(var21=0.05,var22=9.876)
fun3.params <- list(var31=1.396,var32=14.387,var33=3.219)

(fun3(var31=1.396,var32=14.387,var33=3.219))

(do.call(fun3, fun3.params))

(replicate(200, fun2(fun3.params, var21=0.05,var22=9.876)))

(replicate(200, do.call(fun2, c(list(fun3.params=fun3.params,
                                     var21=0.05,
                                     var22=9.876)))))

(fun1(fun2.params, fun3.params, var11=10, var12=150, var13=365))

do.call(fun1, c(list(fun2.params = fun2.params,
                     fun3.params = fun3.params),
                fun1.params))

