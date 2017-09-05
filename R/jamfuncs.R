#' Test function level 1
#' @param var11 first variable for function 1
#' @param var12 second variable for function 1
#' @param var13 third variable for function 1
#' @export fun1

fun1 <- function (fun2.params, fun3.params, var11, var12, var13, ...) {

    results <- data.frame (foreach::`%dopar%`(
               foreach::`%:%`(foreach::foreach(j = 1:var11, .combine = cbind),
               foreach::foreach (i = 1:var12, .combine=rbind)),
               {
                   out3 <- replicate(var13,
                                     do.call(fun2,
                                             c(list(fun3.params=fun3.params),
                                               fun2.params)))
                   output2 <- data.frame(mean(out3))
        }
    )
)
    ## save outputs for subsequent analyses if required
saveRDS(results, file = paste("./outputs/", var13 ,"_", var12, "_", var11, "_",
                              format(Sys.time(), "%d_%m_%Y"), ".rds", sep=""))
}

#' Test function level 2
#' @param var21 first variable for function 2
#' @param var22 second variable for function 2
#' @export fun2

fun2 <- function (fun3.params, var21, var22, ...) {
    out2 <- `if` (rpois(1, var21) > 0, var22 * do.call(fun3, fun3.params), 0)
}

#' Test function level 3
#' @param var31 first variable for function 3
#' @param var32 second variable for function 3
#' @param var33 third variable for function 3
#' @export fun3

fun3 <- function (var31, var32, var33, ...) {
    out3 <- var31 * rnorm(1, mean=var32, sd= var33)
}
