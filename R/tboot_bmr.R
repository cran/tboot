#' @title Function tboot_bmr
#' @description Bootstrap \code{nrow} rows of \code{dataset} using
#' the given row-level weights.
#' @seealso \code{\link{tweights}}
#' @export
#' @param nrow Number of rows in the new bootstrapped dataset.
#' @param weights_bmr An object of class 'tweights_bmr' output from the 'tweights_bmr' function.
#' @param tol_rel_sd An error will be called if for some simulation if the target is not achievable with the data. However, the error will only be called if max absolute difference releative to the marginal standard is greater than specified.
#' @details
#' Simulates a dataset by first simulating from the posterior distribution of the column means and then simulating a dataset with that underlying mean. Details a further documented in the vignette.
#' @return 
#' A simulated dataset with 'nrow' rows. The underlying 'true' posterior parameter value is an attribute which can be extracted useing \code{attr(ret, "post_bmr")} where 'ret' is the matrix.
#' @examples
#' #Use winsorized marginal to keep marginal simulation within feasible bootstrap region
#' winsor=function(marginalSims,y)  {
#'   l=min(y)
#'   u=max(y)
#'   ifelse(marginalSims<l,l,ifelse(marginalSims>u,u, marginalSims))
#' }
#' #Create an example marginal posterior
#' marginal = list(Sepal.Length=winsor(rnorm(10000,mean=5.8, sd=.2),iris$Sepal.Length),
#'                Sepal.Width=winsor(rnorm(10000,mean=3,sd=.2), iris$Sepal.Width),
#'                Petal.Length=winsor(rnorm(10000,mean=3.7,sd=.2), iris$Petal.Length)
#' )
#' 
#' #simulate
#' w = tweights_bmr(dataset = iris, marginal = marginal, silent = TRUE)
#' sample_data = tboot_bmr(1000, weights = w)
#' 


tboot_bmr=function(nrow, weights_bmr, tol_rel_sd=.01) {
  if(length(tol_rel_sd)!=1 | !is.numeric(tol_rel_sd))
    stop("'tol_rel_sd' must be a numeric fector")
  if(missing(nrow))
    stop("'nrow' is missing")
  if(missing(weights_bmr))
    stop("'weights_bmr' is missing")
  if(!("tweights_bmr" %in% class(weights_bmr)))
    stop("'weights_bmr' must be an object of class 'tweights_bmr' from the 'tweights_bmr' function.")
  if(!is.numeric(nrow))
    stop("'nrow' must be numeric.")
  if(length(nrow)!=1)
    stop("'nrow' must be length 1.")
  
  
  #Will try to recover once, just error out if it happens twice.
  weights=tryCatch(.get_new_weights(nrow, weights_bmr, tol_rel_sd=tol_rel_sd),
                                    error=function(e) {
                                      warning(paste("Resimulating new draw from posterior becuase unable to find weights which achieve current sim from posterior.", e$message))
                                      return(.get_new_weights(nrow, weights_bmr, tol_rel_sd=tol_rel_sd))
                                    })
  
  ret=tboot(nrow=nrow,
            weights=weights,
            dataset=weights$X, #for now don't include extra columns
            fillMissingAug=FALSE)
  tmp=weights$mu
  names(tmp)=names(weights_bmr$marginal)
  attr(ret, "post_bmr")=tmp
  return(ret)
}

.get_new_weights=function(nrow, weights_bmr, tol_rel_sd=.05) {
  
  p=nrow(weights_bmr$Csqrt)
  z = weights_bmr$Csqrt %*% rnorm(p)
  u = pnorm(z)
  mu= mapply(function(u,marginal) as.numeric(quantile(marginal, probs = u)),
             u, weights_bmr$marginal)
  names(mu)=names(weights_bmr$marginal)
  
  
  weights=suppressWarnings(tweights(weights_bmr$tweights$X, target=mu,
                                             distance=weights_bmr$distance,
                                             maxit = weights_bmr$maxit,
                                             tol=weights_bmr$tol,
                                             warningcut=weights_bmr$warningcut,
                                             silent=TRUE,
                                             Nindependent=weights_bmr$Nindependent))
  if(max(abs(weights$achievedMean - mu)/weights_bmr$marginal_sd) > tol_rel_sd){
    stop("Unable to simulate accurately.")
  }
  
  weights$mu=mu
  return(weights)
}
