#' @title Hypothesis Test and Interval Estimation of the Mean of Normal Population with the variance is known
#' @description Hypothesis test and interval estimation of the mean when the variance of the normal population is known using R
#' @param x data samples
#' @param mu the mean to test
#' @param sigma the variance
#' @param alpha the confidence level
#' @param alternative alternative hypothesis
#' @return test results and confidence intervals
#' @examples
#' \dontrun{
#' N.test(x=X,mu=0,sigma=1,alpha=0.05,alternative='two.side')
#' }
#' @importFrom stats qnorm
#' @useDynLib StatComp20067
#' @export
N.test<- function(x,mu,sigma,alpha,alternative='two.side')
{  xbar=mean(x) 
   n=length(x)	 
if(alternative=='two.side') 
{ N=qnorm(alpha/2)
  lower=xbar-sqrt((sigma/n))*N
  higher=xbar+sqrt((sigma/n))*N
  area=c(lower,higher)
  TS=(xbar-mu)/sqrt(sigma/n) 
  TF='reject' 
  if ( abs(TS)<abs(N)) 
  {
    TF='accept'  
  }
  result=list('accept_or_reject'=TF,'interval estimation'=area)
  
}
if(alternative=='less')
{
  N=qnorm(alpha)
  lower=xbar+sqrt((sigma/n))*N
  higher=Inf 
  area=c(lower,higher)
  TS=(xbar-mu)/sqrt(sigma/n)
  TF='reject'
  if (TS>N)
  {
    TF='accept'
  }
  result=list('accept_or_reject'=TF,'interval estimation'=area)
  
}
if(alternative=='greater')
{
  N=-qnorm(alpha)
  lower=-Inf
  higher=xbar-sqrt((sigma/n))*N
  area=c(lower,higher)
  TS=(xbar-mu)/sqrt(sigma/n)
  TF='reject'
  if (TS<N)
  {
    TF='accept'
  }
  result=list('accept_or_reject'=TF,'interval estimation'=area)
}
return(result)
}

#' @title Hypothesis Test and Interval Estimation of the Mean of Normal Population with the variance is unknown
#' @description Hypothesis test and interval estimation of the mean when the variance of the normal population is unknown using R
#' @param x data samples
#' @param mu the mean to test
#' @param alpha the confidence level
#' @param alternative alternative hypothesis
#' @return test results and confidence intervals
#' @examples
#' \dontrun{
#' T.test(x=X,mu=0,alpha=0.05,alternative='two.side')
#' }
#' @importFrom stats qt sd
#' @useDynLib StatComp20067
#' @export
T.test<- function(x,mu,alpha,alternative='two.side')
{  xbar=mean(x)
   s=sd(x)
   n=length(x)	 
if(alternative=='two.side') 
{
  N=qt(alpha/2,df=n-1) 
  lower=xbar-s/sqrt(n)*N
  higher=xbar+s/sqrt(n)*N
  area=c(lower,higher)
  TS=(xbar-mu)/(s/sqrt(n)) 
  TF='reject' 
  if ( abs(TS)<abs(N)) 
  {
    TF='accept'  
  }
  result=list('accept_or_reject'=TF,'interval estimation'=area)
  
}
if(alternative=='less')
{
  N=qt(alpha,df=n-1)
  lower=xbar+s/sqrt(n)*N
  higher=Inf 
  area=c(lower,higher)
  TS=(xbar-mu)/(s/sqrt(n)) 
  TF='reject'
  if (TS>N)
  {
    TF='accept'
  }
  result=list('accept_or_reject'=TF,'interval estimation'=area)
  
}
if(alternative=='greater')
{
  N=-qt(alpha,df=n-1)
  lower=-Inf
  higher=xbar-s/sqrt(n)*N
  area=c(lower,higher)
  TS=(xbar-mu)/(s/sqrt(n))
  TF='reject'
  if (TS<N)
  {
    TF='accept'
  }
  result=list('accept_or_reject'=TF,'interval estimation'=area)
}
return(result)
}
