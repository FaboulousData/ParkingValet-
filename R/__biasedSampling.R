biasedSampling<-function(n, pop, order, distr, repl)
{
  
  # Author: Fabrizio Miorelli
  # Licence: GNU General Public License v3.0 with restrictions
  # Purpose: only educational, training.
  #
  # Language: R
  # Packages: rbase, RODBC
  #
  #
  # This file is a part of the project ParkingValet+. The aim is to develop some applications for training and personal learning purposes. 
  # I do not intend to provide any part of code or script or file for commercial use. 
  # I do not own any responsability for the misuse of the scripts/data/files in this project nor I do own any resposability for the data in it.
  #
  #
  # The aim of this script is to develop a procedure for a biased sampling procedure with a given underlying distribution (gamma)
  #
  # INPUT: n (sample numerosity), pop (data - vector), order('desc', 'asc'), distr ('gamma'), repl ('FALSE')
  # OUTPUT: a biased sample of n elements, based on a random paramterized gamma distribution
  #
  #
  #
  #
  
  
  
  
  
  ## 1) checking the data-type of the population
  
  if(is.numeric(pop)==TRUE)
  {
    # insert code
  }
  
  if(is.character(pop)==TRUE)
  {
    # insert code
  }
  
  
  
  
  ## 2) checking if n is not 
  if( !(n > 0) )
  {
    stop('the n is not valid')
  }
  
  
  
  
  ## 3) ordering rule; transforming into dataframe
  if(order=='asc')
  {
    x<-sort(pop,decreasing=FALSE)
  }
  
  if(order=='desc')
  {
    x<-sort(pop,decreasing=TRUE)
  }
  
  data<-data.frame(x)
  data$index<-1:length(x)

  
  out<-c()
  
  
  
  
  
  ## 4) choosing the sampling distribution and generate random sample
  if(distr=='gamma')
  {

    if(repl==FALSE)
    {
      shape=7
      y<-rgamma(n,sample(1:7,1),1)
      
      if(!(n==1))
      {
        maxpop<-max(pop)
        fact<-maxpop/max(y)
        y<-round(y*fact)
      }
      
      y<-round(y)
      hist(y)
      y[y==0]<-1
      maxpop<-max(pop)

      if(n==1)
      {
        if(y>=maxpop)
        {
          y<-maxpop
        }
        
        out<-y
      }
      
      if(!(n==1))
      {
        for(i in 1:length(y))
        {
          if(y[i]>maxpop)
          {
            y[i]<-maxpop
          }
          
          buffer<-subset(data, data$index %in% y[i], select=x)
          out[i]<-buffer$x[[1]]
        }
      }
      
    }
  }

return(out)  
}