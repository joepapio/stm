grabEst <-function(FX){
  #Function to pull parameter estimates from object produced by stm estimateEffects object
  
  ## ensure that object is stm estimateEffect object, otherwise give error
  if(!length(FX)==8){
    stop(cat("input is not an stm estimateEffect"))
  }else{
    if(!attributes(FX)$class=="estimateEffect"){
      stop(cat("input is not an stm estimateEffect"))
    }else{
      ##start function if object is of correct type
      
      ##create null objects for results storage
      parm <- NULL  
      vcvm <- rep(list(NULL),length(FX[[2]]))
      vv <- NULL
      
      ###for loop to extract betas and stdevs from estimateEffect object for the i-th topic
      for (i in 1:length(FX[[2]])){
        parm1 <-   transpose(FX[[1]][[i]])$est %>% ##use purrr tranpose function to gather all parameter 
          ##estimates together
          transpose() %>% ### use purrr transpose function to gather similar simulation values together together
          melt() %>%   ### use reshape2 melt funcion to turn list into data.frame
          group_by(L1) %>% ## use dplyr group_by function to group separate parameter simulations
          summarize(   ### use dplyr summarize function to find mean of simulations for each parameter
            Topic =mean(value)
          )
        if(is.null(parm)){
          ##for first topic, store results
          parm <- parm1
          names(parm)[i+1] <- paste( "Topic", i, sep="" ) 
        }else{
          ##for subsequent topics, store results
          parm <- merge(parm, parm1, by="L1") 
          names(parm)[i+1] <- paste( "Topic", i, sep="" )   
        }
        ##nested forloop to  extract variances for the j-th simulation
        for (j in 1:length(transpose(FX[[1]][[1]])$vcov)){
          if(is.null(vcvm[[i]])){
            #v-cov matrix for first simulation 
            vcvm[[i]] <-  (transpose(FX[[1]][[i]])$vcov)[[j]]    
          }else{
            #add v-cov matrix for the j-th simulation
            vcvm[[i]] <- vcvm[[i]] + (transpose(FX[[1]][[i]])$vcov)[[j]]
          }
        }
        ##multiply by inverse of number of simluations squared for i-th topic
        vcvm[[i]]<- vcvm[[i]]*(1/length(transpose(FX[[1]][[1]])$vcov))^2
        if(is.null(vv)){
          ##for first topic, set up storage object, getting first column from parms object so names match
          vv<- as.data.frame(parm[,1])
        }
        #extract square root of diagonal elements for i-th topic
        vv<- cbind(vv,sqrt(diag(vcvm[[i]])) )  
        
        #label column as the i-th topic
        names(vv)[i+1] <- paste( "Topic", i, sep="" )
      }
      
      ###combine storage objects
      ##rename first column
      names(parm)[1] <- "Parameter"
      names(vv)[1] <- "Parameter"
      ##melt parm and vv
      stdev <- vv %>% melt() %>% rename(sd=value)
      parms <- parm %>% melt() %>% rename(beta=value)
      #combine
      res <- cbind(parms, stdev[,3])
      #rename
      names(res)[4] <- "sd"
      
      vars <- FX$varlist
      #store number of covariates
      lvars <- length(vars)
      for(j in 1:lvars){
        res$Parameter <- gsub(vars[j], "", res$Parameter)
      }
    }
  }
  
  res$interact <- grepl(":", res$Parameter)
  
  restest <- res %>% nest(-variable, -interact) %>% spread(key=interact, data)
  
  restest %>% select(`TRUE`) %>% mutate(
    `TRUE` %>% purrr::map( separate(Parameter, into=c("year", "int"), sep=":"))
  )
  
  #res <- res %>% separate( Parameter, into=c("year", "int"), sep=":", remove = FALSE) 
  
  # for (i in 1:length(res$int)){
  #   if(res$interact[i]==FALSE) res$year[i] <- NA
  #   if(res$interact[i]==TRUE) res$Parameter[i] <- NA
  # }
  
  
#   resnest <- res %>% nest(-variable)
#  mini <-  scaff %>% nest(-1) 
#  mini$data[[1]][,1] 
# scaff %>% nest(-3)
 

for(i in 1:length(restest$`TRUE`)){
  restest$`TRUE`[[i]] <- restest$`TRUE`[[i]] %>% separate(col = Parameter, into=c("year", "int"), sep=":")
}

  vlist <- map(FX$data, unique)
  elems <- unlist(lapply(vlist,length ))
  scaff <- data.frame(expand.grid(vlist))
  
  
  
  
  combParms <- function(A){
    scaff$beta <- A$beta[1]
    scaff$variance <- (A$sd[1])^2
    for (j in  2:length(A$Parameter)){ ##row of the estimates tibble
      for (k in 1:length(scaff[,1])){ ##row of the scaffold
        for (l in 1:lvars){ ## column of the scaffold
          if((A$Parameter[j])==scaff[k,l])
            scaff$beta[k] <- scaff$beta[k] + A$beta[j]
          scaff$variance[k] <- scaff$variance[k] + (A$sd[j])^2
          
        }
      }
    }
    scaff
  }
  resnest <- resnest %>% mutate (
    parameters = purrr::map(data, combParms))
  
  ready <- resnest %>% select(topic = variable, parameters) %>% 
    unnest() %>% mutate( sd = sqrt(variance)) 
  
  ready
} 




inter <- grabEst(FX.Prez.int2)

inter %>% filter(partyID == "1. Republican" | 
                   partyID == "2. Independent" | 
                   partyID == "5. Democrat") %>%
  filter( race != "9. Missing")






ready %>% filter(partyID == "1. Republican" | 
                    partyID == "2. Independent" | 
                    partyID == "5. Democrat") %>% 
  filter(edu != "0. DK; NA; no Pre IW; short-form 'new' Cross Section") %>%
  ggplot(aes(x=yearf, y = beta)) +
  geom_point( aes( shape=edu, color= partyID)) +
  facet_wrap(~topic)

