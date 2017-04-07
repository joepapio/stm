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
      
      ##store number of topics
      ktops <- length(FX$topics)
      
      #store list of covariates
      
      
      vlist <- map(FX$data, unique)
      elems <- unlist(lapply(vlist,length ))
      
      #store number of covariates
      lvars <- length(vlist)
      
      #number of cells for each topic
      numb <- prod(elems)
      
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

      
      for(j in 1:lvars){
        res$Parameter <- gsub(vlist[j], "", res$Parameter)
      }
    }
  }


##beginning of second half of grabEst function





scaff <- data.frame(expand.grid(vlist))


#drop dummy colum
scaff <- scaff[,-1]

dframe <- data.frame(NULL)

for (i in 1:ktops){
  
  scaffT <- scaff
  ##filter just the i-th topic
  tee <- res %>% filter( variable == paste("Topic", i, sep=""))
  
  #remove added variable names from covariate levels
  
  #add intercept to all cells
  scaffT$beta <- tee$beta[1]
  scaffT$variance <-  (tee$sd[1])^2
  scaffT$topic <- paste("Topic", i, sep="")
  
  for (j in 1:lvars){ ##identify covariate in the collection object
    for (k in 2:length(scaffT$beta)){  ##identify row of covariate of collection object
      for (l in 2:length(tee$Parameter)){  ##identify row from results object
        
        if(tee$Parameter[l]==scaffT[k,j]){ #if row of covariate matches Parameter, 
          #add beta and sd^2
          scaffT$beta[k] <- scaffT$beta[k] + tee$beta[l]
          scaffT$variance[k] <- scaffT$variance[k] + (tee$sd[l])^2
        }
      }
    }    
  }
  
  
  
  
  if(is.null(dframe)){
    dframe <- scaffT
  }
  dframe <- rbind(dframe, scaffT)
  
}
dframe
}