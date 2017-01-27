library(stm)
library(ggplot2)
library(dplyr)
library(purrr)
library(reshape2)
library(broom)
library(tidyr)
library(rlist)

# typeof(stm.Prez$meta$feel)
# summary(head(stm.Prez$meta))
# 
# 
# stm.Prez$meta$party <- as.factor(stm.Prez$meta$party)
# stm.Prez$meta$feel <- as.factor(stm.Prez$meta$feel)
# stm.Prez$meta$mood <- as.factor(stm.Prez$meta$mood)

#k=0 to see what number of topics the model identifies on its own
##party + year + feel + feel*party
#year fit with a spline instead of a line
stmFit.Prez0 <- stm(stm.Prez$documents, 
                    stm.Prez$vocab,
                    K=0, 
                    prevalence= ~s(year)+party+feel+party*feel, 
                    data=stm.Prez$meta, 
                    init.type="Spectral")

FX.Prez0.all <- estimateEffect(1:52 ~s(year)+party+feel+party*feel, stmFit.Prez0, meta=stm.Prez$meta)

FX.test2 <- estimateEffect(1 ~s(year)+party+feel+party*feel, stmFit.Prez0, meta=stm.Prez$meta)

# head(FX.Prez.all[[1]])[1]
# 
# length(FX.Prez.all$parameters[[1]])

#k=0 to see what number of topics the model identifies on its own
##party + year + feel #skipping interaction
#year fit with a spline instead of a line
stmFit.Prez0a <- stm(stm.Prez$documents, 
                    stm.Prez$vocab,
                    K=0, 
                    prevalence= ~s(year)+party+feel, 
                    data=stm.Prez$meta, 
                    init.type="Spectral")

FX.Prez0a.all <- estimateEffect(1:56 ~s(year)+party+feel, stmFit.Prez0a, meta=stm.Prez$meta)

length(FX.Prez0a.all$parameters)
##56, one for each topic

length((FX.Prez0a.all$parameters)[[1]])
##25, default number of simulations 

names(((FX.Prez0a.all$parameters)[[1]])[[1]])
##$est, $cov

(((FX.Prez0a.all$parameters)[[1]])[[1]])$est

plot.estimateEffect(FX.Prez0a.all, 
                    covariate="year", 
                    method="continuous", 
                    topics =c(1:14))
plot.estimateEffect(FX.Prez0a.all, 
                    covariate="year", 
                    method="continuous", 
                    topics =c(15:28))
plot.estimateEffect(FX.Prez0a.all, 
                    covariate="year", 
                    method="continuous", 
                    topics =c(29:42))
plot.estimateEffect(FX.Prez0a.all, 
                    covariate="year", 
                    method="continuous", 
                    topics =c(42:56))


##########################
#k=20 
##party + year + feel #skipping interaction
#year fit with a spline instead of a line
stmFit.Prez20 <- stm(stm.Prez$documents, 
                     stm.Prez$vocab,
                     K=20, 
                     prevalence= ~s(year)+party+feel, 
                     data=stm.Prez$meta, 
                     init.type="Spectral")

FX.Prez20.all <- estimateEffect(1:20 ~s(year)+party+feel, stmFit.Prez0a, meta=stm.Prez$meta)


###estimate effects for 1 
FX.test <- estimateEffect(1 ~s(year)+party+feel, stmFit.Prez0a, meta=stm.Prez$meta)





map(FX.test[[1]][[1]], length)
tidy(FX.test[[1]][[1]])

tidy(FX.test[[1]][[1]])

melt(transpose(transpose(FX.test[[1]][[1]])$est))

test <- flatten(FX.test[[1]][[1]])

map(test[[1]][[1]], length)

head(FX.test[[1]][[1]])

test1 <- FX.test[[1]][[1]] %>%
  transpose() 

head(test1,1)

try <- transpose(test1$est)

map(try, length)

dim(as.data.frame(try))

try1 <- as.data.frame(matrix(unlist(try), ncol=8, byrow=F))

head(try1,2)

dim(try1)

col_mean <- function(df) {
  results <- numeric(length(df))
  for (i in seq_along(df)) {
    results[i] <- mean(df[[i]])
  }
  results
}

means <- col_mean(try1) 

means
means[1]

#<- seq(from=1984, to=2004, by =4)

mat <- matrix(rep(means[1], 24), nrow=4, ncol=6)

colnames(mat) <- seq(from=1984, to=2004, by =4)
rownames(mat) <- c("dem_dislike","dem_like","rep_dislike","rep_like")

mat[,2] <- mat[,1] + means[2]
mat[,3] <- mat[,1] + means[3]
mat[,4] <- mat[,1] + means[4]
mat[,5] <- mat[,1] + means[5]
mat[,6] <- mat[,1] + means[6]

mat[2,] <- mat[1,] + means[8]
mat[3,] <- mat[1,] + means[7]
mat[4,] <- mat[1,] + means[7] + means[8]

dframe <- as.data.frame(mat)

dframe
library(ggplot2)

mdframe <- melt(dframe)
mdframe$mood <- rep(c("dem_dislike","dem_like","rep_dislike","rep_like"))

names(mdframe) <- c("year", "value","mood")

ggplot(mdframe, aes(year, value)) +
  geom_point()
  
plot.estimateEffect(FX.test, 
                    covariate="year", 
                    method="continuous", 
                    topics =1)


###for a single topic
transpose(FX.test[[1]][[1]])$est %>% 
  transpose() %>% 
  melt() %>% 
  group_by(L1) %>%
  summarize(
    mean=mean(value)
  ) %>%
  as.data.frame()

transpose(FX.test[[1]][[1]])$est
transpose(FX.Prez20.all[[1]][[1]])$est

map(FX.test[[1]][[1]], length) 
map(FX.Prez20.all[[1]], length)




grabEst <-function(FX){
  #Function to pull parameter estimates from object produced by stm estimateEffects object
  #will automatically 
  
 res <- NULL  ##results storage object
 
 ###for loop to extract simulations from estimateEffect object
   for (i in 1:length(FX[[2]])){
  res1 <-   transpose(FX[[1]][[i]])$est %>% ##use purrr tranpose function to gather all parameter 
                                            ##estimates together
      transpose() %>% ### use purrr transpose function to gather similar simulation values together together
      melt() %>%   ### use reshape2 melt funcion to turn list into data.frame
      group_by(L1) %>% ## use dplyr group_by function to group separate parameter simulations
      summarize(   ### use dplyr summarize function to find mean of simulations for eachparameter
        Topic =mean(value)
      )
  if(is.null(res)){
    ##for first topic, store results
    res <- res1
    names(res)[i+1] <- paste( "Topic", i, sep="" ) 
  } else{
    ##for subsequent topics, store results
    res <- merge(res, res1, by="L1") 
    names(res)[i+1] <- paste( "Topic", i, sep="" )   
  }
   }
 ##rename first column
 names(res)[1] <- "Parameter"
 ##print results
 res
}

works <- grabEst(FX.Prez20.all)


dim(summary)

grabEst(FX.Prez0a.all)

 <- function(FX){
  factors <- NULL
  for (i in 1:length(FX$varlist){
    factors[i]  <- dim(summary(FX.$data[i]))[1]
  }
}
  
     
     
     