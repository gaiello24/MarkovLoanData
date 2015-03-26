#Given a dataset describing delinquent loans, we attempt to determine the transition
#matrix for loans moving between each of the following states:
#(0,1-5,6-10,11-30,31-60,61+). This is described by the two variables
#days_delinquent_old and days_delinquent_new. First, we organize the loans into
#each state using the following function sort():

#sort takes a list of numeric data and then orginizes it depending on the numeric
#starts vector.  For example, the starts vector (0,5,10,30,60) separates the data
# into the groups (0,1-5,6-10,11-30,31-60,61+).

sort <- function(data,starts){
  
  count <- rep(0,length(starts)+1)
  for (i in 1:(length(starts)+1)){
    if (i == 1){
      sorted_index <- list(which(data == starts[i]))
    }
    else if (i == (length(starts)+1)){
      sorted_index <- append(sorted_index, list(which(data > starts[i-1])))
    }
    else {
      sorted_index <- append(sorted_index, list(which(data > starts[i-1] & 			data <= starts[i])))
    }
  }
  return(sorted_index)
}

#With the indicies sorted, it is easy to separate the days_delinquent_old and
#days_delinquent_new data.  It is done as follows:

starts <- c(0,5,10,30,60)
index_a <- sort(data[,2],starts)
index_b <- sort(data[,3],starts)
sorted_days_del_old <- list(data[index_a[[1]],], data[index_a[[2]],],data[index_a[[3]],],data[index_a[[4]],],data[index_a[[5]],],data[index_a[[6]],])
sorted_days_del_new <- list(data[index_b[[1]],], data[index_b[[2]],],data[index_b[[3]],],data[index_b[[4]],],data[index_ab[5]],],data[index_b[[6]],])

#sorted_days_del_old and _new return lists of all the variables in the original
#data frame.  To isolate only the number of days, we use:

sorted_days_del_old <- list(data[index_a[[1]],2], data[index_a[[2]],2],data[index_a[[3]],2],data[index_a[[4]],2],data[index_a[[5]],2],data[index_a[[6]],2])
sorted_days_del_new <- list(data[index_b[[1]],3], data[index_b[[2]],3],data[index_b[[3]],3],data[index_b[[4]],3],data[index_ab[5]],3],data[index_b[[6]],3])

#To find the transition matrix of the movement from one group to the other, we
#use the following function called transition_matrix():

#transition_matrix requires two sets of data that represent the two observations
#we will use to build the probabilities.  It also requests the states the data
#will move between

transition_matrix <- function(obs_one,obs_two,states){
  trans_mat <- matrix(0,length(states)+1, length(states)+1)
  
  #transition_matrix uses the two observations and the states to sort the 
  #data into the necessary indicies
  
  sorted_index_one <- sort(obs_one, states)
  sorted_index_two <- sort(obs_two, states)
  
  for (i in 1:length(sorted_index_one)){
    for (j in 1:length(sorted_index_two)){
      if (length(sorted_index_one[[i]]) == 0){
        break
      }
      for (k in 1:length(sorted_index_one[[i]])){
        if (length(sorted_index_two[[j]]) == 0){
          break
        }
        if (sorted_index_one[[i]][k] %in% sorted_index_two[[j]]){
          
          #here is where we count the number of elements that
          #transitioned from state i to state j and store the value
          #in a matrix
          
          trans_mat[i,j] <- trans_mat[i,j] + 1
        }			
      }
      
      #here we divide each row of the trans_mat matrix by the number
      #of elements that were initially in each state, achieving the 
      #needed probabilities 
      
      trans_mat[i,j] <- trans_mat[i,j]/length(sorted_index_one[[i]])
    }	
  }
  return(trans_mat)
}

#Running this on days_delinquent_old, days_delinquent_new, with states (0,5,10,30,60) we have:

transition_matrix(data[,2],data[,3], starts)
[,1]       [,2]      [,3]      [,4]      [,5]      [,6]
[1,] 0.00000000 0.00000000 0.0000000 0.0000000 0.0000000 0.0000000
[2,] 0.20000000 0.45000000 0.1285714 0.2214286 0.0000000 0.0000000
[3,] 0.05882353 0.01470588 0.3529412 0.5735294 0.0000000 0.0000000
[4,] 0.07964602 0.00000000 0.0000000 0.3451327 0.5752212 0.0000000
[5,] 0.05769231 0.00000000 0.0000000 0.0000000 0.5000000 0.4423077
[6,] 0.17307692 0.00000000 0.0000000 0.0000000 0.0000000 0.8269231

#The first row is clearly all 0's because an element that starts at 0 days
#delinquent would not be included in data of delinquent loans.

#Now to create the transition matrix weighted by outstanding principal balance
#we use a slightly modified version of transition_matrix:

#transition_matrix_weight takes the same three arguments as transition_matrix
#but requires a fourth to define the weights.  It then proceeds as 
#transition_matrix did.

transition_matrix_weight <- function(obs_one,obs_two,weight,states){
  trans_mat <- matrix(0,length(states)+1, length(states)+1)
  sorted_index_one <- sort(obs_one, states)
  sorted_index_two <- sort(obs_two, states)
  for (i in 1:length(sorted_index_one)){
    for (j in 1:length(sorted_index_two)){
      if (length(sorted_index_one[[i]]) == 0){
        break
      }
      for (k in 1:length(sorted_index_one[[i]])){
        if (length(sorted_index_two[[j]]) == 0){
          break
        }
        if (sorted_index_one[[i]][k] %in% sorted_index_two[[j]]){
          
          #this is where the differences between the two functions
          #arise.  Instead of incrementing trans_mat[i,j] by 1, we 
          #add the weighted value of the elements that transition
          #between state i and j
          
          trans_mat[i,j] <- trans_mat[i,j] + weight[sorted_index_one[[i]][k]]
        }			
      }
      
      #and here, we divide by the total weighted value of all elements
      #that began in state i
      
      trans_mat[i,j] <- trans_mat[i,j]/sum(weight[sorted_index_one[[i]]])
    }
    
  }
  return(trans_mat)
}

#Running this function with days_delinquent_old, days_delinquent_new,
#new_principal_outstanding_balance, and starts (0,5,10,30,60) we have:

transition_matrix_weight(data[,2], data[,3], data[,4], starts)
[,1]        [,2]      [,3]      [,4]     [,5]      [,6]
[1,] 0.000000000 0.000000000 0.0000000 0.0000000 0.000000 0.0000000
[2,] 0.170003857 0.465036565 0.1321260 0.2328336 0.000000 0.0000000
[3,] 0.007633241 0.002838222 0.3537567 0.6357718 0.000000 0.0000000
[4,] 0.017980280 0.000000000 0.0000000 0.3861677 0.595852 0.0000000
[5,] 0.008673025 0.000000000 0.0000000 0.0000000 0.516369 0.4749579
[6,] 0.075863489 0.000000000 0.0000000 0.0000000 0.000000 0.9241365

#Once again, the top row is all 0's since no loan starts at 0 days outstanding.