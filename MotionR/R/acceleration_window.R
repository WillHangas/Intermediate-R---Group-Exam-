
#This function takes for each axis x, y, z and calculates the average if acceleration increases/decreases by 0.5



#' Extracts and applies window min or max
#'
#' @param data Data frame containing index, time, three columns containing x,y,z, and the euclidean_norm
#' @param column Can be "x", "y", "z" or "euclidean_norm". Default is "x"
#' @param window How large you want the indexed window to be. Default is 1000
#' @param threshold A numeric threshold where changes in acceleration exceeding the threshold is manipulated. Default is 0.5
#'
#' @returns New columns "mean" and "binary"
#' @export
#'
#' @examples
#' # example code
#'  #Upload the data
#'  data("accelerometer_data")
#'
#'  #euclidean_norm
#'  accelerometer_data <- euclidean_calculation(accelerometer_data)
#'
#'  #Run function
#'  accelerometer_data <- window_threshold_test(accelerometer_data)
#'
window_threshold_test <- function(data,column="euclidean_norm",window=1000,threshold=0.5){

  #Creat new columns to contain the smoothen data, and a binary vector if there is an increase or decrease
  #in acceleration
  data[,c("mean","binary")] <- NA

  #Find index of data.column
  colnr <-  which(colnames(data)==column)

  #Goes through the column
  for(i in 1:(nrow(data)-window)){

  minacc <- min(data[i:(i+window),colnr])
  maxacc <- max(data[i:(i+window),colnr])
  iass <- data[i,colnr]
  negative_acc <- minacc-iass
  positive_acc <- maxacc-iass
  if(abs(negative_acc)>abs(positive_acc) & abs(negative_acc)>threshold){
    data[i,"mean"] <- abs(negative_acc)
    data[i,"binary"] <- (-1)
  } else if (abs(negative_acc)<abs(positive_acc) & abs(positive_acc)>threshold){
    data[i,"mean"] <- abs(positive_acc)
    data[i,"binary"] <- (1)
  } else {
    data[i,"mean"] <- data[i,colnr]
    data[i,"binary"] <- (0)
  }
  }

  #Removes data not assessed
  data <- data[1:(nrow(data)-window),]

  return(data)
}





#' Calculates the mean value of a sliding window
#'
#' @param data Data frame containing index, time, three columns containing x,y,z, and the euclidean_norm
#' @param column Can be "x", "y", "z" or "euclidean_norm". Default is "x"
#' @param window How large you want the indexed window to be. Default is 1000
#' @param threshold A numeric threshold where changes in acceleration exceeding the threshold is manipulated. Default is 0.5
#'
#' @returns Two columns "mean" (whenever acceleration exceed threshold mean for window is applied) and "binary" ("+1" or "-1" if acceleration is positive or negative)
#' @export
#'
#' @examples
#' # example code
#'  #Upload the data
#'  data("accelerometer_data")
#'
#'  #euclidean_norm
#'  accelerometer_data <- euclidean_calculation(accelerometer_data)
#'
#'  #Run function
#'  accelerometer_data <- window_threshold_test(accelerometer_data)
#'
window_function_test_mean <- function(data,column,window,threshold){

  #Creat new columns to contain the smoothen data, and a binary vectro if there is an increase or deacrease
  #in acceleration
  data[,c("mean","binary")] <- NA

  #Find index of data.column
  colnr <-  which(colnames(data)==column)

  #Goes through the column
  for(i in 1:(nrow(data)-window)){
    #If the increase/decrease in acceleration above the threshold the value will take the average of that window
    if((data[i+1,colnr]-data[i,colnr])>threshold){#Increase in acceleration
      #New smoothen value
      data[i,"mean"] <- mean(data[i:(i+window),colnr])
      data[i,"binary"] <- (1)
    } else if ((data[i+1,colnr]-data[i,colnr])<(-threshold)){#Decrease in acceleration
      data[i,"mean"] <- mean(data[i:(i+window),colnr])
      data[i-1,"binary"] <- (-1)
    } else {
      data[i,"mean"] <- data[i,colnr]
      data[i,"binary"] <- (0)}
  }
  #Removes data not assessed
  data <- data[1:(nrow(data)-window),]
  return(data)
}

