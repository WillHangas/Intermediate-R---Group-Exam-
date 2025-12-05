
#' Plot accelerometer data
#'
#' @param data Data.frame containing variable "mean" and "binary" which you get from running
#' window_threshold_test() or window_function_test_mean().
#' @param variable Could be any axis "x", "y", "z", or "euclidean_norm". Default is "euclidean_norm"
#' @param option Specify if you want to plot "mean", "binary", or "mean_binary"
#'
#' @returns A plot object
#' @export
#'
#'
plot_accelerometer <- function(data,variable="euclidean_norm",option="mean"){

colnames(data)[which(colnames(data)==variable)]  <- "variable"

if (option=="mean"){
  ggplot(data=data,aes(x=time,y=variable))+
    geom_line(color="#FF876F")+
    geom_line(aes(x=time,y=mean),color="#4F0433")+
    labs(#I use stringr to make title and axis dynamic
      title=str_to_title(paste("Raw",str_replace(variable,"_"," "),"and mean")),
      y=str_to_title(str_replace(variable,"_"," ")))
} else if(option=="binary"){
  ggplot(data=data,aes(x=time,y=variable))+
    geom_line(color="#FF876F")+
    geom_line(aes(x=time,y=binary),color="#4F0433")+
    labs(#I use stringr to make title and axis dynamic
      title=str_to_title(paste("Raw",str_replace(variable,"_"," "),"and binary")),
      y=str_to_title(str_replace(variable,"_"," ")))
}
else if(option=="mean_binary"){
  ggplot(data=data,aes(x=time,y=variable))+
    geom_line(color="#FF876F")+
    geom_line(aes(x=time,y=mean),color="#4F0433")+
    geom_line(aes(x=time,y=binary))+
    labs(#I use stringr to make title and axis dynamic
      title=str_to_title(paste("Raw",str_replace(variable,"_"," "),"mean, and binary")),
      y=str_to_title(str_replace(variable,"_"," ")))
      }
}






#' Plot accelerometer data depending on method (mean or peak averages for values over threshold)
#'
#' @param data Data.frame containing containing euclidean_norm.
#' @param calculation Supply "mean" if you want to calculate mean if acceleration is above threshold
#' or "peak" if you want to supply largest differences (positive or negative)
#' @param column Which column to run function on. Supply "x", "y", "z", or "euclidean_norm"
#' @param option Specify if you want to plot "mean", "binary", or both "mean_binary"
#'
#' @returns A plot object
#' @export
#'
plot_accelerometer_data <- function(data,calculation="mean",column="euclidean_norm",option="mean"){

#Depending on calculation mean or peak different function are runned
  if(calculation=="mean"){
    data <- window_threshold_mean(data,column)
  } else if (calculation=="peak"){
    data <- window_threshold_min_max(data,column)
  }


#  colnames(data)[which(colnames(data)==option)]  <- "option"
  colnames(data)[which(colnames(data)==column)]  <- "column"

  if (option=="mean"){
    ggplot(data=data,aes(x=time,y=column))+
      geom_line(color="#FF876F")+
      geom_line(aes(x=time,y=mean),color="#4F0433")+
      labs(#I use stringr to make title and axis dynamic
        title=str_to_title(paste("Raw",str_replace(column,"_"," "),"and mean",str_replace(column,"_"," "))),
        y=str_to_title(str_replace(column,"_"," ")))
  } else if(option=="binary"){
    ggplot(data=data,aes(x=time,y=column))+
      geom_line(color="#FF876F")+
      geom_line(aes(x=time,y=binary),color="#4F0433")+
      labs(#I use stringr to make title and axis dynamic
        title=str_to_title(paste("Raw",str_replace(column,"_"," "),"and binary")),
        y=str_to_title(str_replace(column,"_"," ")))
  }
  else if(option=="mean_binary"){
    ggplot(data=data,aes(x=time,y=column))+
      geom_line(color="#FF876F")+
      geom_line(aes(x=time,y=mean),color="#4F0433")+
      geom_line(aes(x=time,y=binary))+
      labs(#I use stringr to make title and axis dynamic
        title=str_to_title(paste("Raw",str_replace(column,"_"," "),"mean",str_replace(column,"_"," "),"and binary")),
        y=str_to_title(str_replace(column,"_"," ")))
  }
}



