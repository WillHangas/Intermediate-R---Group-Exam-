
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
plot_acceleration <- function(data,variable="euclidean_norm",option="mean"){

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

