

#' Title Import of accelerometer data and change class on time variable
#'
#' @param data The name of the file which the data are to be read from.
#'
#' @returns A dataframe with the calculated "acceleration_vector"
#' @export
#'
#'
#'
accelerometer_data <- function(data){
#Store data in df
  df <- read.csv(file=data)

#Convert time to POSIXct
  df$time <- as.POSIXct(df$time)

return(df)

}






#' Calculation of euclidean norm
#'
#' @param data Data frame containing index, time, three columns containing x,y,and z
#'
#' @returns A euclidean_norm, and time vector
#' @export
#'
#'
#' @examples
#' # example code
#'  #Upload the practice data
#'  data("accelerometer_data")
#'
#'  #Run the function
#'  euclidean_calculation <- function(accelerometer_data)
#'
euclidean_calculation <- function(data){

  #Creat accelerometer vector
    data <- data%>%
    mutate(euclidean_norm=sqrt((x^2)+(y^2)+(z^2)))

return(data)
}


