library(ggplot2)
library(dplyr)

#Adds ow = Ozone/Wind column to airquality dataframe
addOW = function(){
  aqow = airquality %>%
    mutate(ow = Ozone/Wind)%>% 
    group_by(Month)
  return(aqow)
}
#Builds plot max(OW)/Month; needs dataframe with ow = Ozone/Wind column 
OWMonth_s= function(aqow){
  aqMonth = aqow %>% summarise(maxOW = max(ow, na.rm = TRUE))
  gplot = ggplot(aqMonth, aes(Month, maxOW)) + geom_point(shape = 21, stroke = 2, size = 8) +
    theme_bw()
  return(gplot)
}

#Builds plot for i month; if there is no info for i month, returns nothing, else ggplot ow(Day), 
#where "Day" is a day of i month
OWMonth = function(aqow, i){
  months <- aqow %>% summarise(na.rm = TRUE)
  
  if(is.element(i, months)) return("there is no data for this month")
  
  aqmon = aqow %>% filter(Month == i)
  gplot = ggplot(aqmon, aes(Day, ow)) + geom_point(shape = 21, size = 4, stroke = 2, color = "orange") +
    geom_line(color = "forestgreen") + theme_bw()
  return(gplot)
}

#Builds plot (points) OW/Temp;needs dataframe with ow = Ozone/Wind column, color is for temperature (TEMP)
OWTemp = function(aqow){
  gplot = ggplot(aqow, aes(Temp, ow)) + geom_point(aes(color = Temp, size = 1), na.rm = TRUE, show.legend = FALSE) + 
    ylab("Ozone / Wind") + scale_color_gradient(low = "lightblue", high = "red")
  return(gplot)
}

#Builds plot (histogram) with ow count; requires dataframe with ow = Ozone/Wind
OWCount = function(aqow) {
  gplot = ggplot(aqow, aes(ow)) + geom_bar() +
    scale_x_binned() + theme_bw()
  return(gplot)
}

#Checks shapiro.test; needs dataframe with ow column; if p < 0.1 returns "ow = norm" and "ow = no norm" otherwise
ifNorm = function(aqow){
  x <- aqow$ow
  shapiro = shapiro.test(x)
  answer = 'ow = no norm'
  if(shapiro$p.value < 0.1) answer = 'ow = norm'
  return(answer)
}

