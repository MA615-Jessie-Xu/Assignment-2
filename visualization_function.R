
# function 1: show the general situation
generalplot <- function(indicator){
  # indicator <- "birthrate"
  if(indicator == "birthrate"){
    ggplot(data = finetidy) +
      geom_smooth(method = 'loess',formula=y~x, mapping = aes(x = year, y = birthrate, group = continent, color = continent),se = FALSE)+
      expand_limits(x=2010, y = 50) +
      labs(title = "the birthrate of each continent")
  }
  else if(indicator == "schoolyears"){
    ggplot(data = finetidy) +
      geom_smooth(method = 'loess',formula=y~x, mapping = aes(x = year, y = schoolyears, group = continent, color = continent),se = FALSE)+
      expand_limits(x=2010, y = 12) + 
      scale_y_continuous(breaks = c(3,6,9,12)) +
      labs(title = "mean years of school attended by woman")
  }
  else{
    stop("warning: indicator does not exist. ")
  }
}
# generalplot("schoolyears")
# generalplot("birthrate")



# function 2: show specific situation
relationplot <- function(continent1, continent2 ){
  # country1 <- "Africa"
  # country2 <- "Europe"
  ggplot(filter(finetidy, continent == continent1 | continent == continent2), aes(x = birthrate, y = schoolyears)) +
    geom_point(mapping = aes(group = continent, color = continent), size = 1)+
    labs(title = "Exploration of schoolyears and birthrate")
}

# relationplot( "Africa", "Europe")



