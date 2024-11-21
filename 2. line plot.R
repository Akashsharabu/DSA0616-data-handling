library(ggplot2)
population_data=data.frame(
  year=c(2010,2011,2012,2013,2014),
  population=c(1000,1020,1040,1060,1080)
)
ggplot(population_data, aes(x=year, y=population))+
  geom_line(color="blue",size=2)+
  geom_point(color="red",size=4)+
  labs(x="year",
       y="population",
       title="population data")
