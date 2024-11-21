library(ggplot2)
revenue_data=data.frame(
  year=c(2015,2016,2017,2018,2019),
  revenue=c(30,35,40,45,50)
)
ggplot(revenue_data, aes(x=year, y=revenue))+
  geom_area(fill="blue",alpha=0.5)+
  labs(title="yearly revenue",
       x="year",
       y="revenue")
