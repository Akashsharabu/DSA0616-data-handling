library(ggplot2)
sales_data=data.frame(
  month=factor(c("jan","feb","mar","apr","may"),levels=c("jan","feb","mar","apr","may")),
  cumulative_sales=c(100,200,300,400,500)
)
ggplot(sales_data, aes(x=month,y=cumulative_sales,group=1))+
  geom_step()+
  labs(title="cumulative sales",
       x="month",
       y="sales")
