library(ggplot2)
sales_data=data.frame(
  product=c("A","B","C","D","E"),
  sales=c(300,45,500,350,400)
)
ggplot(sales_data,aes(x=product, y=sales))+
  geom_bar(fill="blue",stat="identity",alpha=1.5,width=0.8)+
  labs(x="product",
       y="sales",
       title="sales data")
