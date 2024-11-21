library(ggplot2)
age_data=data.frame(
  age=c(25,30,35,40,50,55,60,65,70)
)
ggplot(age_data,aes(x=age))+
  geom_histogram(binwidth=5,fill="blue",color="black",alpha=0.7)+
  labs(x="age",
       y="frequency",
       title="Age Distribution")
