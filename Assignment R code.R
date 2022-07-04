#install.packages("animation")
#install.packages("dplyr")
#Import dataset
path= 'E:/Credit Card Customer Data.csv'
data=read.csv(path)
summary(data)
cat("we have ",nrow(data),"sample")

#Pre-processing on dataset
library(dplyr)


rescale_data= data %>% #we used columns(Customer_Key , Avg_Credit_Limit) so we need rescaling function 
  #but if we used Total_visits_bank , we don't need rescaling function because data already scaled
  mutate(  Sl_No_scal = scale(Sl_No),
           Customer_Key_scal = scale(Customer_Key),
           Avg_Credit_Limit_scal = scale(Avg_Credit_Limit),
           Total_Credit_Cards_scal = scale(Total_Credit_Cards),
           Total_visits_bank_scal = scale(Total_visits_bank),
           Total_visits_online_scal = scale(Total_visits_online),
           Total_calls_made_scal = scale(Total_calls_made) ) %>%
  select(-c(Sl_No,Customer_Key,Avg_Credit_Limit,Total_Credit_Cards,Total_visits_bank,Total_visits_online,Total_calls_made))

summary(rescale_data)


#other method for scaling data using normalization function
#normalization=function(x)
#{
# num=x-min(x)
#denum=max(x)-min(x)
#res=num/denum
#return(res)
#}
#rescale_data=normalization(data)


#Training the model
result=kmeans(rescale_data , 3)
result


library(animation)
set.seed(2345)

kmeans.ani(rescale_data[2:3], 3 )# we used Customer_Key_scal & Avg_Credit_Limit_scal as features


#Evaluate your model to find  "optimal k"
result$withinss
kmean_withinss= function(k) {
  result= kmeans(rescale_data, k)
  return (result$tot.withinss)
}

# Set maximum cluster
max_k <-30
# Run algorithm over a range of k
wss <- sapply(2:max_k, kmean_withinss)
wss

# Create a data frame to plot the graph
elbow <-data.frame(2:max_k, wss)

#plotting to chose optimal k
library(ggplot2)
ggplot(elbow, aes(x = X2.max_k, y = wss)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 20, by = 1))

#optimal K is 5 
final_result=kmeans(rescale_data , 5)
final_result

set.seed(2345)
kmeans.ani(rescale_data[2:3], 5)# we used Customer_Key_scal & Avg_Credit_Limit_scal as features

final_result$cluster
final_result$centers
final_result$size
final_result$totss
final_result$withinss
final_result$tot.withinss
final_result$betweenss
final_result$iter
final_result$ifault

# plotting data before Scaling
plot(data[c("Customer_Key","Avg_Credit_Limit")],col=result$cluster, main="Cluster before Scaling")
# plotting data after Scaling
plot(rescale_data[c("Customer_Key_scal","Avg_Credit_Limit_scal")],col=final_result$cluster, main="Cluster after Scaling")
