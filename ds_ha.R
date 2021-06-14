library(ggplot2)
library(corrplot)
ptse=read.csv(file.choose())
ptse1=ptse
#data visualization
print(colSums(is.na(ptse)))
#bargraph
<<<<<<< HEAD
visualization1=function(country){
  data_for_visualization=ptse[ptse$Country==country,3:5]
  data_for_visualization$Series=paste(data_for_visualization$Series,data_for_visualization$Year)
  data_for_visualization=data_for_visualization[,2:3]
  ggplot(data_for_visualization, aes(x = Series, y = Value)) +
    geom_col() +
    coord_flip()+
    labs(title=country)
}
visualization1("India")
=======
# visualization1=function(country){
#   data_for_visualization=ptse[ptse$Country==country,3:5]
#   data_for_visualization$Series=paste(data_for_visualization$Series,data_for_visualization$Year)
#   data_for_visualization=data_for_visualization[,2:3]
#   ggplot(data_for_visualization, aes(x = Series, y = Value)) +
#     geom_col() + 
#     coord_flip()+
#     labs(title=country)
# }
>>>>>>> c82446f67dbbe37f7f92c25e79f77e84c225e745
colnames(ptse)
#
visualization2=function(country){
  
  ggplot(ptse[ptse$Country==country,3:5], aes(Series, Value)) +geom_point()+ geom_boxplot(fill = "red")+
    scale_y_continuous("petajoules", breaks= seq(0,581665, by=116333))+labs(title =country, x = "Value")
  
}

visualization2('Total, all countries or areas')
visualization3=function(country){
  data_for_visualization=ptse[ptse$Country==country,3:5]
  ggplot(data = data_for_visualization , aes(x = Year, y = Value)) +labs(title =country)+ geom_point(aes(colour=factor(Series)))
}
# visualization1('Africa')
# visualization1('Total, all countries or areas')
# visualization1('Asia')
<<<<<<< HEAD
print(visualization3('India'))
print(visualization2('Africa'))
print(visualization3('Russian Federation'))
print(visualization3('Japan'))
=======

print(visualization3('India'))
print(visualization2('Asia'))
print(visualization3('Russian Federation'))
>>>>>>> c82446f67dbbe37f7f92c25e79f77e84c225e745
head(ptse)
data_clean<-function(country)
{
  data=ptse[ptse$Country==country,]
  mean_primary=mean(data[data$Series=='Primary energy production (petajoules)',]$Value)
  mean_netimports=mean(data[data$Series=='Net imports [Imports - Exports - Bunkers] (petajoules)',]$Value)
  mean_stocks=mean(data[data$Series=='Changes in stocks (petajoules)',]$Value)
  mean_totalsupply=mean(data[data$Series=='Total supply (petajoules)',]$Value)
  mean_supplypercapita=mean(data[data$Series=='Supply per capita (gigajoules)',]$Value)
  years=unique(data$Year)
  Country=c()
  Primary_energy=c()
  net_imports=c()
  changes_in_stocks=c()
  total_supply=c()
  supply_per_capita=c()
  for(y in years){
    primary=data$Value[data$Series=='Primary energy production (petajoules)' & data$Year==y]
    net=data$Value[data$Series=='Net imports [Imports - Exports - Bunkers] (petajoules)' & data$Year==y]
    stocks=data$Value[data$Series=='Changes in stocks (petajoules)' & data$Year==y]
    supply=data$Value[data$Series=='Total supply (petajoules)' & data$Year==y]
    supply_capita=data$Value[data$Series=='Supply per capita (gigajoules)' & data$Year==y]
    Country=c(Country,country)
    print(primary)
    if(length(primary)){
      Primary_energy=c(Primary_energy,primary)
    }
    else{
      Primary_energy=c(Primary_energy,mean_primary)
    }
    if(length(net)){
      net_imports=c(net_imports,net)
    }
    else{
      net_imports=c(net_imports,mean_netimports)
    }
    if(length(stocks)){
      changes_in_stocks=c(changes_in_stocks,stocks)
    }
    else{
      changes_in_stocks=c(changes_in_stocks,mean_stocks)
    }
    if(length(supply)){
      total_supply=c(total_supply,supply)
    }
    else{
      total_supply=c(total_supply,mean_totalsupply)
    }
    if(length(supply_capita)){
      supply_per_capita=c(supply_per_capita,supply_capita)
    }
    else{
      supply_per_capita=c(supply_per_capita,mean_supplypercapita)
    }
  }
 cat(length(Country),length(years),length(Primary_energy),length(net_imports),length(changes_in_stocks),length(total_supply),length(supply_per_capita))
  a=data.frame(
    Country,
    years,
    Primary_energy,
    net_imports,
    changes_in_stocks,
    total_supply,
    supply_per_capita
  )
  return(a)
}
<<<<<<< HEAD
degree_of_correlation=function(x,y){
  correlation=cor(x,y,use='pairwise',method ="pearson")
  if(correlation>=0.5 || correlation<=-0.5){
    cat("High degree of correlation with coefficient of",correlation,"\n")
  }else if((correlation<0.5 && correlation>=0.3)||(correlation>-0.5 && correlation<=-0.3)){
    cat("Moderate degree of correlation with coefficient of",correlation,"\n")
  }else{
    cat("Low degree of correlation with coefficient of",correlation,"\n")
  }
}
x=data_clean('Russian Federation')
print(x)
#corrplot for country specific
x=subset(x,select=-c(changes_in_stocks,years))
correlations <- cor(x[sapply(x, is.numeric)])
print("Correlation between supply per capita and primary energy")
degree_of_correlation(x$supply_per_capita,x$Primary_energy)
print("Correlation between total supply and primary energy")
degree_of_correlation(x$total_supply,x$Primary_energy)
print("Correlation between supply per capita and net imports")
degree_of_correlation(x$supply_per_capita,x$net_imports)
print("Correlation between primary energy production and net imports")
degree_of_correlation(x$Primary_energy,x$net_imports)
print("Correlation between total supply and net imports")
degree_of_correlation(x$total_supply,x$net_imports)
corrplot(correlations,method = "circle", tl.cex = 0.55, tl.col = 'black', order = "hclust", addrect =4)
=======
x=data_clean('Russian Federation')
print(x)
#corrplot for country specific
correlations <- cor(x[sapply(x, is.numeric)])
corrplot(correlations,method = "circle", tl.cex = 0.55, tl.col = 'black', order = "hclust", addrect =6)
>>>>>>> c82446f67dbbe37f7f92c25e79f77e84c225e745
print(correlations)

#pca
var=apply(subset(x,select=-c(Country)),2,var)
scaled_x=apply(subset(x,select=-c(Country)),2,scale)
# x.cov=cov(scaled_x)
# x.eigen=eigen(x.cov)
# str(x.eigen)
# matrix=-x.eigen$vectors[,1:2]
# row.names(matrix)=c('years' ,'Primary_energy' ,'net_imports', 'changes_in_stocks', 'total_supply', 'supply_per_capita')
# colnames(matrix)=c('PC1','PC2')
# PC1=as.matrix(scaled_x)%*%matrix[,1]
# PC2=as.matrix(scaled_x)%*%matrix[,2]
# PC=data.frame(Series=row.names(matrix),PC1,PC2)
# PC
scaled_x
pca=prcomp(scaled_x)
variance_explained=pca$sdev^2
principle_ve=variance_explained/sum(variance_explained)
print(round(principle_ve,2))


#prediction
prediction=function(country){
x=data_clean(country)
x[is.na(x)]=0
multiple_lm=lm(supply_per_capita~net_imports+Primary_energy,data=x)
<<<<<<< HEAD
print(summary(multiple_lm))
=======
summary(multiple_lm)
>>>>>>> c82446f67dbbe37f7f92c25e79f77e84c225e745
x1=subset(x,select=-c(total_supply))
x_train=subset(x[1:(length(x1)-2),],select=-c(Country))
x_test=subset(x[(length(x)-1):length(x),],select=-c(Country,supply_per_capita))
y_test=x1[(length(x)-1):length(x),6]
multiple_lm=lm(supply_per_capita~Primary_energy+net_imports,data=x_train)
predicted=predict(multiple_lm,x_test)
summary(multiple_lm)
# resid(multiple_lm)
# print(plot(x_train, resid(multiple_lm),main="Supply per capita") )
cat("\npredicted values are:",predicted)
cat("\nactual values are:",y_test)
<<<<<<< HEAD
cat("\n")
}
prediction('India')
prediction('Russian Federation')
prediction('Mongolia')
=======
}
print(prediction('India'))
print(prediction('Russian Federation'))
>>>>>>> c82446f67dbbe37f7f92c25e79f77e84c225e745
Primary_energy=c()
net_imports=c()
changes_in_stocks=c()
total_supply=c()
supply_per_capita=c()
region=c()
count=0
data=ptse[ptse$Country=='India',]
for(country in unique(ptse$Country)){
  data=ptse[ptse$Country==country,]
  mean_primary=mean(data[data$Series=='Primary energy production (petajoules)',]$Value)
  mean_netimports=mean(data[data$Series=='Net imports [Imports - Exports - Bunkers] (petajoules)',]$Value)
  mean_stocks=mean(data[data$Series=='Changes in stocks (petajoules)',]$Value)
  mean_totalsupply=mean(data[data$Series=='Total supply (petajoules)',]$Value)
  mean_supplypercapita=mean(data[data$Series=='Supply per capita (gigajoules)',]$Value)
  for(y in unique(data$Year)){
    count=count+1
    region=c(region,unique(data$Region[data$Year==y]))
    primary=data$Value[data$Series=='Primary energy production (petajoules)' & data$Year==y]
    net=data$Value[data$Series=='Net imports [Imports - Exports - Bunkers] (petajoules)' & data$Year==y]
    stocks=data$Value[data$Series=='Changes in stocks (petajoules)' & data$Year==y]
    supply=data$Value[data$Series=='Total supply (petajoules)' & data$Year==y]
    supply_capita=data$Value[data$Series=='Supply per capita (gigajoules)' & data$Year==y]
    if(length(primary)){
      Primary_energy=c(Primary_energy,primary)
    }
    else{
      Primary_energy=c(Primary_energy,mean_primary)
    }
    if(length(net)){
      net_imports=c(net_imports,net)
    }
    else{
      net_imports=c(net_imports,mean_netimports)
    }
    if(length(stocks)){
      changes_in_stocks=c(changes_in_stocks,stocks)
    }
    else{
      changes_in_stocks=c(changes_in_stocks,mean_stocks)
    }
    if(length(supply)){
      total_supply=c(total_supply,supply)
    }
    else{
      total_supply=c(total_supply,mean_totalsupply)
    }
    if(length(supply_capita)){
      supply_per_capita=c(supply_per_capita,supply_capita)
    }
    else{
      supply_per_capita=c(supply_per_capita,mean_supplypercapita)
    }
  }
}
new_data=data.frame(
  region,
  Primary_energy,
  net_imports,
  changes_in_stocks,
  total_supply,
  supply_per_capita
)
# corrplot of all data
correlations <- cor(new_data[sapply(new_data, is.numeric)], use='pairwise')
corrplot(correlations,method = "circle", tl.cex = 0.55, tl.col = 'black', order = "hclust", addrect = 5)
<<<<<<< HEAD
cor(new)
=======

>>>>>>> c82446f67dbbe37f7f92c25e79f77e84c225e745
# t=(x1-x2)/sqrt((sd(hypo_data$Primary_energy)^2+sd(hypo_data$total_supply)^2)/50)
# t

#hypothesis testing (no correlation btw supply per capita and changes in stocks)
set.seed(123)
data=new_data
data<-data[order(runif(1792)),]
data[is.na(data)] <- 0
hypo_data=data[1:1792,]
hypo=lm(supply_per_capita~changes_in_stocks,hypo_data)
pvalue=summary(hypo)$coefficients[[8]]
x_train=subset(data[1:1440,],select=-c(total_supply))
x_test=subset(data[1400:1792,],select=-c(total_supply,supply_per_capita))
y_test=data$supply_per_capita[1440:1792]
cat("\nP-value is ",pvalue)
if(pvalue>0.05){
  cat("\nFailed to reject null hypothesis for alpha 0.05")
}else {
<<<<<<< HEAD
  cat("\nNull hypothesis rejected for alpha 0.05\n")
}
# new_data1=subset(new_data,select=-c(changes_in_stocks,total_supply))
# new_data1$region=as.factor(new_data1$region)
multiple_lm1=lm(total_supply~Primary_energy,new_data)
summary(multiple_lm1)
plot=ggplot(new_data,aes(y=total_supply,x=Primary_energy))+geom_point()+geom_smooth(method="lm",formula=y~x,se=F,col='blue')+ggtitle('SLR')
print(plot)
multiple_lm=lm(supply_per_capita~Primary_energy+net_imports+region,new_data)
ggplot(new_data, aes(y=net_imports, x=supply_per_capita)) +geom_point(size=2, shape=23)
ggplot(new_data, aes(y=total_supply, x=supply_per_capita)) +geom_point(size=2, shape=23)
ggplot(new_data, aes(y=total_supply, x=Primary_energy)) +geom_point(size=2, shape=23)
=======
  cat("\nNull hypothesis rejected for alpha 0.05")
}
# new_data1=subset(new_data,select=-c(changes_in_stocks,total_supply))
# new_data1$region=as.factor(new_data1$region)
multiple_lm=lm(supply_per_capita~total_supply+net_imports+region,new_data)
ggplot(new_data, aes(x=net_imports, y=supply_per_capita)) +geom_point(size=2, shape=23)
ggplot(new_data, aes(x=total_supply, y=supply_per_capita)) +geom_point(size=2, shape=23)
ggplot(new_data, aes(x=total_supply, y=Primary_energy)) +geom_point(size=2, shape=23)
>>>>>>> c82446f67dbbe37f7f92c25e79f77e84c225e745
summary(multiple_lm)
