library(ggplot2)
library(corrplot)
library("ggpubr")
ptse=read.csv(file.choose())
ptse1=ptse
#data visualization

colSums(is.na(ptse))
visualization1=function(country){
  data_for_visualization=ptse[ptse$Country==country,3:5]
  data_for_visualization$Series=paste(data_for_visualization$Series,data_for_visualization$Year)
  data_for_visualization=data_for_visualization[,2:3]
  ggplot(data_for_visualization, aes(x = Series, y = Value)) +
    geom_col() + 
    coord_flip()+
    labs(title=country)
}

visualization2=function(country){
  dfv=ptse[41:8225,4:5]
  net_imports=dfv$Value[dfv$Series=='Net imports [Imports - Exports - Bunkers] (petajoules)'] 
  length(net_imports)
  changes_in_stocks=dfv$Value[dfv$Series=='Changes in stocks (petajoules)']
  length(changes_in_stocks)
  dfv['net_imports']=net_imports
  ggplot(data,aes(x=net_imports, y=changes_in_stocks))+geom_point()+labs(title=country)
  
}
visualization3=function(country){
  data_for_visualization=ptse[ptse$Country==country,3:5]
  ggplot(data = data_for_visualization , aes(x = Year, y = Value)) +geom_boxplot() + geom_point(aes(colour=factor(Year)))
}
visualization1('Africa')
visualization1('Total, all countries or areas')
visualization1('Asia')

my_grey_theme <- theme(axis.text.x = element_text(colour="grey40", size=12, angle=90, hjust=.5, vjust=.5),
                       axis.text.y = element_text(colour="grey40", size=12), text=element_text(size=16))
ggplot(data =ptse, aes(x = Year, y = Value,group='Asia')) +
  geom_boxplot() + geom_point(colour=factor(ptse$Year))
visualization3('Asia')
visualization2('Asia')
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
  Year=c()
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
    years=c(Year,y)
    if(primary){
      Primary_energy=c(Primary_energy,primary)
    }
    else{
      Primary_energy=c(Primary_energy,mean_primary)
    }
    if(net){
      net_imports=c(net_imports,net)
    }
    else{
      net_imports=c(net_imports,mean_netimports)
    }
    if(stocks){
      changes_in_stocks=c(changes_in_stocks,stocks)
    }
    else{
      changes_in_stocks=c(changes_in_stocks,mean_stocks)
    }
    if(supply){
      total_supply=c(total_supply,supply)
    }
    else{
      total_supply=c(total_supply,mean_totalsupply)
    }
    if(supply_capita){
      supply_per_capita=c(supply_per_capita,supply_capita)
    }
    else{
      supply_per_capita=c(supply_per_capita,mean_supplypercapita)
    }
  }
 cat(length(Country),length(Year),length(Primary_energy),length(net_imports),length(changes_in_stocks),length(total_supply),length(supply_per_capita))
  a=data.frame(
    Country,
    Year,
    Primary_energy,
    net_imports,
    changes_in_stocks,
    total_supply,
    supply_per_capita
  )
  return(a)
}
data_clean('Africa')
country='Africa'
data=ptse[ptse$Country==country,]
data
data$Value[data$Series=='Primary energy production (petajoules)' & data$Year==2017]
