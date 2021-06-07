ptse=read.csv(file.choose())
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
x=new_data
var=apply(subset(x),2,var)
scaled_x=apply(subset(x),2,scale)
scaled_x[is.na(scaled_x)] = 0
pca=prcomp(scaled_x)
pca$rotation=-pca$rotation
pca$x=-pca$x
variance_explained=pca$sdev^2
principle_ve=variance_explained/sum(variance_explained)
print("PCs for primary energy,net imports,changes in stocks, total supply and supply per capita are repectively: ")
print(round(principle_ve,2))
