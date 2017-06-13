library(dplyr)
library(ggplot2)
setwd("C:\\Users\\Vaibhav\\Desktop\\BA\\Analytics\\jigsaw_acad\\R_Sessions\\Case_Studies\\Telecom_Churn_Final CaseStudy")
oj=read.csv("Telecom.csv")
str(oj)
summary(oj)

summary(oj$retdays)
#Missing values here means - 0 calls - as per the data definition
oj$retdays = ifelse(is.na(oj$retdays), 0, oj$retdays)
sum(is.na(oj$retdays))#no NAs in this variable now



#Preparing quality report
quality=data.frame(names(oj))

names(quality)="VariableName"
quality$datatype=class(oj[,names(oj)])
len=length(names(oj))
i=1
for(i in 1:len)
{
  quality$datatype[i]=class(oj[,i])
  quality$NoOfRecords[i] = nrow(oj)
  quality$UniqueRecords[i]=length(unique(oj[,i]))
  quality$DataAvailable[i]= quality$NoOfRecords[i]-sum(is.na(oj[,i]))
  quality$AvailablePercent[i]=round(quality$DataAvailable[i]/quality$NoOfRecords[i],2)
  quality$Missing[i]=sum(is.na(oj[,i]))
  quality$MissingPtage[i] = round(quality$Missing[i]/quality$NoOfRecords[i],2)  
  quality$Minimum[i]=ifelse(quality$datatype[i]=="factor","Factor",min(oj[,i],na.rm=T))
  quality$Maximum[i]=ifelse(quality$datatype[i]=="factor","Factor",max(oj[,i],na.rm=T))
  quality$Mean[i]=ifelse(quality$datatype[i]=="factor","Factor", mean(oj[,i],na.rm=T))
  quality$five_ptile[i] = ifelse(quality$datatype[i]=="factor","Factor", quantile(oj[,i],5/100, na.rm = T)) 
  quality$ten_ptile[i] = ifelse(quality$datatype[i]=="factor","Factor", quantile(oj[,i],10/100, na.rm = T)) 
  quality$twentyfive_ptile[i] = ifelse(quality$datatype[i]=="factor","Factor", quantile(oj[,i],25/100, na.rm = T)) 
  quality$fifty_ptile[i] = ifelse(quality$datatype[i]=="factor","Factor", quantile(oj[,i],50/100, na.rm = T)) 
  quality$seventyfive_ptile[i] = ifelse(quality$datatype[i]=="factor","Factor", quantile(oj[,i],75/100, na.rm = T))
  quality$ninety_ptile[i] = ifelse(quality$datatype[i]=="factor","Factor", quantile(oj[,i],90/100, na.rm = T))
  quality$ninetyfive_ptile[i] = ifelse(quality$datatype[i]=="factor","Factor", quantile(oj[,i],95/100, na.rm = T))
}  

length(names(quality))

write.csv(quality,"quality.csv")


#target variable is churn - 0 or 1 
telecom=oj
summary(telecom$churn)#only 23.92% of values have high churn rate

#How many missing values in the data
colSums(is.na(telecom))

#all the variables with high number of missing values, as per quality report
#we'll understand them and exclude for our modelling, if not critical for churn understanding
cutoff_na=.25 #declaring the percentage cut off of missing values


#getting the variable names with number of missing values more than cut-off
a=quality$VariableName[quality$MissingPtage>cutoff_na]
a=as.character(a)
a

col=length(a)
for (i in 1:length(a))
{
  col[i]=grep(a[i], names(telecom))}
class(col)
#deleting the above columns from main db and quality db
telecom=telecom[,-col]
a=which(quality$MissingPtage>cutoff_na)
quality=quality[-a,]


#let's seperate factor variables and qualitative variables for our analysis
quality_factor=quality[quality$datatype=="factor" | (quality$datatype=="integer" & quality$UniqueRecords<20),]
ser_num=1:nrow(quality_factor)
quality_factor=cbind(ser_num, quality_factor)
p=which(quality$VariableName %in% quality_factor$VariableName)
quality_quant=quality[-p,]
ser_num=(nrow(quality_factor) + 1):(nrow(quality_quant) + nrow(quality_factor))
quality_quant=cbind(ser_num, quality_quant)


#doing the above re-ordering in main telecom dataset as well
a=quality$VariableName[quality$datatype=="factor" | (quality$datatype=="integer" & quality$UniqueRecords<20)]
a=as.character(a)
col=1:length(a)
for (i in 1:length(a))
{
  col[i]=grep(a[i], names(telecom))
#  print(i)
}
telecom_factor=telecom[,col]
telecom_quant=telecom[,-col]



#Removing the churn-event variable from our analysis
cn=grep("churn", names(telecom_factor))
#quality_row1=quality_factor[cn,]
#quality_factor=quality_factor[-cn,]
#quality_factor=rbind(quality_row1, quality_factor)
churn=telecom_factor[,cn]
telecom_factor=telecom_factor[,-cn]
telecom_factor=cbind(churn, telecom_factor)

#Reordering quality and telecom datasets acc to factor var and quantitative variables
#quality=rbind(quality_factor, quality_quant)
telecom=cbind(telecom_factor,telecom_quant)

head(telecom)
colSums(is.na(telecom))

#Analysing quantitative variables
Quant_DA=data.frame()
x=ncol(telecom_factor)+1

i=x
#decile-wise (/n-quantile-wise) binning of quantitative variables, and finding the corresponding event rate:
for(i in x:ncol(telecom))
{
  p=as.name(names(telecom)[i])
  p #remove the quotation mark - it is important for finding the max in each level
  j=10
  for(j in 10:1)
  {
    max=unclass(telecom%>%mutate(Lvl = ntile(telecom[,i],n=j))%>%group_by(Lvl)%>%summarize(round(max(p),2))%>%unname())[[2]]
    #unname will take away the names
    class(max)
    #unclass will convert table into list format
    if(max[1]!=0)#if the max value is 0, then we will try making j-1 quantiles for this variable
    { 
      break
    }
  }
  dat1=data.frame(1:(j+1), 0) #for levels + 1 NA level
  names(dat1)=c("Lvl", "Num_High_Churns")
  dat1[(j+1),1]=NA #11th row represents the number of NA values
  dat1$Num_High_Churns=rep(0,(j+1))
  telecom%>%mutate(Lvl = ntile(telecom[,i],n=j))->telecom
  telecom%>%group_by(Lvl)%>%count(churn,Lvl)%>%filter(churn==1)->dat2
  dat1$Num_High_Churns=rep(0,(j+1))
  dat1$Num_High_Churns[which(dat1$Lvl %in% dat2$Lvl)]=dat2$n
  dat1$ChurnVariable = names(telecom)[i]
  Total = unclass(telecom%>%group_by(Lvl)%>%summarize(n())%>%unname())[[2]]
  if(length(Total)!=j+1) Total[j+1]=NA
  dat1$Total = Total
  dat1$Churn_Rate=round(dat1$Num_High_Churns/dat1$Total,2)
  dat1$Col_Num=i
  dat1$DataType=class(telecom[,i])
  max[j+1]=NA 
  dat1$Max_Lvl=max
  min=unclass(telecom%>%group_by(Lvl)%>%summarize(round(min(p),2))%>%unname())[[2]]
  min[j+1]=NA
  dat1$Min_Lvl=min
  mean=unclass(telecom%>%group_by(Lvl)%>%summarize(round(mean(p),2))%>%unname())[[2]]
  mean[j+1]=NA
  dat1$Mean_Lvl=mean
  dat1$levels = j+1
  Quant_DA=rbind(Quant_DA,dat1)
#  print(i)
  telecom$Lvl=NULL
}
rm(dat1)
write.csv(Quant_DA, "quantitative_data_analysis.csv")


#Missing values imputation of quantitative data. 
# Impute the value of NAs in each col acc. to churn rate

j=1
rn=0
for(i in 1:(nrow(Quant_DA)))
{ 
  if(i!=(rn[j]+1)) next 
  #if we reach row number = last row of dat data-frame, then re-start the loop
  #i.e we can skip the loop for all rows but the first row of each column number
  
  j=Quant_DA$levels[i]
  p=Quant_DA$Col_Num[i]
  Quant_DA%>%filter(Col_Num==p)->dat
  diff=rep(0,(j-1))
  

  #if there is no NA value in the variable (i.e churn rate of NA), then move to next variable  
  if(is.na(dat$Churn_Rate[j]))
  {
    rn=which(Quant_DA$ChurnVariable%in%dat$ChurnVariable)
    next
  }
  #check the difference between the churn rate of NA with churn rate of each decile
  for(k in 1:(j-1))
  {
    diff[k]=dat$Churn_Rate[j]-dat$Churn_Rate[k]
  } 
  a=(min(Mod(diff)))
  #we will take the minimum difference and substitute the mean value in that level
  b=sum(diff%in%a)
  mean=ifelse((b!=0),mean(dat$Mean_Lvl[which(diff%in%a)]), (dat$Mean_Lvl[which(diff%in%(-1*a))]))  
  telecom[is.na(telecom[, dat$Col_Num[1]]), dat$Col_Num[1]]=mean
  rn=which(Quant_DA$ChurnVariable%in%dat$ChurnVariable)
}
telecom_quant=telecom[,((ncol(telecom_factor)+1):ncol(telecom))]

colSums(is.na(telecom_quant))


#Keeping only the continuous variables which show some trend / important for
#so we can exclude the following continuous variables from our modelling data-set: rev_Range, age2, da_Mean, da_Range, hnd_Price
i=length(names(telecom_factor))
telecom_quant=telecom_quant[, -c(21-i,49-i,57-i,58-i)]
rn=which(Quant_DA$Col_Num %in% c(21,49, 57,58))
Quant_DA=Quant_DA[-rn,]

names(telecom_quant)
telecom=cbind(telecom_factor, telecom_quant)

#Updating col. numbers in Quant_DA
for(i in 1:nrow(Quant_DA))
{
  Quant_DA$Col_Num[i]= which(names(telecom) %in% Quant_DA$ChurnVariable[i])
}


#Analysing outliars and doing outliars imputation in continuous variables

x=length(names(telecom_factor))+1
for(i in x:length(names(telecom))) 
{
  #outliar treatment in col i of telecom
  a=boxplot.stats(telecom[,i] )
  if(a$stats[1]==a$stats[5])
  next  
  # if a$stats[1]=a$stats[5], and if we proceed with outliar imputation the whole
  #    column will reduce into a single value. There is a complete loss in information,
  #    which might be important for us. So, we exclude such variables from outliar imputation process
    
  if(length(a$out)!=0)
  { 
    min=min(telecom[,i])
    max=max(telecom[,i])
    mn=min-1
    mx=max+1
    #replacing the bottom outliars with a$stats[1], and top outliars with a$stats[5]
    replacement_value =cut(a$out,breaks=c(mn, a$stats[1], mx), labels=c(a$stats[1], a$stats[5]))
    #converting replacement_value to non-factor - from factor to character
    replacement_value=as.character(replacement_value)
    replacement_value=as.numeric(replacement_value)
    out=data.frame(a$out,replacement_value)
    
    #imputing outliars with min or max values
    out$maindataset_row_numbers=which(telecom[,i]%in% out$a.out )
    rn=out$maindataset_row_numbers
    telecom[rn,i]=out$replacement_value
  }
}
rm(out)
summary(telecom)


#Analyse factor type variables and finding level-wise churn rate
factors_all_lvls=data.frame()#will comprise of level-wise churn rate
factors_3lvls=data.frame()#will comprise of churn-rate when levels are clubbed to 3 levels
decoding_factors=data.frame()#will comprise of decoding of clubbed levels (to new imputed levels)
Levels0ChurnRate=data.frame()#will comprise of factor-levels with churn rate=0. We will impute these levels with 0.

for(i in 2:ncol(telecom_factor))#1st column is dependent variable churn
  #we have to do analysis of independent variables (col no. 2 onwards)
{
  telecom[,i]=as.factor(telecom[,i])
  telecom%>%count(churn,levels=telecom[,i])%>%filter(churn==1)->dat1
  #dat1 will comprise of levels with churn=1
  telecom%>%filter(telecom[,i]%in%dat1$levels)->datC2
  rn=which(telecom[,i]%in%dat1$levels)
  dat1$N=unclass(unname(datC2%>%group_by(datC2[,i])%>%summarize(n())))[[2]]
  dat1$Churn_Rate=round(dat1$n/dat1$N,3)
  dat1$Total_Levels=rep(length(dat1$levels), length(dat1$levels))
  dat1$churn=NULL
  dat1$Col_Num=i
  dat1$Var_Name<-rep(names(telecom)[i],nrow(dat1))
  
  #Missing values imputation for factor variables:
  if(is.na(dat1$levels[nrow(dat1)]))
  {
    dat1$Total_Levels=dat1$Total_Levels-1
    diff=1:(nrow(dat1)-1)
    for(j in 1:(nrow(dat1)-1))
    {
      diff[j]=dat1$Churn_Rate[nrow(dat1)]-dat1$Churn_Rate[j]
    }
    a=(min(Mod(diff)))
    b=sum(diff%in%a)#if a exists in diff then b=1 and if -a exsits in diff then b=0
    row_n=ifelse((b!=0), which(diff%in%a), which(diff%in%(-1*a)))
    p=dat1[row_n,"levels"]#so we will impute the value mentioned in the col level of dat1 dataframe 
    telecom[is.na(telecom[,i]),i]=p
    dat1=dat1[-nrow(dat1),] #we need not include the NA values for our churn analysis
  }
  factors_all_lvls=rbind(factors_all_lvls, dat1) 
  
  #Identify the factor variables that have levels > 3, and clubbing the levels to 3
  if(dat1$Total_Levels[1]>3)
  {
    dat1%>%mutate(levels=ntile(Churn_Rate,3))%>%group_by(levels)%>%summarize(n=sum(n), N=sum(N))->dat2
    dat2$Churn_Rate=dat2$n/dat2$N
    dat2$Total_Levels=3
    dat2$Col_Num=i
    dat2$Var_Name<-names(telecom)[i]
    dat1%>%mutate(imputed_lvl=ntile(Churn_Rate,3))%>%count(imputed_lvl, levels)->dat4
    dat4$Var_Name=names(telecom)[i]
    dat4$Col_Num=i
    dat1=dat2
    decoding_factors=rbind(decoding_factors, dat4)
  }  
  factors_3lvls=rbind(factors_3lvls, dat1)#factors_3lvls dataset after clubbing the levels to 3
#  print(i)
  
  if(length(telecom[-rn,i])==0) #if there are no rows with 0 churn-rate then we skip the further code in for-loop
    next
  
  #if there are rows which have been skipped from above analysis of telecom[,i], these are the rows with churn-rate=0
  #we will append these rows to Level0ChurnRate and eventually impute these levels by level0
  levels=unique(telecom[-rn,i])#to get the levels with 0 churn-rate
  imputed_lvl=rep(0, length(levels))
  datC1=data.frame(imputed_lvl=imputed_lvl, levels=levels)
  datC1$Var_Name=dat1$Var_Name[1]
  datC1$Col_Num=dat1$Col_Num[1]
  Levels0ChurnRate=rbind(Levels0ChurnRate, datC1)#The levels with 0 churn rate are included in df Level0ChurnRate
#We will eventually club the Levels0ChurnRate DataFrame with dataframe level_decoding
#All the levels in Levels0ChurnRate will be coded to Level 0 in telecom database
}
rm(dat1)
rm(dat4)
rm(dat2)
rm(datC2)
rm(datC1)
rm(p)

write.csv(factors_all_lvls,"factors_all_lvls.csv")#comprises of churn-rate for all levels
write.csv(factors_3lvls,"factors_3lvls.csv")
#analysing factor dataframe, there should be a reasonable difference (atleast 2%) in the even

#further reducing the levels of factor type variables based on event-rate. 
#club together the factors if event rate difference is small (chosing 2% difference)
#making correponding entry in decoding_factors dataset as well
factors_final_clubbed=data.frame()
c=1
factors_3lvls=factors_3lvls[order(factors_3lvls$Col_Num),]
for(i in 1:nrow(factors_3lvls))
{
  if(factors_3lvls$Col_Num[i]==c) {next}
  c=factors_3lvls$Col_Num[i]
  
  dat1=factors_3lvls[factors_3lvls$Col_Num==c,]
  num_rows=nrow(dat1)
  if(num_rows==3)
  {
    x=dat1$Churn_Rate[1]-dat1$Churn_Rate[2]
    y=dat1$Churn_Rate[2]-dat1$Churn_Rate[3]
    z=dat1$Churn_Rate[1]-dat1$Churn_Rate[3]
    
    if((x)^2<.0004 & y^2<.0004)
    {
      dat1$n[1]=dat1$n[2]+dat1$n[3]+dat1$n[1]
      dat1$N[1]=dat1$N[2]+dat1$N[3]+dat1$N[1]
      del=c(2,3)#row number to be deleted
      dat1=dat1[-del,]
      dat1$Total_Levels=1
      decoding_factors$imputed_lvl[decoding_factors$Col_Num==c & decoding_factors$imputed_lvl==2]=dat1$levels[1]
      decoding_factors$imputed_lvl[decoding_factors$Col_Num==c & decoding_factors$imputed_lvl==3]=dat1$levels[1]
      
      #if there is no-existing entry in the combined-factor db, then we can make a new entry
      #as this dataset will be useful for substitution in the main 'telecom' dataset
      
    #if level reduction is happening for the first time, then we need make fresh entry in decoding_factors
      if(sum(decoding_factors$Col_Num %in% c)==0)
        {
        dat=data.frame(imputed_lvl=dat1$levels[1], levels=levels(telecom[,c])[c(2,3)], n=1, Col_Num=c)
        decoding_factors=rbind(decoding_factors, dat)
        }
    }
    else if(x^2<.0004)
    {
      dat1$n[1]=dat1$n[1]+dat1$n[2]
      dat1$N[1]=dat1$N[1]+dat1$N[2]
      del=2#row number to be deleted
      dat1=dat1[-del,]
      dat1$Total_Levels=2
      decoding_factors$imputed_lvl[decoding_factors$Col_Num==c & decoding_factors$imputed_lvl==2]=dat1$levels[1]
      if(sum(decoding_factors$Col_Num %in% c)==0)
      {
        dat=data.frame(imputed_lvl=dat1$levels[1], levels=levels(telecom[,c])[2], n=1, Col_Num=c)
        decoding_factors=rbind(decoding_factors, dat)
      }
    }
    else if(y^2<.0004)
    {
      dat1$n[2]=dat1$n[2]+dat1$n[3]
      dat1$N[2]=dat1$N[2]+dat1$N[3]
      del=3#row number to be deleted
      dat1=dat1[-del,]
      dat1$Total_Levels=2
      decoding_factors$imputed_lvl[decoding_factors$Col_Num==c & decoding_factors$imputed_lvl==3]=dat1$levels[2]
      if(sum(decoding_factors$Col_Num %in% c)==0)
      {
        dat=data.frame(imputed_lvl=dat1$levels[2], levels=levels(telecom[,c])[3], n=1, Col_Num=c)
        decoding_factors=rbind(decoding_factors, dat)
      }
    }  
  }
  if((num_rows==2))
  {
    x=dat1$Churn_Rate[1]-dat1$Churn_Rate[2]
    if(x^2<.0004)
    {
      dat1$n[1]=dat1$n[1]+dat1$n[2]
      dat1$N[1]=dat1$N[1]+dat1$N[2]
      del=2#row number to be deleted
      dat1=dat1[-del,]
      dat1$Total_Levels=1
      decoding_factors$imputed_lvl[decoding_factors$Col_Num==c & decoding_factors$imputed_lvl==2]=dat1$levels[1]
      if(sum(decoding_factors$Col_Num %in% c)==0)
      {
        dat=data.frame(imputed_lvl=dat1$levels[1], levels=levels(telecom[,c])[2], n=1, Var_Name = dat1$Var_Name, Col_Num=c)
        decoding_factors=rbind(decoding_factors, dat)
      }
    }
  }
  dat1$Churn_Rate=dat1$n/dat1$N
  factors_final_clubbed=rbind(factors_final_clubbed,dat1)
}        

rm(dat1)

#For the levels with  churn-rate = 0 (i.e not even 1 entry in the level has churn =1)
#we will impute those values by 0 - and accordingly assign in decoding_factors
#i.e we'll append Levels0ChurnRate to decoding_factors
decoding_factors$n=NULL
decoding_factors=rbind(decoding_factors, Levels0ChurnRate)
#So, dataframe: decoding_factors contains the decoding of all the clubbed levels to original levels

#Imputing the values in the main telecom data-set as per the decoding_factors
for(i in 1:nrow(decoding_factors))
{
  rn=which(telecom[, decoding_factors$Col_Num[i]] %in% decoding_factors$levels[i])
  telecom[,decoding_factors$Col_Num[i]]=as.character(telecom[,decoding_factors$Col_Num[i]])
  telecom[rn,decoding_factors$Col_Num[i]]=decoding_factors$imputed_lvl[i]
}
colSums(is.na(telecom)) #confirms no missing values


write.csv(decoding_factors, "decoding_factors.csv")
write.csv(factors_final_clubbed,"factors_final_clubbed.csv")


#converting all the factor-type columns into factor class back
for (i in 1:ncol(telecom_factor))
{
  telecom[,i]=as.factor(telecom[,i])
}
summary(telecom[,(1:ncol(telecom_factor))])

telecom_factor=telecom[,c(1:ncol(telecom_factor))]
dat=data.frame()

#if number of levels = 1, then we'll not include the variable for modeling
for(i in 1:ncol(telecom_factor))
{
  if(length((levels(telecom_factor[,i])))==1)
  {
    dat=rbind(dat,i)
  }
}
names(dat)="col_num"
telecom=telecom[,-dat$col_num]
telecom_factor=telecom_factor[,-dat$col_num]
summary(telecom[,1:ncol(telecom_factor)])
#note: the decoding of the levels is given in decoding_factors


#Creating New Transformation Variables

#Percentage of completed voice calls can be given by following formula:
#Comp_Ptage_VCE=comp_vce_mean/plcd_vce_Mean
telecom$Comp_Ptage_VCE=ifelse(telecom$plcd_vce_Mean!=0,telecom$comp_vce_Mean/telecom$plcd_vce_Mean,0)
telecom$Comp_Ptage_VCE=round(telecom$Comp_Ptage_VCE,2)
telecom%>%mutate(Lvl=ntile(telecom$Comp_Ptage_VCE,10))%>%count(Lvl,churn)%>%filter(churn==1)->dat
dat$ChurnVariable="Comp_Ptage_VCE"
dat$Total=unclass(telecom%>%mutate(Lvl=ntile(telecom$Comp_Ptage_VCE,10))%>%count(Lvl)%>%unname())[[2]]
dat$Churn_Rate=dat$n/dat$Total
dat$Col_Num=ncol(telecom)
dat$DataType="numeric"
dat$Max_Lvl=unclass(telecom%>%mutate(Lvl=ntile(Comp_Ptage_VCE,10))%>%group_by(Lvl)%>%summarize(max(Comp_Ptage_VCE))%>%unname())[[2]]
dat$Max_Lvl=unclass(telecom%>%mutate(Lvl=ntile(telecom$Comp_Ptage_VCE,10))%>%group_by(Lvl)%>%summarize(max(Comp_Ptage_VCE))%>%unname())[[2]]
dat$Min_Lvl=unclass(telecom%>%mutate(Lvl=ntile(telecom$Comp_Ptage_VCE,10))%>%group_by(Lvl)%>%summarize(min(Comp_Ptage_VCE))%>%unname())[[2]]
dat$Mean_Lvl=unclass(telecom%>%mutate(Lvl=ntile(telecom$Comp_Ptage_VCE,10))%>%group_by(Lvl)%>%summarize(mean(Comp_Ptage_VCE))%>%unname())[[2]]
dat$churn=NULL
dat$levels=10
names(dat)=names(Quant_DA)
Quant_DA=rbind(dat, Quant_DA)

a=cbind(telecom$comp_vce_Mean, telecom$Comp_Ptage_VCE, telecom$plcd_vce_Mean)
cor(a)
pairs(a)
#Since cor is not very high, we will keep these columns


plot(x=1:10, y=dat$Churn_Rate[1:10], type="l", col="red", xlab="Completed Percentage of Voice Calls", ylab="Churn Rate")
#We observe clear decreasing trend (i.e inverse relationship)
#As we increase the percentage of completed voice calls, the churn rate will decrease


#Percentage of dropped voice calls can be given by following formula:
#drop_vce_Mean/plcd_vce_Mean
telecom$drop_Ptage_VCE=ifelse(telecom$plcd_vce_Mean!=0, telecom$drop_vce_Mean/telecom$plcd_vce_Mean,NA)
telecom$drop_Ptage_VCE=round(telecom$drop_Ptage_VCE,2)
obj=telecom[is.na(telecom$drop_Ptage_VCE)!=1,]
obj%>%mutate(Lvl=ntile(obj$drop_Ptage_VCE,4))%>%count(Lvl,churn)%>%filter(churn==1)->dat
dat$ChurnVariable="drop_Ptage_VCE"
dat$Total=unclass(obj%>%mutate(Lvl=ntile(obj$drop_Ptage_VCE,4))%>%count(Lvl)%>%unname())[[2]]
dat$Churn_Rate=dat$n/dat$Total
dat$Col_Num=ncol(obj)
dat$DataType="numeric"
dat$Max_Lvl=unclass(obj%>%mutate(Lvl=ntile(drop_Ptage_VCE,4))%>%group_by(Lvl)%>%summarize(max(drop_Ptage_VCE))%>%unname())[[2]]
dat$Max_Lvl=unclass(obj%>%mutate(Lvl=ntile(obj$drop_Ptage_VCE,4))%>%group_by(Lvl)%>%summarize(max(drop_Ptage_VCE))%>%unname())[[2]]
dat$Min_Lvl=unclass(obj%>%mutate(Lvl=ntile(obj$drop_Ptage_VCE,4))%>%group_by(Lvl)%>%summarize(min(drop_Ptage_VCE))%>%unname())[[2]]
dat$Mean_Lvl=unclass(obj%>%mutate(Lvl=ntile(obj$drop_Ptage_VCE,4))%>%group_by(Lvl)%>%summarize(mean(drop_Ptage_VCE))%>%unname())[[2]]
dat$churn=NULL
dat$levels=4
names(dat)=names(Quant_DA)
plot(x=1:4, y=dat$Churn_Rate[1:4], type="l", col="red", xlab="Percentage of Failed Voice Calls", ylab="Churn Rate")
#As the percentage of failed calls increases, the churn rate decreases. 
#     This is counter-intuitive, hence we drop this variable
#telecom$drop_Ptage_VCE=NULL

#imputing the na values by median values
telecom$drop_Ptage_VCE[which(is.na(telecom$drop_Ptage_VCE))]=median(telecom$drop_Ptage_VCE,na.rm = TRUE)



#Making a new variable MRC_ASL

#Divide the totMRC_Mean into 2 categories Low and High Cost
telecom%>%mutate(quantile=ntile(totmrc_Mean,2))%>%filter(churn==1)%>%count(quantile)->dat
dat$total=unclass(telecom%>%mutate(quantile=ntile(totmrc_Mean,2))%>%count(quantile)%>%unname())[[2]]
dat$churn_rate=dat$n/dat$total
dat$max=unclass(telecom%>%mutate(quantile=ntile(totmrc_Mean,2))%>%group_by(quantile)%>%summarize(max(totmrc_Mean))%>%unname())[[2]]
dat$min=unclass(telecom%>%mutate(quantile=ntile(totmrc_Mean,2))%>%group_by(quantile)%>%summarize(min(totmrc_Mean))%>%unname())[[2]]
#So we notice that churn rate reduces with high cost voice plans

#Making a new variable MRC_ASL
telecom$MRC_ASL=ifelse(telecom$asl_flag=="N"& telecom$totmrc_Mean<=44.9, "MRCL_ASLN", ifelse(telecom$asl_flag=="Y"&telecom$totmrc_Mean<=44.9, "MRCL_ASLY", ifelse(telecom$asl_flag=="Y"&telecom$totmrc_Mean>44.99, "MRCH_ASLY", "MRCH_ASLN")))
telecom$MRC_ASL=as.factor(telecom$MRC_ASL)

#finding churn rate of MRC_ASL as per the levels
telecom%>%count(MRC_ASL, churn)%>%filter(churn==1)->dat
dat$total=unclass(telecom%>%count(MRC_ASL)%>%unname())[[2]]
dat$churn_rate=dat$n/dat$total
#So highest churn where cost of calling plan is low and ASL flag is No
#Lowest churn is base cost of calling plan is high and ASL flag is Yes
rm(dat)
rm(obj)


#Creating dummies of certain quatitative variables based on churn rate in dataframe "Quant_DA"

telecom$owylis_vce_Range=ifelse(telecom$owylis_vce_Range<=1,"Low","High")
telecom$owylis_vce_Range=as.factor(telecom$owylis_vce_Range)

telecom$mou_opkv_Range=ifelse(telecom$mou_opkv_Range<=1.01,"Low","High")
telecom$mou_opkv_Range=as.factor(telecom$mou_opkv_Range)

telecom$iwylis_vce_Mean=ifelse(telecom$iwylis_vce_Mean<=.33,"Low","High")
telecom$iwylis_vce_Mean=as.factor(telecom$iwylis_vce_Mean)

telecom$ovrrev_Mean=ifelse(telecom$ovrrev_Mean<=.96,"Low","High")
telecom$ovrrev_Mean=as.factor(telecom$ovrrev_Mean)

telecom$comp_vce_Mean=ifelse(telecom$comp_vce_Mean<=4, "Low", ifelse(telecom$comp_vce_Mean<=22, "Med", ifelse(telecom$comp_vce_Mean<=177.67,"High", "V_High")))
telecom$comp_vce_Mean=as.factor(telecom$comp_vce_Mean)

telecom$plcd_vce_Mean=ifelse(telecom$plcd_vce_Mean<=5.67, 1, ifelse(telecom$plcd_vce_Mean<=29.33, 2,3))
telecom$plcd_vce_Mean=as.factor(telecom$plcd_vce_Mean)

telecom$avg6mou=ifelse(telecom$avg6mou<=137, "Low", ifelse(telecom$avg6mou<=896,"Med", "High"))
telecom$avg6mou=as.factor(telecom$avg6mou)

telecom$avg3qty=ifelse(telecom$avg3qty<=21,"Low", ifelse(telecom$avg3qty<=283, "Med", "High"))
telecom$avg3qty=as.factor(telecom$avg3qty)

telecom$avgqty=ifelse(telecom$avgqty<=373.83,"Low","High")
telecom$avgqty=as.factor(telecom$avgqty)


#We observe a counter-intuitive trend in drop_vce_Range, 
#           so we drop this variable from our modelling
telecom$drop_vce_Range=NULL

telecom$callwait_Mean=ifelse(telecom$callwait_Mean<=.33,"Low", "High")
telecom$callwait_Mean=as.factor(telecom$callwait_Mean)

telecom$ovrmou_Mean=ifelse(telecom$ovrmou_Mean<=2.75, "Low", "High")
telecom$ovrmou_Mean=as.factor(telecom$ovrmou_Mean)


#We observe a counter-intuitive trend in adjmou. 
#     As adjmou increases, churn should decrease. Hence we'll not include this variable for modelling
telecom$adjmou=NULL
#Also excluding the categorical variables which don't seem to impact churn behaviour
telecom$models=NULL
telecom$area=NULL
#Since drop_blk_mean is also showing counter-intuitive trend, so we drop it from our analysis
telecom$drop_blk_Mean=NULL


#Dividing data into training and validation sets

set.seed(100)
row_n=sample(1:nrow(telecom), .7*nrow(telecom), FALSE)
training=telecom[row_n,]
test=telecom[row_n,]

a=grep("Customer_ID", names(training))

#Applying logistic regression model on data:
myresult=glm(churn~., data=training[,-a], family=binomial)
summary(myresult)



#Creating more dummies
telecom$MRCH_ASLY=ifelse(telecom$MRC_ASL=="MRCH_ASLY", 1,0)
telecom$MRCL_ASLN=ifelse(telecom$MRC_ASL=="MRCL_ASLN", 1,0)
telecom$MRCL_ASLY=ifelse(telecom$MRC_ASL=="MRCL_ASLY", 1,0)
telecom$MRC_ASL=NULL
telecom$aslY_MRC=NULL

telecom$comp_vce_Mean_Low=ifelse(telecom$comp_vce_Mean=="Low",1,0)
telecom$comp_vce_Mean_Med=ifelse(telecom$comp_vce_Mean=="Med",1,0)
telecom$comp_vce_Mean_VHigh=ifelse(telecom$comp_vce_Mean=="V_High",1,0)
telecom$comp_vce_Mean=NULL

telecom$avg3qty_Low=ifelse(telecom$avg3qty=="Low",1,0)
telecom$avg3qty_Med=ifelse(telecom$avg3qty=="Med",1,0)
telecom$avg3qty=NULL

telecom$hnd_webcap_WC=ifelse(telecom$hnd_webcap=="WC",1,0)
telecom$hnd_webcap_WCMB=ifelse(telecom$hnd_webcap=="WCMB", 1,0)
telecom$hnd_webcap=NULL


telecom$adjrev=cut(telecom$adjrev, breaks= c(0,282.45,389.1,490.39, 605.8,28000), labels = c(1,2,3,4,5))
head(telecom$adjrev)

#Dividing data into training and validation sets
set.seed(100)
row_n=sample(1:nrow(telecom), .7*nrow(telecom), FALSE)
#training=telecom[row_n,]
#test=telecom[row_n,]
training=telecom
test=telecom
a=grep("Customer_ID", names(training))

#Applying logistic regression model on data:
myresult=glm(churn~., data=training[,-a], family=binomial)
summary(myresult)

step(myresult, direction="forward")
myresult_1= glm(formula = churn ~ asl_flag + ethnic + mou_Mean + totmrc_Mean + mou_Range + 
                  change_mou + owylis_vce_Range + mou_opkv_Range + months + 
                  totcalls + eqpdays + custcare_Mean + callwait_Mean + iwylis_vce_Mean + 
                  callwait_Range + ccrndmou_Range + adjqty + ovrrev_Mean + 
                  rev_Mean + ovrmou_Mean + plcd_vce_Mean + avg3mou + avgmou + 
                  avgqty + avg6mou + avg6qty + age1 + hnd_price + retdays + 
                  roam_Mean + drop_vce_Mean + totrev + adjrev + avgrev + Comp_Ptage_VCE + 
                  drop_Ptage_VCE + MRCH_ASLY + MRCL_ASLN + MRCL_ASLY + comp_vce_Mean_Low + 
                  comp_vce_Mean_Med + comp_vce_Mean_VHigh + avg3qty_Low + avg3qty_Med + 
                  hnd_webcap_WC + hnd_webcap_WCMB, family = binomial, data = training[,-a])
summary(myresult_1)

step(myresult_1, direction="backward")

myresult_2=glm(formula = churn ~ asl_flag + ethnic + mou_Mean + totmrc_Mean + 
      mou_Range + change_mou + owylis_vce_Range + mou_opkv_Range + 
      months + totcalls + eqpdays + custcare_Mean + iwylis_vce_Mean + 
      adjqty + rev_Mean + ovrmou_Mean + plcd_vce_Mean + avgmou + 
      avgqty + avg6mou + age1 + hnd_price + retdays + drop_vce_Mean + 
      totrev + adjrev + Comp_Ptage_VCE + drop_Ptage_VCE + MRCL_ASLN + 
      MRCL_ASLY + comp_vce_Mean_Low + avg3qty_Low, family = binomial, 
    data = training[, -a])
summary(myresult_2)

mod=glm(formula = churn ~ asl_flag + ethnic + mou_Mean + totmrc_Mean + 
      mou_Range + change_mou + owylis_vce_Range + totcalls+
      months + eqpdays + custcare_Mean + iwylis_vce_Mean + 
      adjqty + ovrrev_Mean  + avgqty + age1 + 
      hnd_price + retdays + drop_vce_Mean + 
      Comp_Ptage_VCE + drop_Ptage_VCE + MRCL_ASLN +
      avg3qty_Low + avgrev, family = binomial, data = training[, -a])
summary(mod)


#We remove avgmou and comp_vce_Mean - as they are giving opposite signs in univariate and multivariate settings

#asl_flagY - Account spending limit - inversely proportional
#   i.e ASL is Y - prob. of churn is low
#   i.e where-ever account spending limit is set, churn rate is low
#ethnic3 - direct relationship
#   i.e people of enthnicity 3 have higher probability of churn
#   According to level decoding given in decoding_factors, 
#   level-3 represents B (Asian - Non Oriental), D(South European), I(Italian), J(Jewish), O (Asian)
#mou_Mean - Mean of minutes of usage - inversely proportional
#   as  MOU increase, churn rate decreases
#totmrc_Mean - mean total monthly recurring charges - inversely proportional
#   as Monthly Recurring Charges increase, churn decreases
#   Monthly Recurring Charge is the base cost of the calling plan regardless of actual minutes used.
#   So people who have taken higher MRC calling plans have lower probability of churn
#mou_Range - directly proportional
#   as the range of MOU increase, churn rate increase
#change_mou - %age change in monthly mou - inversely proportional
#   churn rate decreases as %age change in monthly mou increases
#months - number of months in service - inversely proportional
#   churn rate decreases as number of months in service increases
#eqpdays - age of current equipment - directly proportional
#custcare_Mean - inversely proportional 
#   so more and bigger calls made to customer care, means less churn rate
#iwylis_vce_meanLow - Directly proportional - indicates if wireless to wireless calls are low, churn is high
#adjqty - Billing adjusted total number of calls over the life of the customer
#     inversely proportional - As the billing adjusted total number of calls increases, churn increases
#avgqtyLow - Positive correlation - As the average quantity is low, churn rate is high. 
#   As average quantity increases, churn rate decreases 
#avg6qty - inversely proportional, As the average number of calls in the past 6 months increases, churn decreases
#age1 - inverse relationship - as age increases churn decreases
#hnd_price - as the handset price increases, churn decreases. 
#   High handset price may lead to better quality hand-set, and hence more comfortable calls, hence reduced churn 
#   High handset price affordabilty, might also indicate, more affluent customers have reduced churn
#retdays - Number of days since last retention call-positive correlation - More number of days have passed since last retention call, higher the churn
#drop_vce_Mean - Positive correlation - As the dropped calls increase, churn increases
#ovrrev_Mean - When mean of over-head revenue is low, churn rate is low. 
#   Churn rate increases as overhead revenue increases
#Comp_Ptage_VCE - Negative Correlation - As the percentage of completed voice calls increases, churn decreases
#drop_Ptage_VCE - Positive correlation - As the pecentage of dropped voice calls increases, churn increases
#MRCL_ASLN - When monthly recurring charges are low, and ASL flag is set to 0, churn rate is high
# So, we have to migrate these customers to better (and higher MRC calling plan, and ASL flag set to 1)
#Avg3qty - If average quantity of calls is low in last 3 months, the churn rate is high
#Avgrev - As average revenue from a customer increases, the churn rate increases. # This comprises of charge of voice, data, roaming, over-head revenue etc.
#   So, if a customer is spending more, he has higher probability of churn.

1-pchisq(51068-49345, 46406-46382)#=0

#The above p-value (=0) is exceedingly small, so we can reject the null hypothesis 
#   that the deviance of the model with only constant term and the deviance of the model
#   with independent terms is exactly the same

#Final logistic regression equation:
#log(p/(1-p))=a=-(7.808e-01) -(4.076e-01)*asl_flag +(2.062e-01)*ethnic3 -(6.904e-04)*mou_Mean -(3.802e-03)*totmrc_Mean + 
#   +(6.197e-04)*mou_Range + (6.197e-04)*change_mou - (1.192e-01)*owylis_vce_Range + 
#   -(2.241e-02)*months + 5.875e-04*totcalls - (2.241e-02)*months + (8.8e^-4)*eqpdays - (3.69ed^-2)*custcare_Mean + (9.447e-02)*iwylis_vce_Mean + 
#   -(5.458e^-4)*adjqty - (1.712e-01)*ovrrev_MeanLow  + .144*avgqtyLow - (5.062e-03)*age1 + avg6qty + 
#   - (2.008e-03)*hnd_price + (9.988e-04)*retdays + (1.406e-02)*drop_vce_Mean  + 
#   -.4846*Comp_Ptage_VCE + 1.036*drop_Ptage_VCE + .12*MRCL_ASLN +
#   +.29*avg3qty_Low + (4.136e-03)*avgrev

#p=exp(a)/(1+exp(a))


#Finding confidence interval for the model:
confint(mod)
#Confidence Interval is narrow for all the variables 
#   - indicates that every time we reproduce the modelling process (on different samples) 
#     the MLE coefficients would vary within the confidence interval range. 

#Predict the probability using the above model in training data
train_pred=mod$fitted.values
head(train_pred)

#Predict the probability using the above model in test data
test_pred=predict(mod, type="response", newdata=test)
head(test_pred)

table(telecom$churn)/nrow(telecom)
#average rate = .239 

training$predicted = ifelse(train_pred>=.239,1,0)
nrow(training)
test$predicted = ifelse(test_pred>=.239,1,0)


#Confusion Matrix to test model efficacy
library(caret)
confusionMatrix(training$predicted, training$churn, positive = "1")
confusionMatrix(test$predicted, test$churn, positive = "1")
mean(training$predicted!=training$churn)#mean error = 41%
mean(test$predicted!=test$churn)#Mean error = 41%


#Kappa Metric to test model efficacy
library(irr)
kappa2(data.frame(training$churn, training$predicted))
kappa2(data.frame(test$churn, test$predicted))


#Lift curve to test model efficacy
library(ROCR)
prediction=prediction(test_pred, test$churn)
perf=performance(prediction, "tpr", "fpr")
plot(perf, col="red", xlab="FALSE POSITIVE RATE", ylab="TRUE POSITIVE RATE")
abline(0,1)
#the curve shows a fairly accurate model. 

#to calculate area under the curve
auc=performance(prediction, "auc")
auc
p=unlist(auc@y.values)
p #Value = 62.7% 

#Concordance and discordance ratio measurement on test data-set
dat=data.frame(test_pred, test$churn)
head(dat)
#seperating the prob. values where actual values = 1

ones=dat[dat$test.churn==1,]
zeroes=dat[dat$test.churn==0,]

pairs_tested = 0
concor=0
discor=0
ties=0

for(i in 1:nrow(ones))
{
  for(j in 1:nrow(zeroes))
  {
    pairs_tested=pairs_tested+1
    ifelse((ones[i,1]>zeroes[j,1]), (concor=concor+1), 
           (ifelse((ones[i,1]<zeroes[j,1]), (discor=discor+1),(ties=ties+1))))
  }
}

concor
discor
ties
pairs_tested

concordance_ratio=concor/pairs_tested #=.624
discordance_ratio=discor/pairs_tested#=.376
tie_ratio = ties/pairs_tested#=0

#concordance ratio of .624 indicates a fair model



#Questions

#Q1. Top 5 factors driving churn at Mobicom

#1. Minutes of Use - Higher the minutes of use, lower the churn. Following significant variables in the equation indicate the same:
  #mou_Mean - Mean of minutes of usage - inversely proportional
  #   as  MOU increase, churn rate decreases
  #mou_Range - directly proportional
  #   as the range of MOU increase, churn rate increase
  #change_mou - %age change in monthly mou - inversely proportional
  #   churn rate decreases as %age change in monthly mou increases

#2. Better the network quality (for voice), lower the churn. Following variables indicate the same
  #Comp_Ptage_VCE - Negative Correlation - As the percentage of completed voice calls increases, churn decreases
  #drop_Ptage_VCE - Positive correlation - As the pecentage of dropped voice calls increases, churn increases
  #drop_vce_Mean - Positive correlation - As the dropped calls increase, churn increases
#3. More the number of calls made by the customer, lower the churn. The customers who are making less calls,
  #are higher prone to churn. Following variables confirm to the same.
  #adjqty - Billing adjusted total number of calls over the life of the customer
  #inversely proportional - As the billing adjusted total number of calls increases, churn increases
  #Avg3qty - If average quantity of calls is low in last 3 months, the churn rate is high
  #avg6qty - inversely proportional, As the average number of calls in the past 6 months increases, churn decreases
  #avgqtyLow - Positive correlation - As the average quantity is low, churn rate is high. 
  #   As average quantity increases, churn rate decreases 
#4. As the customer care mean increases, the churn rate decreases. This indicates that the customers
#   who report and discuss their grievances with customer care are less likely to churn compared to those who don't 
#5. Calling plan: 
#   If the calling plan has ASL Flag set to yes, the churn rate is low
#   If monthly recurring charges are low and ASL flag is set to No, then churn rate is high
#   If Monthly recurring charges are high (base calling plan is of high value), then churn rate is low. 
#6. As the age of the first house-hold member increases, the churn rate decreases.
#   So higher age people are better target group
#   Also the customers of ethnicity B (Asian - Non Oriental), D(South European), 
#   I(Italian), J(Jewish), O (Asian) are less likely to churn.


#Q2. Validation of survey findings. 
#a) Whether "cost and billing" and "network and service quality" are 
#   important factors influencing churn behaviour.  
#   Cost and billing: 
#     We observe the behavior of variable Total MRC (base cost of the calling plan)
#     Higher cost calling plans are yielding lower churn, may be due to additional benefits
#     Hence, we can't conclude that higher cost is increasing churn, rather, 
#     higher base cost calling plans providing benefit to customer are helping reduce churn

#     plot trend based on totmrc
dat=Quant_DA[Quant_DA$ChurnVariable=="totmrc_Mean",]
plot(x=1:10, y=dat$Churn_Rate[1:10], type="l", col="red", xlab="Total Monthly Recurring Charges", ylab="Churn_Rate")
#     So this graph shows an that as the Monthly recurring charges increase, churn decreases
#     So, higher cost of calls doesn't have an influence on churn behavior
#     Rather high MRC calling plans are leading to reduced churns because of may-be "other" benefits

#However, high over-head revenue and average revenue in a month from a customer 
#     is leading to high probability of  churn. So, we have to keep a tab on 
#     overall costs (which includes voice, data, roaming, sms etc). 


#   Network and Service Quality
#Network Quality
# Better the network quality (for voice), lower the churn. Following variables indicate the same
#comp_vce_Mean_Low - When mean number of completed voice calls is low 
#  - churn is low. As mean number of completed voice calls increase, churn rate decreases
#Comp_Ptage_VCE - Negative Correlation - As the percentage of completed voice calls increases, churn decreases
#drop_Ptage_VCE - Positive correlation - As the pecentage of dropped voice calls increases, churn increases
#drop_vce_Mean - Positive correlation - As the dropped calls increase, churn increases

#Service Quality - Better service leads to lower churn
#cust_care_Mean - As the customer care mean increases, the churn rate decreases. This indicates that the customers
#   who report and discuss their grievances with customer care are less likely to churn compared to those who don't 


#b) Are data usage connectivity issues turning out to be costly?  
#   In other words, is it leading to churn?
#Ans. Since none of the data variables are coming significant in our modelling equation, 
#   we can't conclude with certainty that the data issues are turning costly

#Q3.Would you recommend rate plan migration as a proactive retention strategy?
#Variable totmrc_mean (significant as per the regression model) is inversely proportional to churn
dat=Quant_DA[Quant_DA$ChurnVariable=="Totmrc_Mean",]
plot(x=1:10, y=Quant_DA$Churn_Rate[1:10], type="l", col="red", xlab="Totmrc_Mean", ylab="Churn_Rate")
# We observe inverse trend between Total MRC and churn rate
# The cost of calling plan reduces, the churn rate increases, 
#      so, high MRC calling plans are yielding low churn, may be due to some extra benefits / higher duration benefits
#      So we will recommend rate plan migration (of customers having low MRC)

#variable asl_flag (significant as per the regression model)
#     We observe that the churn is low when ASL flag is set to yes
#     So, we will recommend rate plan migration (of customers having ASL Flag "No")

#Variable MRCL_ASLN is coming significant as per the regression model, directly proportional to churn
#     Customers having Monthly recurring cost low and ASL flag set to No, will have very high probability of churn
#     These customers need to focussed for rate plan migration

#variable ovrrev_Mean
#As overrev_Mean (Mean overhead revenue) increases churn rate increases
#Hence we need to plan their rate-plan migration 

#variable retdays (number of days since the last retention call was made) is signficant in the model
#   As the retdays increases, the churn increases
#   Hence, we should be making more frequent retention calls 
#   providing plan upgrade option


#Q4.What would be your recommendation on how to use this churn model for 
#   prioritisation of customers for a proactive retention campaigns in the future?

#Ans. Based on the data analysis, we have identified the churn drivers. 
# We will identify customers with high probability of churn (>.5) and 
#     we will target them for the retention campaigns. 
# Based on the churn drivers following strategies will be recommended for proactive retention campaigns
#     increase the MOU of the customers - increased retention calls providing bundling offers: data / sms etc. which will lead to increased minutes of use
# Rate plan migration - helping the customers migrate from less attractive rate plans to better rate plans
#     customers with high over-head revenue need to be targeted for rate plan migration
# Increased target on higher age customers while customer acquisition (better plans based on age), as they have lower probability of churn.
# Increased target on customers of ethnicity 3 while customer acquisition as they have lower probability of churn.
# We have to motivate customers to voice their grievances to customer care, so that customer care can help them resolve the queries.
#     The customers who don't seek solution of their grievances from the customer-care, have a higher tendency to churn. 
#     May be in our retention calls, we can ask customers to share if they have any grievances / discomfort with the telecom provider


#Following are the customers in the test data who have high probability of churn and need to be targetted:
customers_high_churn=test$Customer_ID[test$predicted=1]
#We need to target above customer ids for our retention campaigns. 
#Specifically we'll target the customers with high prob of churn and high average revenue

#To make a table of high revenue and high churn probability customers in the validation dataset

#Make a table of customers churning (Low, Medium, High) with revenue in the validation data-set
test$probability_churn=test_pred
test%>%mutate(Prob_level=ntile(test$probability_churn,3))->test
test%>%group_by(Prob_level)%>%summarize(Max_Prob=max(probability_churn), Min_Prob=min(probability_churn))
test$Prob_level=cut(test$Prob_level, breaks=c(0,1,2,3), labels=c("Low(0 -.197)","Med (.197 - .27)", "High (.27 - 1)"))
test%>%mutate(Revenue_Level=ntile(test$avgrev, 3))%>%arrange(Revenue_Level)->test
test%>%group_by(Revenue_Level)%>%summarize(Max_Revenue=max(avgrev), Min_Revenue=min(avgrev))
test$Revenue_Level=cut(test$Revenue_Level, breaks=c(0,1,2,3), labels=c("Low (.94 - 39)", "Med (39-60.94)", "High (60.94-121.3)"))
table(Revenue=test$Revenue_Level, ChurnProbability=test$Prob_level)

#We will be targetting customers are in Med & High ChurnProbability and Med & High Revenue
Target_Customers=test$Customer_ID[test$probability_churn>.197 & test$avgrev>39]

#Q5.What would be the target segments for proactive retention campaigns? 
#   Falling ARPU forecast is also a concern and therefore, 
#   Mobicom would like to save their high revenue customers besides managing 
#   churn. Given a budget constraint of a contact list of 20% of the subscriber 
#   pool, which subscribers should prioritized if "revenue saves" is also a 
#   priority besides controlling churn. 
#   In other words, controlling churn is the primary telecomive 
#   and revenue saves is the secondary telecomive.


#Since our budget constraint is only 20% of people, and we want to prioritize people with high revenue;
test$Prob_Revenue = test$predicted*test$avgrev
test%>%mutate(levels=ntile(Prob_Revenue,10))%>%filter(levels==9 | levels==10)%>%count(levels, Customer_ID)->target_customers
#dataframe target_customers contains the customer-ids of our target customers from the test data
target_customers$Customer_ID
nrow(target_customers)/nrow(test) #=20% customers
