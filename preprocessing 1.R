aqidata1<- read.csv(file.choose())
library(dplyr)

aqidata1 %>%
  distinct(City)

colSums(is.na(aqidata1))

nona=aqidata1 %>%
  filter(!is.na(CO))
nona

nona %>%
  distinct(City)

cities=nona %>%
  distinct(City)

cities

colSums(is.na(nona))

nona %>%
  filter(City=="Ahmedabad")

nona$NO[is.na(nona$NO[1:62])]<- mean(nona$NO[1:62], na.rm=TRUE)

nona %>%
  filter(City=="Amritsar")

nona$NO[is.na(nona$NO[63:124])]<- mean(nona$NO[63:124], na.rm=TRUE)

nona %>%
  filter(is.na(NO))

nona %>%
  filter(City==cities[3,])

nona$NO[is.na(nona$NO[125:186])]<- mean(nona$NO[125:186], na.rm=TRUE)
nona$NO[is.na(nona$NO[187:248])]<- mean(nona$NO[187:248], na.rm=TRUE)
nona %>%
  filter(City==cities[4,])

nona$NO[is.na(nona$NO[nona$City=="Delhi"])]<- mean(nona$NO[nona$City=="Delhi"], na.rm=TRUE)
nona %>%
  filter(City=="Delhi")


for (i in cities[,1]){
  nona$NO[is.na(nona$NO[nona$City==i])]<-mean(nona$NO[nona$City==i], na.rm=TRUE)
}

for (i in cities[,1]){
  nona$PM2.5[is.na(nona$PM2.5[nona$City==i])]<-mean(nona$PM2.5[nona$City==i], na.rm=TRUE)
}

for (i in cities[,1]){
  nona$PM10[is.na(nona$PM10[nona$City==i])]<-mean(nona$PM10[nona$City==i], na.rm=TRUE)
}

for (i in cities[,1]){
  nona$NO2[is.na(nona$NO2[nona$City==i])]<-mean(nona$NO2[nona$City==i], na.rm=TRUE)
}

for (i in cities[,1]){
  nona$NOx[is.na(nona$NOx[nona$City==i])]<-mean(nona$NOx[nona$City==i], na.rm=TRUE)
}

for (i in cities[,1]){
  nona$NH3[is.na(nona$NH3[nona$City==i])]<-mean(nona$NH3[nona$City==i], na.rm=TRUE)
}

for (i in cities[,1]){
  nona$SO2[is.na(nona$SO2[nona$City==i])]<-mean(nona$SO2[nona$City==i], na.rm=TRUE)
}

for (i in cities[,1]){
  nona$NH3[is.na(nona$NH3[nona$City==i])]<-mean(nona$NH3[nona$City==i], na.rm=TRUE)
}
nona["NH3"]=aqidata1["NH3"]
library(tidyr)
nona %>% 
  drop_na()

df = subset(nona, select = -c(8) )
df

colSums(is.na(df))

for (i in cities[,1]){
  df$O3[is.na(df$O3[df$City==i])]<-mean(df$O3[df$City==i], na.rm=TRUE)
}

for (i in cities[,1]){
  df$Benzene[is.na(df$Benzene[df$City==i])]<-mean(df$Benzene[df$City==i], na.rm=TRUE)
}

for (i in cities[,1]){
  df$Toluene[is.na(df$Toluene[df$City==i])]<-mean(df$Toluene[df$City==i], na.rm=TRUE)
}

for (i in cities[,1]){
  df$Xylene[is.na(df$Xylene[df$City==i])]<-mean(df$Xylene[df$City==i], na.rm=TRUE)
}

df2 = subset(df, select = -c(13) )
df2

colSums(is.na(df2))

df3=df2
df3

for(i in 1:nrow(df4)){
  if(df4[i,13]==df4[i,3]){
    df4[i,13]=df4[i,3]+df4[i,4]+df4[i,5]+df4[i,6]+df4[i,7]+df4[i,8]+df4[i,9]+df4[i,10]+df4[i,11]+df4[i,12]
    
  }
}
  
aqi=df3[,13]
aqi
df4<-df3 %>%
  na.omit()

colSums(is.na(df4))

df5=df4

for(i in 1:nrow(df5))
{
  if(df5[i,14]==""){
    if(df5[i,13]>=400) df5[i,14]="Severe"
    else if(df5[i,13]>=300) df5[i,14]="Very Poor"
    else if(df5[i,13]>=200) df5[i,14]="Poor"
    else if (df5[i,13]>=100) df5[i,14]="Moderate"
    else if (df5[i,13]>=50) df5[i,14]="satisfactory"
    else df5[i,14]="Good"
  }
}

colSums(is.na(df5))

write.table(df5, "D:\\AI\\data science\\dashboard project\\finaldf.csv",
            append = TRUE,
            sep = ",",
            col.names = TRUE,
            row.names = FALSE,
            quote = FALSE)

colSums(is.na(df5))
cities
delhi<-subset(df5,City=="Delhi")
delhi
bglr<-subset(df5,City=="Bengaluru")
bglr
chni<-subset(df5,City=="Chennai")
mbai<-subset(df5,City=="Mumbai")
kkta<-subset(df5,City=="Kolkata")
shlg<-subset(df5,City=="Shillong")
ggrm<-subset(df5,City=="Gurugram")
hybd<-subset(df5,City=="Hyderabad")
chdh<-subset(df5,City=="Chandigarh")


write.table(delhi, "D:\\AI\\data science\\dashboard project\\delhidf.csv",
            append = TRUE,
            sep = ",",
            col.names = TRUE,
            row.names = FALSE,
            quote = FALSE)

write.table(bglr, "D:\\AI\\data science\\dashboard project\\bengaloredf.csv",
            append = TRUE,
            sep = ",",
            col.names = TRUE,
            row.names = FALSE,
            quote = FALSE)

write.table(chni, "D:\\AI\\data science\\dashboard project\\chennaidf.csv",
            append = TRUE,
            sep = ",",
            col.names = TRUE,
            row.names = FALSE,
            quote = FALSE)

write.table(mbai, "D:\\AI\\data science\\dashboard project\\mumbai.csv",
            append = TRUE,
            sep = ",",
            col.names = TRUE,
            row.names = FALSE,
            quote = FALSE)

write.table(kkta, "D:\\AI\\data science\\dashboard project\\kolkatadf.csv",
            append = TRUE,
            sep = ",",
            col.names = TRUE,
            row.names = FALSE,
            quote = FALSE)

write.table(shlg, "D:\\AI\\data science\\dashboard project\\shillongdf.csv",
            append = TRUE,
            sep = ",",
            col.names = TRUE,
            row.names = FALSE,
            quote = FALSE)

write.table(ggrm, "D:\\AI\\data science\\dashboard project\\gurugramdf.csv",
            append = TRUE,
            sep = ",",
            col.names = TRUE,
            row.names = FALSE,
            quote = FALSE)

write.table(hybd, "D:\\AI\\data science\\dashboard project\\hyderabaddf.csv",
            append = TRUE,
            sep = ",",
            col.names = TRUE,
            row.names = FALSE,
            quote = FALSE)

write.table(chdh, "D:\\AI\\data science\\dashboard project\\chandigarhdf.csv",
            append = TRUE,
            sep = ",",
            col.names = TRUE,
            row.names = FALSE,
            quote = FALSE)

df=merge(df2,df3,by.df2 = "AQI" , by.df3 = "AQI")


