Inflation_Data <- read_xlsx("data.xlsx", sheet = "data")
head(Inflation_Data)

# Retrieving all the column names in the dataset
colnames(Inflation_Data)

#deleting unnecessary columns(I have deleted in excel)
InflationData <- Inflation_Data %>% 
   rename('Country_Name' ='Country Name', 'Series_Name'=  'Series Name')
View(InflationData)

# Extracting first 3 rows
Inflation_Afghanistan <- Inflation_Data[1:3,]
View(Inflation_Afghanistan)
# Extracting first 3 columns
Inflation_Afghanistan <- Inflation_Data[,1:3]
View(Inflation_Afghanistan)
# Extracting 1,4 and 5 columns
Inflation_Afghanistan <- Inflation_Data[,c(1,4,5)]
View(Inflation_Afghanistan)

# Filtering the data for Canada
Canada_details <- InflationData %>% 
  filter(Country_Name == "Canada" & Series_Name == 'Unemployment, total (% of total labor force) (national estimate)' )
View(Canada_details)

# Let's find out the country where unemployment rate is highest in 2000
Highest_unemployment <- InflationData %>%
  rename('Y2000' ='2000 [YR2000]') %>%
  filter(Series_Name == 'Unemployment, total (% of total labor force) (national estimate)') %>%
  select('Country_Name','Series_Name','Y2000') 
View(Highest_unemployment)
summarize(Highest_unemployment, Max_employment_rate = max(Y2000), CountryName = Country_Name[which(Y2000 == max(Y2000))])


# Let's find out the country where inflation rate is highest in 2000
Highest_inflation <- InflationData %>%
  rename('Y2000' ='2000 [YR2000]') %>%
  filter(Series_Name == 'Inflation, GDP deflator (annual %)'& Y2000 <= 100) %>%
  select('Country_Name','Series_Name','Y2000')
View(Highest_inflation)
summarize(Highest_inflation, Max_inflation_rate = max(Y2000), CountryName = Country_Name[which(Y2000 == max(Y2000))])


# (There was a wrong data which has 2630. Inflation Rate is measured in percentage so filtered out data more than 100 and ran the program again)
# Find unemployed male and female rate in India
Unemployment_India <-InflationData %>%
  rename('Y2019' ='2019 [YR2019]') %>%
  filter(Series_Name %in% c('Unemployment, male (% of male labor force) (national estimate)','Unemployment, female (% of female labor force) (national estimate)')& Y2019 <100)%>%
  select('Country_Name','Series_Name','Y2019') %>%
  arrange(Y2019)
View(Unemployment_India)

#Installed tidyr
## separating a column name Series_name in 2 columns
Separate_column <- separate(InflationData,Series_Name, into = c('1','2'),sep=',' )
Separate_column
# Uniting 1 and 2 columns
Unite_column <- unite(Separate_column,Series_Name,2,3,sep=',')
Unite_column

# If want to find Statistical parameters like mean, Standard deviation
Statistical <- InflationData %>%
  filter(`2000 [YR2000]`<100) %>%
  select(Country_Name, Series_Name,`2000 [YR2000]`) %>%
  summarise(mean(`2000 [YR2000]`))
Statistical

Inflation_Countries<- InflationData %>%
  filter(Country_Name %in% c('Afghanistan','Albania','Algeria','American Samoa','Andorra','Angola','Antigua and Barbuda','Argentina','Armenia','Aruba','Australia','Azerbaijan','Bangladesh','Barbados',' Belarus','Belgium','Belize','Benin','Bermuda','Brazil',' Bulgaria',' Canada','Chad','Chile','Cuba','Cyprus','Ecuador','Fiji','India','Nauru','Nepal','Niger','Oman','Pakistan'),Series_Name=='Inflation, GDP deflator (annual %)')
View(Inflation_Countries)


# Graphs for visualization
plots<-ggplot(data=Inflation_Countries)+ geom_point(mapping = aes(x= Country_Name,y= `2000 [YR2000]`,color= "purple"))+theme(axis.text.x = element_text(angle=45))
plots +labs(title = 'Inflation rate - some countries in 2000')+annotate("text", x=10,y=9,label="Mean is in between 0 & 25",color="Blue")+ ylim(0, 100)





