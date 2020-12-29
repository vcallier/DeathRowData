#Bring In Libraries
library(rvest)
library(plyr)
library(dplyr)
library(ggplot2)
library("stringr")

install.packages('devtools')
devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)

install.packages(c("maps", "mapdata"))


#Create a trim function which uses regular expressions to clean white space
trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

  #Website to scrap
  theurl<-paste0("https://www.tdcj.texas.gov/death_row/dr_offenders_on_dr.html")

  #Download/Read the html
  html<- read_html(theurl)

  #I use CSS selector to figure out what table to read
  get_roster<-html_nodes(html, ".indent")

  #Make previous object into a table
  table_p<-html_table(get_roster)

#Cleaning up the table
death_row<-tibble(table_p[[1]][[1]], table_p[[1]][[3]], table_p[[1]][[4]], table_p[[1]][[5]],
                      table_p[[1]][[6]], table_p[[1]][[7]], table_p[[1]][[8]], table_p[[1]][[9]], table_p[[1]][[10]])
death_row<-death_row %>% dplyr::rename(TDCJ_Number=1, Last_name=2, First_name=3, DOB=4, Gender=5, Race=6, Date_received=7, County=8, Date_offense=9)


  #Build Container for links
  all_links<-data.frame(html=character())
 # for(j in 1:nrow(death_row)){
    #This is the link to grab details about each person on death row
    get_links<-html_nodes(html, paste0("td a")) %>% html_attr("href")
    link<-data.frame(get_links)
    link<-link %>% dplyr::rename(html=1)
    all_links<-rbind(all_links, link) #Bind All Link Data
 # }

  #Column Bind player data and links
 death_row<-cbind(death_row, all_links)
 head(death_row)

 path<-death_row$html

 stats<-tibble(TDCJ_number=character(),
                   Last_name=character(),
                   First_name=character(),
                   DOB=date(),
                   Gender=character(),
                   Race=character(),
                   Date_received=date(),
                   County=character(),
                   Date_offense=date(),
                   html=character(),
                   grade=character(),
                   native_county=character(),
                   native_state=character())


 #Create URL of Game Logs
 theurl1<-paste("https://www.tdcj.texas.gov/death_row/",path, sep="")


######Create Loop#####
for(i in 1:166){
  tryCatch({
    html1<- read_html(theurl1[i])

    #I use CSS selector to figure out what table to read
    get_info1 <- html_nodes(html1, ".indent")

    #Make previous object into a table
    table_p1 <- html_table(get_info1)

    #use CSS selector to get more info
    #  get_info2 <- html_nodes(html1, "hr+ p , p:nth-child(8) , p:nth-child(8) .bold , p:nth-child(11) , hr+ p .bold"
    #  table_p2 <- html_table(get_info2[1], fill=TRUE)


    more_data <- tibble(
      grade = table_p1[[1]][[3]][[6]],
      native_county = table_p1[[1]][[3]][[16]],
      native_state = table_p1[[1]][[3]][[17]]
    )


    #Attach Team

    stats[i,]<-cbind(death_row[i,], more_data)


    #print(paste0(stats))
  }, error=function(e){cat(conditionMessage(e))})

}

 head(stats)

 #study the data with ggplot

 #plot by race
ggplot(data=stats)+
  geom_bar(mapping = aes(x=forcats::fct_infreq (Race)))

#plot by gender
ggplot(data=stats)+
  geom_bar(mapping = aes(x=forcats::fct_infreq (Gender)))

#bar plot for native state
#first, cleaning the data
stats$native_state
stats$native_state <- gsub("No\r\n      \r\n      rth Carolina", "North Carolina", stats$native_state)
stats$native_state <- gsub("MO", "Missouri", stats$native_state)
stats$native_state <- sub("^$", "unknown", stats$native_state)
stats$native_state <- gsub("n/a", "unknown", stats$native_state)
stats$native_state <- gsub("N/A", "unknown", stats$native_state)

ggplot(data=stats)+
  geom_bar(mapping = aes(x= forcats::fct_infreq(native_state)))+
  coord_flip()

#filtering for native states that have more than one offender
state_counts <- count(stats,stats$native_state)

state_counts1 <- filter(state_counts, n>1)

ggplot(data=state_counts1)+
  geom_bar(mapping = aes(x= forcats::fct_infreq(n)))+
  coord_flip()



#bar plot for native county in texas only
TX_only <- filter(stats, native_state=="Texas")

#need to clean up the county list in the stats dataset.
TX_only$County <- word(TX_only$County, 1)
TX_only$native_county <- word(TX_only$native_county, 1)

#putting El Paso back into the county lists
TX_only$County <- gsub("El", "El Paso", TX_only$County)
TX_only$native_county <- gsub("El", "El Paso", TX_only$native_county)

#plot the native county on bar graph
ggplot(data=TX_only)+
  geom_bar(mapping = aes(x = forcats::fct_infreq(native_county)))+
  coord_flip()

#plot the counties where offenders are received
ggplot(data=TX_only)+
  geom_bar(mapping = aes(x = forcats::fct_infreq(County)))+
  coord_flip()

#getting to a map of Texas
counties_sf <- get_urbn_map(map="counties", sf = TRUE)
TX_counties <- filter(counties_sf, state_name=="Texas")
TX_counties$county_name <- word(TX_counties$county_name, 1)
TX_counties$county_name <- gsub("El", "El Paso", TX_counties$county_name)

names(TX_counties)[4] <-"County"

#joining the Texas Death Row data with the Texas county map data
county_counts <- count(TX_only,TX_only$County)
names(county_counts)[1] <-"County"
names(county_counts)[2] <-"Number of offenders"

TX_map <- left_join(TX_counties, as.data.frame(county_counts), by = "County")

#making maps
install.packages("choroplethr")
library(choroplethr)
library(choroplethrMaps)
data(df_pop_county)

county_choropleth(df_pop_county)

TX_map[[7]][is.na(TX_map[[7]])]=0

reduced_data <- data.frame(as.numeric(TX_map[[1]]), as.numeric(TX_map[[7]]))
  names(reduced_data)[1]<- "region"
  names(reduced_data)[2]<- "value"

## mapping THE MAP where death row offenders are received
county_choropleth(reduced_data,
                  state_zoom="texas",
                  title = "Counties where Death Row Offenders are Received",
                  num_colors = 1)


#now, mapping where death row offenders come from originally.

#joining the Texas Death Row data with the Texas county map data
county_counts2 <- count(TX_only,TX_only$native_county)
names(county_counts2)[1] <-"County"
names(county_counts2)[2] <-"Number of offenders"

TX_map2 <- left_join(TX_counties, as.data.frame(county_counts2), by = "County")

#change the NA to 0
TX_map2[[7]][is.na(TX_map2[[7]])]=0

reduced_data2 <- data.frame(as.numeric(TX_map2[[1]]), as.numeric(TX_map2[[7]]))
names(reduced_data2)[1]<- "region"
names(reduced_data2)[2]<- "value"

## mapping THE MAP where death row offenders come from
county_choropleth(reduced_data2,
                  state_zoom="texas",
                  title = "Counties from which Death Row Offenders Come",
                  num_colors = 1)



#table comparing where offenders are received and where they are from
table1 <- as.data.frame(left_join(county_counts2, county_counts, by = "County"))
names(table1)[1]<- "County"
names(table1)[2]<- "Num offenders native county"
names(table1)[3]<- "Num offenders received"

#change the NA to 0
table1[[3]][is.na(table1[[3]])]=0

#final table to print out
table1[order(table1$'Num offenders native county'),]

# WHAT IS UP WITH HOUSTON??
# Let's look at a map of the population in Texas and look at the proportion of death row offenders in each county

#importing data from http://worldpopulationreview.com/us-counties/tx/
mydata = read.csv("data.csv")  # read csv file

mydata$CTYNAME <- gsub("\\s*\\w*$", "", mydata$CTYNAME)
#change the heading
names(mydata)[1] <-"County"

TX_map3 <- left_join(TX_counties, as.data.frame(mydata), by = "County")
reduced_data3 <- data.frame(as.numeric(TX_map3[[1]]), as.numeric(TX_map3[[7]]))
names(reduced_data3)[1]<- "region"
names(reduced_data3)[2]<- "value"
#change the NA to 0
reduced_data3[[2]][is.na(reduced_data3[[2]])]=0

## mapping THE MAP of texas population
county_choropleth(reduced_data3,
                  state_zoom="texas",
                  title = "Population of Texas in 2020",
                  num_colors = 9)

# now we would like to know, the proportion of death row offenders as a proportion of population in the county
# TX_map4 is where offenders are received, and county population
TX_map4 <- left_join(as.data.frame(reduced_data2), as.data.frame(reduced_data3), by="region")

#plotting the number of offenders vs the population in each county
ggplot(data=TX_map4)+
  geom_point(mapping = aes(x=value.y , y= value.x))+
  geom_smooth(mapping = aes(x=value.y , y= value.x))+
  labs(x = "Population in County", y="Death Row Offenders from County")+
  scale_x_continuous(trans='log2') +
  scale_y_continuous(trans='log2')

#linear model attempt
sim1_mod <- lm( value.x ~ value.y, data = TX_map4)
coef(sim1_mod)



#calculate a proportion of offenders out of total population of each county
prop <- mutate(TX_map4,
       proportion = 1000000*as.numeric(TX_map4[[2]])/as.numeric(TX_map4[[3]]))

#remove the NA values
prop[[4]][is.na(prop[[4]])]=0

#remove the infinity values
prop[[4]][is.infinite(prop[[4]])]=0

reduced_data4 <- data.frame(as.numeric(prop[[1]]), as.numeric(prop[[4]]))

names(reduced_data4)[1]<- "region"
names(reduced_data4)[2]<- "value"

## mapping THE MAP of proportion of county population
county_choropleth(reduced_data4,
                  state_zoom="texas",
                  title = "Death Row Offenders as a Proportion of County Population",
                  num_colors = 1)


#level of education
as.character(stats$grade)

stats$grade <- word(stats$grade, 1)

stats$grade <- gsub("GED", "12", stats$grade)
stats$grade <- gsub("1st", "1", stats$grade)
stats$grade <- gsub("4th", "4", stats$grade)
stats$grade <- gsub("6th", "6", stats$grade)
stats$grade <- gsub("7th", "7", stats$grade)
stats$grade <- gsub("8th", "8", stats$grade)
stats$grade <- gsub("9th", "9", stats$grade)
stats$grade <- gsub("10th", "10", stats$grade)
stats$grade <- gsub("11th", "11", stats$grade)
stats$grade <- gsub("12th", "12", stats$grade)
stats$grade <- gsub("Unknown", "", stats$grade)
stats$grade <- gsub("06", "6", stats$grade)
stats$grade <- gsub("07", "7", stats$grade)
stats$grade <- gsub("08", "8", stats$grade)
stats$grade <- gsub("09", "9", stats$grade)

# histogram of grades
hist(as.numeric(stats$grade))

ggplot(data=stats)+
  geom_bar(mapping = aes(x= forcats::fct_infreq(stats$grade)))





*HISTORICAL DATA
#Website to scrap
theurl2<-paste0("https://www.tdcj.texas.gov/death_row/dr_by_county_1923-1973.html")

#Download/Read the html
html2<- read_html(theurl2)

#I use CSS selector to figure out what table to read
get_roster2<-html_nodes(html2, ".indent")

#Make previous object into a table
table_p2<-html_table(get_roster2)


TX_map5 <- left_join(TX_counties, as.data.frame(table_p2), by = "County")


reduced_data5 <- data.frame(as.numeric(TX_map5[[1]]), as.numeric(TX_map5[[7]]))
names(reduced_data5)[1]<- "region"
names(reduced_data5)[2]<- "value"

reduced_data5[[2]][is.na(reduced_data5[[2]])]=0

## mapping THE MAP of offenders sentenced to death by each county 1923-1973
county_choropleth(reduced_data5,
                  state_zoom="texas",
                  title = "Offenders Sentenced to Death by Each County from 1923 to 1973",
                  num_colors = 0)




#figuring out differences between date received and date of offense
stats$date_diff <- as.Date(as.character(stats$Date_received), format="%m/%d/%Y")-
  as.Date(as.character(stats$Date_offense), format="%m/%d/%Y")

#need to fix this plot
ggplot(data=stats)+
  hist(as.numeric(stats$date_diff/365))



