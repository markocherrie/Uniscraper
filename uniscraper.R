## scaping the edinburgh university website
library(rvest)

# Gather the optional courses for Earth Observation and Geoinformation Management
MScOptionsURLS<-function(MSc){
if(MSc=="EOGM"){
  MSc<-"https://www.ed.ac.uk/geosciences/postgraduate/taught-masters/msc-earth-observation/structure-teaching-assessment/optional-courses"
  url<-MSc %>%
  read_html()%>%
  html_nodes('#block-system-main p:nth-child(1) a , p+ p a')%>%
  html_attr('href')
  } else if(MSc=="GIS"){
  MSc<-"https://www.ed.ac.uk/geosciences/postgraduate/taught-masters/geographical-information-science/structure-teaching-assessment/optional-courses"
  url<-MSc %>%
    read_html()%>%
    html_nodes('#block-system-main p:nth-child(1) a , p+ p a')%>%
    html_attr('href')
  }else if(MSc=="ARCH"){
  MSc<-"https://www.ed.ac.uk/geosciences/postgraduate/taught-masters/msc-gis-and-archaeology/structure-teaching-and-assessment/optional-courses"
  url<-MSc %>%
    read_html()%>%
    html_nodes('.col-sm-9 , p+ p a')%>%
    html_attr('href')
  # not perfect, but get the dpt links
  url<-url[3:10]
  # weird, not picking up the final two, must be formatted differently, quick fix
  url[9]<-"http://www.drps.ed.ac.uk/17-18/dpt/cxpghc11412.htm"
  url[10]<-"http://www.drps.ed.ac.uk/17-18/dpt/cxpghc11075.htm"
  }else {
    "Pick a MSc course that's in the Spatial Informatics and Earth Observation cluster"
  }
url<-as.data.frame(url)
colnames(url)<-"url"
url$url<-as.character(url$url)
return(url)
}

# Let's do it for the 3 MSc courses
EOGM_options<-MScOptionsURLS(MSc = "EOGM")
GIS_options<-MScOptionsURLS(MSc = "GIS")
ARCH_options<-MScOptionsURLS(MSc = "ARCH")

# Now let's scrape the optional courses pages
eduniscraper<-function(url){
Urlcatch<-tryCatch({
# Name of course
Name<-tryCatch({
Name<-url %>%
  read_html() %>%
  html_node("#sitspagetitle") %>%
    html_text()
Name<-gsub("Postgraduate Course: ", "", Name)
  }, error=function(e){
  Name<-NA}
  )

# Get the whole table
tableinfo <- url %>% 
  read_html() %>%
  html_table(fill = TRUE)

## Credits
SCQFcredits<-tryCatch({
SCQFcredits<-tableinfo[[4]][[2]][3]
}, error=function(e){
  SCQFcredits<-NA}
)

## Time
Time<-tryCatch({
    Time<-rapply(tableinfo, function(x) grep("Total Hours",x, value=TRUE))
    Time<-Time[length(Time)]
    Time<-gsub("[\n]", "", Time)
}, error=function(e){
  Time<-NA}
)

## Assessment
assessment<-tryCatch({
assessment<-tableinfo[[7]][[5]][5][1]
assessment<-gsub(" |[\n]", "", assessment)
assessment<-gsub("WrittenExam|Coursework|PracticalExam", "", assessment)
WrittenExam=unlist(strsplit(assessment, ","))[1]
Coursework=unlist(strsplit(assessment, ","))[2]
PracticalExam=unlist(strsplit(assessment, ","))[3]
}, error=function(e){
  WrittenExam<-NA
  Coursework<-NA
  PracticalExam<-NA}
)

# learning outcomes
learningoutcomes<-tryCatch({
  learningoutcomes<-rapply(tableinfo, function(x) grep("On completion of this course",x, value=TRUE))
  learningoutcomes<-learningoutcomes[length(learningoutcomes)]
  learningoutcomes<-gsub("[\n]", "", learningoutcomes)
}, error=function(e){
  learningoutcomes<-NA}
)

## Skills
skills<-tryCatch({
skills<-tableinfo[[10]][[2]][1]
skills<-gsub("-|[\n]", "", skills)
}, error=function(e){
  skills<-NA}
)

df<-data.frame(Name, SCQFcredits, Time, learningoutcomes, skills, WrittenExam, Coursework, PracticalExam)
return(df)
},error=function(e){
  df<-data.frame(Name=NA, SCQFcredits=NA, Time=NA, learningoutcomes=NA, skills=NA, WrittenExam=NA, Coursework=NA, PracticalExam=NA)
}
)
}

# test
#url<-"http://www.drps.ed.ac.uk/17-18/dpt/cxpgge11186.htm"  
#eduniscraper(url)

# loop
library(plyr)
# Technologies for Sustainable Energy, i.e. number 6, doesn't work
#EOGM_options <- subset(EOGM_options, url!= "http://www.drps.ed.ac.uk/16-17/dpt/cxpgee10001.htm")
#Put in error handling instead
EOGM_df<-mdply(EOGM_options, eduniscraper)
