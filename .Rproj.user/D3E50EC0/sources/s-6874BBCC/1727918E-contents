############### DATA COLLECTION #########################

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
     #Name<-gsub("Undergraduate Course:|Postgraduate Course: ", "", Name)
    }, error=function(e){
      Name<-NA}
    )
    
    # Get the whole table
    tableinfo<-url %>%
      read_html() %>%
      html_nodes('.sitstablegrid') %>%
      html_text()
    
    ## Credits
    SCQFcredits<-tryCatch({
      subtable<-tableinfo[gregexpr(pattern ='SCQF Credits',tableinfo)>1]
      start<-gregexpr(pattern ='SCQF Credits',subtable)[[1]][1]+12
      end<-gregexpr(pattern ='SCQF Credits',subtable)[[1]][1]+13
      SCQFcredits<-as.numeric(substring(subtable, start,end))
    }, error=function(e){
      SCQFcredits<-NA}
    )
    
    ## Time
    Time<-tryCatch({
      subtable<-tableinfo[gregexpr(pattern ='Total Hours',tableinfo)>1]
      start<-gregexpr(pattern ='Total Hours',subtable)[[1]][1]
      end<-gregexpr(pattern ='Assessment \\(Further Info\\)',subtable)[[1]][1]-1
      Time<-substring(subtable, start,end)
      Time<-gsub("[\n]|:|\\)|\\(\\)|W|Assessment|(Further Info)|", "", Time)
      Time<-gsub("\\(", ",", Time)
    }, error=function(e){
      Time<-NA}
    )
    
    ## Assessment
    assessment<-tryCatch({
      subtable<-tableinfo[gregexpr(pattern ='Written Exam',tableinfo)>1]
      start<-gregexpr(pattern ='Written Exam',subtable)[[1]][1]
      end<-start+55
      assessment<-substring(subtable, start,end)
      assessment<-gsub(" |[\t]|-|[\n]", "", assessment)
      assessment<-gsub("WrittenExam|Coursework|PracticalExam", "", assessment)
      assessment<-gsub("%", "", assessment)
      WrittenExam=as.numeric(unlist(strsplit(assessment, ","))[1])
      Coursework=as.numeric(unlist(strsplit(assessment, ","))[2])
      PracticalExam=as.numeric(unlist(strsplit(assessment, ","))[3])
    }, error=function(e){
      WrittenExam<-NA
      Coursework<-NA
      PracticalExam<-NA}
    )
    
    # learning outcomes
    learningoutcomes<-tryCatch({
      learningoutcomes<-tableinfo[gregexpr(pattern ='By the end of this course students will be able to|On completion of this course|On completion of the course|Students will be able to:',tableinfo)>1][2]
      #if (is.na(learningoutcomes)){
      #learningoutcomes<-tableinfo[gregexpr(pattern ='On completion of this course|On completion of the course|Students will be able to:',tableinfo)>1][1]
      #}
      learningoutcomes<-gsub("Learning Outcomes|[\t]|-|[\n]", "", learningoutcomes)
    }, error=function(e){
      learningoutcomes<-NA}
    )
    
    ## Skills
    skills<-tryCatch({
      subtable<-tableinfo[gregexpr(pattern ='Graduate Attributes and Skills',tableinfo)>1][2]
      start<-gregexpr(pattern ='Graduate Attributes and Skills',subtable)[[1]][1]
      end<-gregexpr(pattern ='Special Arrangements|Keywords|Additional Class Delivery Information',subtable)[[1]][1]-1
      skills<-substring(subtable, start,end)
      skills<-gsub("Graduate Attributes and Skills|[\t]|-|[\n]", "", skills)
    }, error=function(e){
      skills<-NA}
    )
    
    df<-data.frame(Name, url, SCQFcredits, Time, learningoutcomes, skills, WrittenExam, Coursework, PracticalExam)
    #write.csv(df, paste0("data/", url, ".csv"), row.names=F)
    return(df)
  },error=function(e){
    df<-data.frame(Name=NA, url=url, SCQFcredits=NA, Time=NA, learningoutcomes=NA, skills=NA, WrittenExam=NA, Coursework=NA, PracticalExam=NA)}
  )
}

# test
url="http://www.drps.ed.ac.uk/17-18/dpt/cxpghc11412.htm"
test<-eduniscraper(url)

# loop
library(plyr)
# Technologies for Sustainable Energy, i.e. number 6, doesn't work
#EOGM_options <- subset(EOGM_options, url!= "http://www.drps.ed.ac.uk/16-17/dpt/cxpgee10001.htm")
#Put in error handling instead
EOGM_df<-mdply(EOGM_options, eduniscraper)
GIS_df<-mdply(GIS_options, eduniscraper)
ARCH_df<-mdply(ARCH_options, eduniscraper)

############### DATA CLEANING #########################

# master data
EOGM_df$cluster<-"EOGM"
GIS_df$cluster<-"GIS"
ARCH_df$cluster<-"ARCH"
master_df<-rbind(EOGM_df, GIS_df, ARCH_df)

# 19 courses are duplicates
library(dplyr)
master_df<-master_df %>% 
  group_by(url) %>% 
  mutate(cluster_group = paste0(cluster, collapse = ",")) 
master_df<-master_df[!duplicated(master_df$Name),]
master_df$cluster<-NULL

# 3 course links don't work, but only one remains,
# as it is considered 'unique in above
master_df<-master_df[!is.na(master_df$Name),]


# Code NA to not entered
master_df[is.na(master_df)]<-"Not Entered"

# postgrad or undergrad
master_df$coursetype<-ifelse(grepl("Undergraduate", master_df$Name), "U", "P")
master_df$Name<-gsub("Undergraduate Course:|Postgraduate Course:", "", master_df$Name)

# get time vars
result<-list()

for(i in 1:33){
Time=unlist(strsplit(master_df$Time[i], ","))
Time<-gsub(" ", "", Time)
Time<-as.data.frame(Time)
colnames(Time)<-"TimeVars"
library(tidyr)
Time<-Time %>%
separate(TimeVars, 
         into = c("text", "num"), 
         sep = "(?<=[A-Za-z])(?=[0-9])") 
Time$Name<-master_df$Name[i]
result[[i]] <- Time
}
timevars = do.call(rbind.fill, result)
timevars<-timevars %>%
  spread(text, num)
timevars[is.na(timevars)]<-0
timevars<-subset(timevars, Name!=' Water Resource Management (PGGE11018)')
timevars<-subset(timevars, Name!=' Epidemiology for Public Health (PUHR11016)')

# merge with master df
master_df<-merge(master_df, timevars, by="Name", all.x=T)
master_df$Time<-NULL
master_df$NotEntered<-NULL

############## DATA VISUALISATION ############

# credits
library(ggplot2)
ggplot(master_df) + stat_count(mapping = aes(x=SCQFcredits, y=..prop.., group=1))
summary(master_df$SCQFcredits)
# 2/3 of the optional courses are currently 20 credits

# cluster group
library(ggplot2)
g<- ggplot(master_df) + aes(cluster_group)
g + geom_bar()

# EOGM has the most unique master courses (10)
# EOGM and GIS share 11 courses
# There is only one course listed in all of the masters (Principles and Practice of Remote Sensing (PGGE11233))

# Coursework
master_df$assessmentbreakdown<-paste0("C", master_df$Coursework, 
                                      "W", master_df$WrittenExam, 
                                      "P", master_df$PracticalExam)
ggplot(master_df) + stat_count(mapping = aes(x=assessmentbreakdown, y=..prop.., group=1))
table(master_df$assessmentbreakdown)
# 22/33; 2/3 of the courses only assess via coursework
# Notable other, would be to split 50/50 with Written exam

### topic modelling for the skills and learning outcomes
library(topicmodels)
library(tm)
master_df$learningoutcomes<-gsub("On completion of this course, the student will be able to:|By the end of this course students will be able to:|Students will be able to:|On completion of the course, a student should", "", master_df$learningoutcomes)
text_corpus <- VCorpus(VectorSource(master_df$learningoutcomes))

# clean up
text_corpus_clean <- tm_map(text_corpus,
                            content_transformer(tolower))
#text_corpus_clean <- tm_map(text_corpus_clean, stemDocument)
text_corpus_clean <- tm_map(text_corpus_clean, removeNumbers)
text_corpus_clean <- tm_map(text_corpus, removeWords, c(stopwords("english"),"understand","understanding",
                                                        "demonstrate","including",
                                                        "different", "able", 
                                                        "students", "ability", "coursework",
                                                        "examination", "have", "use"
                                                        ,"key", "group")) 
text_corpus_clean <- tm_map(text_corpus_clean, removePunctuation)
text_corpus_clean <- tm_map(text_corpus_clean, stripWhitespace)

# word cloud
library(wordcloud)
wordcloud(text_corpus_clean, min.freq = 2, random.order = FALSE,
          colors=brewer.pal(8, "Dark2"))

# frequency of words
text_dtm <- DocumentTermMatrix(text_corpus_clean)
text_dtm
findFreqTerms(text_dtm, lowfreq = 5)

library(topicmodels)
text_lda <- LDA(text_dtm, k = 2, method = "VEM", control = NULL)
library(tidytext)
text_topics <- tidy(text_lda, matrix = "beta")
text_top_terms <- text_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

text_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# Time
test<-master_df %>% tidyr::gather("Name", "value", 11:21)
  test<-test[,13:14]

  ggplot(., aes(Name, value, color=Name))+
  geom_point()+
  geom_smooth(method = "lm", se=FALSE, color="black")+
  facet_wrap(~Name)




