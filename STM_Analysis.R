###Prep###
library(tidyverse)
library(quanteda) 
library(ggplot2)
library(stm)
library(tm)
library(igraph)



getwd()
setwd("/Users/carminadietrich/Desktop/Project/newspaper-data-seperated/American/Final Analysis")

###Load the Data###
guardian<-read.csv("guardian.csv", stringsAsFactors = FALSE)
data<-guardian %>% dplyr::select(text, gender) %>% mutate(gender = factor(gender))
remove(guardian)
data$text<- as.character(data$text)
data$textLength<-nchar(data$text)
summary(data$textLength)
data<- data%>% filter(textLength>=1500)
data <-data[c("text", "gender")] # to only keep relevant columns
summary(is.na(data$text))

######Pre-processing & Conversion to STM Format######
processed<- textProcessor(data$text, metadata = data, stem = FALSE, verbose = TRUE, wordLengths = c(4, Inf)) #lower cases, removes stopwords, numbers, punctuation, stems
plotRemoved(processed$documents, lower.thresh=seq(1,50, by=5))
out<- prepDocuments(processed$documents,
                    processed$vocab,
                    processed$meta, 
                    lower.thresh = 5) #explain why you chose 5
docs <- out$documents
vocab <- out$vocab
meta <- out$meta
rm(processed) #important for tomorrow!

save(data, file ="data")
save(processed, file = "processed")
save(out, file = "out")
load("out")
load("data")

######Determining the Optimal Number of Topics for Topic Prevalence######
load("stmFit")
K<-c(40, 60, 80, 100) 
kresult <- searchK(out$documents,
                   out$vocab,
                   K, 
                   prevalence=~gender, 
                   data = out$meta, 
                   init.type = "Spectral")
save(kresult, file = "kresult")
save(kresult, file = "kresult.RData")
plot(kresult) 

######Run Topic Model for Topic Prevalence######
k <-100
stmFit <- stm(out$documents, out$vocab, K = k, prevalence =~ gender,
              max.em.its = 75, data = out$meta, init.type = "Spectral", seed = 8458302)

save(stmFit, file = "stmFit")
save(stmFit, file = "stmFit.RData")

#######Understanding the Topic Prevalence Model######

#Evaluate the topic interpretability by plotting exclusivity and semantic coherence, can only be done for prevalence model
par(mfrow=c(1, 1))
topicQuality(stmFit,
             documents = out$documents, 
             main = "Topic Interpretability: Exclusivity and Semantic Coherence") 

#Expeced Topic Proportions
plot(stmFit, type = "summary",
     text.cex = 0.7, 
     main = "Topics in the Guardian Corpus",
     labeltype = "frex",  #comment out in case you want to use words with highest probability instead
     n =4,
     xlim = c(0,.06),
     xlab = "Expected Topic Proportions",
     )

plot.STM(stmFit, type = c("summary"), xlim = c(0,.06), custom.labels = c("Headlines", "Online department stores", "Sexual harassment allegations", "Extreme weather", "Sports", "Movies", "Health", "NA", "Technological advice",  "Financial misconduct", "Manslaughter/murder", "Trump investigation", "Cancer treatment", "Trump and Obama", "Japanese whaling", "Experiences", "Celebrities", "North Korea", "Astronomy", "Technological advancement", "Gilets jaunes", "Sexual harassment allegations", "Smartphones", "Universal basic income", "Land ownership", "Refugee policy", "Gymnastics", "Sackler family", "Natural history", "Unhealthy nutrition", "Hamburgers", "Elections developing countries", "Traffic disruptions", "Tech corporations", "Brett Kavanaugh", "Water research", "Tennis", "AUS politics", "Criminal investigations", "Corporations' stock values", "Intra-state conflict", "Sports events", "City centres", "Mediterranean migration", "Airplane crashes", "US environmental policy", "Global economy", "Church sexual abuse", "Waste pollution", "Ethnic violence",  "Michael Cohen investigations", "Western EU elections",  "Nature and humans", "Epidemics", "Drugs", "NA", "Brexit", "Health standards", "Novichock poisoning", "Prisoners", "Khashoggi", "Space research", "Student protests", "Golf tournaments", "Mental health", "Small islands",  "Syrian conflict", "Soccer", "Eulogies", "Messages", "AUS financial misconduct", "US presidential elections", "Video games", "Genetic engineering", "Natural disasters", "Human rights", "Feminism", "Far-right extremism", "Illusions", "Trump criticism", "Climate change", "Low-wage work", "Chinese autonomous regions", "Mexican drug cartels", "Rightwing activism", "Extradition", "Medical misconduct", "UK industry", "Biodiversity conservation", "International relations", "Female health", "Political history", "Social media", "US immigration", "Authoritarian regimes", "US midterm elections", "Archaeology", "Life events", "Health risks", "US presidential elections"))


#Histograms of Expected Topic Proportions
plot(stmFit, type = "hist", topics = sample(1:k, size = 10), 
     main = "Histograms of the Expected Topic Proportions")


#Looking at the Words in Topics, plug in desired topic number
cloud(stmFit, topic = 1, scale = c(2.25,.5))
#or visualize the words like this:
plot(stmFit, type = "labels", topics = c(1, 2, 3), main = "Topics")

#Looking at Examples of Articles that are indicative of the Topic, plug in desired topic
shortdoc<- substr(data$text, 1, 200)
save(shortdoc, file = "shortdoc")

thoughts1 <- findThoughts(stmFit, texts=shortdoc, n=3, topics=1)$docs[[1]]
plotQuote(thoughts1, width=50, main="Topic 35")

#Analyze how topics differ
plot(stmFit, type = "perspectives", topics = c(1,5), main = "Theme Contrast")


######Labeling the Topics######

topicNames <- labelTopics(stmFit)
k <- 100
topic <- data.frame(topicNames = c("Headlines", "Online department stores", "Sexual harassment allegations", "Extreme weather", "Sports", "Movies", "Health", "NA", "Technological advice",  "Financial misconduct", "Manslaughter/murder", "Trump investigation", "Cancer treatment", "Trump and Obama", "Japanese whaling", "Experiences", "Celebrities", "North Korea", "Astronomy", "Technological advancement", "Gilets jaunes", "Sexual harassment allegations", "Smartphones", "Universal basic income", "Land ownership", "Refugee policy", "Gymnastics", "Sackler family", "Natural history", "Unhealthy nutrition", "Hamburgers", "Elections developing countries", "Traffic disruptions", "Tech corporations", "Brett Kavanaugh", "Water research", "Tennis", "AUS politics", "Criminal investigations", "Corporations' stock values", "Intra-state conflict", "Sports events", "City centres", "Mediterranean migration", "Airplane crashes", "US environmental policy", "Global economy", "Church sexual abuse", "Waste pollution", "Ethnic violence",  "Michael Cohen investigations", "Western EU elections",  "Nature and humans", "Epidemics", "Drugs", "NA", "Brexit", "Health standards", "Novichock poisoning", "Prisoners", "Khashoggi", "Space research", "Student protests", "Golf tournaments", "Mental health", "Small islands",  "Syrian conflict", "Soccer", "Eulogies", "Messages", "AUS financial misconduct", "US presidential elections", "Video games", "Genetic engineering", "Natural disasters", "Human rights", "Feminism", "Far-right extremism", "Illusions", "Trump criticism", "Climate change", "Low-wage work", "Chinese autonomous regions", "Mexican drug cartels", "Rightwing activism", "Extradition", "Medical misconduct", "UK industry", "Biodiversity conservation", "International relations", "Female health", "Political history", "Social media", "US immigration", "Authoritarian regimes", "US midterm elections", "Archaeology", "Life events", "Health risks", "US presidential elections"),
                    TopicNumber = 1:k,TopicProportions = colMeans(stmFit$theta))




#####Estimate effect######
out$meta$gender <- as.factor(out$meta$gender)
prep <- estimateEffect(1:100 ~ gender, stmFit, meta=out$meta, 
                       uncertainty="Global")
summary(prep, topics = c(1:100))
plot1<- plot(prep, 
             covariate="gender",
             topics=c(1:100),
             model=stmFit, 
             method="difference",
             cov.value1="male", 
             cov.value2="female",
             ylab = "Expected Difference in Topic Probability by Gender (with 95% Confidence Intervals)",
             xlab="More likely Female ... More Likely Male", main="Effect of Gender on Topic Prevalence for the Guardian Corpus ",
             xlim=c(-0.08, 0.08), 
             labeltype ="custom")


#Order based on Expected Topic Proportion
rank = order(unlist(plot1$means))
topicRnk <- topic[rank, ]
par()
par(cex=0.5)
plot2<- plot(prep, 
             covariate="gender",
             topics=topicRnk$TopicNumber,
             model=stmFit, 
             fontsize_col = 3,
             method="difference",
             verbose.labels = F,
             cov.value1="male", 
             cov.value2="female",
             ylab = "Expected Difference in Topic Probability by Gender (with 95% Confidence Intervals)",
             xlab="More likely Female ... More Likely Male",
             main="Effect of Gender on Topic Prevalence for the Guardian Corpus",
             xlim=c(-0.025, 0.025), 
             labeltype ="custom",
             custom.labels = topicRnk$topicNames)



######Plot the topic correlations###
mod.out.corr <- topicCorr(stmFit)
plot.topicCorr(mod.out.corr)
plot.topicCorr(mod.out.corr,
               labeltype = "custom",
               vertex.color="orange", 
               vertex.label.cex=.7,
               vertex.label.color= "black",
               vlabels= c("Headlines", "Online department stores", "Sexual harassment allegations", "Extreme weather", "Sports", "Movies", "Health", "NA", "Technological advice",  "Financial misconduct", "Manslaughter/murder", "Trump investigation", "Cancer treatment", "Trump and Obama", "Japanese whaling", "Experiences", "Celebrities", "North Korea", "Astronomy", "Technological advancement", "Gilets jaunes", "Sexual harassment allegations", "Smartphones", "Universal basic income", "Land ownership", "Refugee policy", "Gymnastics", "Sackler family", "Natural history", "Unhealthy nutrition", "Hamburgers", "Elections developing countries", "Traffic disruptions", "Tech corporations", "Brett Kavanaugh", "Water research", "Tennis", "AUS politics", "Criminal investigations", "Corporations' stock values", "Intra-state conflict", "Sports events", "City centres", "Mediterranean migration", "Airplane crashes", "US environmental policy", "Global economy", "Church sexual abuse", "Waste pollution", "Ethnic violence",  "Michael Cohen investigations", "Western EU elections",  "Nature and humans", "Epidemics", "Drugs", "NA", "Brexit", "Health standards", "Novichock poisoning", "Prisoners", "Khashoggi", "Space research", "Student protests", "Golf tournaments", "Mental health", "Small islands",  "Syrian conflict", "Soccer", "Eulogies", "Messages", "AUS financial misconduct", "US presidential elections", "Video games", "Genetic engineering", "Natural disasters", "Human rights", "Feminism", "Far-right extremism", "Illusions", "Trump criticism", "Climate change", "Low-wage work", "Chinese autonomous regions", "Mexican drug cartels", "Rightwing activism", "Extradition", "Medical misconduct", "UK industry", "Biodiversity conservation", "International relations", "Female health", "Political history", "Social media", "US immigration", "Authoritarian regimes", "US midterm elections", "Archaeology", "Life events", "Health risks", "US presidential elections")
               )

