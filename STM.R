###Prep###
library(tidyverse)
library(quanteda)
library(ggplot2)
library(stm)

#getwd()
#setwd("/Users/carminadietrich/Desktop/Project/newspaper-data-seperated/American")

###Load the Data###
#load("en_list_clean.RData")
#bloomberg_df <- en_list2[[3]]
#data <- bloomberg_df[c("text","gender")] # to only keep relevant columns

######Pre-processing & Conversion to STM Format######

processed<- textProcessor(data$text, metadata = data) #lower cases, removes stopwords, numbers, punctuation, stems
plotRemoved(processed$documents, lower.thresh=seq(1,50, by=5))
out<- prepDocuments(processed$documents,
                    processed$vocab,
                    processed$meta, 
                    lower.thresh = 5) #explain why you chose 5
docs <- out$documents
vocab <- out$vocab
meta <- out$meta
rm(processed)
save(out, file = "out")
save(docs, file ="docs")


######Determining the Optimal Number of Topics for Topic Prevalence######
K<-c(40, 60, 80, 100) 
kresult <- searchK(out$documents,
                   out$vocab,
                   K, 
                   prevalence=~gender, 
                   data = out$meta, 
                   init.type = "Spectral")
plot(kresult) 

######Determining the Optimal Number of Topics for Topical Content######
kresult2 <- searchK(out$documents,
                    out$vocab,
                    K, 
                    prevalence=~gender, 
                    content=~gender,
                    data = out$meta, 
                    init.type = "Spectral")

kresult2
plot(kresult2)


######Run Topic Model for Topic Prevalence######
k <- 60
stmFit <- stm(out$documents, out$vocab, K = k, prevalence =~ gender,
              max.em.its = 75, data = out$meta, init.type = "Spectral", seed = 8458302)

#######Understanding the Topic Prevalence Model######

#Evaluate the topic interpretability by plotting exclusivity and semantic coherence, can only be done for prevalence model
par(mfrow=c(1, 1))
topicQuality(stmFit,
             documents = out$documents, 
             main = "Topic Interpretability: Exclusivity and Semantic Coherence") 

#Expeced Topic Proportions
plot(stmFit, type = "summary",
     text.cex = 0.7, 
     main = "Topics in the Bloomberg Corpus",
     #labeltype = "frex",  #comment out in case you want to use words with highest probability instead
     n =4,
     xlim = c(0,.06),
     xlab = "Expected Topic Proportions")

#Histograms of Expected Topic Proportions
#plot(stmFit, type = "hist", topics = sample(1:k, size = 10), 
#     main = "Histograms of the Expected Topic Proportions")

#Looking at the Words in Topics, plug in desired topic number
cloud(stmFit, topic = 35, scale = c(2.25,.5))
#or visualize the words like this:
plot(stmFit, type = "labels", topics = c(1, 2, 3), main = "Topics")

#Looking at Examples of Articles that are indicative of the Topic, plug in desired topic
shortdoc<- substr(data$text, 1, 200)
save(shortdoc, file = "shortdoc")

thoughts35 <- findThoughts(stmFit, texts=shortdoc, n=3, topics=35)$docs[[1]]
plotQuote(thoughts35, width=50, main="Topic 35")

#Analyze how topics differ
plot(stmFit, type = "perspectives", topics = c(1,5), main = "Theme Contrast")


######Labeling the Topics######

topicNames <- labelTopics(stmFit)
k <- 60
topic <- data.frame(topicNames = c("TUR economy","NA","SC Retirement Kennedy","Political cybersecurity","BRA truckers' strike","ITA populist parties","Phishing hacks","MYS elections","Commodity markets","US Immigrant Family separation","NAFTA negotiations","Californian emissions standards","High-profile gaffes","MENA region Conflict","PRK denuclearization","NA","Corporate mergers","Paul Manafort trial","Contested governments","Corporate expansion","Online political advertising","Natural disasters","ESP elections","IRN nuclear deal","Criminal allegations","Arab-Israeli conflict","New Jersey state budget","Presidential/prime-ministerial elections","Trump energy policy","Newsletter","Michael Cohen affair - Novartis","Humanitarian disasters","Foreign security policy","US primary elections","US tariffs on China","Economic stagnation","Legal action","NA","Trump allegations","VEN crisis","Business","US domestic policy","Healthcare policy","Michael Cohen affair - Evidence","RUS-Western relations","Environmental Protection Agency","US-PRK summit","Tax policy","MENA region - Oil prices","UK politics internal division","Brexit","Paul Manafort trial - Indictment","Tech corporations","Brexit - Ireland","Illegal political intelligence trading","Air and space travel","Constitutional changes","EU migration policy","Municipal government","Mueller investigation"),
                    TopicNumber = 1:k,TopicProportions = colMeans(stmFit$theta))

#####Estimate effect######
out$meta$gender <- as.factor(out$meta$gender)
prep <- estimateEffect(1:60 ~ gender, stmFit, meta=out$meta, 
                       uncertainty="Global")
summary(prep, topics = c(1:60))
plot1<- plot(prep, 
             covariate="gender",
             topics=c(1:60),
             model=stmFit, 
             method="difference",
             cov.value1="male", 
             cov.value2="female",
             ylab = "Expected Difference in Topic Probability by Gender (with 95% Confidence Intervals)",
             xlab="More likely Female ... More Likely Male", main="Effect of Gender on Topic Prevalence for the Bloomberg Corpus ",
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
             method="difference",
             verbose.labels = F,
             cov.value1="male", 
             cov.value2="female",
             ylab = "Expected Difference in Topic Probability by Gender (with 95% Confidence Intervals)",
             xlab="More likely Female ... More Likely Male",
             main="Effect of Gender on Topic Prevalence for the Bloomberg Corpus",
             xlim=c(-0.06, 0.06), 
             labeltype ="custom",
             custom.labels = topicRnk$topicNames)


######Plot the topic correlations###
mod.out.corr <- topicCorr(stmFit)
plot.topicCorr(mod.out.corr)
plot.topicCorr(mod.out.corr,
               labeltype = "custom",
               custom.labels = topicNames)

######Run the Topical Content Model######

#Estimate the Topic Model
stmContent2 <- stm(out$documents, 
                   out$vocab,
                   K = 80, 
                   prevalence =~ gender,
                   content =~ gender,
                   max.em.its = 75,
                   data = out$meta, 
                   init.type = "Spectral",
                   seed = 8458302)

#Expeced Topic Proportions
plot(stmContent2, type = "summary",
     text.cex = 0.7, 
     main = "Topics in the Bloomberg Corpus",
     n =4,
     xlim = c(0,.06),
     xlab = "Expected Topic Proportions")


#Analyze how vocabularies differ
plot(stmContent2, type = "perspectives", topics = 76)

prep1 <- estimateEffect(1:80 ~ gender, stmContent2, meta=out$meta, 
                        uncertainty="Global")
summary(prep1, topics = c(1:80))

plot2<- plot(prep1, 
             covariate="gender",
             topics=c(1:80),
             model=stmContent2, 
             method="difference",
             cov.value1="male", 
             cov.value2="female",
             main="Effect of Gender on Topical Content for the Bloomberg Corpus ",
             xlim=c(-0.08, 0.08), 
             labeltype ="custom")
