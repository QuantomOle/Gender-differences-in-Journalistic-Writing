# Gender-differences-in-Journalistic-Writing
This project aims to investigate gender differences in journalistic writing based on a large corpus of newspaper articles. We want to explore to what extent men and women write about different topics and to what extent they write about topics differently.

To approach these questions we combine a set of descriptive and predictive NLP and ML techniques. We use structural topic modeling (STM) to test if male and female authors write about different topics. We apply Sentiment Analysis to explore whether men and women write about topics differently. Based on the insights from this quantitative text analysis, we build a classification model to predict authors' gender based on their article texts. The predictive performance provides evidence about how different texts are by gender. A common approach in this realm is to employ topic modeling (LDA) to generate features for the predictive models. We implement this approach as our baseline which we compare to a feature selection method developed by ourselves: uniqueK. This method is based on using the most frequently used unique words by gender as input for the prediction model. 

The code for each of these steps can be found in the corresponding folder. The folder "Other" contains experimental code revelant for the topic but not used to produce the final results. 
