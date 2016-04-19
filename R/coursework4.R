#' ---
#' title: "Coursework 3"
#' author: "Fortunat Mutunda"
#' date: "Mar,  02 2016"
#' #' output: pdf_document
#' ---

require(graphics)
library(sm)
attach(mtcars)
library("lubridate")
require(ggplot2)
library(reshape2)
library("rmarkdown")

#' Question 1
#'  Watch the video presentation by Tamara Munzner: 
#'  Keynote on Visualization Principles, https://vimeo.com/26205288 and slides 
#'  - http://www.cs.ubc.ca/~tmm/talks/vizbi11/vizbi11.pdf 
#'  Summarize the key take-home messages from her presentation.

#

#'The Lecture talks about Visualization Principles,
#' In summmary it talks about 7 principles of (data) visualization:
#' • know your visual channel types and ranks 
#' • categorical color constraints
#' • power of the plane
#' • danger of depth
#' • resolution beats immersion 
#' • eyes beat memory
#' • validate against the right threat.
#'  In general one may consider what kind of plot the want to use for a given task,
#'  is it convinient? does it interprate right what we want to show about some given data?
#'  Many collors don't always make things easy to see, it can sometimes create a mess instead of beatifying whatever we are doing.
#'  or shoutld you use one shape and many colors or just on shape and one color, or two shapes and one color for a better interpretation.
#'  

#
#
#

#' Question 2
#' Fetch the UK child height/weight measurements data from here. (File).
#' Study a sufficiently large subset of measurements - 
#' plot dotplots comparing age, height, weight, BMI;
#' Experiment with color - highlighting gender, BMI clinical score, 
#' or age, by different markers, color or color scales.
#' (hint: you may want to develop ideas first with smaller subsets of data).
df = read.csv("ncmp_1415_final_non_disclosive.csv")
ss = df
#' I had to remove some outlier because some of the calculations were
#' 
#' 
#' 
noOutliers = ss[ss$ageinmonths>=0 & ss$height>=0 & ss$weight>=0 & ss$bmi>=0,]

#'  Gender
hage = head(noOutliers$ageinmonths, n=200L)
tage = tail(noOutliers$ageinmonths , n = 200L)
sage = sample(noOutliers$ageinmonths, 200)

dotchart(hage,cex=.45,main="First 200 Age in dataset. ",xlab="Age ")
dotchart(tage,cex=.45,main="Last 200 Age in dataset. ",xlab="Age")
dotchart(sage,cex=.45,main="Randomly picked 200 Ages over dataset ",xlab="Age")
#' all are around 60 and around 120 ~ 140 months 


#' Heights 
subset = head(noOutliers$height, n=200L)
subset2 = tail(noOutliers$height , n = 200L)
subset3 = sample(noOutliers$height, 200)
dotchart(subset,cex=.45,main="First 200 heights in dataset. ",xlab="Height ")
dotchart(subset2,cex=.45,main="Last 200 heights in dataset. ",xlab="Height")
dotchart(subset3,cex=.45,main="Randomly picked 200 heights over dataset ",xlab="Height")

#' there are some crazy heights which are negative something, for both and tale but in general,
#' there is one group which surrounds 110 and a second gourp which is between 
#' 140 ~ 150...

# 

#' Weight 
hwieght = head(noOutliers$weight, n=200L)
tweight = tail(noOutliers$weight , n = 200L)
sweight = sample(noOutliers$weight, 200)

dotchart(hwieght,cex=.45,main="First 200 Wieght in dataset. ",xlab="Weight ")
# this is strange cause there are some negative weights 
dotchart(tweight,cex=.45,main="Last 200 Wieght in dataset. ",xlab="Weight")
# this is strange cause there are some negative weights 
dotchart(sweight,cex=.45,main="Randomly picked 200 Weight over dataset ",xlab="Weight")
#' most of them are concentrated around 10 ~ 30, but for the rest they are scattered around,
#' 40 and some unique ones around 80

#

#' BMI
hbmi = head(noOutliers$bmi, n=200L)
tbmi = tail(noOutliers$bmi , n = 200L)
sbmi = sample(noOutliers$bmi, 100)
dotchart(hbmi,cex=.45,main="First 200 BMI in dataset. ",xlab="BMI ")
dotchart(tbmi,cex=.45,main="Last 200 BMI in dataset. ",xlab="BMI")
dotchart(sbmi,cex=.45,main="Randomly picked 200 BMI over dataset ",xlab="BMI")
#' BMI clusters around 15 more than the rest 



#' Color highlighing 
#' Gender BMI Clinical score 

gbmi = table(noOutliers$genderdescription,noOutliers$bmiclinicalcategory)
dotchart(gbmi, cex = .7, color = c(2,3))

#' Given the the dotcahrt here, I have realize that there are 4 categories:
#' healthy wieght, overwieght, underwieght, very overweight:
#' healthy wieght: The are more males who are healthier than the females
#' and it is the same case for males when it is about being overwieght and very overwieght 
#' but we have the same or almost the same amount of males and females who are underwieght.
#' I can also see that there are more overweight and and very overweight than the underwieght,
#' although all are not really healthy.
#

#
#
#

#' 3. Derive new features to plot: height*weight ; 
#' height/weight; Body Surface Area ; Plot them against each other; 
#' and against BMI, height, or weight. 
#' Try to identify interesting meaningful trends or examples, 
#' provide some interpretation.
#
#' Height and weight charts can determine whether you’re the correct weight for your height. 
#' Healthcare providers use these tools to monitor:
#' childhood growth and development
#' weight management
#' weight loss

clion = c(noOutliers$height * noOutliers$weight)
cmonkey = c(noOutliers$height/noOutliers$weight)
bsa = sample(sqrt(clion/3600), 1000)
plot(density(bsa))
plot(bsa)
scatter.smooth(bsa)
dotchart(bsa)


#'Average body surface area for children (9 years): 1.07 m2
#'Average body surface area for children (10 years): 1.14 m2
#'Average body surface area for children (12-13 years): 1.33 m2
#' My BSA study shows that most of the children in the UK are healthy.
#' With the largest part of them being the ones around 4 years old and the second largest part 
#' is the ones between 10 and 13 years old, which matches almost perfectly the details mentioned previously.

#

#' 4. Normalise height and weight based on the gender and age,
#'  repeat some of the plots from above.

tepandi = noOutliers
tepandi$height = (tepandi$height-min(tepandi$height))/(max(tepandi$height)-min(tepandi$height))
tepandi$weight = (tepandi$weight-min(tepandi$weight))/(max(tepandi$weight)-min(tepandi$weight))

#'  Gender
hage = head(tepandi$ageinmonths, n=200L)
tage = tail(tepandi$ageinmonths , n = 200L)
sage = sample(tepandi$ageinmonths, 200)

dotchart(hage,cex=.45,main="First 200 Age in dataset. ",xlab="Age ")
dotchart(tage,cex=.45,main="Last 200 Age in dataset. ",xlab="Age")
dotchart(sage,cex=.45,main="Randomly picked 200 Ages over dataset ",xlab="Age")



#' Heights 
subset = head(tepandi$height, n=200L)
subset2 = tail(tepandi$height , n = 200L)
subset3 = sample(tepandi$height, 200)
dotchart(subset,cex=.45,main="First 200 heights in dataset. ",xlab="Height ")
dotchart(subset2,cex=.45,main="Last 200 heights in dataset. ",xlab="Height")
dotchart(subset3,cex=.45,main="Randomly picked 200 heights over dataset ",xlab="Height")



#' Weight 
hwieght = head(tepandi$weight, n=200L)
tweight = tail(tepandi$weight , n = 200L)
sweight = sample(tepandi$weight, 200)

dotchart(hwieght,cex=.45,main="First 200 Wieght in dataset. ",xlab="Weight ")
# this is strange cause there are some negative weights 
dotchart(tweight,cex=.45,main="Last 200 Wieght in dataset. ",xlab="Weight")
# this is strange cause there are some negative weights 
dotchart(sweight,cex=.45,main="Randomly picked 200 Weight over dataset ",xlab="Weight")


#' BMI
hbmi = head(tepandi$bmi, n=200L)
tbmi = tail(tepandi$bmi , n = 200L)
sbmi = sample(tepandi$bmi, 100)
dotchart(hbmi,cex=.45,main="First 200 BMI in dataset. ",xlab="BMI ")
dotchart(tbmi,cex=.45,main="Last 200 BMI in dataset. ",xlab="BMI")
dotchart(sbmi,cex=.45,main="Randomly picked 200 BMI over dataset ",xlab="BMI")
#' BMI clusters around 15 more than the rest 



#' Color highlighing 
#' Gender BMI Clinical score 
gbmi = table(tepandi$genderdescription,tepandi$bmiclinicalcategory)
dotchart(gbmi, cex = .7, color = c(2,3))



#' I don't think there is a major change after normalization, what I have realize that 
#' there is a kind of more concentration around the area that I have monetionned 
#' when I plotted everything previously in question 2.

#
#
#

#' 5. Draw approximate growth curves over age.
#'  Calculate and plot growth curves of the different deciles 
#'  (0%-10%, 10%-20%, 20%-30%, ...90%-100%) of obesity 
#'  categories both by BMI and by weight, for both genders.

decLocations = quantile(noOutliers$bmi, probs = seq(0.1,0.9,by=0.1))
noOutliers$decile = findInterval(noOutliers$bmi,c(-Inf,decLocations, Inf))
maleOnly = noOutliers[noOutliers$genderdescription=="Male",]
femaleOnly = noOutliers[noOutliers$genderdescription=="Female",]

ggplot(data = noOutliers, aes(x = ageinmonths, y = bmi)) + geom_line() +
  facet_wrap(~decile)

ggplot(data = maleOnly, aes(x = ageinmonths, y = bmi)) + geom_line() +
  facet_wrap(~decile)

ggplot(data = femaleOnly, aes(x = ageinmonths, y = bmi)) + geom_line() +
  facet_wrap(~decile)

#'For the males the most meaningful changes in weight is of course in the 10th decile, and we can also see
#'some meaningful changes in the first and  the 9th decile, both according to the two dominant age groups.
#'for the rest the changes are not really great but they still meaningful. In comparison to the females, the level of fatness
#'is really huge in the 10 the decile the first and the 9th, it is really more noticeable here than former group.
#'


#
#
#

#'6. Last saturtday I took part into the open data, I have attached the pdf file. 
