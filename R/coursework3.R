#' ---
#' title: "Coursework 3"
#' author: "Fortunat Mutunda"
#' date: "Feb, 28 2016"
#' #' output: pdf_document
#' ---

require(graphics)
library(sm)
attach(mtcars)
library("lubridate")
require(ggplot2)
library(reshape2)
library("rmarkdown")


#' ***Question 1 ***
#' Kernel density estimation: use these two data sets - klient1.txt and klient3.txt. 
#' Plot the density distribution of these two data sets. 
#' This represents the time of week,when two different groups of people go 
#' shopping over the entire year. Choose an informative kernel width and justify
#'  your choice.Characterise briefly these two data sets (klient1 and klient3)?


x = read.table("klient1.txt")
y =  read.table("klient3.txt")
dfx = as.data.frame(x)
dfy = as.data.frame(y)
##densities
dsx <- density(dfx$V1, main = "Klient 1")
dsy <- density(dfy$V1, main  ="klient 3")
#' plots 
plot(dsx)
polygon(dsx, col="red", border="blue")
plot(dsy)
polygon(dsy,col = "magenta", border = "cyan")

#' merged plots

plot(dsx)
plot(dsy)
polygon(dsx, col="red", border="blue")
polygon(dsy,col = "magenta", border = "cyan")

#' As we can see them merged here up there.
#' at a simple glimpse I notice that there is not a huge difference between the
#'  buying habbits of these two types of buyers
#' the two gaussian kernles 
#' smoothed or ajusted kernels 
smoothed_dfx = density(dfx$V1, adjust = 20, kernel = "gaussian")
plot(smoothed_dfx)
smoothed_dfy = density(dfy$V1, adjust = 20, kernel = "gaussian")
plot(smoothed_dfy)

#'I have reajusted the bamdwith to 20 because it looks way better that way, 
#'it looks like those ancient amercans pyramides
#'and In this sense it is more readable because It is just one sigle curve that
#'shows the Kernel's evolution although the two kernels represent two datasets
#'of two diffent kind of people going for shopping at different times withing a year
#'the kernels look very similar.
#' but a slight different which could be considered as insignificant 
#'depedning on what use we want to make out of it.
#
#' ***Question 2***
#' Study the data Attach:product_time_shop.txt. 
#' There is a information about a number of shops and times when particular 
#' products (items) sold through the week.
#' Describe the data - what products, shops,
#'  how many purchases of different products in different shops? 

shopt = read.csv(file = "product_time_shop.txt", sep = ";")
sumshop = summary(shopt)
(sumshop)
shoptFrame = as.data.frame(shopt)
#density(shoptFrame)
shoptDate = shoptFrame$date
shoptTime = shoptFrame$time
shoptProduct = shoptFrame$product
shoptId = shoptFrame$shop_id


#' describe the data:
#' The data describe the buying habits of customers 
producShopt = table(shoptId,shoptProduct)
thislegend = c(rownames(producShopt))
as.graphicsAnnot(thislegend)
barplot(producShopt, main="Product Distribution per shop",xlab="Product baught",
        beside=TRUE , col = rainbow(20))#, legend = thislegend)
#'in different shops throughout the year of 2014
#' the data informs us about 5 shops with different ids' (3,4,18,21,32),
#'  5 to be precise and
#' diffrent producrs sold among these shops, namely :
#' Banana, Coffee_Cream, Eggs_1, Eggs_2, Grapes, Milk_1, Milk_2, Sour_cream_1, 
#' Sour_cream_2, Vastlakukkel, Whipped_Cream
#' itemsShops = table(shoptProduct,as.Date(shoptFrame$date,origin = "20140104"))
#' barplot(itemsShops, col = c(2:6) , beside = TRUE, legend = thislegend )
#' heatmap(itemsShops)
#' these are the diffent sales of different products in different shops.

#

#' ***Question 3***
#'Draw boxplots that would allow comparing different weekdays, shops, 
#'and product sales. Identify some meaningful illustrations to draw conclusions about
#'1) different weekdays,
#'2) different products , 
#'3) shops. 
#'State your hypothesis and then draw respective analysis of data.

#' first I am going to get the time to the right format   
dates = parse_date_time(shopt$date, truncated = 3, orders = "Ymd",tz="Europe/Tallinn")
datesTable = table(dates)
boxplot(shoptTime ~ shoptProduct, ylab ="Time", 
        main="Time when prodcuts are most sold",  
        col = rainbow(20), las=2)
#' The creams are the products that are sold mostly in the morning and slightly all the way to before 3 pm
#' then comes grappes and Milk_1 which are sold somewhere close to 3pm and at 3 pm alongside sour_cream_1 and sour_cream_2
#' then the bananas and the rest come after that, the item that is sold very late is the Eggs_2, which are sold way after 3pm
boxplot((month(dates)) ~ shoptId, ylab = "Months" ,
        main = "Months on which shops are the most visited"  , 
        las = 2 , col = rainbow(20))

#'The distribution of the shop 3 and 4 is well,   and 21 and 32 as well
#'the shop with the id of 3 is the one with less sales.



#' weekdays 
shoptFrame = as.data.frame(shopt)
#weekdays = weekdays(as.Date(unique(as.character( dates))))
weekdays = as.Date(as.character(shoptFrame$date), format = "Ymd")
shoptFrame$date = factor(shoptFrame$date, levels=unique(shoptFrame$date))

#barplot(table(shoptFrame$date,shoptFrame$shop_id), beside = T , col= rainbow(20))

barplot( (distribution), col = rainbow(100), beside = T , 
         args.legend = table(shoptFrame$product))
boxplot.matrix( (distribution), col = rainbow(100), beside = F, 
                args.legend = table(shoptFrame$product))


#' products
products = table(shoptFrame$product, shoptFrame$shop_id)
boxplot.matrix(products, col = rainbow(20), main = "Product per shop"  )


#'According the these boxplots, my conclusion is as follow: 
#'Shop of id 4 is the one that makes the most sales comparingthe shop 21,
#'which has the lowest sells of all shop 3 comes as second which can be a bit
#'comparable to the shop 18, just a bit but shop 3 definitely makes the most sales 
#' and lastly shop 32 which has, I can say the average sales in general 

#' shops
shops = table(shoptFrame$time,shoptFrame$product)
boxplot.matrix(shops,col = rainbow(20))

#'The bananas and milk_2 are hot cakes throughout the whols day comparing the 
#'the Eggs_2 ehich sales almost nothing eggs_2 don't sale at all,
#' I can safely say that they can be removed totaly from the store,
#'  there is not going to be any loss



#' ***Question 4***
#'.Use the same data as in 2. Explore the data and identify if any of the shops 
#'has run out of any popular product during the  day 
#'(which shops, products, days?). Draw the density plots to convince the reader 
#'or shop manager. Formulate the principles 
#' of an automated procedure to identify (all) such events.

dayIDProduct = table(shoptFrame$date,shoptFrame$shop_id,shoptFrame$product)
plot(density(dayIDProduct))


#' After a study of different densities I decided to pick some two specific 
#' graphes of two different shops to show
#' that some product can run out during the day and it has to be refilled 
#' and some others not .


shop21 = subset(shoptFrame, shop_id == 21 & product == "Milk_1" & date == 20141231)
plot(density(shop21$time))

#' The graphe above presents an interesting curve, in the sense that there is a big
#' increase from 12 to 16 then slowly it starts to decrease, 
#' I guess that the time people drink most milk
#' the later it just increase a bit agin the totaly decrease, 
#' maybe people who have cats went to buy some milk for thier beloved pets
#' 
#' 
#' 
shop4 = subset(shoptFrame, shop_id == 4 & product == "Banana" & date == 20140407)
plot(density(shop4$time))

#' This seconde plot has a very different curve, 
#' from morning it starts increasing bit by bit till
#' it gets somewhere close to 20 then there is a sharp decrease, this shows 
#' that the more people go to the shop the more they get to buy the bananas.
#' then later at night they just stop because it is time to sleep I guess.



#' ***Question 5***
#'  Calculate the nr of purchases of each product during every day 
#'  and every hour in each shop. 
#'  Make a table with a product sold in that day in particular shop as rows
#'   (one row for every day) and time by hour as columns; 
#'  In each cell a nr of purchases in that hour. 
#'  Draw a heatmap version of the sales data. E.g. use Excel "Conditional 
#'  formatting" => "Color Scales"


shoptFrame$time =  round(shoptFrame$time/100)
purchases_per_hour = dcast( shoptFrame, product+shop_id+date~time)

row.names(purchases_per_hour) = paste( purchases_per_hour$product,
                                       purchases_per_hour$shop_id,
                                       purchases_per_hour$date, sep=" ")

x  <- as.matrix(purchases_per_hour[,4:20])
rc <- rainbow(20, start = 0, end = .3)
cc <- rainbow(ncol(x), start = 0, end = .3)
hv <- heatmap(x[1:20,], scale = "column",
              RowSideColors = rc, ColSideColors = cc, margins = c(5,10),
              xlab = "specification variables", ylab =  "Products ",
              main = "heatmap(<Products>, ..., scale = \"column\")")
utils::str(hv) # the two re-ordering index vectors

#'here is a chunk of a heatmap that shows the sales Bananas for shop 3
#'and 4 during a certain day from the time of the opeining of the shop
#'till it was closed.
#'For this particular question I just took a chunck of the date to show a heatmap
#'because showing a map for the enire dataframe wouldn't fit on a single heatmap
#'by many of them.


#' *** Question 6 ***
#'Bonus (2p). Clearly, people visit shops more frequently at certain times. 
#'This can obscure analysis.
#' 1. describe the overall visiting behavior of customers based on data from
#' 2. normalise frequency of purchases to reflect a relative share of purchasing
#'  that product. (state how)
#' 3. identify if different products are purchased in different relative 
#' frequency over the weekdays
#' does this normalisation help in task 
#' 4, to identify when shop has ran out of products?
#'
#' Let me first visualize a table of how frequent people visit shops 
#' at a certain time of the day

mytable <- table(shoptFrame$time, shoptFrame$shop_id)
margin2 = margin.table(mytable,1)
margin3 = margin.table(mytable,2)



plot(margin3,col =  rainbow(20) , 
     main = "The shop that recieve the most customers in a day" ,
     xlab = "Different shops ids ")
plot(margin2 , lines(margin2, type = "l") , col= "orange", 
     main = "Time with the most people throughout a day!")

#'1. In general the shop with the id of 4 is the most visited during everyday 
#'in opposite to shop 21 
#'which has the least cusomers of all. The second shop to have the most
#' custmores is shop 3, comes somewhere below 
#'shop 4. The rest two, shop 18 and 32  are average depending on the sales.

#' 2. After normalisation, I have concluded that the top sales go
#'  between 12 pm and 5 pm and that the curve is almost perfect, 
#'  it starts from and ends with zero but it is easily noticeable 
#' that sales are much more in the morning in opposite 
#' to when the shops are about to close late at 11 pm

#' 3 products and week days
#' 4 Well the normalization can help to better anticipate when the product has
#' run out and how this can be helpful to refill it. 












