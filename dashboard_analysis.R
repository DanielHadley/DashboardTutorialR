# Created By Daniel Hadley Thu Feb 25 10:03:25 EST 2016 #
setwd("/Users/DHadley/Github/DashboardTutorialR")
library(knitr)


# Load Data
dates <- c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
Tokyo <- c(7.0, 6.9, 9.5, 14.5, 18.2, 21.5, 25.2, 26.5, 23.3, 18.3, 13.9, 9.6)
London <- c(3.9, 4.2, 5.7, 8.5, 11.9, 15.2, 17.0, 16.6, 14.2, 10.3, 6.6, 4.8)

# Calculate an average
mean_of_Tokyo <- mean(Tokyo)
mean_of_London <- mean(London)


## Now knit together the data and HTML ##
knit("./index.Rhtml", output = "./index.html")

# Now you can upload index.html to your server 
