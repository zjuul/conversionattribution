# multi channel attribution and path conversion rate calculation.
#
# see http://stuifbergen.succesvol-online.nl/2016/11/conversion-attribution-markov-model-r

# install.packages('googleAnalyticsR')
# install.packages('ChannelAttribution')

library(googleAnalyticsR)
library(ChannelAttribution)

# auth first - this opens a browser (first time)
# ga_auth(new_user = TRUE)
# this one doesn't (refreshes token)
ga_auth()

# view id of your Google Analytics view where 1 conversion = visit
vid <- "115546653"

# transaction filter
trans_filter <- "mcf:conversionType==Transaction" # the conversion you're interested in
visit_filter <- "mcf:conversionGoalNumber==012"   # "visit" conversion

# date range
from <- "2016-09-01"
to   <- "2016-09-30"

# what do you want to know the attribution of?
dim = "sourceMediumPath"

# the function
get_data <- function(vid, from, to, filters = "",
                     dim = "sourceMediumPath", max = 5000) {
  df <- google_analytics(id = vid, 
                         start = from, end = to, 
                         metrics = c("totalConversions"), 
                         dimensions = dim,
                         filters = filters,
                         type="mcf",
                         max_results = max)
  # clean up and set class
  df[,1] <- gsub(" / ", "/", df[,1])              # remove spacing
  df[,1] <- gsub(":?(NA|CLICK|NA):?", "", df[,1]) # remove CLICK and NA
  df[,2] <- as.numeric(df[,2])                    # conversion column is character :-/
  
  # return the dataframe
  df
}

# get transactions
transactions <- get_data(vid=vid, from=from, to=to, dim=dim, filters=trans_filter)
colnames(transactions) <- c("path", "transactions")

# get visits (remember: this view has a goal where a visit = a conversion)
visits       <- get_data(vid=vid, from=from, to=to, dim=dim, filters=visit_filter)
colnames(visits) <- c("path", "visits")

# merge dataframes, based on path
alldata <- merge.data.frame(visits, transactions, by = "path", all=T)
# not all visit paths have conversions. Change those NA's to zero.
alldata[is.na(alldata$transactions), "transactions"] <- 0

# calculate conversion rate
alldata$rate <- alldata$transactions / alldata$visits
# null = visits without transaction
alldata$null  <- alldata$visits - alldata$transactions

# run the markov model
mm <- markov_model(alldata, var_path = "path",
                   var_conv = "transactions",
                   #var_value = "value", #use this if you have conversion values
                   var_null = "null",
                   order=1, nsim=NULL, max_step=NULL, out_more=FALSE)


# run the heuristic model
hm <- heuristic_models(alldata, var_path = "path",
                       #var_value = "value",
                       var_conv = "transactions")

# merge mm + hm, and voila.. your modeled acquisition sources
modeled <- merge.data.frame(hm, mm, all=T, by="channel_name")

# and View it (Rstudio)
View(alldata) # the conversion rate of each path
View(modeled) # the attribution table per channel
