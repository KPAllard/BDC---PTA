# Big Data Challenge
# Paige Allard, Ahmed Faarah, and Tully Henke
# Statistical Analysis with R - Veen
# Quest University Canada
# December 11, 2018

# ==== PROJECT, PACKAGES, AND VERSIONS ====
# This project is an analysis of the relationship between astronomical phenomena and UFO sightings.
# The project focused on solar flux and meteor showers as two examples of these phenomena
# UFO sightings data was sourced from Kaggle, meteor shower data from IMO.net, and solar flux data from NOAA
# This script requires the following files in the working directory: "srubbed.csv", "naoo_radio_flux.csv", and "Rate-IMO-VMDB-2005-1-2-2010-1-1.csv"

# Version Used
  R.version.string 
    # "R version 3.5.1 (2018-07-02)"
# Libraries Used
  library(stringr)  
  library(dplyr)

# ==== DEFINING PATHS ====
# Get working directory and assign variable
p.working.dir <- getwd()

# Assign vector of project folder names
output.folder.names <- c( "Raw Data","Clean Data","Figures","Fxns.Packages.Versions")

# Create folders if not yet made
for(i in 1:length(output.folder.names)){
  if(!file.exists(output.folder.names[i])){
    print("Folder Created") # When folders are made this message will print
    print(output.folder.names[i])
    dir.create(output.folder.names[i])
  }
}

# Assign variables for folder paths within working directory
p.raw.data <- paste(p.working.dir, "/Raw Data", sep = "")
p.clean.data <- paste(p.working.dir, "/Clean Data", sep = "")
p.figures <- paste(p.working.dir, "/Figures", sep = "")
p.fxns.packages.versions <- paste(p.working.dir, "/Fxns.Packages.Versions", sep = "")


# #### SOLAR FLUX ####

# ==== SOLAR FLUX DATA CLEANUP =======
# Read csv file from working directory and view top lines
flux <- read.csv("noaa_radio_flux.csv")
head(flux)

# We have dates listed all as one within this data set so we must create a new column for just year and then again for just month
# For this we will use the structure subset function which will create a new column from the range of noted noted elements
# For year we seperate it into a new coloum from the 1st element to the fourth
flux$year <- str_sub(flux$time..yyyy.MM.dd., start = 1, end = 4)

# We now create a new column just for months by subsets the 6th to the 7th elements in the time..yyyy.MM.dd. column
flux$month <- str_sub(flux$time..yyyy.MM.dd., start = 6, end = 7)

# We then check to make sure these operations worked properly by viewing the top rows again
head(flux)

# We now have our yea and month in seperate columns but we want them to be connected so we use the paste function to connect them
flux$year.month <- paste(flux$year, flux$month, sep = "")

# This gives us a standardized date format of yearmm which we can use for both of our data sets. 
head(flux)

# We need to make sure we have loaded dplyr package to use the which() function
library(dplyr)

# Because N/A flux value are listed as -99999 we want to remove these from our data sets 
# First use the which function to create an object of the rows with this -99999 value in them
row.to.remove <- which(flux$f107..solar.flux.unit..SFU..== -99999)
str(row.to.remove)
head(row.to.remove)

# Now we can actually remove them by 'subtracting' them from the flux data.frame and creating a new data frame without them
new.flux <- flux [-row.to.remove,]

# We want to analyze our data by the average monthly solar flux, to sort this we use a pipe. First grouping all of the rows by their data
# using the group_by function and then taking the mean of that. Create a new object from these modifications called groupedflux
groupedflux <- new.flux %>%
  group_by(year.month) %>%
  summarise_at(vars(f107..solar.flux.unit..SFU..), funs(mean(., na.rm=TRUE)))

# We can plot this to see what the monthly averages look like over time
plot(groupedflux)


# #### UFOs ####
# ==== UFO DATA CLEANUP ====
# We first read the csv file from the working directory and view the top few rows
UFO.data <- read.csv("scrubbed.csv")
head(UFO.data)

# We can then remove unecessary columns from the data frame
cols.dont.want <- c("shape", "duration..seconds","comments", "duration..hours.min.","latitude","longitude","date.posted")
UFO.data.lesscol <- UFO.data[,!names(UFO.data) %in% cols.dont.want, drop=F]
# We can check that these columns have been removed
head(UFO.data.lesscol)

# We then want to remove all but US observations (in country column)
rows2remove <- which(!UFO.data.lesscol$country=="us") # find which rows do not contain "us" in the country column
rows2remove
UFO.us <- UFO.data.lesscol[-rows2remove, ] # remove these rows from the data frame
# We can then view the new data frame to be sure rows were removed 
head(UFO.us)

# We can isolate year and month from the datetime column by first trimming the dates away from the time data
str_sub(UFO.us$datetime, start=1, end=-7)
trimmed.dates.t <- str_sub(UFO.us$datetime, start=1, end=-7)

# We can turn these trimmed dates into a date format to assure they are properly formatted before trimming further
strDates.t <- trimmed.dates.t
dates.t <- as.Date(strDates.t, "%m/%d/%Y")
dates.t # variable created with newly formatted dates contained in it

# We can then assign these formatted dates to a new column within the data frame
UFO.us$newdates <- dates.t
# And view the top of our data frame again
head(UFO.us)

# We can then generate a year and a month column from these dates using the str_sub function
UFO.us$Year <- str_sub(UFO.us$newdates, start=1, end=4)
UFO.us$Month <- str_sub(UFO.us$newdates, start=6, end=7)
# We then make a yearmm formatted column that will be comparable to the dates from our other data frames
UFO.us$year.month <- paste(UFO.us$Year, UFO.us$Month, sep = "")

# We can make sure that we properly generated 12 months by viewing the table
table(UFO.us$Month)
# And check our new columns
head(UFO.us)


# #### METEORS ####
# ==== METEOR DATA CLEANUP ====
# Read semicolon seperated csv from working directory using “read.table” with a “;” seperator
Meteor.Rate.20050102.20100101 <- read.table("Rate-IMO-VMDB-2005-1-2-2010-1-1.csv", sep=";", header = T)
# Check that file has been read correctly
head(Meteor.Rate.20050102.20100101)
# Update column names
meteor.col.names.vect <- c("Rate.ID", "User.ID", "Obs.Session.ID", "Start.Date", "End.Date","Ra","Decl","Teff","F","Lm","Shower","Method","Number")
colnames(Meteor.Rate.20050102.20100101) <- meteor.col.names.vect
# Confirm file has been properly formatted
head(Meteor.Rate.20050102.20100101)

# Save new csv file into “Raw Data” folder within project
write.csv(Meteor.Rate.20050102.20100101, paste(p.raw.data, "/", "Meteor.Rate.20050102.20100101.csv", sep = ""))

# Reopen saved csv file to assure it saved properly
Meteor.Rate.20050102.20100101 <- read.csv(paste(p.working.dir, "/", "Raw Data/Meteor.Rate.20050102.20100101.csv", sep = ""))
# Check that file loaded correctly
head(Meteor.Rate.20050102.20100101)

# Make sure we have loaded “stringr” library
library(stringr)
# We can then cut year out of start date and into new year column using sub_str
Meteor.Rate.20050102.20100101$Year <- str_sub(Meteor.Rate.20050102.20100101$Start.Date, start=1, end=4)
# And do the same cutting month out of start date and into new month column
Meteor.Rate.20050102.20100101$Month <- str_sub(Meteor.Rate.20050102.20100101$Start.Date, start=6, end=7)
# We can confirm that the 12 months divided correctly
table(Meteor.Rate.20050102.20100101$Month)
# And check updated columns of file
head(Meteor.Rate.20050102.20100101)

# We then create new yearmm format column pasting year together with month to make this comparable to our other data frames
Meteor.Rate.20050102.20100101$year.month <- paste(Meteor.Rate.20050102.20100101$Year, Meteor.Rate.20050102.20100101$Month, sep = "")
# And confirm that this worked correctly
head(Meteor.Rate.20050102.20100101)

# We can then trim down our columns to only have those necessary
cols.delete <- c("User.ID", "End.Date","Ra", "Decl","Teff","F","Shower", "Method", "Obs.Session.ID", "Lm", "Number")
Meteor.Rate.20050102.20100101.abridged <- Meteor.Rate.20050102.20100101[,!names(Meteor.Rate.20050102.20100101) %in% cols.delete, drop=F]
head(Meteor.Rate.20050102.20100101.abridged)

# ==== MERGE CODE FOR UFOS AND FLUX ====
# We use the "merge" function to filter overlapping dates for UFOs and solar flux indicating that we merge by the shared format year.month column
flux.UFO <- merge(UFO.us, groupedflux, by = "year.month")
# And check that this worked properly
head(flux.UFO)

# View frequency distribution of merged data using "hist" function
hist(flux.UFO$f107..solar.flux.unit..SFU.., breaks = 40, xlim = c(0, max(flux.UFO$f107..solar.flux.unit..SFU..)))
# Check range of flux values in merge
range(flux.UFO$f107..solar.flux.unit..SFU..)

# Assign function for drawing normal distribution
draw.norm.dist.t <- function(mean,sd){
  equation.norm.dist = function(Y)((1/sqrt(2*pi*(sd)^2))*exp(-(((Y-mean)^2)/(2*(sd^2)))))
  return(plot(equation.norm.dist,xlim = c(0,max(flux.UFO$f107..solar.flux.unit..SFU..)),ylab="Probability Density"))
}

# Define plot area using "par" function
par(mfrow=c(2,1), mar = c(4, 4, 2, 1), cex = 0.7) # define two rows of plots

# Find standard deviation and mean to draw normal distribution for all flux data
st.dv.t <- sd(groupedflux$f107..solar.flux.unit..SFU..) # standard deviation
mn.fl.t <- mean(groupedflux$f107..solar.flux.unit..SFU..) # mean
draw.norm.dist.t(mean = mn.fl.t, sd = st.dv.t) # draw normal distribution curve
abline(v=mn.fl.t) # draw line at mean

# Normal distribution for merged data
st.dv.merge.t <- sd(flux.UFO$f107..solar.flux.unit..SFU..) # standard deviation
mn.fl.merge.t <- mean(flux.UFO$f107..solar.flux.unit..SFU..) # mean
draw.norm.dist.t(mean = mn.fl.merge.t, sd = st.dv.merge.t) # draw normal distribution curve
abline(v=mn.fl.merge.t) # draw line at mean

# Run t-test to check statistical significance
t.test(groupedflux$f107..solar.flux.unit..SFU..,flux.UFO$f107..solar.flux.unit..SFU..)
# t = 6.5811, df = 870.79, p-value = 8.054e-11

# Effect size absolute value
abs.effect.sz.t <- mn.fl.t-mn.fl.merge.t
# 11.32706
# Pooled standard deviation
pooled.sd.t <- ((st.dv.merge.t)^2+(st.dv.t)^2)/2

# Determine actual size of effect using Cohen's effect size
abs.effect.sz.t/pooled.sd.t
# = 0.005588175
  # Note that a small effect size corresponds to a value of 0.2, a medium to 0.5, and a large to 0.8

# ==== NULL DISTRIBUTION ====
# We can also generate a null distribution of random samples, limiting our need to assume normal distribution
number.of.iterations.t <- 100000

# Generate random sample vector of length number of iterations
flux.sample.vector.t <- c(1:number.of.iterations.t)
for (i in 1:number.of.iterations.t){
  # i <- 1
  flux.sample.vector.t[i] <- mean(sample(groupedflux$f107..solar.flux.unit..SFU.., size = 100))
}

# View resulting vector to be sure it sampled properly
flux.sample.vector.t
# Visualize sampling distribution
par(mar=c(5,5,2,1))
hist(flux.sample.vector.t, breaks = 150, xlim=c(105,145), main = NULL, xlab = "Mean Flux Value", cex.lab=1.5, cex.axis=1 )
# And add the mean value of UFO flux days as a vertical line
abline(v=mean(flux.UFO$f107..solar.flux.unit..SFU..))

# Determining the p-value for our UFO mean flux value (vertical line)
# Non proportional value
non.prop.p.value <- (sum(flux.sample.vector.t<mn.fl.merge.t)+1)*2
# P-value controlled for sample size
non.prop.p.value/sum(flux.sample.vector.t)
# = 0.0001166934
  # NOTE: This will vary slightly depending on results from sampling distribution

# Calculating effect size
mn.flux.dist.t <- mean(flux.sample.vector.t)
sd.flux.dist.t <- sd(flux.sample.vector.t)
mn.fl.merge.t <- mean(flux.UFO$f107..solar.flux.unit..SFU..)
st.dv.merge.t <- sd(flux.UFO$f107..solar.flux.unit..SFU..)

# Effect size absolute value
abs.effect.sz.dist.t <- mn.flux.dist.t-mn.fl.merge.t
# 11.29714
  # NOTE: This will vary slightly depending on results from sampling distribution
# Pooled standard deviation
pooled.sd.dist.t <- ((st.dv.merge.t)^2+(sd.flux.dist.t)^2)/2

# Determine actual size of effect
abs.effect.sz.dist.t/pooled.sd.dist.t
# = 0.01453182
  # NOTE: This will vary slightly depending on results from sampling distribution

# ==== MERGE CODE FOR UFOS AND METEORS ====
# Assure that data frames have already been properly assigned
head(Meteor.Rate.20050102.20100101.abridged)
head(UFO.us)

# Generate data frame of monthly meteor shower frequency from table of months
Meteor.Monthly.Date.Frame <- as.data.frame(table(Meteor.Rate.20050102.20100101.abridged$year.month))
Meteor.Monthly.Date.Frame
# This can be visualized
plot(Meteor.Monthly.Date.Frame)

# Repeat this for UFO data by month
UFO.us.monthly.data.frame <- as.data.frame(table(UFO.us$year.month))
UFO.us.monthly.data.frame
# Visualize
plot(UFO.us.monthly.data.frame)

# Use the merge function to combine these by year.month value
UFO.Meteor.Monthly.Merge <- merge(UFO.us.monthly.data.frame,Meteor.Monthly.Date.Frame, by="Var1")
UFO.Meteor.Monthly.Merge
# NOTE: COlumn names Freq.x=UFOs and Freq.y=Meteors

# Correlation
cor(UFO.Meteor.Monthly.Merge$Freq.y, UFO.Meteor.Monthly.Merge$Freq.x)
# = 0.203079

# Generate a linear model for the relationship between meteors and UFOs
lm.UFO.Meteor.t <- lm(UFO.Meteor.Monthly.Merge$Freq.x ~ UFO.Meteor.Monthly.Merge$Freq.y)
# Check model values using “summary” function
summary(lm.UFO.Meteor.t)
  #Multiple R-squared:  0.04124,	Adjusted R-squared:  0.02471 
  # F-statistic: 2.495 on 1 and 58 DF,  p-value: 0.1197

# ==== VISUALS CODE ====
# Drawing normal distributions of flux and UFOs
{
draw.2.norm.dist.t <- function(mean1,sd1,mean2,sd2){
  equation1.norm.dist = function(Y)((1/sqrt(2*pi*(sd1)^2))*exp(-(((Y-mean1)^2)/(2*(sd1^2)))))
  plot(equation1.norm.dist,xlim = c(0,max(flux.UFO$f107..solar.flux.unit..SFU..)),ylim=c(0,0.01),ylab="Probability Density",xlab = "Flux Value (SFUs)", col="blue", lwd=4)
  par(new=TRUE)
  equation2.norm.dist = function(Y)((1/sqrt(2*pi*(sd2)^2))*exp(-(((Y-mean2)^2)/(2*(sd2^2)))))
  plot(equation2.norm.dist,xlim = c(0,max(flux.UFO$f107..solar.flux.unit..SFU..)),ylim=c(0,0.01),ylab="Probability Density", col="red", lwd=4)
}

draw.2.norm.dist.t(mean1 = mn.fl.t,sd1 = st.dv.t,mean2 = mn.fl.merge.t,sd2 = st.dv.merge.t)  
abline(v=mn.fl.t, col="blue", lty="dashed", lwd=2)
abline(v=mn.fl.merge.t, col="red", lty="dashed", lwd=2)

# Click to add legend after running
legend( locator(1), legend = c("UFO Distribution", "Flux Distribution"), 
        pch = 16, 
        col=c("blue","red"), cex=1)
}
# Graphing Meteors vs UFOs
{
  # Scatter correlation plot
  {
  # Plot meteors vs UFOs
  plot(UFO.Meteor.Monthly.Merge$Freq.y, UFO.Meteor.Monthly.Merge$Freq.x, xlab = "Meteor Frequency by Month", ylab = "UFO Frequency by Month")
  abline(lm(UFO.Meteor.Monthly.Merge$Freq.x ~ UFO.Meteor.Monthly.Merge$Freq.y))
  cor(UFO.Meteor.Monthly.Merge$Freq.y, UFO.Meteor.Monthly.Merge$Freq.x)
  }
  
  # Barplot
  {
    par( mar=c(5,5,2,5)) # defining plot margins
    barplot(UFO.Meteor.Monthly.Merge$Freq.x, col = "grey", ylab = "UFO Frequency by Month", xlab = ) # plot UFO frequency
    par(new=TRUE) # indicate new plot
    barplot(UFO.Meteor.Monthly.Merge$Freq.y, col = "#FF450080" , axes=FALSE) # plot meteor frequency without axes
    axis(side=4) # add second y-axis to right side
    mtext(side = 4, line = 3, "Meteor Frequency by Month") # label new axis
    axis(side=1,labels = c(2005,2006, 2007, 2008, 2009, 2010),at = c(0,72/5,2*72/5,3*72/5,4*72/5,72)) # define x-axis and labels
    mtext(side = 1, line = 3, "Monthly Data from 2005-2010") # add x-axis label
  
  # Add the legend in two components to customize spacing
  # Click on graph after running each legend to place it
  legend( locator(1), legend = c("UFO Frequency"), 
          pch = 16, 
          col=c("grey"), cex=2, bty = "n")
  legend( locator(1), legend = c("Meteor Frequency"), 
          pch = 16, 
          col=c("#FF450080"), cex=2, bty = "n")
  }
}