
# UFO data ====
UFO.data.1 <- read.csv("scrubbed.csv")
getwd()
head(UFO.data.1)


table(UFO.data.1$country)

table(UFO.data.1$shape)
  

# Solar flux data ====
Solar.Flux.Data.1 <- read.csv("noaa_radio_flux.csv")
head(Solar.Flux.Data.1)
plot(Solar.Flux.Data.1$f107..solar.flux.unit..SFU.., ylim = c(50,400))

rows.to.remove <- which(Solar.Flux.Data.1$f107..solar.flux.unit..SFU..==-99999)

Solar.Flux.Data.Minus.Negatives <- Solar.Flux.Data.1[-rows.to.remove,]
Solar.Flux.Data.1$time..yyyy.MM.dd.

plot(Solar.Flux.Data.Minus.Negatives)

# Meteor Showers Data ====
Meteor.Rate.20050102.20100101 <- read.table("Rate-IMO-VMDB-2005-1-2-2010-1-1.csv", sep=";", header = T)
head(Meteor.Rate.20050102.20100101)
meteor.col.names.vect <- c("Rate ID", "User ID", "Obs Session ID", "Start Date", "End Date","Ra","Decl","Teff","F","Lm","Shower","Method","Number")
colnames(Meteor.Rate.20050102.20100101) <- meteor.col.names.vect
Meteor.Rate.20050102.20100101[1,]<-NULL
head(Meteor.Rate.20050102.20100101)

write.csv(Meteor.Rate.20050102.20100101, paste(p.raw.data, "/", "Meteor.Rate.20050102.20100101.csv", sep = ""))

# Fixing date formats ====
  UFO.data.1[2000,1]
  # UFO Date format: MONTH/DAY/YEAR TIME
  Solar.Flux.Data.1$time..yyyy.MM.dd.
  # Solar Flux Date Format: YEAR MONTH DAY
  as.Date(UFO.data.1[2000,1])
  strsplit(UFO.data.1[2000,1]," ")[[1]][1]
  gsub('([0-9]+) .*', '\\1', UFO.data.1[2000,1])
  UFO.data.1[2000,1]  
  gsub('([0-9]+) .*', '\\1', Solar.Flux.Data.1$time..yyyy.MM.dd.)
  
  # Converting dates
# Cutting Date Solution ====
  library(stringr)
  str_sub(UFO.data.1[2000,1], start=1, end=-3)
  
  str_sub(UFO.data.1$datetime, start=1, end=-7)
  trimmed.dates.t <- str_sub(UFO.data.1$datetime, start=1, end=-7)
  
  
strDates.t <- trimmed.dates.t
dates.t <- as.Date(strDates.t, "%m/%d/%Y")
dates.t

UFO.data.1$newdates <- dates.t
head(UFO.data.1)

# Adding New Dates To UFO Data
cols.dont.want <- c("shape", "duration..seconds","comments", "duration..hours.min.","latitude","longitude","date.posted")
UFO.us <- UFO.data.1[,!names(UFO.data.1) %in% cols.dont.want, drop=F]
UFO.us.dates <- UFO.us$newdates
UFO.us.dates
head(UFO.us)



# Notes from Thor ====
#substr
  # Subdivide when there is a dash
  # "R split string by symbol
    # strsplit
# Read.table
  # "read semicolon seperated file"

table(UFO.data.1$state)
