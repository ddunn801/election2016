
#https://www.kaggle.com/joelwilson/2012-2016-presidential-elections
# Prepare -----------------------------------------------------------------
rm(list = ls())
gc()
pkg <- c("tidyverse", "data.table", "ggplot2", "rpart", "rpart.plot", "rattle")
inst <- pkg %in% installed.packages()
if(length(pkg[!inst]) > 0) install.packages(pkg[!inst])
lapply(pkg, library, character.only = TRUE)
rm(list = c("inst", "pkg"))
setwd("/Users/danieldunn/Dropbox/DD Cloud/R/election2016")
set.seed(4444)


# Read in data ------------------------------------------------------------
if(!exists("v0")) v0 <- fread("votes.csv")


# Transform data ----------------------------------------------------------
v1 <- copy(v0)
v1[, combined_fips := as.factor(combined_fips)]
v1[, state_abbr := as.factor(state_abbr)]
v1[, county_name := as.factor(county_name)]
v1[, state_fips := as.factor(state_fips)]
v1[, county_fips := as.factor(county_fips)]

v1[, V1 := NULL]
v1[, X := NULL]
v1[, fips := NULL]
v1[, area_name := NULL]
v1[, state_abbreviation := NULL]
v1[, Clinton_Obama := NULL]
v1[, Trump_Romney := NULL]
v1[, Trump_Prediction := NULL]
v1[, Clinton_Prediction := NULL]
v1[, Trump_Deviation := NULL]
v1[, Clinton_Deviation := NULL]

v1$ClintonDigit <- substring(v1$votes_dem_2016, 1, 1)
v1$TrumpDigit <- substring(v1$votes_gop_2016, 1, 1)
v1$fipsWinner <- ifelse(v1$votes_dem_2016 > v1$votes_gop_2016, "Clinton", "Trump")
v1$StateWinner[v1$state_abbr %in% c("CA", "CO", "CT", "DE", "DC", 
                                    "HI", "IL", "MD", "MA", "MN", 
                                    "NV", "NH", "NJ", "NM", "NY", 
                                    "OR", "RI", "VT", "VA", "WA")] <- "Clinton"
v1$StateWinner[v1$state_abbr %in% c("AL", "AK", "AZ", "AR", "FL", 
                                    "GA", "ID", "IN", "IA", "KS", 
                                    "KY", "LA", "MI", "MS", "MO", 
                                    "MT", "NE", "NC", "ND", "OH", 
                                    "OK", "PA", "SC", "SD", "TN", 
                                    "TX", "UT", "WV", "WI", "WY")] <- "Trump"
v1$StateWinner[v1$state_abbr %in% c("ME")] <- "Split"
v1[, StateCounties := .N, by = state_abbr]


# Validate total digit frequency ------------------------------------------
nums <- data.frame(Digit = ordered(1:9), Clinton = as.integer(NA), Trump = as.integer(NA))
dem_num <- paste(c(v1$ClintonDigit), collapse = "")
gop_num <- paste(c(v1$TrumpDigit), collapse = "")
for(i in 1:nrow(nums)) {
  nums$Clinton[i] <- nchar(gregexpr(nums$Digit[i], dem_num))
  nums$Trump[i] <- nchar(gregexpr(nums$Digit[i], gop_num))
}
nums <- gather(data = nums, key = Digit)
names(nums) <- c("Digit", "Candidate", "Frequency")

g1 <- ggplot(data = nums, aes(x = Digit, y = Frequency, group = Candidate, color = Candidate)) +
  geom_line() +
  geom_point()
g1
ggsave("Election2016Overall.png")


# Validate state digit frequency ------------------------------------------
states <- unique(v1$state_abbr)
numsState <- data.frame(expand.grid(states, ordered(1:9)))
names(numsState) <- c("State", "Digit")
for(i in 1:nrow(numsState)) {
  numsState$Clinton[i] <- length(v1$votes_dem_2016[v1$state_abbr == numsState$State[i] & 
                                                     v1$ClintonDigit == numsState$Digit[i]])
  numsState$Trump[i] <- length(v1$votes_gop_2016[v1$state_abbr == numsState$State[i] & 
                                                   v1$TrumpDigit == numsState$Digit[i]])
  numsState$ClintonPct[i] <- length(v1$votes_dem_2016[v1$state_abbr == numsState$State[i] & 
                                                        v1$ClintonDigit == numsState$Digit[i]]) / 
    length(v1$votes_dem_2016[v1$state_abbr == numsState$State[i]])
  numsState$TrumpPct[i] <- length(v1$votes_gop_2016[v1$state_abbr == numsState$State[i] & 
                                                      v1$TrumpDigit == numsState$Digit[i]]) / 
    length(v1$votes_gop_2016[v1$state_abbr == numsState$State[i]])
}

numsState$StateWinner <- v1$StateWinner[match(numsState$State, v1$state_abbr)]
numsState$StateCounties <- v1$StateCounties[match(numsState$State, v1$state_abbr)]
numsState$State <- ordered(unique(numsState$State), 
                           levels = unique(numsState$State[order(-numsState$StateCounties)]))

setDT(numsState)
numsState <- melt(data = numsState, id.vars = c("State", "Digit", "StateWinner", "StateCounties"), 
                  measure.vars = c("ClintonPct", "TrumpPct"))
names(numsState) <- c("State", "Digit", "StateWinner", "StateCounties", "Candidate", "Frequency")
numsState$StateWinner <- factor(numsState$StateWinner)

g2 <- ggplot(data = numsState, 
             aes(x = Digit, y = Frequency, group = Candidate, color = Candidate)) +
  geom_line(size = 0.5) +
  geom_point(size = 0.5) + 
  facet_wrap(~ State, ncol = 8) + 
  scale_color_manual(values = c("#0000FF", "#FF0000")) + 
  geom_rect(data = numsState, aes(fill = StateWinner), xmin = -Inf, xmax = Inf,
            ymin = -Inf, ymax = Inf, alpha = 0.01) + 
  scale_fill_manual(values = c("#0000FF", "#00ff00", "#FF0000"), guide = "none") + 
  ggtitle(label = "Leading Digit of County Vote Totals", 
          subtitle = "Facet Background Indicates State Winner") + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA, size = 1))
g2
ggsave("Election2016State.png", height = 6, width = 10)


# Fit classification tree -------------------------------------------------
setnames(v1, c("RHI825214", "HSG096213", "AFN120207", "RHI325214", 
               "MAN450207", "BZA115213", "AGE295214", "NES010213", 
               "RHI425214", "RHI625214", "SBO315207", "SBO415207", 
               "HSG495213", "HSG445213"), 
         c("White alone, not Hispanic or Latino, percent, 2014", 
           "Housing units in multi-unit structures, percent, 2009-2013", 
           "Accommodation and food services sales, 2007 ($1,000)", 
           "American Indian and Alaska Native alone, percent, 2014", 
           "Manufacturers shipments, 2007 ($1,000)", 
           "Private nonfarm employment, percent change, 2012-2013", 
           "Persons under 18 years, percent, 2014", 
           "Nonemployer establishments, 2013", 
           "Asian alone, percent, 2014", 
           "Two or More Races, percent, 2014", 
           "Black-owned firms, percent, 2007", 
           "Hispanic-owned firms, percent, 2007", 
           "Median value of owner-occupied housing units, 2009-2013", 
           "Homeownership rate, 2009-2013"))

fit <- rpart(fipsWinner ~ ., method = "class", 
             data = v1[, c(24:25, 28, 32:34, 37:38, 42:45, 47:49, 51, 53:54, 57:63, 65:67, 69:70, 74), with = FALSE])
printcp(fit)
plotcp(fit)

cp <- fit$cptable[5, "CP"]
pruned <- prune(fit, cp)
summary(pruned)

pred <- predict(pruned, type="class")
table(v1$fipsWinner, pred)
round(table(v1$fipsWinner, pred) / length(v1$fipsWinner), 2)

fancyRpartPlot(model = pruned, main = "Clinton/Trump 2016", palettes = c("Greys", "Oranges"))


# Correlations ------------------------------------------------------------
round(cor(v1$population2014, v1$Trump), 2)
round(cor(v1$White, v1$Trump), 2)


# Findings ----------------------------------------------------------------
#The states are sorted by number of counties since Benford's law breaks down with fewer records. The last couple of rows of states have zig-zag lines, probably due to this.
#There are four interesting states that do not have low county counts:
#Virginia (VA) looks a bit off [Clinton won this state by 5%]
#Kentucky (KY), Iowa (IA), and Mississppi (MA) look a bit off [Trump won these states by 30%, 10%, and 19%, respectively]
#Basically, the states with the oddest results were typically so lopsided as to scarcely matter.
#An interesting observation is that the states with more counties clearly tilt toward Trump, while the states with fewer counties clearly tilt toward Clinton. Why might that be? Maybe the number of counties in a state is a proxy for average population size per county. If true, then the Clinton vote should correlate with the county population size. Does it?
#Indeed. Perhaps a theme to the election was the divergent preferences between counties with high populations versus the rest of the country. That suggests a potential solution: density-based laws, which might effectively be the return of city-states operating under one national flag. Individuals could pick their preferred city based on its basket of laws, obviating the imposition of a heavy blanket of laws from a national government that seesaws left and right, alienating an alternative half of its citizens each cycle.
#Maybe the Constitution even effectively implemented this flavor of federalism in 1789, since state populations were then the size of present day cities. Just food for thought, let's not get carried away.
#This chart of overall county vote totals by leading digit looks like it passes Benford's Law, but what about state-by-state results?


