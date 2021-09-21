# scatter plot
ggplot(data=..., aes(x=..., y=...)) +
  geom_...


# histogram
ggplot(data=..., aes(x=...)) +
  geom_...(fill="...",
           col="...",
           binwidth = ...) +
  xlim(..., ...)

# time series plot
years <- seq(1950,2015,1)       # Create some dummy data
readings <- (years-1900) + runif(66,0,20)
mydata <- data.frame(years,readings)
ggplot(data=..., aes(x=...,y=...)) +
  geom_...(color="...", size = 1)    + # add line
  geom_...(shape=10, size=2.5)    + # add points
  geom_...(method=lm) +    # Add a linear best fit line
  xlab("...") + ylab("...") +   # Change axis labels
  ggtitle("...") # Add a title


