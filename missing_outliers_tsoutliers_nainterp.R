##### Missing values #####

library(tidyverse)
library(fpp2)

gold2 <- na.interp(gold)
autoplot(gold2, series="Interpolated") +
  autolayer(gold, series="Original") +
  scale_colour_manual(
    values=c(`Interpolated`="red",`Original`="gray"))
##### for non-seasonal data like this, simple linear interpolation is used to fill in the missing sections

##### Outliers #####
tsoutliers(gold)

gold[768:772]

##### another useful function is tsclean() which identifies and replaces outliers, and also replaces missing values
gold %>%
  tsclean() %>%
  ets() %>%
  forecast(h=50) %>%
  autoplot()
