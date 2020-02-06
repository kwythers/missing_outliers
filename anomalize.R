library(tidyverse)
library(anomalize)

# daily counts of tidyverse package downloads
tidyverse_cran_downloads

# use the three tidy functions: time_decompose(), anomalize(), and time_recompose() 
# to detect anomalies, and plot_anomalies() to visualize
tidyverse_cran_downloads %>%
  time_decompose(count) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)

##### more detail #####

##### Time series decomposition
#
# The first step is time series decomposition using time_decompose(). The “count” column is decomposed 
# into “observed”, “season”, “trend”, and “remainder” columns. The default values for time series decompose 
# are method = "stl", which is just seasonal decomposition using a Loess smoother (refer to stats::stl()). 
# The frequency and trend parameters are automatically set based on the time scale (or periodicity) of the 
# time series using tibbletime based function under the hood.
#
# A nice aspect is that the frequency and trend are automatically selected for you. If you want to see what 
# was selected, set message = TRUE. Also, you can change the selection by inputting a time-based period such 
# as “1 week” or “2 quarters”, which is typically more intuitive that figuring out how many observations fall into 
# a time span. Under the hood, time_frequency() and time_trend() convert these from time-based periods to numeric 
# values using tibbletime!
  
tidyverse_cran_downloads %>%
  time_decompose(count, method = "stl", frequency = "auto", trend = "auto")

##### Anomaly Detection Of Remainder
#
# The next step is to perform anomaly detection on the decomposed data, specifically the “remainder” column. 
# We did this using anomalize(), which produces three new columns: “remainder_l1” (lower limit), 
# “remainder_l2” (upper limit), and “anomaly” (Yes/No Flag). The default method is method = "iqr", which is 
# fast and relatively accurate at detecting anomalies. The alpha parameter is by default set to alpha = 0.05, but can be 
# adjusted to increase or decrease the height of the anomaly bands, making it more difficult or less difficult for 
# data to be anomalous. The max_anoms parameter is by default set to a maximum of max_anoms = 0.2 for 20% of data that 
# can be anomalous. This is the second parameter that can be adjusted. Finally, verbose = FALSE by default which 
# returns a data frame. Try setting verbose = TRUE to get an outlier report as a list.

tidyverse_cran_downloads %>%
  time_decompose(count, method = "stl", frequency = "auto", trend = "auto") %>%
  anomalize(remainder, method = "iqr", alpha = 0.05, max_anoms = 0.2)

# If you want to visualize what’s happening, now’s a good point to try out another plotting function, 
# plot_anomaly_decomposition(). It only works on a single time series so we’ll need to select just one to review. 
# The “season” is removing the weekly cyclic seasonality. The trend is smooth, which is desirable to remove the 
# central tendency without overfitting. Finally, the remainder is analyzed for anomalies detecting the most significant 
# outliers. 

tidyverse_cran_downloads %>%
  # Select a single time series
  filter(package == "lubridate") %>%
  ungroup() %>%
  # Anomalize
  time_decompose(count, method = "stl", frequency = "auto", trend = "auto") %>%
  anomalize(remainder, method = "iqr", alpha = 0.05, max_anoms = 0.2) %>%
  # Plot Anomaly Decomposition
  plot_anomaly_decomposition() +
  ggtitle("Lubridate Downloads: Anomaly Decomposition")

##### Anomaly Lower and Upper Bounds
#
# The last step is to create the lower and upper bounds around the “observed” values. This is the work of time_recompose(), 
# which recomposes the lower and upper bounds of the anomalies around the observed values. Two new columns were 
# created: “recomposed_l1” (lower limit) and “recomposed_l2” (upper limit).

tidyverse_cran_downloads %>%
  time_decompose(count, method = "stl", frequency = "auto", trend = "auto") %>%
  anomalize(remainder, method = "iqr", alpha = 0.05, max_anoms = 0.2) %>%
  time_recompose()

# Let’s visualize on just the “lubridate” data. We can do so using plot_anomalies() and setting time_recomposed = TRUE. 
# This function works on both single and grouped data.

tidyverse_cran_downloads %>%
  # Select single time series
  filter(package == "lubridate") %>%
  ungroup() %>%
  # Anomalize
  time_decompose(count, method = "stl", frequency = "auto", trend = "auto") %>%
  anomalize(remainder, method = "iqr", alpha = 0.05, max_anoms = 0.2) %>%
  time_recompose() %>%
  # Plot Anomaly Decomposition
  plot_anomalies(time_recomposed = TRUE) +
  ggtitle("Lubridate Downloads: Anomalies Detected")

