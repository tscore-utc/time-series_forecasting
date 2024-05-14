This repository is the companion to our paper "A Comparison of Time Series Methods for Post-COVID Transit Ridership Forecasting." These functions are based off of Minneapolis Metro's Route Trends and Info shinyapp (https://github.com/metrotransit/route-trends). Also, all methods used in these functions come from Rob Hyndman's book "Forecasting Principles and Practice 2nd edition" (https://otexts.com/fpp2/index.html) or Rob Hyndman's blog "Hyndsight." You may also note that Rob Hyndman has a 3rd edition of "Forecasting Principles and Practice," which introuced the fable package (as opposed to the forecast package used in these functions). The fable package does not include the TBATS method, but otherwise is pretty similar to the forecast package.

In this repository, I have listed the packages in R needed to reproduce the results of the paper, including the time series models, time series forecasts, time series plots, forecast plots, seasonal plots, and residuals. Alternatively, my hope is that these functions may help you produce your own time series forecasts. I tried to make the functions accessible for someone with very little knowledge of R; if you are unfamiliar with R and still find the functions confusing, I encourage you to try to help yourself using resources like datacamp.com or ChatGPT.

I have left comments in the functions to help guide you. Start with the separating function, followed by the forecasting function, followed by the plotting function. Then you can also use the other functions as you'd like.

These functions are also available at https://github.com/ashley2876/forecasting_repo. Additional functions for a moving window approach are included at the link.

Good luck and happy forecasting!
