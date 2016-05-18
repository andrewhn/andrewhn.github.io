---
title: BOM wind forecast accuracy: St Kilda RMYS edition
date: 2016-05-07
tags: kites
---

Certainly the best and worst aspects of kiting (and perhaps elemental sports more generally) are the reliance on mother nature. There's nothing better than a sunny day blowing a smooth 23 knots all afternoon, but there's also nothing worse than driving down to Rosebud on the basis of a blistering forecast all for nothing. The wind-sport community uses forecasts to minimise the risk of gearing up and traveling for no reason. These forecasts can come from a number of different websites, but the sources are generally from one of two places.

.. raw:: html

  <!--more-->

The first is the US National Oceanic and Atmospheric Administration's (NOAA) global forecasting system (GFS), for which global gridded forecasts are free to download. Sites that show these forecasts are `earth.nullschool.net <http://earth.nullschool.net/#current/wind/surface/level/orthographic=-223.66,-26.14,1105>`_ and `windyty <http://windyty.com>`_, which are nice to look at but don't capture localised effects, so not great if you're deciding whether to go for a kite tomorrow.

The second source is one of several Bureau of Meteorology (BOM) forecast products, the best (to my knowledge) of which is publically available as `MetEye <http://www.bom.gov.au/australia/meteye/>`_. MetEye provides gridded forecasts of wind speed and direction at three hour intervals, seven days ahead, via map-based and textual interfaces. The map-based MetEye user interface is pictured below and gives a good feeling for direction and speed in the area at a relatively granular resolution.

.. image:: /images/bom-meteye.png
   :width: 100%
   :target: /images/bom-meteye.png

I generally trust the forecasts produce by MetEye - especially within a couple of days - but there are certainly occasions where wind is forecast and none arrives, or wind is not forecast and it does arrive. My feeling is that around 90% of the time in summer, the forecasts would be a sufficient indicator of kitable conditions, although perhaps this is fading affect bias in action. So, I have the data -- just how good are the MetEye forecasts?

In assessing forecast accuracy, I take three properties into account: wind speed, wind direction, and time since forecast. Each observation (meaning measured value) has a number of forecast values associated with it: those from 7 days prior, those from 6 days prior, and so on. The graphic below demonstrates this - the coloured arrows are observations, the black arrows are forecasts. If you fix your eye on a single point in time (the blue circle), you'll notice the forecasts change as the forecast issue time approaches the observation time.

.. image:: /images/forecast-performance-eg.gif
   :width: 100%
   :target: /images/forecast-performance-eg.gif

A consequence of the three-hour forecast granularity is that one forecast point represents 18 different of observations, which may have a lot of variability. In three hours, winds can change a lot, and so we should take this in to account when assessing forecast accuracy metrics.

The data
---------------------------------------

To assess these forecasts, I'm using a years worth of observation and forecast data spanning May 2015-May 2016. The observation data is from the St Kilda RMYS station, and the forecast data from the respective forecast grid cell. In total, the dataset has:

* 1,464 forecasts, equating to four per day; each forecast has a wind speed and direction data point out seven days in three hour intervals (76,100 forecast data points in total).

* 52,572 observations, equating to six per hour or one every 10 minutes.

Observations are more granular than the forecasts (10 minute vs three hourly), so I needed a way to match them. I lieu of any actual documentation, I have assumed that observations are associated with the forecasts closest to them; in practice this means a forecast data point is associated with observations spanning 1.5 hours before and after.

Exploring the observations
---------------------------------------

Of course, a single year of data will be prone to seasonal effects; many have condemned the 2015-16 summer a terrible summer for wind sports in Victoria. However, while the following may not be representative of every year, we can still see some interesting patterns emerging.

The following chart shows distributions of wind speed by hour and month, and will have no surprises for wind-sport enthusiasts in Melbourne or residents of St Kilda. In the warmer months, we get reliable sea breezes which build throughout the morning, peak in the afternoon, and drop off in the evening. In the cooler months, there is a much weaker correlation between wind speed and time of day, but greater variability in wind speed at any given hour.

.. image:: /images/stk-mth-hr-boxplot.png
   :width: 100%
   :target: /images/stk-mth-hr-boxplot.png

Including direction in our analysis will also hold no surprises for Melbourne wind-sport enthusiasts. The roses below show the count of observations by direction of origin, and are coloured in 10 knot bands by speed. Southerlies from the sea breezes dominate in the warmer months, with the strongest winds most present in the hottest month (February). Northerlies dominate the cooler months, particularly in autumn, as we are experiencing right now.

.. image:: /images/stk-mth-dir-rose.png
   :width: 100%
   :target: /images/stk-mth-dir-rose.png


Assessing the forecasts
---------------------------------------

There are a 

Error density plots:

- Scratching the surface here

.. image:: /images/forecast-speed-error-density.png
   :width: 100%
   :target: /images/forecast-speed-error-density.png

More text

.. image:: /images/stk-mth-fcint-boxplot.png
   :width: 100%
   :target: /images/stk-mth-fcint-boxplot.png

More text. Todo - lim - do we want to lim based on actuals (dir & speed) or forecast (dir & speed)?

.. image:: /images/stk-mth-fcint-lim-boxplot.png
   :width: 100%
   :target: /images/stk-mth-fcint-lim-boxplot.png

More text
