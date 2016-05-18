---
title: Olympics dataset
date: 2012-08-11
tags: computers, data
---

Reading Stephen King's blog post on comments about Australia's Olympic performance inspired me to have a look at  some Olympics data. Surprisingly, I couldn't find an easily accessible database of medals by event, country, and year. I did find a website that contains all this information, so I grabbed it from there with a scraper in Python (downloadable below).

.. raw:: html

   <!--more-->

The raw data isn't exactly how I wanted -- there are multiple rows (medals listed) for team events. For example:

.. code:: r

    > df[10:16, ]
    #   year country      sport              event                    name score  medal
    #10 1896  Greece Gymnastics              Rings     Ioannis Mitropoulos         GOLD
    #11 1896  Greece Gymnastics      Rope climbing Nicolaos Andriakopoulos  23.4   GOLD
    #12 1896  Greece Gymnastics Parallel Bars Team          Thomas Xenakis       SILVER
    #13 1896  Greece Gymnastics Parallel Bars Team Nicolaos Andriakopoulos       SILVER
    #14 1896  Greece Gymnastics Parallel Bars Team      Philippos Karvelas       BRONZE
    #15 1896  Greece Gymnastics Parallel Bars Team      Ioannis Khrisaphos       BRONZE
    #16 1896  Greece   Shooting         Pistol 25m       Ioannis Fragoudis   344   GOLD


I wanted to have one row per medal so medal tallies could be calculated from this table. The ``plyr`` package in R has a nice function ``ddply`` which finds rows with common elements. I wanted to isolate rows with everything in common except for the name. With these subsets, I wanted to compress them into one row with the names concatenated. This is done as follows:

.. code:: r

    df.clean <- ddply(df, c("year", "country", "sport", "event", "score", "medal"),
            function(subset) paste(subset[, "name"], collapse=","))

    df.clean[10:14, ]
    #   year country      sport              event score  medal
    #10 1896  Greece Gymnastics              Rings         GOLD
    #11 1896  Greece Gymnastics      Rope climbing  23.4   GOLD
    #12 1896  Greece Gymnastics Parallel Bars Team       SILVER
    #13 1896  Greece Gymnastics Parallel Bars Team       BRONZE
    #14 1896  Greece   Shooting         Pistol 25m   344   GOLD
    #                                                                            names
    #10                                                            Ioannis Mitropoulos
    #11                                                        Nicolaos Andriakopoulos
    #12 Thomas Xenakis,Nicolaos Andriakopoulos,Petros Persakis,Sotirios Athanasopoulos
    #13   Philippos Karvelas,Ioannis Khrisaphos,Ioannis Mitropoulos,Dimitrios Loundras
    #14                                                              Ioannis Fragoudis

(Note: the number of names between examples isn't an error -- the data is not sorted.)

I haven't had time to play with this dataset properly, but I wanted to post it while it's still topical. You can download a zip file with everything I've written about in this post here.

