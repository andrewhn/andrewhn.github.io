require(RPostgreSQL)
require(dplyr)
require(tidyr)
require(lubridate)
require(ggplot2)

opt <- list(
  station="ST KILDA HARBOUR - RMYS",
  output_dir="../images",
  img_format="png"
)

## cardinal point ordering
cardinal <- c("N", "NNE", "NE", "ENE",
              "E", "ESE", "SE", "SSE",
              "S", "SSW", "SW", "WSW",
              "W", "WNW", "NW", "NNW")

## colours
pal <- c("#3d348b", "#7678ed", "#f7b801", "#f18701", "#f35b04")

## calculate mode of discrete (e.g. character) vector
vec.mode <- function(x) {
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

runq <- function(query) {
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, dbname="hwindy", user="hwindy", password="hwindy", host="localhost")
  rs <- dbSendQuery(con, query)
  selected <- fetch(rs, n=-1)
  dbDisconnect(con)
  dbUnloadDriver(drv)
  return(selected)
}

export_boxplot <- function(bp, filename_stub, split_on) {
  export <- as.data.frame.matrix(t(bp$stats))
  export$key <- bp$names
  export %>%
    separate(key, split_on, "\\.") %>%
    write.csv(., file=paste0(filename_stub, "box.csv"), row.names=FALSE)
  export <- data.frame(V1=bp$out)
  export$idx <- bp$group
  export <- merge(export, data.frame(idx=1:length(bp$names), key=bp$names))
  export %>%
    separate(key, split_on, "\\.") %>%
    select(-idx) %>%
    write.csv(., file=paste0(filename_stub, "outliers.csv"), row.names=FALSE)
}

img_path <- function(name) {
  return(file.path(opt$output_dir, paste(name, opt$img_format, sep=".")))
}

## station metadata
stmd <- runq(paste0("SELECT * FROM bom_station WHERE name='", opt$station, "'"))

## rpostgres has an issue with retrieving utc times at a time zone spanning dst changes, hence to_char
obsq <- paste(
  "SELECT to_char(datetime at time zone 'AEST', 'YYYY-MM-DD HH24:MI') as dt, wind_spd_kts, wind_gust_kts, wind_dir, temp",
  "FROM bom_observation",
  paste0("WHERE station_id=", stmd$wmo)  
)
obs <- runq(obsq) %>% mutate(dt=ymd_hm(dt))

fcq <- paste(
  "SELECT fcg.id, to_char(fcg.datetime at time zone 'AEST', 'YYYY-MM-DD HH24:MI') as issued,",
  "to_char(fc.datetime at time zone 'AEST', 'YYYY-MM-DD HH24:MI') as dt, fc.wind_spd_kts, fc.wind_dir, fc.airtemp",
  "FROM bom_forecastgroup fcg",
  "LEFT JOIN bom_forecast fc on fcg.id=fc.group_id",
  paste0("WHERE fcg.forecast_location_id=", stmd$forecast_location_id)
)
fc <- runq(fcq) %>% mutate(issued=ymd_hm(issued), dt=ymd_hm(dt))

## forecasts per day
length(unique(fc$id)) / 365  #  /day
## observations per hour
nrow(obs) / 8760  # /hour ~6 an hour (10 minute intervals)

## annotate month/hour
obs <- obs %>%
  mutate(hour=hour(dt),
         month=factor(format(dt, "%B"), levels=month.name))

## monthly speed distributions
plt <- obs %>%
  select(dt, month, wind_spd_kts, wind_gust_kts) %>%
  gather(variable, kts, -c(dt, month))
p <- ggplot(plt, aes(x=month, y=kts))
p <- p + facet_grid(.~variable)
p <- p + geom_boxplot()

## monthly speed distributions by hour
plt <- obs %>%
  select(dt, month, hour, wind_spd_kts, wind_gust_kts) %>%
  gather(variable, kts, -c(dt, month, hour))
p <- ggplot(plt, aes(x=factor(hour), y=kts))
p <- p + geom_boxplot()
p <- p + facet_grid(month~variable)

## just kts, with wrap
plt <- plt %>% filter(variable=="wind_spd_kts")
p <- ggplot(plt, aes(x=hour, y=kts))
p <- p + facet_wrap(~month, ncol=3)
p <- p + labs(x='Hour of day', y='Average wind speed (knots)')
ggsave(img_path("stk-mth-hr-boxplot"), p, width=6.5, height=8, units="in")

## wind roses
plt <- obs %>%
  filter(!is.na(wind_dir) & wind_dir != "CALM") %>%
  mutate(lvl=cut(wind_spd_kts, breaks=seq(0, 40, 10)),
         card=factor(wind_dir, levels=cardinal))
cols <- colorRampPalette(pal)(5)
p <- ggplot(plt, aes(x=card, fill=lvl))
p <- p + geom_bar()
p <- p + facet_wrap(~month, ncol=3)
p <- p + coord_polar(start=-(1 / length(cardinal) * pi))
p <- p + labs(x="", y="Observations")
p <- p + scale_fill_manual(name="Wind speed (knots)", values=cols)
p <- p + theme(legend.position="bottom")
ggsave(img_path("stk-mth-dir-rose"), p, width=6.5, height=9, units="in")

## consistency distributions
plt <- obs %>%
  mutate(cons_kt=wind_gust_kts - wind_spd_kts,
         cons_pct=wind_spd_kts / wind_gust_kts) %>%
  gather(variable, value, c(cons_kt, cons_pct))
p <- ggplot(plt, aes(x=factor(hour), y=value))
p <- p + geom_boxplot()
p <- p + facet_grid(variable ~ month, scales="free_y")

## monthly average readings/gust, max gust by month by hour
plt <- obs %>%
  group_by(hour, month) %>%
  summarise(mean_spd_kts=mean(wind_spd_kts),
            max_gust=max(wind_gust_kts),
            mean_gust_kts=mean(wind_gust_kts)) %>%
  gather(variable, kts, -c(hour, month))
p <- ggplot(plt, aes(x=hour, y=kts, color=variable))
p <- p + facet_wrap(~month)
p <- p + geom_line()

## match observations to nearest forecast hour
obs <- obs %>%
  mutate(rounded=round_date(dt, unit="hour"),
         mod_hour=hour(rounded),
         target_hour=(mod_hour - mod_hour %% 3) + 1,
         fcdt=rounded)
hour(obs$fcdt) <- obs$target_hour
obs <- obs %>%
  mutate(fctd_final=fcdt + days(ifelse(mod_hour == 0, 1, 0))) %>%
  select(dt, wind_spd_kts, wind_gust_kts, wind_dir, temp, fcdt)

## combine observations and forecasts
comb <- merge(obs, fc, by.x="fcdt", by.y="dt", all=TRUE) %>%
  filter(!is.na(dt) & !is.na(issued)) %>%  # missing some observations/forecasts
  group_by(issued, fcdt, wind_spd_kts.y, wind_dir.y, airtemp) %>%
  summarise(wind_spd_kts=mean(wind_spd_kts.x),
            wind_gust_kts=mean(wind_gust_kts),
            temp=mean(temp),
            wind_dir=vec.mode(wind_dir.x)) %>%
  ## speed errors
  mutate(error=wind_spd_kts - wind_spd_kts.y,
         abserror=abs(error),
         abspcterror=abserror / wind_spd_kts,
         hod=hour(fcdt),
         ## directional errors
         didx_obs=match(wind_dir, cardinal),  # get index in list
         didx_fc=match(wind_dir.y, cardinal),
         oneway_dir_error=pmax(didx_obs, didx_fc) - pmin(didx_obs, didx_fc),  # number of cardinal points away in the list
         dir_error=min(length(cardinal) - oneway_dir_error, oneway_dir_error)) %>%  # account for wrap-around
  group_by(issued) %>%
  mutate(fcint=as.numeric(fcdt-min(fcdt)) / 60 / 60)  # hours since first forecast

comb %>% group_by(fcint) %>% summarise(c=length(fcint))
comb <- comb %>% filter(fcint <= 162)  # not enough obs in the 165 category

## absolute error, grouped by forecast interval
p <- ggplot(comb, aes(x=error, group=fcint))
p <- p + geom_density(aes(colour=fcint))
p <- p + scale_color_gradientn(colours=pal, name="Hours since forecast")
p <- p + labs(x="Error (knots)", y="Density")
p <- p + theme(legend.position="bottom")
ggsave(img_path("forecast-speed-error-density"), p, width=6.5, height=4, units="in")

## wrap by direction
p <- p + facet_wrap(~wind_dir)
p <- p + scale_y_continuous(limits=c(0, 0.25))

## as a box plot
p <- ggplot(comb, aes(x=fcint, y=error, group=fcint))
p <- p + geom_boxplot()

## directional errors
p <- ggplot(comb, aes(x=dir_error))
p <- p + geom_histogram()
p <- p + facet_wrap(~fcint)

## by direction
p <- ggplot(comb, aes(x=dir_error))
p <- p + geom_histogram()
p <- p + facet_grid(wind_dir~., scales="free_y")

## and by dir
p <- ggplot(comb, aes(x=fcint, y=abspcterror, group=fcint))
p <- p + geom_boxplot()
p <- p + coord_cartesian(ylim=c(0, 1))
p <- p + facet_grid(wind_dir~.)

comb <- comb %>% mutate(month=factor(format(fcdt, "%B"), levels=month.name))

## monthly errors by forecast interval
p <- ggplot(comb, aes(x=fcint, y=abspcterror, group=fcint))
p <- p + geom_boxplot()
p <- p + coord_cartesian(ylim=c(0, 1))
p <- p + facet_wrap(~month, ncol=3)
p <- p + labs(x="Hours since forecast", y="Absolute percentage error (%)")
ggsave(img_path("stk-mth-fcint-boxplot"), p, width=6.5, height=8, units="in")

plt <- comb %>% filter(wind_spd_kts.y >= 15 & hour(fcdt) %in% 7:20 & grepl('S', wind_dir) & !grepl('E', wind_dir))
p <- ggplot(plt, aes(x=fcint, y=abspcterror, group=fcint))
p <- p + geom_boxplot()
p <- p + coord_cartesian(ylim=c(0, 1))
p <- p + facet_wrap(~month, ncol=3)
p <- p + labs(x="Hours since forecast", y="Absolute percentage error (%)")
ggsave(img_path("stk-mth-fcint-lim-boxplot"), p, width=6.5, height=8, units="in")
