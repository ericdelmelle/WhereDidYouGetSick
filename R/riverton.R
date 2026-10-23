# Riverton: fictional town + 23 fictional bladder cancer cases (2021-2025)
# Planar coordinates in metres, no CRS
library(sf)

# extent
xlim <- c(0, 8000)
ylim <- c(0, 4200)
bbox_poly <- st_as_sfc(st_bbox(c(xmin = xlim[1], ymin = ylim[1],
                                 xmax = xlim[2], ymax = ylim[2])))

# river centreline (W -> E)
river_xy <- rbind(c(-200, 1920), c(0, 1900), c(1200, 1750), c(2200, 1550),
                  c(3000, 1500), c(3800, 1600), c(4600, 1850), c(5600, 1950),
                  c(6600, 1800), c(8000, 1700), c(8200, 1690))
smooth_line <- function(m, n = 400) {
  t <- seq_len(nrow(m))
  tt <- seq(1, nrow(m), length.out = n)
  cbind(spline(t, m[, 1], xout = tt)$y, spline(t, m[, 2], xout = tt)$y)
}
river_line <- st_sfc(st_linestring(smooth_line(river_xy)))
river <- st_intersection(st_buffer(river_line, 75), bbox_poly)

# north / south halves
north_half <- st_sfc(st_polygon(list(rbind(
  smooth_line(river_xy), c(8200, 5000), c(-200, 5000), smooth_line(river_xy)[1, ]))))
north_half <- st_intersection(st_make_valid(north_half), bbox_poly)
south_half <- st_difference(bbox_poly, north_half)

rect <- function(x0, x1, y0, y1)
  st_as_sfc(st_bbox(c(xmin = x0, ymin = y0, xmax = x1, ymax = y1)))

# neighborhoods
nb <- list(
  "Westend"   = st_intersection(rect(0, 2200, 0, 4200), south_half),
  "The Flats" = st_intersection(rect(2200, 4300, 0, 4200), south_half),
  "Southridge"= st_intersection(rect(4300, 8000, 0, 4200), south_half),
  "Northside" = st_intersection(st_union(rect(0, 2600, 0, 4200), rect(2600, 4600, 3000, 4200)), north_half),
  "Downtown"  = st_intersection(rect(2600, 4600, 0, 3000), north_half),
  "Hillcrest" = st_intersection(rect(4600, 8000, 0, 4200), north_half)
)
hoods <- st_sf(name = names(nb), geometry = do.call(c, nb))
hoods$pop <- c(3000, 2800, 2400, 4200, 2300, 3300)  # ~18,000 total
hood_labels <- data.frame(
  name = c("WESTEND", "THE FLATS", "SOUTHRIDGE", "NORTHSIDE", "DOWNTOWN", "HILLCREST"),
  x = c(750, 3150, 6300, 1100, 3600, 6600),
  y = c(1560, 200, 200, 3950, 2950, 3950))

# streets
line <- function(...) st_linestring(rbind(...))
roads <- st_sfc(
  line(c(3000, 0), c(3000, 4200)),                     # Bridge St
  line(c(5400, 0), c(5400, 4200)),                     # Hill Rd bridge
  line(c(0, 2350), c(2500, 2250), c(5000, 2400), c(8000, 2500)),  # Main St
  line(c(0, 1050), c(4000, 1000), c(8000, 1150)),      # River Rd
  line(c(1500, 1900), c(1500, 4200)),                  # Northside Ave
  line(c(5400, 2400), c(7900, 4100)),                  # Crest Dr
  line(c(1800, 0), c(2100, 1200))                      # Westend Rd
)
rail_xy <- smooth_line(river_xy)
rail_xy[, 2] <- rail_xy[, 2] + 190
rail <- st_sfc(st_linestring(rail_xy))
rail <- st_intersection(rail, bbox_poly)

# sites
poly <- function(...) st_polygon(list(rbind(..., list(...)[[1]])))
keller <- st_sfc(poly(c(3480, 1180), c(3920, 1150), c(3980, 1420), c(3500, 1440)))
ashby  <- st_sfc(poly(c(1100, 1960), c(1500, 1950), c(1520, 2200), c(1120, 2210)))
commons <- st_sfc(poly(c(2790, 1060), c(3010, 1060), c(3010, 1240), c(2790, 1240)))
keller_c <- st_coordinates(st_centroid(keller))
ashby_c  <- st_coordinates(st_centroid(ashby))

places <- data.frame(
  place = c("Valley Regional Hospital", "Riverton Community College"),
  x = c(1900, 3300), y = c(3500, 2700))

# cases: current home, 1985 home, work and residence history
cases <- read.csv(text = "
id,nbhd,x,y,age_dx,year_dx,since,x85,y85,nbhd85,keller,ashby,senior_ctr,commons,history
F1,The Flats,2830,1110,78,2022,2017,900,1100,Westend,1968-1986,,0,1,Lived in Westend 1966-2017; moved to Riverside Commons 2017
F2,The Flats,2900,1110,81,2021,2016,,,Out of town,,,1,1,Lived in New Jersey until 2016; moved to Riverside Commons to be near family
F3,The Flats,2970,1110,74,2023,2018,6200,3000,Hillcrest,,,1,1,Lived in Hillcrest 1979-2018; moved to Riverside Commons 2018
F4,The Flats,2865,1190,79,2024,2019,,,Out of town,,,1,1,Lived in a town 40 miles away until 2019; moved to Riverside Commons 2019
F5,The Flats,2935,1190,83,2025,2020,400,700,Westend,,,1,1,Lived in Westend 1970-2020; moved to Riverside Commons 2020
F6,The Flats,3300,1300,80,2021,1961,3300,1300,The Flats,1962-1990,,1,0,Same Flats house since 1961
F7,The Flats,3150,1050,73,2023,1950,3150,1050,The Flats,1969-1994,,0,0,Born in the Flats in 1950; never moved
F8,The Flats,3420,980,72,2024,1975,3420,980,The Flats,1975-1992,,0,0,Moved to the Flats in 1975 for a job at Keller
F9,The Flats,2650,1320,77,2022,1958,2650,1320,The Flats,,,1,0,Same Flats house since 1958; worked in a downtown store
F10,The Flats,3050,820,66,2025,1970,3050,820,The Flats,,,0,0,Grew up in the Flats from 1970; elementary school teacher
F11,The Flats,2500,980,70,2022,1999,1500,3300,Northside,,1985-2009,0,0,Lived in Northside 1980-1999; moved to the Flats 1999
F12,The Flats,3600,900,64,2023,2004,2300,3600,Northside,,,0,0,Grew up in Northside; moved to the Flats 2004
F13,The Flats,2750,700,59,2025,2011,,,Out of town,,,0,0,Lived out of state until 2011
D1,Downtown,3200,2150,75,2021,1990,1000,3000,Northside,1972-1990,,0,0,Lived in Northside until 1990; then Downtown
D2,Downtown,3900,2500,71,2024,1990,3000,1250,The Flats,,,0,0,Grew up and lived in the Flats 1953-1990; then Downtown
D3,Downtown,3500,2800,62,2023,2015,,,Out of town,,,0,0,Lived out of state until 2015
W1,Westend,1200,900,74,2022,1979,1200,900,Westend,,1979-2005,0,0,Same Westend house since 1979
W2,Westend,600,1300,69,2025,1988,,,Out of town,,,0,0,Lived in another county until 1988
N1,Northside,1700,2900,68,2023,1990,3250,1150,The Flats,,,0,0,Grew up and lived in the Flats 1955-1990; then Northside
N2,Northside,3800,3600,76,2024,2009,,,Out of town,,,0,0,Lived out of state until 2009
H1,Hillcrest,5600,2700,69,2023,2001,2800,1000,The Flats,1976-1990,,0,0,Frank: grew up in the Flats; lived there until 2001; then Hillcrest
H2,Hillcrest,6800,3300,67,2024,1995,1500,700,Westend,,1990-2009,0,0,Lived in Westend 1980-1995; then Hillcrest
H3,Hillcrest,5200,3500,72,2021,2007,,,Out of town,,,0,0,Lived out of state until 2007
", stringsAsFactors = FALSE, na.strings = "")
cases$keller_worker <- !is.na(cases$keller)
cases$ashby_worker  <- !is.na(cases$ashby)
cases$in_flats_85   <- cases$nbhd85 == "The Flats"
cases$dist_keller   <- sqrt((cases$x - keller_c[1])^2 + (cases$y - keller_c[2])^2)

# headline numbers used in slides and handouts
nums <- list(
  n          = nrow(cases),
  flats_now  = sum(cases$nbhd == "The Flats"),
  flats_pop_share = round(100 * 2800 / sum(hoods$pop)),
  within_1km = sum(cases$dist_keller <= 1000),
  keller     = sum(cases$keller_worker),
  ashby      = sum(cases$ashby_worker),
  workers_flats = sum((cases$keller_worker | cases$ashby_worker) & cases$nbhd == "The Flats"),
  commons    = sum(cases$commons == 1),
  senior_ctr = sum(cases$senior_ctr == 1),
  flats_85   = sum(cases$in_flats_85),
  out_85     = sum(cases$nbhd85 == "Out of town"),
  flats_85_moved = sum(cases$in_flats_85 & cases$nbhd != "The Flats"),
  expected   = 22  # ~25 per 100,000 per yr x 18,000 x 5 yr
)
