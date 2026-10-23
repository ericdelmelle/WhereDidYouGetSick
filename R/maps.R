# Map functions for the Riverton case; source after riverton.R
library(ggplot2)
library(patchwork)

col <- list(
  land = "#f3f1ec", edge = "#cfc9be", road = "#ddd8ce", rail = "#8f8a80",
  river = "#b9d4ec", river_txt = "#4f78a3", hood_txt = "#9a958b",
  case = "#c62828", site = "#4d4d4d", keller = "#eb6834", ashby = "#2a78d6",
  ghost = "#9a958c", ink = "#1f1f1f", muted = "#5f5c56", path = "#2a78d6")

pt_size <- 4.2
tm <- function() getOption("riverton.tm", 1)  # text scale

# pull segment ends back so arrowheads clear the dots
shorten <- function(x, y, xend, yend, d0 = 0, d1 = 130) {
  L <- sqrt((xend - x)^2 + (yend - y)^2); ux <- (xend - x) / L; uy <- (yend - y) / L
  data.frame(x = x + ux * d0, y = y + uy * d0, xend = xend - ux * d1, yend = yend - uy * d1)
}

# base layers
base_map <- function(hood_labels_on = TRUE) {
  g <- ggplot() +
    geom_sf(data = hoods, fill = col$land, colour = NA) +
    geom_sf(data = roads, colour = col$road, linewidth = 1.1) +
    geom_sf(data = rail, colour = col$rail, linewidth = 0.45, linetype = "22") +
    geom_sf(data = river, fill = col$river, colour = NA) +
    annotate("text", x = 6900, y = 1770, label = "Ashby River", angle = -4,
             colour = col$river_txt, fontface = "italic", size = 4.2 * tm())
  if (hood_labels_on)
    g <- g + geom_text(data = hood_labels, aes(x, y, label = name),
                       colour = col$hood_txt, fontface = "bold", size = 4.6 * tm())
  g
}

finish <- function(g) {
  g + coord_sf(xlim = xlim, ylim = ylim, expand = FALSE, datum = NA) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white", colour = NA),
          plot.margin = margin(2, 2, 2, 2),
          legend.position = "none")
}

add_cases <- function(g, d = cases, x = "x", y = "y", fill = col$case, size = pt_size) {
  g + geom_point(data = d, aes(.data[[x]], .data[[y]]), shape = 21,
                 fill = fill, colour = "white", stroke = 0.9, size = size)
}

add_site <- function(g, geom, label, lx, ly, hjust = 0, fill = col$site) {
  g + geom_sf(data = geom, fill = fill, colour = "white", linewidth = 0.6) +
    annotate("label", x = lx, y = ly, label = label, hjust = hjust, size = 4 * tm(),
             fill = "white", colour = col$ink, label.size = 0, lineheight = 0.9)
}

callout <- function(g, label, x = 7900, y = 1530, hjust = 1) {
  g + annotate("label", x = x, y = y, label = label, hjust = hjust, vjust = 1,
               size = 4.4 * tm(), fill = "white", colour = col$ink, label.size = 0.3,
               label.padding = unit(0.45, "lines"), lineheight = 0.95)
}

# 0. Riverton basemap
map_town <- function() {
  g <- base_map()
  g <- g + geom_point(data = places, aes(x, y), shape = 22, size = 3.2,
                      fill = col$muted, colour = "white") +
    geom_text(data = places, aes(x, y, label = place), hjust = 0, nudge_x = 110,
              size = 3.6 * tm(), colour = col$muted) +
    annotate("text", x = 7950, y = 2120, label = "rail line", hjust = 1,
             size = 3.4 * tm(), colour = col$rail)
  finish(callout(g, "Riverton (fictional)\n~18,000 residents"))
}

# 1. residential cases
map_cases <- function(note = TRUE) {
  g <- add_cases(base_map())
  if (note) g <- callout(g, sprintf("%d people diagnosed with\nbladder cancer, 2021-2025\n(home address at diagnosis)", nums$n))
  finish(g)
}

# 2. + former dye works and 1 km ring
ring_1km <- st_buffer(st_centroid(keller), 1000)
map_source <- function(note = FALSE) {
  g <- base_map() +
    geom_sf(data = ring_1km, fill = NA, colour = col$keller, linewidth = 0.7, linetype = "42")
  g <- add_site(g, keller, "Former Keller Dye Works\n(1948-1994)", 4080, 1150)
  g <- add_cases(g)
  if (note) g <- callout(g, sprintf("%d of %d cases live within\n1 km of the former plant", nums$within_1km, nums$n))
  finish(g)
}

# 3. + workplaces
map_work <- function(note = FALSE) {
  wk <- cases[cases$keller_worker, ]
  wa <- cases[cases$ashby_worker, ]
  g <- base_map() +
    geom_segment(data = wk, aes(x, y, xend = keller_c[1], yend = keller_c[2]),
                 colour = col$keller, linewidth = 0.9, alpha = 0.9) +
    geom_segment(data = wa, aes(x, y, xend = ashby_c[1], yend = ashby_c[2]),
                 colour = col$ashby, linewidth = 0.9, alpha = 0.9)
  g <- add_site(g, keller, "Former Keller Dye Works\n(1948-1994)", 4080, 1150)
  g <- add_site(g, ashby, "Former Ashby Rubber Co.\n(1946-2009)", 100, 2620, hjust = 0)
  g <- add_cases(g)
  if (note) g <- callout(g, sprintf(
    "Worked at Keller: %d   (orange)\nWorked at Ashby: %d   (blue)\nNeither plant: %d",
    nums$keller, nums$ashby, nums$n - nums$keller - nums$ashby))
  finish(g)
}

# 4. where the same people lived in 1985
map_1985 <- function(note = FALSE) {
  d <- cases[!is.na(cases$x85), ]
  mv <- d[d$x85 != d$x | d$y85 != d$y, ]
  a <- shorten(mv$x85, mv$y85, mv$x, mv$y, 90, 110)
  g <- base_map() +
    geom_curve(data = a, aes(x, y, xend = xend, yend = yend), curvature = 0.15,
               colour = col$ghost, linewidth = 0.4, alpha = 0.75,
               arrow = arrow(length = unit(0.1, "inches"), type = "closed"))
  g <- add_site(g, keller, "Keller Dye Works\n(operating in 1985)", 4080, 1150)
  g <- g + geom_point(data = cases, aes(x, y), shape = 21, fill = "white",
                      colour = col$ghost, stroke = 0.8, size = pt_size - 0.6)
  g <- add_cases(g, d, "x85", "y85")
  lg <- data.frame(x = 5950, y = c(820, 540), lab = c("Home in 1985", "Home at diagnosis"))
  g <- g + geom_point(data = lg, aes(x, y), shape = 21, size = pt_size, stroke = 0.9,
                      fill = c(col$case, "white"), colour = c("white", col$ghost)) +
    geom_text(data = lg, aes(x + 150, y, label = lab), hjust = 0, size = 4 * tm(), colour = col$ink)
  if (note) g <- callout(g, sprintf("%d of %d lived outside\nRiverton in 1985", nums$out_85, nums$n),
                         x = 7900, y = 4080)
  finish(g)
}

# 4b. senior apartments highlighted (current homes)
map_commons <- function(note = FALSE) {
  g <- base_map()
  g <- add_site(g, keller, "Former Keller Dye Works\n(1948-1994)", 4080, 1150)
  g <- g + geom_sf(data = st_buffer(commons, 60), fill = NA, colour = col$ink, linewidth = 0.7)
  g <- add_cases(g)
  g <- g + annotate("label", x = 150, y = 430, hjust = 0, size = 4 * tm(),
                    label = "Riverside Commons\nsenior apartments\n(opened 2016)",
                    fill = "white", colour = col$ink, label.size = 0, lineheight = 0.9) +
    annotate("segment", x = 150 + 1250 * tm(), y = 500, xend = 2790, yend = 1100,
             colour = col$ink, linewidth = 0.5)
  if (note) g <- callout(g, sprintf(
    "%d cases live in one building.\nAll moved in after age 65,\nmore than 20 years after\nthe plant closed.", nums$commons))
  finish(g)
}

# Maya: dot, then week
maya <- data.frame(
  stop = c("Home", "Classes", "Coffee shop job", "Gym", "Friend's place"),
  x = c(2580, 3300, 5300, 1000, 2200), y = c(1180, 2700, 2600, 1250, 3600))
maya_trips <- data.frame(from = c(1, 2, 3, 1, 4, 1), to = c(2, 3, 1, 4, 1, 5))

map_maya_dot <- function() {
  g <- base_map() +
    geom_point(data = maya[1, ], aes(x, y), shape = 21, size = 6,
               fill = col$path, colour = "white", stroke = 1.2) +
    annotate("label", x = 2580, y = 800, label = "Maya, 19", size = 4.6 * tm(),
             fill = "white", label.size = 0)
  finish(g)
}

map_maya_week <- function() {
  tr <- shorten(maya$x[maya_trips$from], maya$y[maya_trips$from],
                maya$x[maya_trips$to], maya$y[maya_trips$to], 110, 150)
  g <- base_map() +
    geom_curve(data = tr, aes(x, y, xend = xend, yend = yend), curvature = 0.18,
               colour = col$path, linewidth = 0.8, alpha = 0.8,
               arrow = arrow(length = unit(0.12, "inches"), type = "closed")) +
    geom_point(data = maya, aes(x, y), shape = 21, size = 5.2,
               fill = col$path, colour = "white", stroke = 1.1) +
    geom_label(data = maya, aes(x, y, label = stop), nudge_y = -260, size = 4.2 * tm(),
               fill = "white", label.size = 0)
  finish(callout(g, "Activity space:\nthe places where someone\nspends time, and may meet\nrisks or protections"))
}

# Frank: one person, three places, fifty years
frank_pts <- data.frame(
  lab = c("Home 1954-2001", "Worked at Keller\n1976-1990", "Home since 2001",
          "Diagnosed here, 2023"),
  x = c(2800, keller_c[1], 5600, 1900), y = c(1000, keller_c[2], 2700, 3500),
  nx = c(-150, 330, 150, 150), ny = c(-330, -380, 0, 0), hj = c(0.5, 0, 0, 0))

map_frank <- function(step = 3) {
  g <- base_map()
  if (step >= 3) g <- g + geom_sf(data = keller, fill = col$site, colour = "white")
  k <- c(3, 4, 1, 2)[seq_len(step + 1)]  # reveal: now, hospital, past home, work
  if (step >= 2) g <- g + geom_curve(data = shorten(2800, 1000, 5600, 2700, 120, 150),
                                     aes(x, y, xend = xend, yend = yend),
                                     curvature = -0.25, colour = col$ghost, linewidth = 0.5,
                                     arrow = arrow(length = unit(0.12, "inches"), type = "closed"))
  d <- frank_pts[k, ]
  g <- g + geom_point(data = d, aes(x, y), shape = 21, size = 5.5,
                      fill = c(col$case, col$keller, col$case, col$muted)[k],
                      colour = "white", stroke = 1.1) +
    geom_label(data = d, aes(x + nx, y + ny, label = lab, hjust = hj), size = 4.2 * tm(),
               fill = "white", label.size = 0, lineheight = 0.9)
  finish(g)
}

frank_timeline <- function() {
  bars <- data.frame(
    what = factor(c("Keller plant operating", "Lived in the Flats", "Worked at Keller", "Lived in Hillcrest"),
                  levels = rev(c("Keller plant operating", "Lived in the Flats", "Worked at Keller", "Lived in Hillcrest"))),
    start = c(1948, 1954, 1976, 2001), end = c(1994, 2001, 1990, 2025),
    fill = c(col$site, col$case, col$keller, col$case))
  ggplot(bars) +
    geom_segment(aes(x = start, xend = end, y = what, yend = what), colour = bars$fill,
                 linewidth = 6, lineend = "round") +
    geom_vline(xintercept = 2023, colour = col$ink, linewidth = 0.6) +
    annotate("text", x = 2023, y = 4.6, label = "Diagnosed\n2023", hjust = 1.05, vjust = 1,
             size = 3.8 * tm(), lineheight = 0.9) +
    scale_x_continuous(breaks = seq(1950, 2020, 10), limits = c(1945, 2027)) +
    labs(x = NULL, y = NULL) +
    theme_minimal(base_size = 14 * tm()) +
    theme(panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(colour = "#ebe8e2"),
          axis.text.y = element_text(colour = col$ink),
          plot.background = element_rect(fill = "white", colour = NA))
}

frank_combo <- function() {
  map_frank(3) / frank_timeline() + plot_layout(heights = c(3.3, 1))
}

# Sam: infectious disease, real places, approximate positions
sam <- data.frame(
  place = c("Bethlehem\nhome + Lehigh classes", "Allentown\npart-time job",
            "Easton\ngym", "Philadelphia\nweekend visit"),
  lon = c(-75.37, -75.49, -75.22, -75.165), lat = c(40.61, 40.60, 40.69, 39.95),
  hj = c(1, 1, 0, 0), vj = c(1, 0, 0.5, 0.5),
  nx = c(-0.04, -0.04, 0.05, 0.06), ny = c(-0.05, 0.04, 0, 0))

map_sam <- function() {
  tr <- shorten(sam$lon[c(1, 1, 1)], sam$lat[c(1, 1, 1)], sam$lon[2:4], sam$lat[2:4], 0.03, 0.025)
  ggplot() +
    geom_curve(data = tr, aes(x, y, xend = xend, yend = yend), curvature = 0.15,
               colour = col$path, linewidth = 0.9,
               arrow = arrow(length = unit(0.12, "inches"), ends = "both", type = "closed")) +
    geom_point(data = sam, aes(lon, lat), shape = 21, size = c(7, 5, 5, 5),
               fill = c(col$case, col$path, col$path, col$path), colour = "white", stroke = 1.1) +
    geom_text(data = sam, aes(lon + nx, lat + ny, label = place, hjust = hj, vjust = vj),
              size = 4.4 * tm(), lineheight = 0.9, colour = col$ink) +
    annotate("text", x = -76.35, y = 39.9, hjust = 0, size = 3.2 * tm(), colour = col$muted,
             label = "approximate locations") +
    coord_quickmap(xlim = c(-76.4, -74.1), ylim = c(39.84, 40.84), expand = FALSE) +
    theme_void() +
    theme(panel.background = element_rect(fill = col$land, colour = NA),
          plot.background = element_rect(fill = "white", colour = NA))
}

# four reveals side by side
map_summary <- function() {
  op <- options(riverton.tm = min(tm(), 1.1)); on.exit(options(op))
  t <- function(g, s) g + ggtitle(s) +
    theme(plot.title = element_text(size = 13 * tm(), face = "bold", colour = col$ink, margin = margin(b = 3)))
  (t(map_cases(FALSE), "1. Where they live now") | t(map_source(FALSE), "2. + a suspected source")) /
    (t(map_work(FALSE), "3. + where they worked") | t(map_1985(FALSE), "4. Where they lived in 1985"))
}
