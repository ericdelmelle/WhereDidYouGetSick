# Export every map as PNG to figures/ (for Canvas, other slides, printing)
source("R/riverton.R")
source("R/maps.R")

w <- 10.5; h <- w * 4200 / 8000
sv <- function(g, f, wd = w, ht = h) ggsave(file.path("figures", f), g, width = wd, height = ht, dpi = 200, bg = "white")

options(riverton.tm = 1.5)  # projection-sized labels
sv(map_town(), "00_riverton.png")
sv(map_cases(), "01_cases.png")
sv(map_source(), "02_source.png")
sv(map_work(), "03_workplaces.png")
sv(map_commons(), "04a_senior_housing.png")
sv(map_1985(), "04b_homes_1985.png")
sv(map_maya_dot(), "maya_dot.png")
sv(map_maya_week(), "maya_week.png")
sv(map_frank(1), "frank_record.png")
sv(map_frank(3), "frank_places.png")
sv(frank_timeline(), "frank_timeline.png", w, 3.6)
sv(map_sam(), "sam_infection.png", 8, 4.3)
sv(map_summary(), "summary_four_reveals.png", 14, 7.6)
