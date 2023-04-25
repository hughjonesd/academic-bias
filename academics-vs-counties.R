
#== Data ====
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(ggforce)
library(glue)

cp <- readr::read_csv("dataverse_files/countypres_2000-2020.csv")
cpl <- cp |> 
        filter(mode == "TOTAL", year == 2020) |> 
        tidyr::pivot_wider(id_cols = -candidate, names_from = party, 
                           values_from = candidatevotes) |> 
        mutate(
          dem_share = DEMOCRAT/totalvotes,
          dem_rep   = DEMOCRAT/(DEMOCRAT+REPUBLICAN),
          dem_rep_other = dem_rep
        )

copop <- readr::read_csv("co-est2022-alldata.csv") |> 
         filter(SUMLEV == "050") |> 
         select(
           STATE, COUNTY, pop = POPESTIMATE2020
         ) |> 
         mutate(
           county_fips = paste0(STATE, COUNTY),
           pop_share = pop/sum(pop)
         )
cpl <- cpl |> 
         left_join(copop, by = "county_fips", relationship = "one-to-many") |> 
         filter(! is.na(pop))

acad <- readr::read_csv("academics-by-discipline.csv", col_types = "cnnnnnc") |> 
          rename(Discipline = `...1`) |> 
          filter(Discipline != "Total") |> 
          rowwise() |> 
          mutate(
            Discipline = gsub("/.*", "", Discipline),
            dem_rep = Democratic/(Democratic + Republican),
            prop_more_dem = sum(cpl$pop_share[cpl$dem_rep > dem_rep]),
            Other = `Not Affiliated` + `Not Registered`,
            dem_rep_other = (Democratic + Other/2)/
              (Democratic + Republican + Other),
            prop_more_dem_other = mean(cpl$dem_rep > dem_rep_other)
          )

dens_data <- density(cpl$dem_rep, weights = cpl$pop/sum(cpl$pop))
dens_data <- as.data.frame(dens_data[1:2])
acad$density_y <- sapply(acad$dem_rep, function (x) {
  dens_data$y[which.min(abs(x - dens_data$x))]
})
acad$colour <- palette("Dark 2")[2:6]


libarts <- tribble(
  ~Discipline, ~dem_rep,
  "Engineering", 1.6,
  "Chemistry", 5.2, 
  "Economics", 5.5, 
  "Professional", 5.5,
  "Mathematics", 5.6,
  "Physics", 6.2,
  "Computers", 6.3,
  "Polisci", 8.2,
  "Psychology", 16.8,
  "History", 17.4,
  "Philosophy", 17.5,
  "Biology", 20.8,
  "Language", 21.1,
  "Environmental", 25.3,
  "Geoscience", 27, 
  "Classics", 27.3,
  "Theatre", 29.5,
  "Music", 32.8,
  "Art", 40.3,
  "Sociology", 43.8,
  "English", 48.3,
  "Religion", 70,
  "Anthropology", 1e6, # no republicans
  "Communications", 1e6
) |> 
  rowwise() |> 
  mutate(
    dem_rep = dem_rep/(dem_rep+1),
    prop_more_dem = sum(cpl$pop_share[cpl$dem_rep > dem_rep]),
    density_y = sapply(dem_rep, function (x) {
                  dens_data$y[which.min(abs(x - dens_data$x))]
                })
  )

# == Plot 1 ====

subtitle <- glue("<span style='color:{acad$colour}'>{tolower(acad$Discipline)}</span>") |> 
            glue_collapse(sep = ", ", last = " and ") |> 
            paste0("Disciplines: ", ... = _, ".\n", 
            "Numbers: % of US population in counties with a higher Democrat share.", 
            " Density is population-weighted.")
ggp <- ggplot(cpl, aes(dem_rep)) + 
  geom_density(aes(weight = pop), fill = "grey80", color = NA) +
  geom_segment(data = acad, y = 0,  
              mapping = aes(xend = dem_rep, yend = density_y,
                            color = Discipline), linewidth = 1.1) +
  geom_point(data = acad, aes(y = density_y, color = Discipline), 
             size = 2.5) +
  scale_color_manual(values = setNames(acad$colour, acad$Discipline)) +
  scale_y_continuous(breaks = NULL, expand = expansion(0, 0)) +
  theme_minimal() +
  labs(
    x = "Democrat share", 
    y = "",
    title = "Share of Democrats in academic disciplines and US counties",
    subtitle = subtitle,
    caption = "Source: Langbert, Quain, and Klein (2016); 2020 county presidential returns; Census population estimates."
  ) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.title = element_textbox_simple(family = "Helvetica"),
    plot.subtitle = element_textbox_simple(size = 11, lineheight = 1.2)
  )

ggp_main <- ggp + 
  geom_text(data = acad, aes(y = density_y, color = Discipline, 
                             label = scales::percent(prop_more_dem, 1)), 
            nudge_y = 0.12) +
  coord_cartesian(ylim = c(-0.1, 4))

ggp_zoom <- ggp + 
  geom_text(data = acad, aes(y = density_y, color = Discipline, 
                             label = scales::percent(prop_more_dem, 1)), 
            nudge_y = 0.01) +
  coord_cartesian(xlim = c(0.89, 0.99), ylim = c(0, 0.2)) +
  labs(title = "", subtitle = "", x = "", caption = "") +
  theme(
    plot.background = element_rect(color = "grey20"),
    plot.margin = unit(c(0,3,1,3), "mm"),
    panel.spacing = unit(0, "mm"),
    plot.caption = element_blank(),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.title = element_blank()
  )
  
grob_zoom <- ggplotGrob(ggp_zoom)


ggp_main + 
  annotation_custom(grob_zoom, xmin = 0.65, xmax = 0.97, ymin = 2.3, ymax = 3.9) +
  annotate("rect", xmin = .87, xmax = 0.99, 
           ymin = -0.05, ymax = 0.5, fill = NA,
           linewidth = 0.3, color = "grey20") +
  annotate("segment", x = c(0.87, 0.99), xend = c(0.65, 0.97), y = 0.5, yend = 2.3, 
           linewidth = 0.3, linetype = 3)


#== Plot 2 ====
libarts_pal <- c(palette("Dark 2")[2:6], rep("black", 19))
names(libarts_pal) <- libarts$Discipline

ggpla <- ggplot(cpl, aes(dem_rep)) + 
  geom_density(aes(weight = pop), fill = "grey80", color = NA) +
  geom_segment(data = libarts, y = 0,  
              mapping = aes(xend = dem_rep, yend = density_y, x = dem_rep,
                            colour = Discipline), 
              linewidth = 0.5, position = position_dodge(width = 0.002)) +
  geom_point(data = libarts, aes(y = density_y, colour = Discipline)) +
  scale_y_continuous(breaks = NULL, expand = expansion(0, 0)) +
  theme_minimal() +
  labs(
    x = "Democrat share", 
    y = "",
    title = "Share of Democrats in academic disciplines and US counties",
    subtitle = "Professors in liberal arts colleges. Numbers: % of US population in counties with a higher Democrat share. Density is population-weighted.",
    caption = "Source: Langbert (2018); 2020 county presidential returns; 2020 census population estimates"
  ) +
  coord_cartesian(ylim = c(0, 4)) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    panel.border = element_blank(),
    plot.title = element_textbox_simple(family = "Helvetica"),
    plot.subtitle = element_textbox_simple(size = 11, lineheight = 1.2)
  ) 

ggpla 

ggpla_main <- ggpla + 
  geom_text(data = libarts |> filter(dem_rep < 0.9),
            aes(y = density_y, label = Discipline, colour = Discipline),
            nudge_y = 0.04, nudge_x = 0.00, angle = 45, hjust = 0, vjust = 1,
            # size = 0.5,
            check_overlap = TRUE)

ggpla_zoom <- ggpla + 
  geom_text(data = libarts,
            aes(y = density_y, label = Discipline, colour = Discipline),
            nudge_y = 0.002, nudge_x = 0.00, angle = 45, hjust = 0, vjust = 1,
            # size = 0.5,
            check_overlap = TRUE) +
  scale_color_discrete() +
  coord_cartesian(xlim = c(0.94, 1.02), ylim = c(0, 0.025)) 

ggpla_window <- ggpla_zoom + 
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    plot.caption = element_blank(),
    plot.background = element_rect(color = "grey20"),
    axis.title = element_blank()
  )
ggpla_main + 
  annotation_custom(ggplotGrob(ggpla_window), xmin = 0.6, xmax = 1, ymin = 2.7, 
                    ymax = 4) + 
  annotate("rect", xmin = 0.94, xmax = 1, ymin = 0, ymax = 0.1, fill = NA,
           color = "grey20", linewidth = 0.3) +
  annotate("segment", x = c(0.94, 1), xend = c(0.6, 1), y = 0.1, yend = 2.7,
           linewidth = 0.3, linetype = 3)
