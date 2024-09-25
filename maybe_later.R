acled_filtered |> 
  group_by(event_type) |> 
  summarise(fatalities = sum(fatalities, na.rm = TRUE)) |> 
  arrange(desc(fatalities)) |> 
  mutate(fatalities = format(fatalities, big.mark = ",")) |> 
  kable(caption = "Fatalities by event type, ACLED") %>%
  kable_styling(bootstrap_options = c("striped", "hover"),
                font_size = 12, full_width = FALSE) 

acled_actors |> 
  group_by(country, year, actor_description) |> 
  summarise(num_actors = n_distinct(actor), .groups = "drop") |> 
  filter(!is.na(actor_description)) |> 
  mutate(country = fct_relevel(country, acled_actors_order)) |> 
  ggplot(aes(x = year, y = num_actors, group = actor_description)) + 
  geom_col(aes(fill = actor_description)) + 
  facet_wrap(~country, scales = "free_y") + 
  scale_fill_viridis_d(option = "turbo") + 
  scale_x_continuous(breaks = c(2014, 2016, 2018, 2020, 2022)) + 
  scale_y_continuous(labels = number_format(scale = 1)) + 
  theme(axis.text.x = element_text(hjust = 1, angle = 45), 
        legend.position = "top", 
        legend.text = element_text(size = 5), 
        legend.key.width = unit(.3, "cm"), 
        legend.key.height = unit(.3, "cm"), 
        legend.margin=margin(t = 0, unit='cm')) + 
  labs(title = "ACLED: Breakdown of conflict actor types in the Asia-Pacific, 2014-2023", 
       x = "", y = "Number of actors", 
       fill = "") + 
  guides(fill = guide_legend(nrow = 1)) + 
  theme(
    axis.text.y = element_text(size = 5)
  )

tracker_words <- tracker |> 
  filter(start_date >= "2014-01-01") |> 
  select(id, incident_description) |> 
  unnest_tokens(word, incident_description) |> 
  anti_join(stop_words, by = "word") |>
  add_count(word) |> 
  arrange(desc(n)) |>
  left_join(
    tracker |> 
      select(id, province, district, llg,
             status, total_dead, total_injured, 
             conflict_name),
    by = "id")

ged_png |> 
  filter(year >= 1995) |> 
  group_by(year) |>
  summarise(events = n()) |> 
  ggplot(aes(y = events, x = year)) + 
  geom_line() + 
  scale_x_continuous(breaks = seq(1996, 2024, 2))
scale_x_date(d
             ate_breaks = "2 years", 
             date_label = "%Y")

lashio_captured<- tribble(~x, ~y, ~year,
                          97.7525, 22.9665, 2024, 
                          92.6, 28, 2024
)

lashio_captured_text <- tribble(~x, ~y, ~year, ~label,
                                92.9, 28, 2024, "Captured")
# Lashio captured
geom_point(aes(x = x, y = y), size = 2.5, data = lashio_captured,
           colour = "mediumseagreen", pch = 15) +
  geom_text(aes(x = x, y = y), 
            label = "Captured",
            size = 2, data = lashio_captured_text, 
            face = "bold", 
            colour = "seashell")
  
  
  protest_country_words |>
  # filter(covid == "post-covid") |> 
  distinct(event_id_cnty, word, covid) |> 
  add_count(word) |> 
  pivot_wider(names_from = covid, values_from = n, values_fill = 0) |> 
  janitor::clean_names() |> 
  mutate(n = post_covid + pre_covid) |> 
  arrange(desc(n))

  acled_actors_indonesia |>
    mutate(covid = ifelse(event_date >= "2020-03-11", "Post-covid", "Pre-covid"), 
           covid = fct_relevel(covid, "Pre-covid", "Post-covid")) |>
    group_by(actor_description, admin1, admin1_label, covid) |> 
    summarise(events = n_distinct(event_id_cnty), 
              fatalities = sum(fatalities, na.rm = TRUE), 
              .groups = "drop") |> 
    mutate(actor_description = fct_relevel(actor_description, 
                                           c("Civilians", 
                                             "State Forces",  
                                             "Protesters",
                                             "Identity Militias", 
                                             "Rebel Groups", 
                                             "Rioters", 
                                             "Political Militias", 
                                             "Other Forces"
                                           ))) |> 
    left_join(
      indo_adm1_pop |>  
        mutate(
          adm1_en = str_trim(adm1_en), 
          adm1_en = case_when(
            str_detect(adm1_en, "Jakarta") ~ "Jakarta", 
            str_detect(adm1_en, "Yogyakarta") ~ "Yogyakarta",
            TRUE ~ adm1_en)),
      by = c("admin1" = "adm1_en")
    ) |> 
    mutate(
      events_100k = events / population * 100000, 
      fatalities_100k = fatalities / population * 100000, 
      covid = fct_relevel(covid, c("Pre-covid", "Post-covid"))
    ) |>
    ggplot(aes(x = events_100k + 0.0001, y = fatalities_100k + 0.0001)) + 
    geom_hline(yintercept = 0.0001, lwd = .2, linetype = "dashed", alpha = .5) + 
    geom_vline(xintercept = 0.0001, lwd = .2, linetype = "dashed", alpha = .5) + 
    geom_point(alpha = 0, aes(size = fatalities, colour = actor_description)) + 
    geom_text_repel(aes(label = admin1_label,  size = fatalities, colour = actor_description), 
                    show.legend = FALSE, max.overlaps = 20, 
                    box.padding = .1, 
                    label.padding = .1, 
                    label.r = .1, 
                    force = .5,
                    force_pull = .5, 
                    vjust = "inward") + 
    facet_wrap(~ covid) + 
    scale_x_log10() +
    scale_y_log10() + 
    scale_color_manual(values = c(
      "Civilians" = "#fde725ff",
      "State Forces" = "#7A0403FF",
      "Protesters" = "#4AC16DFF",
      "Identity Militias" = "#4777EFFF",
      "Rebel Groups" = "#FE9B2DFF",
      "Rioters" = "#1BD0D5FF",
      "Political Militias" = "#D3436EFF",
      "Other Forces" = "grey30")) +
    scale_size_continuous(range = c(1, 5)) +
    guides(colour = guide_legend(override.aes = list(alpha = 1, 
                                                     size = 1.8)), 
           size = guide_legend(override.aes = list(alpha = 1, 
                                                   colour = "grey"))) + 
    labs(x = "No. of Events per 100k", 
         y = "No. of Fatalities per 100k",
         title = "Conflict actor types in Indonesia by number of events and fatalities (2014-2024)", 
         size = "Fatalities", 
         colour = "Actor type", 
         caption = "Source: www.acleddata.com") + 
    theme(plot.caption = element_text(hjust = .5), 
          strip.background = element_rect(fill = "black"), 
          strip.text = element_text(face = "bold"), 
          legend.text = element_text(size = 6), 
          legend.title = element_text(size = 7)) 
  
  ggsave(here("plots", "conflict_actors_indonesia.png"), dpi = 300, 
         height = 7, width = 11, units = "in")
  
  
  
  acled_filtered |> 
    group_by(country, event_type, sub_event_type) |> 
    summarise(events = n(), 
              fatalities = sum(fatalities), .groups = "drop") |>
    filter(country %in% top_10_annual_fatalities) |>
    mutate(country = fct_relevel(country, top_10_annual_fatalities)) |> 
    ggplot(aes(x = events + 0.01, 
               y = fatalities + 0.01)) + 
    geom_hline(yintercept = .01, alpha = .5, linetype = "dashed", linewidth = .5) +
    geom_vline(xintercept = .01, alpha = .5, linetype = "dashed", linewidth = .5) +
    geom_point(aes(colour = event_type)) + 
    geom_text_repel(aes(label = sub_event_type), 
                    size = 1.5, 
                    max.overlaps = 16) + 
    facet_wrap(~ country, scales = "free", ncol = 3) +
    scale_x_log10(breaks = c(1, 10, 100, 1000, 10000, 60000)) + 
    scale_y_log10(breaks = c(1, 10, 100, 1000, 10000, 60000)) + 
    theme(strip.background = element_rect(fill = "black"))
  
  
  acled_actors_indonesia <- acled_actors |> 
    filter(country == "Indonesia") |> 
    left_join(
      acled_filtered |> 
        filter(country == "Indonesia") |> 
        select(event_id_cnty, admin1), 
      by = "event_id_cnty"
    ) |> 
    # This matches the ACLED and location gazette and pcode datasets
    mutate(admin1 = ifelse(
      str_detect(admin1, "West|East|South|North|Southeast|Southwest|Central|Highland") & 
        !str_detect(admin1, "Nusa"), 
      paste0(stringr::word(admin1, 2), " ", stringr::word(admin1, 1)), 
      admin1
    )) |> ungroup() |> 
    mutate(admin1 = case_when(
      str_detect(admin1, "Southwest") ~ str_replace(admin1, "Southwest", "Barat Daya"), 
      str_detect(admin1, "Southeast") ~ str_replace(admin1, "Southeast", "Tenggara"), 
      str_detect(admin1, "North") ~ str_replace(admin1, "North", "Utara"), 
      str_detect(admin1, "South") ~ str_replace(admin1, "South", "Selatan"), 
      str_detect(admin1, "East") ~ str_replace(admin1, "East", "Timur"), 
      str_detect(admin1, "West") ~ str_replace(admin1, "West", "Barat"), 
      str_detect(admin1, "Central") ~ str_replace(admin1, "Central", "Tengah"), 
      str_detect(admin1, "Highland") ~ str_replace(admin1, "Highland", "Pegunungan"),
      TRUE ~ admin1
    )) |> 
    mutate(
      admin1 = case_when(
        str_detect(admin1, "Java") ~ str_replace_all(admin1, "Java", "Jawa"), 
        admin1 == "Barat Nusa Tenggara" ~ "Nusa Tenggara Barat", 
        admin1 == "Timur Nusa Tenggara" ~ "Nusa Tenggara Timur",
        str_detect(admin1, "Bangka") ~ "Kepulauan Bangka Belitung", 
        str_detect(admin1, "Sumatra") ~ str_replace(admin1, "Sumatra", "Sumatera"), 
        admin1 == "Riau Islands" ~ "Kepulauan Riau", 
        str_detect(admin1, "Jakarta") ~ "Jakarta",
        str_detect(admin1, "Yogyakarta") ~ "Yogyakarta",
        admin1 == "Papua Tengah" ~ "Papua", 
        admin1 == "Papua Selatan" ~ "Papua", 
        admin1 == "Papua Pegunungan" ~ "Papua", 
        admin1 == "Papua Barat Daya" ~ "Papua Barat", 
        TRUE ~ admin1),
      admin1 = str_trim(admin1)) |> 
    left_join(
      indo_adm1_pop |>  
        mutate(
          adm1_en = str_trim(adm1_en), 
          adm1_en = case_when(
            str_detect(adm1_en, "Jakarta") ~ "Jakarta", 
            str_detect(adm1_en, "Yogyakarta") ~ "Yogyakarta",
            TRUE ~ adm1_en)),
      by = c("admin1" = "adm1_en")
    ) |> 
    mutate(admin1_label = str_replace_all(admin1, 
                                          c("Timur" = "Ti", 
                                            "Barat" = "B", 
                                            "Tengah" = "Tgh", 
                                            "Tenggara" = "Tgg", 
                                            "Utata" = "U", 
                                            "Selatan" = "S")))  
  
  actor_list_indonesia <- acled_actors_indonesia |>   
    mutate(quarter = floor_date(event_date, unit = "quarter")) |>
    group_by(admin1, quarter) |> 
    summarise(actors = n_distinct(actor), .groups = "drop") |> 
    group_by(admin1) |> 
    summarise(actors = mean(actors)) |> 
    arrange(desc(actors)) |> 
    pull(admin1)
  
  lbn_food |> 
    mutate(quantity = parse_number(unit),
           unit = str_extract(unit, "[A-Za-z]+"), 
           usdprice = as.numeric(usdprice), 
           price = as.numeric(price)) |> 
    filter(pricetype == "Retail") |> 
    mutate(unit_price = price / quantity, 
           category_unit = paste0(category, " ", date)) |> 
    group_by(commodity, category, unit, date) |>
    summarise(unit_price = mean(unit_price, na.rm = TRUE), .groups = "drop") |> 
    filter(!is.nan(unit_price)) |> 
    group_by(commodity, category, date, unit) |> 
    summarise(unit_price = mean(unit_price), .groups = "drop") |> 
    filter(commodity %in% 
             c("Eggs", "Oil (sunflower)", "Cheese (picon)", 
               "Fuel (diesel)", "Fuel (gas)", "Fuel (petrol-gasoline, 95 octane)", 
               "Beans (white)", "Bread (pita)", "Bulgur (brown)") | 
             str_detect(commodity, "beef|tuna|sardine")) |>  
    ggplot(aes(x = date, 
               y = unit_price)) + 
    geom_line(aes(colour = category), 
              linewidth = .7, 
              alpha = .9) + 
    facet_wrap(~ commodity, scales = "free_y") + 
    scale_x_date(date_breaks = "2 years", date_labels = "%Y") + 
    labs(y = "LBP price per unit (g, L, pc)", 
         x = "", 
         title = "Mean commodity prices in Lebanon, from WFP", 
         colour = "") + 
    theme(axis.text.x = element_text(angle = 30, hjust = 1), 
          strip.background = element_rect(fill = "black")) + 
    guides(colour = guide_legend(override.aes = list(linewidth = 1.5)))
 
lebanon_actors |> 
  filter(country == "Lebanon") |> 
  group_by(actor_description) |> 
  summarise(events = n_distinct(event_id_cnty), 
            fatalities = sum(fatalities, na.rm = TRUE)) |> 
  pivot_longer(cols = -actor_description, 
               names_to = "type", 
               values_to = "value") |> 
  ggplot(aes(y = fct_rev(actor_description), x = value)) + 
  geom_col() + 
  facet_wrap(~type, scales = "free_x")
  count(actor_description)



Major protests topics include: political instability and dissatisfaction with the government, economic woes and the subsequent neglect of public infrastructure, the Israel-Palestinian conflict, legal and illegal detainees, and the port of Beirut explosion. 

Protests in Lebanon have largely revolved around the severe economic and political instability which has plagued the country. We also note few descriptors related to sectarian violence (i.e ), indicating that none of the various political and communal militias and groups are currently agitating (at a national level) for separatism, 


In this section, we explore the concerns of protesters in Lebanon. Below is a network graph of event descriptions of protests in Lebanon since 2016. Network graphs shows the connections between various observations (in this case, protest descriptors from Lebanon) and are commonly used to visualise social networks. 

The links between each of the words indicate the strength of the relationship (transparency) and the number of times this word pair has occurred (thickness). Only the most common word pairs with correlations above 0.2 are included (for legibility). 

The following major protest topics (from 2016-) have been identified from descriptions of protests events in Lebanon: 
  
  * Political instability and dissatisfaction with the government 
* Economic woes and the subsequent neglect of public infrastructure
* The Israel-Palestine conflict
* Legal and illegal detentions, calls for release
* Port of Beirut explosion 

Relatively fewer descriptors related to sectarian violence (i.e. Islam, Muslim, Sunni, Shiite, Christian) were noted. 

forecasts |> 
  filter(str_detect(indicator, "Aggreg") & 
           !str_detect(indicator, "Rank")) |> 
  mutate(indicator = str_remove(indicator, "AggregMetric-"), 
         indicator = str_replace(indicator, "m", " months")) |> 
  arrange(indicator) |> 
  filter(iso != "IRN") |> 
  group_by(indicator) |> 
  mutate(range = range_wna(value)) |> 
  ungroup() |> 
  left_join(world_shape |> filter(iso3 %in% c("ISR", "JOR", "LBN", "PSE", "SYR")), 
            by = c("iso" = "iso3"), 
            relationship = "many-to-many") |> 
  st_as_sf() |> 
  ggplot() +
  geom_sf(aes(fill = value)) + 
  facet_wrap(~ factor(indicator, 
                      levels = c("3 months", "6 months", "12 months")), nrow = 1) + 
  scale_fill_viridis_c(option = "magma", direction = -1) + 
  theme(strip.background = element_rect(fill = "black"), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        plot.caption = element_text(hjust = .5, size = 6)) + 
  labs(fill = "Normalised\nvalue", 
       title = "Conflict forecasts -- RAH Aggregates", 
       subtitle = "From September 2024", 
       caption = "The designations employed and the presentation of the material on this map do not imply the expression of any opinion whatsoever on the part of the Secretariat of the United Nations concerning the legal status\nof any country, territory, city or area or of its authorities, or concerning the delimitation of its frontiers or boundaries. This map is provided without any warranty of any kind whatsoever, either express or implied.")

forecasts |> 
  filter(str_detect(indicator, "6m") & 
           indicator %out% c("CAST-VaC_events-6m", 
                             "CAST-Battles_events-6m", 
                             "AggregMetric-6m", 
                             "AggregRank-6m")) |> 
  filter(iso != "IRN") |> 
  select(iso, country, indicator, value) |> 
  group_by(indicator) |> 
  mutate(range = range_wna(value), 
         indicator = str_replace(indicator, "ConfInt", "ConflictIntensity")) |> 
  ungroup() |> 
  left_join(world_shape |> filter(iso3 %in% c("ISR", "JOR", "LBN", "PSE", "SYR")), 
            by = c("iso" = "iso3"), 
            relationship = "many-to-many") |> 
  st_as_sf() |> 
  ggplot() +
  geom_sf(aes(fill = range)) + 
  facet_wrap(~indicator, nrow = 2) + 
  scale_fill_viridis_c(option = "magma", direction = -1) + 
  theme(strip.background = element_rect(fill = "black")) + 
  labs(fill = "Normalised\nvalue", 
       title = "Conflict forecasts -- 6 months from 2024-09")
       
       