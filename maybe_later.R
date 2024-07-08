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
