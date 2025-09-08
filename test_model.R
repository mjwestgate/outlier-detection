library(readr)
library(boot)

data <- read_csv(here("frogs_clean.csv")) |>
  mutate(is_citsci = as.factor(is_citsci),
         is_specimen = as.factor(as.logical(is_specimen)),
         outside_range = as.integer(outside_range)) |>
  filter(eventDate > ymd("1800-01-01"))

# xtabs(~ is_citsci + is_specimen, data = data)

model <- glm(outside_range ~ 
            eventDate * is_citsci  +
            eventDate * is_specimen +
            coord_sum +
            log_uncert_fixed,
            family = binomial(link = "logit"),
            data = data)

summary(model)


prediction_df <- tibble(
  eventDate = c(
    seq(
      {data |> filter(is_citsci == FALSE) |> pull(eventDate) |> min(na.rm = TRUE)},
      {data |> filter(is_citsci == FALSE) |> pull(eventDate) |> max(na.rm = TRUE)},
      # min(data$eventDate, na.rm= TRUE), 
      # max(data$eventDate, na.rm = TRUE), 
      length.out = 100),
    seq(
      {data |> filter(is_citsci == TRUE) |> pull(eventDate) |> min(na.rm = TRUE)},
      {data |> filter(is_citsci == TRUE) |> pull(eventDate) |> max(na.rm = TRUE)},
      length.out = 100)) |> rep(2),
  is_specimen =  as.factor(c(rep(FALSE, 200), rep(TRUE, 200))),
  is_citsci = as.factor(rep(rep(c(FALSE, TRUE), each = 100), 2)),
  coord_sum = mean(data$coord_sum),
  log_uncert_fixed = mean(data$log_uncert_fixed))

test <- predict(model, 
                newdata = prediction_df, 
                se.fit = TRUE,
                type = "link")
prediction_df$fit <- inv.logit(test$fit)
prediction_df$lci <- inv.logit(test$fit - (2 & test$se.fit))
prediction_df$uci <- inv.logit(test$fit + (2 & test$se.fit))

# and for plotting
p <- ggplot(prediction_df,
       aes(x = eventDate, 
           y = fit, 
           group = is_citsci,
           fill = is_citsci)) +
  facet_wrap(~is_specimen) +
  geom_ribbon(
    aes(ymin = lci, ymax = uci),
    alpha = 0.5)+
  geom_path() 

ggsave("model_plot.pdf", p)
