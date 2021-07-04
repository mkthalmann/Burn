library(tidyverse)
library(hrbrthemes)
library(here)
library(glue)
library(patchwork)
library(ggtext)
library(scales)
library(lubridate)

# import the data and compute (daily) total games
d_plot <- read.csv(here("data", "results.csv"), sep = ";") %>%
    mutate(
        date = as.Date(date, format = "%d.%m.%Y"),
        total_day = rowSums(across(where(is.numeric)), na.rm = T),
        total = sum(total_day),
        games_cumsum = cumsum(total_day)
    ) %>%
    pivot_longer(
        cols = !c(date, total_day, total, games_cumsum),
        names_to = "player",
        values_to = "wins"
    ) %>%
    mutate(
        player = str_to_title(player),
        wins = as.double(wins),
        there = if_else(is.na(wins), "no", "yes"),
        wins = if_else(is.na(wins), 0, wins)
    ) %>%
    group_by(player) %>%
    mutate(
        # how many games did each player participate in?
        games_part = sum(ifelse(there == "yes", total_day, 0)),
        year = year(date),
        month = month(date, label = TRUE),
        day = day(date),
        week_day = wday(date, label = TRUE),
    ) %>%
    relocate(date, year, month, day, week_day)

# import data with the most cards drawn per day (only for online sessions)
d_most <- read.csv(here("data", "most.csv"), sep = ";") %>%
    pivot_longer(
        cols = c(
            hanna_most,
            stine_most,
            lena_most,
            maik_most,
            martha_most,
            lotti_most
        ),
        names_to = "player", values_to = "most"
    ) %>%
    mutate(
        player = str_to_title(gsub("_most", "", player)),
        date = as.Date(date, format = "%d.%m.%Y"),
        most = as.double(most),
        most = if_else(is.na(most), 0, most)
    ) %>%
    filter(most != 0) %>%
    group_by(player) %>%
    mutate(
        most_mean = mean(most, na.rm = TRUE),
        most_min = min(most, na.rm = TRUE),
        most_max = max(most, na.rm = TRUE),
        most_low = most_mean - sciplot::se(most) * 1.96,
        most_high = most_mean + sciplot::se(most) * 1.96
    )

# get the number of months played
months_played <- round(as.numeric(max(d_plot$date) - min(d_plot$date)) / 30.417, 1)

# source my custom theme
source(here("theme.R"))

# generic plotting function for the two variants of the plot
plot_panel <- function(d_plot, d_most, pnt_size = 3) {
    # cumulative wins over time
    p_time <- d_plot %>%
        group_by(player) %>%
        mutate(
            wins_cumsum = cumsum(wins),
            wins_percent = scales::label_percent(accuracy = 0.1)(wins_cumsum / games_cumsum),
            # only show final win percentage
            wins_percent = if_else(date != max(date), "", wins_percent),
            wins_cumsum_label = if_else(
                date != max(date),
                "",
                as.character(glue("{wins_cumsum}/{games_part}"))
            ),
            date = gsub("2021-", "", as.factor(date)),
            date = gsub("-", ".", as.factor(date))
        ) %>%
        ggplot(aes(
            x = date,
            y = wins_cumsum,
            color = player,
            group = player
        )) +
        geom_line(alpha = .8, size = .2) +
        geom_point(aes(shape = there), size = pnt_size, stroke = 1) +
        geom_text(
            aes(
                label = wins_percent,
                x = date,
                y = wins_cumsum
            ),
            nudge_x = .6,
            nudge_y = -2.4,
            hjust = "left",
            size = 4,
            fontface = "bold"
        ) +
        geom_text(
            aes(
                label = wins_cumsum_label,
                x = date,
                y = wins_cumsum
            ),
            nudge_x = .6,
            hjust = "left",
            size = 4,
            fontface = "bold"
        ) +
        facet_wrap(~player, nrow = 1) +
        guides(color = FALSE, shape = FALSE) +
        theme(
            axis.title.y = element_markdown(),
            axis.text.x = element_text(size = 11, angle = 90)
        ) +
        labs(
            subtitle = glue(
                "**Kumulative Gewinne über insgesamt
            <span style='color:gray;'>{length(unique(d_plot$date))}</span>
            Spieltage (über <span style='color:gray;'>~{months_played}</span> Monate)**<br>
            Teilnahme am jeweiligen Spieldatum
            wird durch Art des Datenpunktes angezeigt (× abwesend)."
            ),
            x = "◴ Datum ◴",
            y = "✓ Kumulative<br>Gewinne"
        ) +
        scale_color_manual(values = colors) +
        # scale_x_date(expand = c(.2, .2)) +
        scale_y_continuous(breaks = scales::pretty_breaks()) +
        scale_shape_manual(values = c(4, 15)) +
        coord_cartesian(clip = "off", expand = TRUE)


    # daily win rate
    p_rate <- d_plot %>%
        group_by(player) %>%
        filter(there == "yes") %>%
        summarise(
            winrate = mean(wins),
            winrate_low = winrate - sciplot::se(wins) * 1.96,
            winrate_high = winrate + sciplot::se(wins) * 1.96
        ) %>%
        ggplot(aes(x = player, y = winrate, color = player, group = player)) +
        geom_point(size = 4, pch = 15) +
        geom_errorbar(aes(
            ymin = winrate_low,
            ymax = winrate_high
        ), width = .05, alpha = .5) +
        geom_text(
            aes(
                label = round(winrate, 2),
                y = winrate
            ),
            nudge_x = .06,
            hjust = "left",
            size = 4,
            fontface = "bold"
        ) +
        guides(color = FALSE) +
        theme(axis.title.y = element_markdown()) +
        scale_y_continuous(limits = c(0, NA), oob = rescale_none) +
        scale_color_manual(values = colors) +
        coord_cartesian(clip = "off") +
        labs(
            subtitle = glue(
                "**Abendliche Gewinnrate**<br>
            Durchschnittlich <span style='color:gray;'>{round(mean(d_plot$total_day), 2)}</span>
            \u00B1 <span style='color:gray;'>{round(sd(d_plot$total_day), 2)}</span>
            (Median <span style='color:gray;'>{round(median(d_plot$total_day), 2)}</span>)
            Spiele pro Abend."
            ),
            x = "☺ Spieler*in ☺",
            y = "✓ Gewinnrate<br>\u00B1 95% KonfInt"
        )

    # most drawn cards
    p_most <- d_most %>%
        select(player, most_mean, most_max, most_min, most_low, most_high) %>%
        unique() %>%
        ggplot(aes(x = player, y = most_mean, color = player)) +
        geom_point(shape = 15, size = 4) +
        geom_errorbar(
            aes(ymin = most_low, ymax = most_high),
            width = .05,
            alpha = .5
        ) +
        geom_point(
            aes(y = most_min, fill = player),
            shape = 25,
            size = 2.4,
            alpha = .5
        ) +
        geom_point(
            aes(y = most_max, fill = player),
            shape = 24,
            size = 2.4,
            alpha = .5
        ) +
        guides(color = FALSE, fill = FALSE) +
        scale_color_manual(values = colors) +
        scale_fill_manual(values = colors) +
        scale_y_continuous(limits = c(0, NA), oob = rescale_none) +
        geom_text(
            aes(
                label = round(most_mean, 2),
                y = most_mean
            ),
            nudge_x = .06,
            hjust = "left",
            size = 4,
            fontface = "bold"
        ) +
        theme(axis.title.y = element_markdown()) +
        labs(
            subtitle = "**Maximal aufgenommene Karten**
        <br>
        Maxima und Minima (der Maxima) werden durch Dreiecke dargestellt.",
            x = "☺ Spieler*in ☺",
            y = "Kartenmaxima<br>\u00B1 95% KonfInt"
        )

    p <- p_time / p_rate / p_most + plot_annotation(
        title = paste(
            "♠<span style='color:gray;'>♥</span>♣<span style='color:gray;'>♦</span>",
            glue(
                "Gewinnübersicht über die bisherigen <span style='color:gray;'>{unique(d_plot$total)}</span> Burn-Spiele"
            ),
            "♠<span style='color:gray;'>♥</span>♣<span style='color:gray;'>♦</span>"
        ),
        caption = glue("Visualization by Maik Thalmann<br>Version: {today()}"),
        theme = theme(
            plot.title = element_markdown(),
            plot.caption = element_markdown(),
        )
    )

    p
}

p_all <- plot_panel(d_plot, d_most, pnt_size = 2)

p_freq <- plot_panel(
    filter(d_plot, player != "Martha", player != "Lotti"),
    filter(d_most, player != "Martha", player != "Lotti")
)

# save the plots
walk2(
    c(
        here("images", "time.pdf"),
        here("images", "time_freq.pdf")
    ),
    list(p_all, p_freq),
    ~ ggsave(
        filename = .x,
        plot = .y,
        device = cairo_pdf,
        width = 38,
        height = 28,
        units = "cm",
        dpi = "retina"
    )
)

# export the data
d_plot %>%
    left_join(d_most, by = c("date", "player")) %>%
    write_csv(here("data", "burn-all.csv"))
