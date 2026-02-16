require("tidyverse")
library(tidyverse)
library(lubridate)
library(scales)
library(ggplot2)
library(dplyr)
library(showtext)
library(sysfonts)
library(lubridate)

showtext_auto()

font_add_google("SN Pro", "pro")

setwd('~/Desktop/vibe/miami_snow')
miami <- read_csv("kalshi_miami.csv")

miami_minimal <- miami %>%
  select(-count_fp, -created_time, -no_price, -ticker, -trade_id, -yes_price)

miami_clean <- miami_minimal %>%
  mutate(
    price_dollars = ifelse(taker_side == "yes",
                           yes_price_dollars,
                           no_price_dollars),
    money_bet = count * price_dollars,
    won = ifelse(taker_side == "yes", "Won (YES)", "Lost (NO)")
  )

ggplot(miami_clean,
       aes(x = count,
           y = money_bet,
           color = money_bet,
           shape = won,
           size = money_bet)) +
  geom_point(alpha = 0.7) +
  scale_color_gradient(
    low = "#b6e3b6",
    high = "#006400"
  ) +
  scale_size_continuous(range = c(2, 10), guide = "none") +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title = "Miami hasn't seen snow since 1977. Why was $480k bet on it last month?",
    subtitle = "Green intensity = Dollars Spent | Shape = Outcome",
    x = "Number of Contracts",
    y = NULL,
    color = "Dollars Spent",
    shape = "Outcome",
    size = "Dollars Spent"
  ) +
  theme_minimal()

total_yes_dollars <- miami_clean %>%
  filter(taker_side == "yes") %>%
  summarise(total = sum(count * yes_price_dollars, na.rm = TRUE)) %>%
  pull(total)

total_yes_dollars #6838.65 bet YES (wronrg cus of the betting yes selling no thing so redoing below)

total_no_dollars <- miami_clean %>%
  filter(taker_side == "no") %>%
  summarise(total = sum(count * no_price_dollars, na.rm = TRUE)) %>%
  pull(total)

total_no_dollars #24976.78

#where are the missing hundreds of thousands? well selling no is the same as betting yes

## did an incredibly annoying scrape of Kalshi with Playwright

### found much better summary stats and info where I took into account the selling no/buying yes.

## actual working, correct analysis and graph

miami <- read_csv("scrape_kalshi.csv")

miami <- miami %>%
  mutate(
    outcome = ifelse(tolower(taker_side) == "yes", "Lose", "Win"),
    outcome = factor(outcome, levels = c("Lose", "Win")),
    money_spent = price_dollars * count
  )

subtitle_text <- str_wrap(
  "Nearly 97% of all wagers placed on the possibility of a winter flurry in Miami were for YES on the popular prediction market platform, Kalshi. Snow hasn’t hit Miami in more than 45 years — but, users bet thousands.",
  width = 100
)

avg_spend <- mean(miami$money_spent, na.rm = TRUE)

ggplot(miami,
       aes(x = count,
           y = money_spent,
           color = outcome,
           size = money_spent)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = avg_spend, linetype = "dashed", color = "#222021") +
  annotate("text",
           x = max(miami$count) * 0.95,
           y = avg_spend,
           label = paste0("Average spend = $", round(avg_spend, 2)),
           color = "#222021",
           hjust = 1,
           vjust = -0.75,
           size = 3) +
  scale_color_manual(
    values = c("Lose" = "#d62828", "Win" = "#28CC95"),
    labels = c("Lose", "Win")
  ) +
  scale_size_continuous(range = c(1.5, 12), guide = "none") +
  scale_x_continuous(labels = label_comma()) +
  scale_y_continuous(
    labels = label_dollar(),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title = "Miami hasn't seen snow since 1977. Why was $431k traded on January snowfall?",
    subtitle = subtitle_text,
    x = "Number of Contracts",
    y = NULL,
    caption = "Source: Kalshi",
    color = "Outcome"
  ) +
  theme_minimal(base_size = 11.5) +
  theme(
    text = element_text(family = "pro"),
    legend.position = "right",
    legend.text = element_text(size = 8),
    legend.title = element_text(face = "bold"),
    plot.caption = element_text(color = "#222021", face = "bold"),
    axis.title.x = element_text(size = 10, face = "bold"),
    plot.title = element_text(face = "bold", size = 12, hjust = 0.25),
    plot.subtitle = element_text(color = "#222021", size = 9, lineheight = 1)
  )


## more professional

ggplot(miami, aes(x = count,
                  y = money_spent,
                  color = outcome,
                  size = money_spent)) +
  geom_point(data = subset(miami, outcome == "Lose"),
             alpha = 0.8) +
  geom_point(data = subset(miami, outcome == "Win"),
             alpha = 0.6) +
  geom_hline(yintercept = avg_spend,
             linetype = "dashed",
             linewidth = 0.6,
             color = "#3a3a3a") +
  annotate("text",
           x = max(miami$count) * 0.98,
           y = avg_spend,
           label = paste0("Average spend: ", dollar(avg_spend)),
           hjust = 1, vjust = -0.6,
           size = 3.3,
           family = "pro",
           color = "#3a3a3a") +
  scale_color_manual(values = c("Lose" = "#d62828",
                                "Win"  = "#2a9d8f")) +
  scale_size_continuous(range = c(2, 10), guide = "none") +
  scale_x_continuous(labels = label_comma(),
                     expand = expansion(mult = c(0.02, 0.2))) +
  scale_y_continuous(labels = label_dollar(),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Why Was $431K Bet on Snow in Miami?",
      subtitle = stringr::str_wrap(subtitle_text, width = 80),
      caption = "Source: Kalshi",
      x = "Number of Contracts",
      y = NULL,
      color = "Outcome") +
  theme_minimal(base_size = 12) +
  theme(text = element_text(family = "pro"),
        plot.title = element_text(face = "bold",
                                  size = 17,
                                  hjust = 0.3,
                                  margin = margin(b = 6)),
        plot.subtitle = element_text(size = 10,
                                     color = "#444444",
                                     lineheight = 1.05,
                                     margin = margin(b = 12)),
        axis.title.x = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 9),
        plot.caption = element_text(color = "#222021", face = "bold"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "right",
        legend.title = element_text(face = "bold"),
        legend.text = element_text(size = 9),
        plot.margin = margin(15, 20, 15, 15))


## now, onto analyzing the whale, brand.range

whale <- read.csv("brand_range.csv")

subtitle_two <- str_wrap(
  "Kalshi is experiencing a windfall of investment from users like brand.range in “obvious markets,” such as snow in Miami. Why would anyone bet on the impossible?",
  width = 100
)

whale <- whale %>% 
  mutate(
    maker_nickname = na_if(maker_nickname, ""),
    taker_nickname = na_if(taker_nickname, "")
  )

user_trades <- bind_rows(
  whale %>%
    filter(!is.na(maker_nickname)) %>%
    transmute(user = maker_nickname, action = maker_action,
              count, price_dollars, ticker),
  whale %>%
    filter(!is.na(taker_nickname)) %>%
    transmute(user = taker_nickname, action = taker_action,
              count, price_dollars, ticker)
)

user_summary <- user_trades %>%
  group_by(user) %>%
  summarize(
    contracts_bought = sum(if_else(action == "buy", count, 0)),
    dollars_spent    = sum(if_else(action == "buy", count * price_dollars, 0)),
    contracts_sold   = sum(if_else(action == "sell", count, 0)),
    dollars_earned   = sum(if_else(action == "sell", count * price_dollars, 0)),
    total_trades     = n(),
    .groups = "drop"
  ) %>%
  mutate(
    net_pnl      = dollars_earned - dollars_spent,
    total_volume = contracts_bought + contracts_sold
  )

# Separate brand.range from others
br     <- user_summary %>% filter(user == "brand.range")
others <- user_summary %>% filter(user != "brand.range")

# Label for brand.range
br_label <- paste0(
  "brand.range\n",
  scales::comma(br$contracts_bought), " contracts bought ($", sprintf("%.2f", br$dollars_spent), ")\n",
  scales::comma(br$contracts_sold),   " contracts sold ($", sprintf("%.2f", br$dollars_earned), ")\n",
  "Net P&L: ", scales::dollar(br$net_pnl)
)

ggplot(user_summary, aes(x = contracts_bought, y = contracts_sold)) +
  geom_point(data = others,
             aes(size = total_volume),
             color = "#B0BEC5", alpha = 0.5) +
  annotate("point",
           x = mean(others$contracts_bought),
           y = mean(others$contracts_sold),
           shape = 18, size = 10, color = "#37474F") +
  annotate("text",
           x = mean(others$contracts_bought),
           y = mean(others$contracts_sold),
           label = "AVG USER", fontface = "bold",
           size = 3.2, color = "#444444", vjust = -2, hjust = 0.5) +
  geom_point(data = br, color = "#C62828", size = 14) +
  geom_label(
    data = br,
    aes(x = contracts_bought, y = contracts_sold, label = br_label),
    hjust = 1,       
    vjust = 1.5,    
    size = 3,
    fill = "#FFF8E1",
    color = "#444444",
    label.padding = unit(0.3, "lines"),
    family = "pro"
  ) +  
  scale_x_continuous(
    trans = scales::pseudo_log_trans(),
    breaks = c(0, 10, 100, 1000, 10000, 70000),
    labels = scales::comma,
    expand = expansion(mult = c(0.01, 0.2))
  ) +
  scale_y_continuous(
    trans = scales::pseudo_log_trans(),
    breaks = c(0, 10, 100, 1000, 10000, 70000),
    labels = scales::comma,
    expand = expansion(mult = c(0.1, 0.1))
  ) +
  scale_size_continuous(range = c(1.5, 12), guide = "none") +
  labs(
    title    = "The Whale of Miami Snow Betting",
    subtitle = subtitle_two,
    caption = "Source: Kalshi",
    x = "Contracts Bought",
    y = "Contracts Sold"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "pro"),
    plot.title = element_text(face = "bold",
                              size = 17,
                              hjust = 0.3,
                              margin = margin(b = 6)),
    plot.subtitle = element_text(size = 10,
                                 color = "#444444",
                                 lineheight = 1.05,
                                 margin = margin(b = 12)),
    axis.title.x = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    axis.title.y = element_text(size = 11, face = "bold", margin = margin(r = 1)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 9),
    plot.caption = element_text(color = "#222021", face = "bold")
  )
