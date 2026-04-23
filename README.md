# Why was $431k Bet on Snow in Miami?
## Analysis of the Miami Snow Market on Kalshi

Prediction markets are booming. Billionaries and investors have crept into the largely-unregulated field, hedging big wagers across a range of markets. Micro-bets, in markets such as the weather, are now worth nearly $430 million on Kalshi alone. 

In this project, I analyzed the #1 buyer in this market, a user named *brand.range*, who layed the lion's share of all contract wagers. I also highlighted the unlikely interest in a gamble with an obvious winner.

## But, why are people trading on the *obvious?*

The data presented was scraped from Kalshi's January 2026, [Miami snow market](https://kalshi.com/markets/kxmiasnowm/chicago-snowfall-monthly/kxmiasnowm-26jan?sortMarkets=alphabetical-asc). Cleaning, analysis and plotting was all conducted in R, VS Code and Claude Code. I used Adobe Illustrator to further stylize the graphs.

Kalshi bets aren't easily parsed — the site (largely) blocks scraping attempts to encourage use of their API. In addition, *buying YES* equates to *selling NO*, meaning the two had to be considered one. The cleaning of the scraped data included making this distinction. 

R packages used include: tidyverse, lubridate, scales, ggplot2, dplyr

Future work might include unmasking brand.range, who I suspect might be a bot to inflate the market. 
