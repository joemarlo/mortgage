library(dplyr)
library(ggplot2)
set.seed(44)

# set probabilties of staying in apartment
PrOwn <- tibble(
    year = 1:7,
    PrOwn = c(0.99, 0.98, 0.97, 0.25, 0.1750, 0.1225, 0.0858)
)
qplot(PrOwn$year, PrOwn$PrOwn) + geom_line()

# get mortgage principal and differential from spreadsheet
costs <- googlesheets4::read_sheet(Sys.getenv('GSHEET_ID'), sheet = 'Model inputs')

# constants for market simulation
n_years <- 7
market_return <- 1 + (0.07 * seq(1, 0.95, length.out = n_years))
market_vol <- 0.15

# function to simulate returns
return_sim <- function(n, n_years, market_return, market_vol){
    purrr::map_dfr(seq_len(n), function(sim_i){
        returns_annual <- rnorm(n_years, market_return, market_vol)
        tibble(sim = sim_i, 
               year = seq_len(n_years),
               return = returns_annual,
               return_cum = cumprod(returns_annual),
               return_rev = rev(cumprod(rev(returns_annual))))
    })
}

# simulate 10,000 7year periods
n <- 10000
returns <- return_sim(n, n_years, market_return, market_vol)

# returns over time
ggplot(returns,
       aes(x = year, y = return_cum, group = sim)) +
    geom_line(alpha = 0.01)

# returns %>% 
#     group_by(year) %>% 
#     summarize(mean(return_cum))


# apply returns to cashflows
market_sims <- returns %>% 
    group_by(sim) %>% 
    arrange(year) %>% 
    mutate(mkt_value_1 = cumsum(costs$Mortgage1_inv * return_rev),
           mkt_value_2 = cumsum(costs$Mortgage2_inv * return_rev),
           mkt_value_3 = cumsum(costs$Mortgage3_inv * return_rev)) %>% 
    arrange(sim, year) %>% 
    select(sim, year, starts_with('mkt'))

# add probabilities for staying
equity_sims <- market_sims %>% 
    group_by(sim) %>% 
    mutate(pri1 = costs$Mortgage1_prinicipal,
           pri2 = costs$Mortgage2_prinicipal,
           pri3 = costs$Mortgage3_prinicipal,
           prob_staying = rbinom(7, size = 1, PrOwn$PrOwn),
           is_end = row_number() == which.min(prob_staying))

# extract ending equity
realized_sims <- equity_sims %>% 
    filter(is_end) %>% 
    mutate(equity1 = mkt_value_1 + pri1,
           equity2 = mkt_value_2 + pri2,
           equity3 = mkt_value_3  + pri3) %>% 
    ungroup()

# distributions of equity
realized_sims %>%
    select(sim, year, Mortgage1 = equity1, Mortgage2 = equity2, Mortgage3 = equity3) %>% 
    tidyr::pivot_longer(starts_with('Mortgage')) %>% 
    group_by(name) %>% 
    mutate(mean = mean(value)) %>% 
    ggplot(aes(x = value, fill = name)) +
    geom_histogram(color = 'white') +
    geom_vline(aes(xintercept = mean)) +
    scale_x_continuous(labels = scales::dollar_format()) +
    facet_wrap(~name, ncol = 1) +
    labs(title = 'Distribution of total equity: condo equity + simulated mkt equity',
         subtitle = 'Incorporates probability of selling',
         x = 'Total equity at end of owning apartment',
         fill = NULL)

# distribution of equity by year
realized_sims %>% 
    select(sim, year, Mortgage1 = equity1, Mortgage2 = equity2, Mortgage3 = equity3) %>% 
    tidyr::pivot_longer(starts_with('Mortgage')) %>% 
    group_by(year, name) %>% 
    mutate(mean = mean(value)) %>% 
    filter(year %in% 3:7) %>% 
    ggplot(aes(x = value, fill = name)) +
    geom_vline(aes(xintercept = mean)) +
    geom_histogram(color = 'white') +
    scale_x_continuous(labels = scales::dollar_format()) +
    facet_grid(name~year) +
    labs(title = 'Distribution of total equity: condo equity + simulated mkt equity',
         subtitle = '10,000 simulations\nBy year, after accounting for probability of staying 3, 4, 5, ... years',
         x = 'Total equity at end of owning apartment') +
    theme(legend.position = 'none',
          axis.text.x = element_text(angle = -40, hjust = 0))
# ggsave('output/distribution-by-year.png', width = 14, height = 6)

