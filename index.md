EPL predictability
================

``` r
library(tidyverse)
```

We’ll be using [English Premier League
data](https://github.com/Cervus1983/EPL-data/blob/master/index.md). Make
sure to include columns `B365H`, `B365D` and `B365A` — Bet365 betting
odds (<https://www.football-data.co.uk/notes.txt>):

``` r
epl
```

    ## # A tibble: 7,831 x 8
    ##    Season Date       HomeTeam    AwayTeam      FTR   B365H B365D B365A
    ##     <int> <chr>      <chr>       <chr>         <chr> <dbl> <dbl> <dbl>
    ##  1   2002 17/08/2002 Blackburn   Sunderland    D      1.73  3.25  4.33
    ##  2   2002 17/08/2002 Charlton    Chelsea       A      2.8   3.25  2.2 
    ##  3   2002 17/08/2002 Everton     Tottenham     D      2.25  3.25  2.75
    ##  4   2002 17/08/2002 Fulham      Bolton        H      1.73  3.25  4.33
    ##  5   2002 17/08/2002 Leeds       Man City      H      1.67  3.4   4.5 
    ##  6   2002 17/08/2002 Man United  West Brom     H      1.2   5    12   
    ##  7   2002 17/08/2002 Southampton Middlesbrough D      2.25  3.25  2.75
    ##  8   2002 18/08/2002 Arsenal     Birmingham    H      1.22  5    10   
    ##  9   2002 18/08/2002 Aston Villa Liverpool     A      3     3.25  2.1 
    ## 10   2002 19/08/2002 Newcastle   West Ham      H      1.57  3.4   5.5 
    ## # ... with 7,821 more rows

First, we’ll convert betting odds to probabilities:

``` r
epl_prob <- epl %>% 
    # invert betting odds
    mutate_at(
        vars(
            starts_with(
                "B365"
            )
        ),
        ~ 1/ .
    ) %>% 
    # implied probabilities add up to more than 1
    # otherwise bookmakers would struggle to make profit
    mutate(
        prob_total = B365H + B365D + B365A
    ) %>% 
    # scale implied probabilities down
    # so they add up to exactly 1
    mutate_at(
        vars(
            starts_with(
                "B365"
            )
        ),
        ~ . / prob_total
    ) %>% 
    select(
        -prob_total
    )
```

Out of curiosity, what was the ultimate “David vs Goliath” game? We’ll
assume Goliath was playing at home:

``` r
epl_prob %>% 
    arrange(
        B365H
    ) %>% 
    tail(
        1
    )
```

    ## # A tibble: 1 x 8
    ##   Season Date       HomeTeam AwayTeam FTR   B365H  B365D  B365A
    ##    <int> <chr>      <chr>    <chr>    <chr> <dbl>  <dbl>  <dbl>
    ## 1   2018 03/04/2019 Man City Cardiff  H     0.914 0.0570 0.0285

Makes sense.

Second, we’ll use [Rank Probability
Score](https://stats.stackexchange.com/questions/112250/understanding-the-rank-probability-score)
as a measure of how *unpredictable* a result was:

``` r
epl_rps <- epl_prob %>% 
    mutate(
        RPS = (((FTR == "H") - B365H)^2 + ((FTR %in% c("H", "D")) - B365H - B365D)^2) / 2
    )
```

For example, in Tottenham vs West Ham on 19/02/2023, home side were
heavy favourites and won, which is reflected in a small RPS:

``` r
epl_rps %>% 
    .[7831, ]
```

    ## # A tibble: 1 x 9
    ##   Season Date       HomeTeam  AwayTeam FTR   B365H B365D B365A   RPS
    ##    <int> <chr>      <chr>     <chr>    <chr> <dbl> <dbl> <dbl> <dbl>
    ## 1   2022 19/02/2023 Tottenham West Ham H     0.539 0.262 0.199 0.126

Whereas an unexpected win by Bournemouth at Wolves the day before
results in a big RPS:

``` r
epl_rps %>% 
    .[7828, ]
```

    ## # A tibble: 1 x 9
    ##   Season Date       HomeTeam AwayTeam    FTR   B365H B365D B365A   RPS
    ##    <int> <chr>      <chr>    <chr>       <chr> <dbl> <dbl> <dbl> <dbl>
    ## 1   2022 18/02/2023 Wolves   Bournemouth A     0.574 0.266 0.160 0.518

Both most and least predictable results feature Man City — unsurprising,
since both scenarios require a “David vs Goliath” situation:

``` r
epl_rps %>% 
    arrange(
        RPS
    ) %>% 
    .[c(1, nrow(.)), ]
```

    ## # A tibble: 2 x 9
    ##   Season Date       HomeTeam AwayTeam       FTR   B365H  B365D  B365A     RPS
    ##    <int> <chr>      <chr>    <chr>          <chr> <dbl>  <dbl>  <dbl>   <dbl>
    ## 1   2018 03/04/2019 Man City Cardiff        H     0.914 0.0570 0.0285 0.00406
    ## 2   2018 22/12/2018 Man City Crystal Palace A     0.856 0.0976 0.0465 0.821

How do seasons compare in terms of average RPS? Let’s count games as a
sanity check, too:

``` r
epl_rps %>% 
    group_by(
        Season
    ) %>% 
    summarise(
        Games = n(),
        mean(
            RPS
        )
    ) %>% 
    print.data.frame(
        digits = 3,
        row.names = FALSE
    )
```

    ##  Season Games mean(RPS)
    ##    2002   380     0.203
    ##    2003   380     0.203
    ##    2004   380     0.192
    ##    2005   380     0.195
    ##    2006   380     0.196
    ##    2007   380     0.177
    ##    2008   380     0.192
    ##    2009   380     0.182
    ##    2010   380     0.200
    ##    2011   380     0.201
    ##    2012   380     0.187
    ##    2013   380     0.192
    ##    2014   380     0.197
    ##    2015   380     0.210
    ##    2016   380     0.181
    ##    2017   380     0.186
    ##    2018   380     0.186
    ##    2019   380     0.199
    ##    2020   380     0.214
    ##    2021   380     0.189
    ##    2022   231     0.207

So, 2007/8 was the most predictable season with the average RPS of
0.177, and 2020/21 was the least predictable with the average RPS of
0.214.

Which teams have surprised this season?

``` r
bind_rows(
    # home games
    epl_rps %>% 
        group_by(
            Season,
            Team = HomeTeam
        ) %>% 
        summarise_at(
            vars(
                RPS
            ),
            mean
        ),
    # away games
    epl_rps %>% 
        group_by(
            Season,
            Team = AwayTeam
        ) %>% 
        summarise_at(
            vars(
                RPS
            ),
            mean
        )
) %>% 
    # total average
    group_by(
        Season,
        Team
    ) %>% 
    summarise_at(
        vars(
            RPS
        ),
        mean
    ) %>% 
    ungroup() %>% 
    # this season
    filter(
        Season == 2022
    ) %>% 
    arrange(
        RPS
    ) %>% 
    print.data.frame(
        digits = 3,
        row.names = FALSE
    )
```

    ##  Season           Team   RPS
    ##    2022       Man City 0.165
    ##    2022        Arsenal 0.168
    ##    2022      Newcastle 0.175
    ##    2022    Bournemouth 0.180
    ##    2022      Tottenham 0.183
    ##    2022         Wolves 0.187
    ##    2022 Crystal Palace 0.193
    ##    2022       West Ham 0.200
    ##    2022     Man United 0.204
    ##    2022        Chelsea 0.207
    ##    2022          Leeds 0.209
    ##    2022  Nott'm Forest 0.209
    ##    2022      Leicester 0.213
    ##    2022         Fulham 0.216
    ##    2022        Everton 0.223
    ##    2022    Aston Villa 0.224
    ##    2022      Brentford 0.228
    ##    2022    Southampton 0.232
    ##    2022       Brighton 0.246
    ##    2022      Liverpool 0.271

Three least predictable teams — Man City, Arsenal and Newcastle — must
be proud of having consistently met high expectations. In fourth place,
Bournemouth, sadly, are also where most had expected them to be.

Liverpool have been the most unpredictable team (gotta feel sorry for
Liverpool fans). Brighton come in second — a team punching above their
weight, well done!
