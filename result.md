results
================

## Setup

``` r
library(tidyverse)
library(tidytext)
library(topicmodels)
library(stm)
library(quanteda)
library(granatlib)
```

``` r
news_df <- read_csv("data/us_equities_news_dataset.csv")
```

    ## Rows: 221513 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (6): ticker, title, category, content, provider, url
    ## dbl  (2): id, article_id
    ## date (1): release_date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
load("data/dat_proc.RData")
```

``` r
fit_stm_df <- list.files("data/", full.names = TRUE) %>% 
  keep(str_detect, "fit_stm") %>% 
  enframe(NULL, "file_name") %>%
  transmute(
    k = parse_number(file_name),
    fit = map(file_name, ~ {load(.); mod})
  ) %>% 
  arrange(k)
```

``` r
fit_stm_df %>% 
  pull(fit, k) %>%
  imap(~ {
    broom::tidy(.x) %>% 
      group_by(topic) %>% 
      slice_max(beta, n = 5) %>% 
      summarise(term = str_c(term, collapse = ", ")) %>% 
      kable_output(caption = str_c("Terms with highest beta with ", .y, " topic"))
  })
```

$`2`

| topic |                term                |
|:------|:----------------------------------:|
| 1     | year, compani, earn, zack, quarter |
| 2     |  said, market, will, trade, year   |

Terms with highest beta with 2 topic

$`4`

| topic |                 term                  |
|:------|:-------------------------------------:|
| 1     |   said, will, compani, year, reuter   |
| 2     |  market, trade, week, percent, bank   |
| 3     | year, quarter, compani, million, earn |
| 4     |   stock, zack, compani, earn, rank    |

Terms with highest beta with 4 topic

$`6`

| topic |                   term                   |
|:------|:----------------------------------------:|
| 1     |    said, will, compani, reuter, year     |
| 2     |     market, will, year, price, bank      |
| 3     | compani, nasdaq, will, servic, technolog |
| 4     |  year, quarter, million, compani, earn   |
| 5     |   trade, percent, market, week, index    |
| 6     |    stock, zack, earn, compani, estim     |

Terms with highest beta with 6 topic

$`8`

| topic |                   term                   |
|:------|:----------------------------------------:|
| 1     |     said, reuter, bank, will, state      |
| 2     |  compani, product, will, energi, price   |
| 3     | compani, nasdaq, will, servic, technolog |
| 4     |      earn, zack, stock, estim, rank      |
| 5     |     market, stock, will, price, time     |
| 6     | year, quarter, million, revenu, compani  |
| 7     |   stock, compani, invest, zack, ratio    |
| 8     |   percent, trade, market, index, week    |

Terms with highest beta with 8 topic

$`10`

| topic |                   term                   |
|:------|:----------------------------------------:|
| 1     |    trade, nasdaq, stock, nyse, close     |
| 2     | compani, product, will, energi, industri |
| 3     |    said, reuter, state, year, govern     |
| 4     |  compani, drug, patient, medic, health   |
| 5     |     market, will, time, price, look      |
| 6     | year, quarter, million, revenu, compani  |
| 7     |      earn, zack, stock, estim, rank      |
| 8     |   percent, market, rate, expect, bank    |
| 9     |   compani, will, nasdaq, servic, appl    |
| 10    |   invest, stock, compani, fund, market   |

Terms with highest beta with 10 topic

$`12`

| topic |                  term                   |
|:------|:---------------------------------------:|
| 1     |    trade, nasdaq, stock, nyse, close    |
| 2     |  china, said, vehicl, chines, compani   |
| 3     |    said, reuter, bank, state, govern    |
| 4     | year, quarter, million, revenu, billion |
| 5     |     market, will, time, price, look     |
| 6     |  compani, growth, sale, retail, store   |
| 7     |  compani, drug, patient, medic, health  |
| 8     |   percent, market, bank, rate, expect   |
| 9     |   compani, nasdaq, will, servic, appl   |
| 10    |     earn, zack, estim, stock, rank      |
| 11    |  compani, energi, product, will, nyse   |
| 12    | stock, invest, investor, fund, compani  |

Terms with highest beta with 12 topic

$`14`

| topic |                   term                   |
|:------|:----------------------------------------:|
| 1     |    nasdaq, trade, nyse, close, stock     |
| 2     |  energi, product, price, compani, will   |
| 3     |    said, reuter, compani, state, will    |
| 4     |   quarter, year, million, revenu, earn   |
| 5     |      rate, will, year, china, bank       |
| 6     |  fund, invest, bank, compani, dividend   |
| 7     |  drug, compani, patient, medic, health   |
| 8     |  percent, market, expect, report, said   |
| 9     |      will, time, just, market, like      |
| 10    |     retail, store, sale, brand, food     |
| 11    |    compani, zack, growth, rank, stock    |
| 12    |      earn, stock, zack, estim, rank      |
| 13    | compani, nasdaq, appl, technolog, servic |
| 14    |     week, market, trade, stock, move     |

Terms with highest beta with 14 topic

``` r
fit_stm14 <- fit_stm_df %>% 
  pull(fit, k) %>% 
  .[["14"]]
```

``` r
plot(fit_stm14, type = "summary")
```

![](result_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
labelTopics(fit_stm14)
```

    ## Topic 1 Top Words:
    ##       Highest Prob: nasdaq, trade, nyse, close, stock, point, share 
    ##       FREX: wynn, releasenow, moex, melco, bottler, tlri, aphria 
    ##       Lift: aavl, abgb, acbff, acrgf, actua, adat, adial 
    ##       Score: nasdaq, rose, barrel, fell, ounc, index, outnumb 
    ## Topic 2 Top Words:
    ##       Highest Prob: energi, product, price, compani, will, year, natur 
    ##       FREX: shale, missil, refineri, permian, exxonmobil, cubic, petrobra 
    ##       Lift: aalesund, abakan, abdullatif, abimael, abkhazia, abqaig, abroadth 
    ##       Score: barrel, energi, crude, shale, saudi, opec, steel 
    ## Topic 3 Top Words:
    ##       Highest Prob: said, reuter, compani, state, will, unit, execut 
    ##       FREX: lawyer, attorney, prosecutor, sedan, takata, spacex, planemak 
    ##       Lift: aaib, aarup, aascu, abdirizak, abdulmajid, abdulrehman, abdurrahman 
    ##       Score: said, reuter, told, court, percent, tesla, spokesman 
    ## Topic 4 Top Words:
    ##       Highest Prob: quarter, year, million, revenu, earn, report, share 
    ##       FREX: guidancefor, outlookfor, combinationher, flowa, rpms, saysaccord, updatea 
    ##       Lift: abkatun, absg, acano, ackland, acquisitiondur, acquisitions, acquisitionsdur 
    ##       Score: quarter, zack, revenu, consensus, million, earn, segment 
    ## Topic 5 Top Words:
    ##       Highest Prob: rate, will, year, china, bank, market, economi 
    ##       FREX: ecri, riksbank, debas, bullion, scandi, wlig, forint 
    ##       Lift: aaccord, abct, abdygulov, abeth, abquaia, ackerlof, adem 
    ##       Score: gold, bond, bank, monetari, rate, economi, inflat 
    ## Topic 6 Top Words:
    ##       Highest Prob: fund, invest, bank, compani, dividend, year, asset 
    ##       FREX: underwrit, reinsur, vornado, aimco, annali, bancorpsouth, gerspach 
    ##       Lift: aagpx, abalx, abhyx, abthx, abysx, acchx, accl 
    ##       Score: dividend, fund, bank, ratio, loan, yield, asset 
    ## Topic 7 Top Words:
    ##       Highest Prob: drug, compani, patient, medic, health, treatment, approv 
    ##       FREX: cancer, therapi, clinic, amgen, bristol, diabet, biosimilar 
    ##       Lift: aaaabkbzciwgggngni, aaha, aaic, aamt, aapm, aasld, abacavir 
    ##       Score: drug, cancer, treatment, therapi, patient, diseas, pharmaceut 
    ## Topic 8 Top Words:
    ##       Highest Prob: percent, market, expect, report, said, month, index 
    ##       FREX: nikkei, stoxx, seng, kospi, ibex, topix, currenciesth 
    ##       Lift: abberg, abovenet, accordng, accordong, acto, advancestock, affairson 
    ##       Score: percent, euro, nikkei, index, rose, crude, fell 
    ## Topic 9 Top Words:
    ##       Highest Prob: will, time, just, market, like, think, year 
    ##       FREX: analystokay, officeryeah, yeah, officerthank, analystthank, operatorthank, phonet 
    ##       Lift: abigaildisney, abra, accyy, acebook, activehotel, actx, adamje 
    ##       Score: disney, bitcoin, question, realli, think, buffett, chief 
    ## Topic 10 Top Words:
    ##       Highest Prob: retail, store, sale, brand, food, home, consum 
    ##       FREX: groceri, mortar, athlet, pizza, championship, touchdown, dunkin 
    ##       Lift: abalon, abhijith, accelerateth, acciari, acclam, accordino, acucar 
    ##       Score: store, retail, restaur, brand, comp, food, walmart 
    ## Topic 11 Top Words:
    ##       Highest Prob: compani, zack, growth, rank, stock, market, nyse 
    ##       FREX: hyatt, viasat, vickeri, trima, microinvert, crealogix, calgon 
    ##       Lift: accelerategross, accura, acetaldehyd, achx, acquisitionc, acquisitionsfollow, acquisitionslast 
    ##       Score: zack, rank, growth, stock, compani, custom, moreov 
    ## Topic 12 Top Words:
    ##       Highest Prob: earn, stock, zack, estim, rank, compani, current 
    ##       FREX: unmanag, releasechicago, contactzack, newsmani, garp, ratioa, ratioanoth 
    ##       Lift: accelerationearn, addthank, aemcolo, aerovironmentwhil, agrowth, algnin, alibabath 
    ##       Score: zack, rank, earn, consensus, stock, estim, ratio 
    ## Topic 13 Top Words:
    ##       Highest Prob: compani, nasdaq, appl, technolog, servic, will, googl 
    ##       FREX: smartphon, server, android, xiaomi, handset, whatsapp, encrypt 
    ##       Lift: ablegam, achal, activex, adaski, adelberg, adnroid, adultswin 
    ##       Score: googl, appl, facebook, cloud, user, netflix, smartphon 
    ## Topic 14 Top Words:
    ##       Highest Prob: week, market, trade, stock, move, high, price 
    ##       FREX: overbought, oversold, macd, trendlin, retest, bartosiak, bolling 
    ##       Lift: ablemarket, aboveretail, abreakout, acrophobia, actionforex, admati, aeroviro 
    ##       Score: trader, chart, resist, index, bearish, week, overbought

``` r
findThoughts(fit_stm14, texts = news_df$content[- dat.proc$docs.removed], n = 5, topics = 2)$docs[[1]] %>% 
  gsub(pattern = "Now read$", replacement = "") %>% 
  plotQuote(width = 60)
```

![](result_files/figure-gfm/thoughts-1.png)<!-- -->
