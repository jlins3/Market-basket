Market Basket Analysis
================
Jared
Apr 2019

``` r
# Loading packages for association rules and the groceries dataset
library(arules)
```

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'arules'

    ## The following objects are masked from 'package:base':
    ## 
    ##     abbreviate, write

``` r
library(arulesViz)
```

    ## Loading required package: grid

``` r
data("Groceries")
# The Groceries data set contains 1 month (30 days) of real-world point-of-sale transaction data
# from a typical local grocery outlet. The data set contains 9835 transactions and the items are 
# aggregated to 169 categories.
```

``` r
summary(Groceries)
```

    ## transactions as itemMatrix in sparse format with
    ##  9835 rows (elements/itemsets/transactions) and
    ##  169 columns (items) and a density of 0.02609146 
    ## 
    ## most frequent items:
    ##       whole milk other vegetables       rolls/buns             soda 
    ##             2513             1903             1809             1715 
    ##           yogurt          (Other) 
    ##             1372            34055 
    ## 
    ## element (itemset/transaction) length distribution:
    ## sizes
    ##    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15 
    ## 2159 1643 1299 1005  855  645  545  438  350  246  182  117   78   77   55 
    ##   16   17   18   19   20   21   22   23   24   26   27   28   29   32 
    ##   46   29   14   14    9   11    4    6    1    1    1    1    3    1 
    ## 
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   2.000   3.000   4.409   6.000  32.000 
    ## 
    ## includes extended item information - examples:
    ##        labels  level2           level1
    ## 1 frankfurter sausage meat and sausage
    ## 2     sausage sausage meat and sausage
    ## 3  liver loaf sausage meat and sausage

``` r
rules <- apriori(Groceries,parameter = list(supp= .001, conf=.3, maxlen=5, target='rules'))
```

    ## Apriori
    ## 
    ## Parameter specification:
    ##  confidence minval smax arem  aval originalSupport maxtime support minlen
    ##         0.3    0.1    1 none FALSE            TRUE       5   0.001      1
    ##  maxlen target   ext
    ##       5  rules FALSE
    ## 
    ## Algorithmic control:
    ##  filter tree heap memopt load sort verbose
    ##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
    ## 
    ## Absolute minimum support count: 9 
    ## 
    ## set item appearances ...[0 item(s)] done [0.00s].
    ## set transactions ...[169 item(s), 9835 transaction(s)] done [0.00s].
    ## sorting and recoding items ... [157 item(s)] done [0.00s].
    ## creating transaction tree ... done [0.00s].
    ## checking subsets of size 1 2 3 4 5

    ## Warning in apriori(Groceries, parameter = list(supp = 0.001, conf =
    ## 0.3, : Mining stopped (maxlen reached). Only patterns up to a length of 5
    ## returned!

    ##  done [0.01s].
    ## writing ... [13711 rule(s)] done [0.00s].
    ## creating S4 object  ... done [0.00s].

How many rules have a confidence of 1?

``` r
subrules1 <-rules[quality(rules)$confidence == 1]
subrules1
```

    ## set of 27 rules

``` r
# Alternative approach
rules2 <- apriori(Groceries,parameter = list(supp= .001, conf=1, maxlen=5, target='rules'))
```

    ## Apriori
    ## 
    ## Parameter specification:
    ##  confidence minval smax arem  aval originalSupport maxtime support minlen
    ##           1    0.1    1 none FALSE            TRUE       5   0.001      1
    ##  maxlen target   ext
    ##       5  rules FALSE
    ## 
    ## Algorithmic control:
    ##  filter tree heap memopt load sort verbose
    ##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
    ## 
    ## Absolute minimum support count: 9 
    ## 
    ## set item appearances ...[0 item(s)] done [0.00s].
    ## set transactions ...[169 item(s), 9835 transaction(s)] done [0.00s].
    ## sorting and recoding items ... [157 item(s)] done [0.00s].
    ## creating transaction tree ... done [0.00s].
    ## checking subsets of size 1 2 3 4 5

    ## Warning in apriori(Groceries, parameter = list(supp = 0.001, conf = 1,
    ## maxlen = 5, : Mining stopped (maxlen reached). Only patterns up to a length
    ## of 5 returned!

    ##  done [0.01s].
    ## writing ... [27 rule(s)] done [0.00s].
    ## creating S4 object  ... done [0.00s].

Which pairs have the highest lift?

``` r
# Top 10 lift

inspect(head(sort(rules,by='lift'),10))
```

    ##      lhs                               rhs                support    
    ## [1]  {bottled beer,red/blush wine}  => {liquor}           0.001931876
    ## [2]  {ham,white bread}              => {processed cheese} 0.001931876
    ## [3]  {bottled beer,liquor}          => {red/blush wine}   0.001931876
    ## [4]  {Instant food products,soda}   => {hamburger meat}   0.001220132
    ## [5]  {curd,sugar}                   => {flour}            0.001118454
    ## [6]  {sugar,baking powder}          => {flour}            0.001016777
    ## [7]  {processed cheese,white bread} => {ham}              0.001931876
    ## [8]  {soda,popcorn}                 => {salty snack}      0.001220132
    ## [9]  {flour,baking powder}          => {sugar}            0.001016777
    ## [10] {ham,processed cheese}         => {white bread}      0.001931876
    ##      confidence lift     count
    ## [1]  0.3958333  35.71579 19   
    ## [2]  0.3800000  22.92822 19   
    ## [3]  0.4130435  21.49356 19   
    ## [4]  0.6315789  18.99565 12   
    ## [5]  0.3235294  18.60767 11   
    ## [6]  0.3125000  17.97332 10   
    ## [7]  0.4634146  17.80345 19   
    ## [8]  0.6315789  16.69779 12   
    ## [9]  0.5555556  16.40807 10   
    ## [10] 0.6333333  15.04549 19

Which have the highest count?

``` r
#Highest count
inspect(head(sort(rules,by='count'),10))
```

    ##      lhs                   rhs                support    confidence
    ## [1]  {other vegetables} => {whole milk}       0.07483477 0.3867578 
    ## [2]  {rolls/buns}       => {whole milk}       0.05663447 0.3079049 
    ## [3]  {yogurt}           => {whole milk}       0.05602440 0.4016035 
    ## [4]  {root vegetables}  => {whole milk}       0.04890696 0.4486940 
    ## [5]  {root vegetables}  => {other vegetables} 0.04738180 0.4347015 
    ## [6]  {yogurt}           => {other vegetables} 0.04341637 0.3112245 
    ## [7]  {tropical fruit}   => {whole milk}       0.04229792 0.4031008 
    ## [8]  {tropical fruit}   => {other vegetables} 0.03589222 0.3420543 
    ## [9]  {bottled water}    => {whole milk}       0.03436706 0.3109476 
    ## [10] {pastry}           => {whole milk}       0.03324860 0.3737143 
    ##      lift     count
    ## [1]  1.513634 736  
    ## [2]  1.205032 557  
    ## [3]  1.571735 551  
    ## [4]  1.756031 481  
    ## [5]  2.246605 466  
    ## [6]  1.608457 427  
    ## [7]  1.577595 416  
    ## [8]  1.767790 353  
    ## [9]  1.216940 338  
    ## [10] 1.462587 327

Which left hand side has highest lift when right hand side is only bottled beer?

``` r
rule_bottledbeer <- apriori(Groceries,parameter=list(support =0.001, confidence =0.3, maxlen=5),
                       appearance = list(rhs=("bottled beer"), default="lhs"))
```

    ## Apriori
    ## 
    ## Parameter specification:
    ##  confidence minval smax arem  aval originalSupport maxtime support minlen
    ##         0.3    0.1    1 none FALSE            TRUE       5   0.001      1
    ##  maxlen target   ext
    ##       5  rules FALSE
    ## 
    ## Algorithmic control:
    ##  filter tree heap memopt load sort verbose
    ##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
    ## 
    ## Absolute minimum support count: 9 
    ## 
    ## set item appearances ...[1 item(s)] done [0.00s].
    ## set transactions ...[169 item(s), 9835 transaction(s)] done [0.00s].
    ## sorting and recoding items ... [157 item(s)] done [0.00s].
    ## creating transaction tree ... done [0.00s].
    ## checking subsets of size 1 2 3 4 5

    ## Warning in apriori(Groceries, parameter = list(support = 0.001, confidence
    ## = 0.3, : Mining stopped (maxlen reached). Only patterns up to a length of 5
    ## returned!

    ##  done [0.01s].
    ## writing ... [8 rule(s)] done [0.00s].
    ## creating S4 object  ... done [0.00s].

``` r
inspect(head(sort(rule_bottledbeer,by='lift'),10))
```

    ##     lhs                   rhs                support confidence      lift count
    ## [1] {liquor,                                                                   
    ##      red/blush wine}   => {bottled beer} 0.001931876  0.9047619 11.235269    19
    ## [2] {soda,                                                                     
    ##      liquor}           => {bottled beer} 0.001220132  0.5714286  7.095960    12
    ## [3] {liquor}           => {bottled beer} 0.004677173  0.4220183  5.240594    46
    ## [4] {herbs,                                                                    
    ##      bottled water}    => {bottled beer} 0.001220132  0.4000000  4.967172    12
    ## [5] {whole milk,                                                               
    ##      soups}            => {bottled beer} 0.001118454  0.3793103  4.710249    11
    ## [6] {soda,                                                                     
    ##      red/blush wine}   => {bottled beer} 0.001626843  0.3555556  4.415264    16
    ## [7] {other vegetables,                                                         
    ##      red/blush wine}   => {bottled beer} 0.001525165  0.3061224  3.801407    15
    ## [8] {other vegetables,                                                         
    ##      domestic eggs,                                                            
    ##      bottled water}    => {bottled beer} 0.001220132  0.3000000  3.725379    12

Which right hand side has highest lift when left hand side is whole milk and any type of bread?

``` r
breadItems <- grep("bread", itemLabels(Groceries), value = TRUE)
breadItems
```

    ## [1] "white bread"         "brown bread"         "semi-finished bread"

``` r
rules_bread_milk <- apriori(Groceries, 
                   parameter=list(support =0.001, confidence =0.3, maxlen=5), 
                   appearance = list(lhs=c("whole milk", breadItems), default="rhs"))
```

    ## Apriori
    ## 
    ## Parameter specification:
    ##  confidence minval smax arem  aval originalSupport maxtime support minlen
    ##         0.3    0.1    1 none FALSE            TRUE       5   0.001      1
    ##  maxlen target   ext
    ##       5  rules FALSE
    ## 
    ## Algorithmic control:
    ##  filter tree heap memopt load sort verbose
    ##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
    ## 
    ## Absolute minimum support count: 9 
    ## 
    ## set item appearances ...[4 item(s)] done [0.00s].
    ## set transactions ...[169 item(s), 9835 transaction(s)] done [0.00s].
    ## sorting and recoding items ... [157 item(s)] done [0.00s].
    ## creating transaction tree ... done [0.00s].
    ## checking subsets of size 1 2 3 4 done [0.00s].
    ## writing ... [6 rule(s)] done [0.00s].
    ## creating S4 object  ... done [0.00s].

``` r
inspect(head(sort(rules_bread_milk,by='lift'),10))
```

    ##     lhs                      rhs                    support confidence     lift count
    ## [1] {whole milk,                                                                     
    ##      semi-finished bread} => {tropical fruit}   0.002135231  0.3000000 2.859012    21
    ## [2] {whole milk,                                                                     
    ##      semi-finished bread} => {other vegetables} 0.002643620  0.3714286 1.919601    26
    ## [3] {whole milk,                                                                     
    ##      brown bread}         => {other vegetables} 0.009354347  0.3709677 1.917219    92
    ## [4] {whole milk,                                                                     
    ##      semi-finished bread} => {soda}             0.002236909  0.3142857 1.802332    22
    ## [5] {whole milk,                                                                     
    ##      white bread}         => {other vegetables} 0.005897306  0.3452381 1.784244    58
    ## [6] {white bread}         => {other vegetables} 0.013726487  0.3260870 1.685268   135

``` r
inspect(head(sort(rules_bread_milk,by='count'),10))
```

    ##     lhs                      rhs                    support confidence     lift count
    ## [1] {white bread}         => {other vegetables} 0.013726487  0.3260870 1.685268   135
    ## [2] {whole milk,                                                                     
    ##      brown bread}         => {other vegetables} 0.009354347  0.3709677 1.917219    92
    ## [3] {whole milk,                                                                     
    ##      white bread}         => {other vegetables} 0.005897306  0.3452381 1.784244    58
    ## [4] {whole milk,                                                                     
    ##      semi-finished bread} => {other vegetables} 0.002643620  0.3714286 1.919601    26
    ## [5] {whole milk,                                                                     
    ##      semi-finished bread} => {soda}             0.002236909  0.3142857 1.802332    22
    ## [6] {whole milk,                                                                     
    ##      semi-finished bread} => {tropical fruit}   0.002135231  0.3000000 2.859012    21

Now we visualize our results, and we can even make an interactive plot to pick rules graphically and inspect them.

``` r
plot(rules)
```

    ## To reduce overplotting, jitter is added! Use jitter = 0 to prevent jitter.

![](Market_Basket_files/figure-markdown_github/unnamed-chunk-14-1.png)
