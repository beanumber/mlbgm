
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mlbgm

<!-- badges: start -->

[![Travis-CI Build
Status](https://travis-ci.org/beanumber/mlbgm.svg?branch=master)](https://travis-ci.org/beanumber/mlbgm)
<!-- badges: end -->

The goal of `mlbgm` is to streamline access to data that answers
questions that Major League Baseball executives care about. These
include:

  - How good was/is/will be that player?
      - Player evaluation (past performance)
      - Player projection (future performance)
  - How good was/is/will be that team?
      - Team evalation and projection
  - How much will that player cost?
      - Salaries (past and future)
      - Major League Serice
  - How much money are we making?
      - Franchise valuation and revenue

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("beanumber/mlbgm")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(mlbgm)
lkup_teams()
#> # A tibble: 30 x 19
#>    teamID lahman_name franchID teamIDBR teamIDretro standardized_na… city 
#>    <chr>  <chr>       <chr>    <chr>    <chr>       <chr>            <chr>
#>  1 ARI    Arizona Di… ARI      ARI      ARI         arizona diamond… ariz…
#>  2 ATL    Atlanta Br… ATL      ATL      ATL         atlanta braves   atla…
#>  3 BAL    Baltimore … BAL      BAL      BAL         baltimore oriol… balt…
#>  4 BOS    Boston Red… BOS      BOS      BOS         boston red-sox   bost…
#>  5 CHA    Chicago Wh… CHW      CHW      CHA         chicago white-s… chic…
#>  6 CHN    Chicago Cu… CHC      CHC      CHN         chicago cubs     chic…
#>  7 CIN    Cincinnati… CIN      CIN      CIN         cincinnati reds  cinc…
#>  8 CLE    Cleveland … CLE      CLE      CLE         cleveland india… clev…
#>  9 COL    Colorado R… COL      COL      COL         colorado rockies colo…
#> 10 DET    Detroit Ti… DET      DET      DET         detroit tigers   detr…
#> # … with 20 more rows, and 12 more variables: nickname <chr>, teamID_alt <chr>,
#> #   teamcolors_name <chr>, primary <chr>, secondary <chr>, tertiary <chr>,
#> #   quaternary <chr>, division <chr>, location <chr>, mascot <chr>,
#> #   sportslogos_name <chr>, logo <chr>
lkup_players
#> # A tibble: 111,692 x 15
#>    name_last name_first name_given mlbam_id lahman_id retro_id bp_id
#>    <chr>     <chr>      <chr>         <dbl> <chr>     <chr>    <dbl>
#>  1 Ausmus    Brad       Bradley D…   110385 ausmubr01 ausmb001     3
#>  2 Bagwell   Jeff       Jeffrey R…   110432 bagweje01 bagwj001     4
#>  3 Berkman   Lance      William L…   204020 berkmla01 berkl001     6
#>  4 Biggio    Craig      Craig Alan   110987 biggicr01 biggc001     7
#>  5 Blum      Geoff      Geoffrey …   150398 blumge01  blumg001     8
#>  6 Borbon    Pedro      Pedro Fel…   111228 borbope02 borbp001     9
#>  7 Cruz      Nelson     Nelson       112906 cruzne01  cruzn001    13
#>  8 Dotel     Octavio    Octavio E…   136734 doteloc01 doteo001    14
#>  9 Ensberg   Morgan     Morgan Pa…   348563 ensbemo01 ensbm001    15
#> 10 Everett   Adam       Jeffrey A…   276361 everead01 evera001    16
#> # … with 111,682 more rows, and 8 more variables: davenport_id <chr>,
#> #   fg_id <dbl>, cbs_id <dbl>, espn_id <dbl>, nfbc_id <dbl>, yahoo_id <dbl>,
#> #   ottoneu_id <dbl>, rotowire_id <dbl>
```

# References

`mlbgm` works in conjunctions with other tools, including:

  - [Lahman](https://github.com/cdalzell/Lahman)
  - [teamcolors](https://www.github.com/beanumber/teamcolors)
  - [baseballr](http://www.github.com/BillPetti/baseballr)
  - [statcastr](http://www.github.com/beanumber/statcastr)
  - [retro](http://www.github.com/beanumber/retro)
