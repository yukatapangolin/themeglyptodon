
<!-- README.md is generated from README.Rmd. Please edit that file -->

# theme_glyptodon

<!-- badges: start -->

[![R-CMD-check](https://github.com/yukatapangolin/themeglyptodon/workflows/R-CMD-check/badge.svg)](https://github.com/yukatapangolin/themeglyptodon/actions)
<!-- badges: end -->

Theme for ggplot2 based on hrbthemes

## Installation

You can install the package with:

``` r
# install.packages("remotes")
remotes::install_github("yukatapangolin/themeglyptodon")
```

## Example

``` r
library(glyptodon)
library(ggplot2)
library(sysfonts)
library(showtextdb)
library(showtext)

## Add Arimo as Arial in case it's not installed like in the Github testing container
font_add_google("Arimo", "Arial")
font_add("Roboto", 
         regular = file.path(system.file("fonts", 
                               "roboto", 
                               package="glyptodon"),
                   "Roboto-Regular.ttf"),
         italic = file.path(system.file("fonts", 
                               "roboto", 
                               package="glyptodon"),
                   "Roboto-Italic.ttf"))
font_add("Roboto Condensed", 
         file.path(system.file("fonts", 
                               "roboto-condensed", 
                               package="glyptodon"),
                   "RobotoCondensed-Regular.ttf"))
font_add("Roboto Condensed Light", 
         file.path(system.file("fonts", 
                               "roboto-condensed", 
                               package="glyptodon"),
                   "RobotoCondensed-Light.ttf"))
font_add("Goldman Sans Condensed",
         file.path(system.file("fonts", 
                               "goldman-sans", 
                               package="glyptodon"),
                   "GoldmanSansCd_Rg.ttf"))
font_add("PT Sans",
         file.path(system.file("fonts", 
                               "pt-sans", 
                               package="glyptodon"),
                   "PTSans-Regular.ttf"))
font_add("IBM Plex Sans",
         regular = file.path(system.file("fonts", 
                               "ibm-plex-sans", 
                               package="glyptodon"),
                   "IBMPlexSans-Regular.ttf"),
         bold = file.path(system.file("fonts", 
                               "ibm-plex-sans", 
                               package="glyptodon"),
                   "IBMPlexSans-Bold.ttf"))
font_add("Source Sans Pro",
         file.path(system.file("fonts", 
                               "source-sans-pro", 
                               package="glyptodon"),
                   "SourceSansPro-Regular.ttf"))
showtext_auto()
## basic example code
df <- data.frame(dates = seq(as.Date("2020-01-01"), length.out = 12, 
                             by = "month"),
                 values = 1:12)
ggplot(df, aes(dates, values)) +
  geom_line() +
  expand_limits(y = 0) +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  labs(title = "Change in Stuff",
       subtitle = "You can copy and paste your own content in to see what it looks like with these font combinations",
       caption = "Source: https://github.com/yukatapangolin/themeglyptodon") +
  theme_glyptodon(grid = "Y")
```

<img src="man/figures/README-example-1.png" width="100%" />

## Small Multiple

``` r
df2 <- data.frame(dates = seq(as.Date("2020-01-01"), length.out = 99, 
                             by = "month"),
                 values = 1:99,
                 group = c("Arial1", "Arial2", "Arial3"), each = 33)
ggplot(df2, aes(dates, values)) +
  geom_line() +
  facet_wrap(~ group) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_x_date(breaks = seq.Date(as.Date("2020-01-01"), as.Date("2028-01-01"), 
                                 by = "3 year"),
               labels = c(2020, "Goldman Sans\nCondensed", 2026)) +
  xlab("Arial") +
  ylab("Arial") +
  expand_limits(y = 0) +
  labs(title = "IBM Plex Sans",
       subtitle = "Source Sans Pro - You can copy and paste your own content in to see what it looks like with these font combinations",
       caption = "Roboto: https://github.com/yukatapangolin/themeglyptodon") +
  theme_glyptodon()
```

<img src="man/figures/README-facets-1.png" width="100%" />
