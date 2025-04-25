# turkuwasps

R package for use at the [Zoological Museum of the University of Turku](https://collections.utu.fi/en/zoological-museum/). For analysing the ecology and taxonomy of the wasps collected by Malaise trapping in [Uganda 2014-2015](https://doi.org/10.5281/zenodo.2225643) and [Peru 1998-2011](https://doi.org/10.5281/zenodo.3559054).

Takes the data of (typically) one subfamily of wasps, and can then:
- plot when and where in the forest they flew
- analyse how the weather and forest type affected each species' catch
- help visualise which species resemble each other
- create a preliminary identification key
- create preliminary species descriptions


## Installation

You can install the development version of turkuwasps from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("tapani-hopkins/turkuwasps")
```

## Usage
More usage examples to come, package is still under construction.

### Load package and read data

``` r
# load the package
library(turkuwasps)
```
``` r
# get example wasp data (csv file from Kotka)
f = system.file("extdata", "wasps_example.csv", package = "turkuwasps", mustWork = TRUE)
wasps = read_wasps(f)

# get wasps and samples which can be used in ecological analyses
# (e.g. damaged samples and their wasps removed)
tmp = ecology_usable(wasps)
x = tmp$wasps
m = tmp$samples
```

### Show where wasps were caught

<img src="inst/example_images/plot_place.png" height="140">

``` r
# show wasps caught in each forest type
plot_place(x$forest_type)

# show wasps caught by each trap, scaled by sampling effort
plot_place(x$trap, m, ylab="Wasps / trap day")

# add legend
default_legend("forest_type", "Uganda 2014-2015")

# you can also show each species separately
plot_place(x$forest_type, m, taxon=x$taxon)
```

### Show when wasps were caught

<img src="inst/example_images/plot_time.png" height="140">

``` r
# store when each wasp was caught
xdate = as.interval(x$start, x$end)

# show when wasps were caught
plot_time(xdate)

# show when wasps were caught, split by species and scaled by sampling effort
plot_time(xdate, m, taxon=x$taxon, ylab="wasps / trap day")

# add legend
default_legend(x$taxon, x="topleft")
```

### Explore how wasp catches varied with rain etc

<img src="inst/example_images/explore.png" height="140">

``` r
# show how catches varied with rain and amount of wood 
explore(x$sample, m, what=c("rain", "deadwood", "livewood"))

# show how catches varied with pretty much everything
explore(x$sample, m)
```

### Analyse how wasp catches depend on rain, forest type etc 

``` r
# store model (GLM) which will be fitted to wasp catches
model = "offset(tdiff_log) + days + rain + forest_type + deadwood"

# fit model and get p values (only do three resamples to save time)
a = resample(model, x, m, pairwise="forest_type", nBoot=3)

# show coefficients of the fitted model
a$coefficients

# show which variables affected wasp catches
a$p

# show which forest types differed from each other in number of wasps caught
a$p_pairwise
```

### Show how quickly species accumulated

<img src="inst/example_images/plot_rarefaction.png" height="140">

``` r
## Simple

# plot separate curves for each forest type
col = c(primary="darkgreen", swamp="blue", disturbed="green", clearcut="yellow", farm="orange")
r = plot_rarefaction(wasps, by="forest_type", col=col, pch=1:4)

# add a legend
legend_rarefaction(r=r)


## Complex

# prepare a new column in the wasp data, for drawing separate curves for dry and wet season
# also see to it the curves will be drawn in successional order primary -> farm
levs = c("primary", "swamp", "disturbed", "clearcut", "farm")
wasps$forest_type = factor(wasps$forest_type, levels=levs)
wasps$forest_season = combine_columns(wasps, c("forest_type", "season"), all=TRUE)

# plot rarefaction curves with separate curves for each forest type and season
col = c(primary="darkgreen", swamp="blue", disturbed="green", clearcut="yellow", farm="orange")
col = rep(col, each=2)
r = plot_rarefaction(wasps, by="forest_season", col=col, pch=rep(1:2, 5), xlim=c(0, 100))

# add a legend with dry and wet season in separate columns
txt = rep(levels0(wasps$forest_type), each=2)
legend_rarefaction(txt=txt, r=r, column=rep(1:2, 5), colnames=c("dry", "wet"), title="Uganda")
```
