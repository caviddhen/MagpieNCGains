# Easy conversion MAgPIE nc to GAINS mapping

R package **MagpieNCGains**, version **1.0.0**

  

## Purpose and Functionality

Converts NC generated from MAgPIE outputs to GAINS mapping.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("MagpieNCGains")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact David Meng-Chuen Chen <david.chen@pik-potsdam.de>.

## Citation

To cite package **MagpieNCGains** in publications use:

Chen D (2020). _MagpieNCGains: Easy conversion MAgPIE nc to GAINS
mapping_. R package version 1.0.0.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {MagpieNCGains: Easy conversion MAgPIE nc to GAINS mapping},
  author = {David Meng-Chuen Chen},
  year = {2020},
  note = {R package version 1.0.0},
}
```

