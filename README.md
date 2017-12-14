# Genetic Algorithms  
By: Rui Chen | Hangyu Huang | Eric Kim | Shuyu Zhao

In order to install this package, make sure you have `devtools` and then run `devtools::install_github("erickim/GA")`. If not, please run the following.

```{r}
install.packages("devtools")
library(devtools)
devtools::install_github("erickim/GA")
```

Once you've installed the package, you can load it in via

```{r}
library(GA)
```

If the documentations on the help pages fail to load because of a "corruption" error, please restart your R process. It should work then.

For a basic usage of the genetic algorithm for feature selection in linear regression, run 

```{r}
Y <- MASS::Boston[,1]
X <- MASS::Boston[,-1]

select(Y, X)
```

For more detailed control over the code, check `help("select")` or `help("GA::select")` for extra details on the arguments and default values.

To run test code, make sure to have the `testthat` library installed and run `test_package("GA")`. If not, please run

```{r}
install.packages("testthat")
library(testthat)
test_package("GA")
```

There is one last hidden `.internal` directory. Should you choose to look into that directory, you will find a simplified spec for this project in `spec.md` that we used to split up some of the tasks as well as an early design document in `design.md`. Our code does not strictly adhere to the design document, but it provides a good idea of our early directions. Lastly, `explor.md` has some more test code and example usages that we came up with when initially implementing the functions.