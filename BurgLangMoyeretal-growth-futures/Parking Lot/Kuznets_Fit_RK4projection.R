#### For Matt: Fit Kuznets models and use RK4 to project

## Clean up the environment if running in-memory
rm(list = ls())
gc()

## Packages
library("dplyr")
library("reshape2")
library("readr")
library("magrittr") ## Needed for the self-assignment operator %<>%
library("Hmisc")
library("optimx")

## Data: start with the data already assembled at the desired group level (e.g. Tilman's 7 groups or the 4 IFs groups)
data_groups <- readr::read_csv("data_groups_20220120153201.csv")

## Estimate the standard deviation of each growth rate using my variance-maximizing approach
## NOTE: This may take a minute, but it will print out its progress
data_groups$sd_src_pcrGDP <- NA
data_groups$theta <- NA
for (row in 1:nrow(data_groups)) {
    print(paste0("Estimating SD for observation ", row, " of ", nrow(data_groups)))

    ## Distances to from the focal observation's pcrGDP to every other observation's pcrGDP
    distances <- as.matrix(dist(data_groups$pcrGDP, upper = TRUE, diag = TRUE))[row,]

    ## Pick theta that maximizes variance in distances from focal observation to every other observation
    theta_opt <- optimx::optimx(par = 1, 
                                fn = function(par) {-1 * var(exp(-par * (distances/mean(distances))))}, 
                                method = "L-BFGS-B",
                                lower = 0, 
                                upper = 100)
    theta_opt = theta_opt[1,1]

    weights <- exp(-theta_opt * (distances/mean(distances)))
    
    data_groups$sd_src_pcrGDP[row] = Hmisc::wtd.var(data_groups$src_pcrGDP, weights = weights) %>% sqrt()
    ## Also save the nonlinear parameter theta which will be unique to each unique observation
    data_groups$theta[row] = theta_opt
}

## Fit the Kuznets model, which has three parameters
num_pars <- 3
## The solver needs a starting value for the parameters
par_init <- rep(1, num_pars)

fit_kuznets <- function(par) {

    y_pred <- function(x_vals) {
        par[1] + par[2]*log(x_vals) + par[3]*log(x_vals)^2
    }

    x_vals <- data_groups$pcrGDP
    pred <- y_pred(x_vals)
    obs <- data_groups$src_pcrGDP
    rss <- (obs - pred)^2 %>% sum()

    print(rss)
    return(rss)
}

opt_kuznets <- optimx::optimx(par = par_init, fn = fit_kuznets, method = "BFGS")
par_kuznets <- opt_kuznets[, c(1:num_pars)] %>% as.numeric()

## Fit asymptoting Kuznets model
fit_kuznets_asymptoting <- function(par) {

    y_pred <- function(x_vals) {
        (par[1] + par[2]*log(x_vals) + par[3]*log(x_vals)^2) * (1/x_vals)
    }

    x_vals <- data_groups$pcrGDP
    pred <- y_pred(x_vals)
    obs <- data_groups$src_pcrGDP
    rss <- (obs - pred)^2 %>% sum()

    print(rss)
    return(rss)
}

opt_kuznets_asymptoting <- optimx::optimx(par = par_init, fn = fit_kuznets_asymptoting, method = "BFGS")
par_kuznets_asymptoting <- opt_kuznets_asymptoting[, c(1:num_pars)] %>% as.numeric()

## If you want to plot the fits uncomment these lines, one for each version of the model
# curve(expr = par_kuznets[1] + par_kuznets[2]*log(x) + par_kuznets[3]*log(x)^2, from = min(data_groups$pcrGDP), to = max(data_groups$pcrGDP))
# curve(expr = (par_kuznets_asymptoting[1] + par_kuznets_asymptoting[2]*log(x) + par_kuznets_asymptoting[3]*log(x)^2) * (1/x), from = min(data_groups$pcrGDP), to = max(data_groups$pcrGDP))


#### Project the Kuznets model forward in time using the RK4 method
## Parameters
## Time step of data in years
h_real <- 1
## Number of time steps to estimate for each observed time step
h_frequency <- 20
## Number of time steps to project into the future
t_steps <- 100
## Parameters for RK4
h <- h_real/h_frequency
tmax <- t_steps/h
## Starting year
IC_time <- 2020

## Differential equation, note that this is not the y-axis in the SI figure (read the legend for details, it is the equation multiplied by the x-axis values)
dGroup <- function(x){
    if (is.infinite(x)) {
        Inf
    } else {
        (x * (par_kuznets[1] + par_kuznets[2]*log(x) + par_kuznets[3]*log(x)^2)) + 
        ((dGroup_sigma(x)^2) / 2)
    }
}

dGroup_asymptoting <- function(x){
    if (is.infinite(x)) {
        Inf
    } else {
        (x * (
            (par_kuznets_asymptoting[1] + par_kuznets_asymptoting[2]*log(x) + par_kuznets_asymptoting[3]*log(x)^2) * (1/x)
            )
        ) + 
        ((dGroup_sigma(x)^2) / 2)
    }
}

## STD fix
dGroup_sigma <- function(x, truncate = TRUE){
    if (is.infinite(x)) {
        estimated_sd <- Inf
    } else if (truncate & (x < min(data_groups_entity$pcrGDP, na.rm = TRUE))) {
        estimated_sd <- data_groups_entity[which(data_groups_entity$pcrGDP == min(data_groups_entity$pcrGDP, na.rm = TRUE)),]$sd_src_pcrGDP[1] ## If there are more than one year with the same minimum value, use the first
    } else if (truncate & (x > max(data_groups_entity$pcrGDP, na.rm = TRUE))) {
        estimated_sd <- data_groups_entity[which(data_groups_entity$pcrGDP == max(data_groups_entity$pcrGDP, na.rm = TRUE)),]$sd_src_pcrGDP[1] ## If there are more than one year with the same maximum value, use the first
    } else {
        estimated_sd <- spline(x = data_groups_entity$pcrGDP,
                            y = data_groups_entity$sd_src_pcrGDP,
                            xout = x)$y
    }

    return(estimated_sd)
}

                            # ## Pick which parameters to use and format Kuznets parameters for use in the projection
                            # par_fit <- par_kuznets
                            # p <- par_fit %>% as.list()

## Initial conditions
IC <- c(51450.361, 32396.555, 15456.257, 10160.750, 5411.062, 2561.718, 1275.441)
names(IC) = c("A", "B", "C", "D", "E", "F", "G")

## Initialize matrix to store projections from the ODE
data_RK4 <- matrix(NA,
            ncol = length(IC),
            nrow = tmax)
colnames(data_RK4) = names(IC)
data_RK4[1,] = IC %>% unlist()

for (entity in 1:ncol(data_RK4)) {
    print(paste0("Using RK4 to project group ", colnames(data_RK4)[entity], " using the regular Kuznets model"))

    data_groups_entity <- dplyr::filter(data_groups, Group == colnames(data_RK4)[entity])

    ## RK4
    for (idex in 2:tmax) {
        k1 <- dGroup(data_RK4[idex-1, entity])
        k2 <- dGroup(data_RK4[idex-1, entity] + (h/2*k1) )
        k3 <- dGroup(data_RK4[idex-1, entity] + (h/2*k2) )
        k4 <- dGroup(data_RK4[idex-1, entity] + (h*k3) )

        y_new <- data_RK4[idex-1, entity] + ((1/6) * h * (k1 + (2*k2) + (2*k3) + k4) )

        data_RK4[idex, entity] = y_new
    }
}

## Subsample down to the frequency of the data from the frequency that RK4 was run at
h_conversion <- rep(c(TRUE, rep(FALSE, h_frequency-1)), times = t_steps)
forecast <- dplyr::transmute(data_RK4[h_conversion,] %>% as_tibble(), Year = c(IC_time:(IC_time+t_steps-1)), across(everything()))
forecast = dplyr::transmute(forecast, Year_IC = IC_time, across(everything()))

## Only save predictions out to 2100 just in case t_steps is too large
forecast = dplyr::filter(forecast, Year <= 2100)

## Save regular Kuznets version so can rerun the asymptoting model
forecast_kuznets <- forecast


#### Redo for asymptoting Kuznets
## Initialize matrix to store projections from the ODE
data_RK4 <- matrix(NA,
            ncol = length(IC),
            nrow = tmax)
colnames(data_RK4) = names(IC)
data_RK4[1,] = IC %>% unlist()

for (entity in 1:ncol(data_RK4)) {
    print(paste0("Using RK4 to project group ", colnames(data_RK4)[entity], " using the asymptoting Kuznets model"))

    data_groups_entity <- dplyr::filter(data_groups, Group == colnames(data_RK4)[entity])

    ## RK4
    for (idex in 2:tmax) {
        k1 <- dGroup_asymptoting(data_RK4[idex-1, entity])
        k2 <- dGroup_asymptoting(data_RK4[idex-1, entity] + (h/2*k1) )
        k3 <- dGroup_asymptoting(data_RK4[idex-1, entity] + (h/2*k2) )
        k4 <- dGroup_asymptoting(data_RK4[idex-1, entity] + (h*k3) )

        y_new <- data_RK4[idex-1, entity] + ((1/6) * h * (k1 + (2*k2) + (2*k3) + k4) )

        data_RK4[idex, entity] = y_new
    }
}

## Subsample down to the frequency of the data from the frequency that RK4 was run at
h_conversion <- rep(c(TRUE, rep(FALSE, h_frequency-1)), times = t_steps)
forecast <- dplyr::transmute(data_RK4[h_conversion,] %>% as_tibble(), Year = c(IC_time:(IC_time+t_steps-1)), across(everything()))
forecast = dplyr::transmute(forecast, Year_IC = IC_time, across(everything()))

## Only save predictions out to 2100 just in case t_steps is too large
forecast = dplyr::filter(forecast, Year <= 2100)

forecast_kuznets_asymptoting <- forecast


#### Melt forecasts to make them easier to combine and plot
## Kuznets
forecast_kuznets_melted <- forecast_kuznets %>%
    dplyr::select(-Year_IC) %>%
    dplyr::mutate(Method = "Kuznets") %>%
    reshape2::melt(id.vars = c("Year", "Method")) %>%
    as_tibble()
colnames(forecast_kuznets_melted) = c("Year", "Method", "Income", "pcGDP")
forecast_kuznets_melted$Year %<>% as.character() %>% as.numeric()

## Asymptoting Kuznets
forecast_kuznets_asymptoting_melted <- forecast_kuznets_asymptoting %>%
dplyr::select(-Year_IC) %>% 
    dplyr::mutate(Method = "Kuznets_Asymptoting") %>%
    reshape2::melt(id.vars = c("Year", "Method")) %>%
    as_tibble()
colnames(forecast_kuznets_asymptoting_melted) = c("Year", "Method", "Income", "pcGDP")
forecast_kuznets_asymptoting_melted$Year %<>% as.character() %>% as.numeric()


#### Output
forecast_kuznets
forecast_kuznets_asymptoting

## Melted versions that can be stacked on top of each other along with other data/models
forecast_kuznets_melted
forecast_kuznets_asymptoting_melted
