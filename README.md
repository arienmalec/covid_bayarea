# Comparing the effects of COVID-19 shelter-in-place on Bay Area case totals and outcomes
## Overview

The Bay Area 6-county region implemented an early shelter-in-place order taking effect March 16th. What was the impact of these policies on the slope of the curve for case and death totals, and how did the Bay Area policy impacts compare to other regions of the country that adopted later policy interventions?

Pretty deep questions, but all I've got are R, `ggplot2` and the NY Times data set.

## Limitations

Many, including region comparability and the reliability of case tracking as a valid signal.

## Dependencies

* NYT Github COVID-19 tracking repository (included as a submodule)
* R with the tidyverse stack and `ggthemes`