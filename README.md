# farsr ![alt text][travisbuild]

[travisbuild]: https://travis-ci.org/Alice-MacQueen/farsr.svg?branch=master

Fatality Analysis Reporting System Data R package.

This is an assignment for the "Building R Packages" course. There are two functions available in this package, `fars_summarize_years` and `fars_map_state`.

## Summary Table by Year

`fars_summarize_years` creates summaries of monthly fatalities using Fatality Analysis
Reporting System data for a specified year or years. It takes as arguments a four-digit year or vector of years.

## Maps of Fatalities by State and Year

`fars_map_state` makes a plot of Fatality Analysis Reporting System (FARS)
data for a given state number and year. It takes as arguments the integer number of the state to be plotted, from 1-56, and the year to be plotted.
