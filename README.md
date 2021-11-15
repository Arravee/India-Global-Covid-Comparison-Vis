# India-Global-Covid-Comparison-Vis
Visuals looking at rates of Covid Cases and Deaths both Globally and in India

All Global rates are currently excluding India. If you wish to change this please go into the R scrip file "Covid India v Global Visuals.R" found in the main directory and mark out line 68 "global_covid_deaths <- global_deaths %>% filter((`Country/Region` != keep))" and line 36 "global_covid_cases <- global_cases %>% filter((`Country/Region` != keep))". 

Please also make sure to change the plot titles to reflect the change in code. 

This diretcory contains folders that have png visuals of the graphs as well as the html versions. The htmls are fully interactable and the various tools to interact with them can be found in the top right corner. All the plots are produced with plotly and ggplot. Please note that github cannot preview html files without a client side extension like BitBucket HTML. 

The dataset that is being used to generate these visuals is constantly maintained by John Hopkins University on their github repository and is sourced from various government and international health and medical institutions such as the World Health Organization. If you wish to see a full list of the sources please visit <https://github.com/CSSEGISandData/COVID-19>. 

