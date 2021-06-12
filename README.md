# Dataset: COVID-19 case counts in The Netherlands

We collect numbers on COVID-19 disease count cases in **The Netherlands**. The numbers are collected from various sources on a daily base, like [RIVM (National Institute for Public Health and the Environment)](https://www.rivm.nl/nieuws/actuele-informatie-over-coronavirus), [NICE (Nationale Intensive Care Evaluatie)](https://www.stichting-nice.nl/), and the [National Corona Dashboard](https://coronadashboard.rijksoverheid.nl/). The data in this repository are mainly used to inform the general public with daily updates on COVID-19 disease count cases. This project standardizes, and publishes data and makes it **Findable, Accessible, Interoperable, and Reusable (FAIR)**.

Dutch:
> Wij verzamelen ziektecijfers over COVID-19 in Nederland. Dagelijks worden de cijfers verzameld van het [RIVM (Rijksinstituut voor de Volksgezondheid en Milieu)](https://www.rivm.nl/nieuws/actuele-informatie-over-coronavirus), [NICE (Nationale Intensive Care Evaluatie)](https://www.stichting-nice.nl/), [LCPS (Landelijk Coördinatiecentrum Patiënten Spreiding)](https://www.lcps.nu), en [Nationale Corona Dashboard](https://coronadashboard.rijksoverheid.nl/). De data in deze repository worden voornamelijk gebruikt om het algemene publiek te informeren met dagelijkse updates ten aanzien van ziektecijfers over COVID-19. Dit project standaardiseert en publiceert de gegevens en maakt ze vindbaar, toegankelijk, interoperabel en herbruikbaar (FAIR).

# License
The graphs and data are licensed CC0. The original data is licensed under the 'Public Domain Mark' by the RIVM.

## Datasets
The datasets available in this repository are updated on a daily base. Availability depends on the publication by the respective sources (N.B. since July 1st, the epidemiological reports published by RIVM will be released on a *weekly* instead of daily basis). The project divides the datasets into four main categories:

* [Daily municipal datasets](#daily_municipality_cumulative)
* [Daily casus datasets](#daily_total_cumulative)
* [Intensive care data](https://github.com/mzelst/covid-19/tree/master/data-nice)
* [Data on daily corrections](#corrections)


### NICE data

This folder contains various raw datasets as well as compilations of those raw data from the NICE website. NICE is the national organization for IC data but collected COVID-19 data from clinical departments as well. Data is collected every day at 15:15 from [NICE](www.stichting-nice.nl).

* [Age-related data](data-nice/age)
* [Data on amount of time in hospital](data-nice/treatment-time)
* [Data on discharge](data-nice/exit)
* [Data on hospital intake and hospitalized](data-nice/data-nice-json)

