# Remarks datasets

## Dataset used

The RIVM uploads a dataset with all cases of confirmed COVID-19 patients. The data reports Date statistics, Date statistics type, Agegroup, Sex, Province, Hospital admission, Deceased, Week of death, and Municipal health service.

## Calculation of cases

Each day, the dataset is downloaded and a table is parsed that calculates the cumulative number of reported cases on each calendar day. The [table](cases_perday.csv) is therefore a matrix that shows each calendar date with reported cases in the rows and the cumulative count of cases for each reporting day in the columns.

## Calculation of corrections

To calculate the number of corrections, we subtract the values in this [table](cases_perday.csv) from the values of the table of yesterday. These difference values get added to the [table](cases_perday.csv) that shows the reported cases per day. These corrections, in technical jargon, are mutations of the reported number of cases on each calendar day. The mutations are the net difference between the cumulative number of cases of today and yesterday for each calendar day. This implies that we cannot differentiate between the gross increase and decrease of mutations on each day: we can only see the net mutation of cases per reporting day.

Potential reasons for mutations:
- New cases are added due to people being positively tested for COVID-19. Without errors, this would be the only reason for mutations (positive mutation).
- New cases were added that should not have been added. For example, two GGDs report the same case for which one is removed later on (negative mutation).
- The calendar day of the new case was misreported. The case is moved to another calendar day (no net change in the number of cases, as this gives a positive and a negative mutation simultaneously).
- The type of date statistic is changed. The type of date statistic differentiates between 'date of disease onset', 'date of first positive positive labresult', and 'date of notification' (day that GGD receives notification of case). When the type of date statistic is changed, the reporting day is extremely likely to change as well (no net change in the number of cases, as this gives a positive and a negative mutation simultaneously).

An example: The cumulative count of cases on calendar day 22-07 is reported to be 10 on reporting day 24-07. The next day (reporting day 25-07), we observe that the cumulative count of cases has increased to 12 and we therefore calculate a net increase of +2. Whether the +2 is only made up of two new cases or whether this is the result of new cases and removals (for example, 3 new cases and 1 deleted case) is unknown.

## Substantive interpretation

Given that we cannot differentiate between gross increases and decreases in the mutations, interpreting the mutations becomes somewhat difficult. However, we can differentiate between meaningful and inconsequential mutations if we assume an epidemiological curve where the right hand side of the curve is currently the most important side. For example, mutations in the last two to three weeks where cases get shifted around on reporting dates are inconsequential for understanding transmission: as long as they are still within the last two to three weeks, there is a possibility that these people are still infectious. However, given that we only see net mutations and the number of cases increases mostly in the last two weeks, we cannot see these mutations anyhow and we can therefore mostly ignore them.

Mutations that are done for much earlier reporting days are much more meaningful in the sense that we need to correct for them in the reported cases per day, as these are inconsequential for understanding viral transmission at this point in time. Negative mutations from this period should be removed from the cumulative net increase per day. The same goes for positive mutations: if, for example, cases get added on reporting days that happened two months ago, these are also inconsequential and should be removed from the net increase as well.

## Current daily reports (last updated: 27-07-2020, 14:30 CET)

At this point, we report the net increase in cases each day as well as the net negative mutations that we can detect on each reporting day (as explained above, most of the net negative mutations are reported for days that are weeks ago). In future updates, we want to differentiate between meaningful and inconsequential mutations to get a better view of the real-time transmission of COVID-19.


 
