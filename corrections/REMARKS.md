# Remarks datasets

## Dataset used

The RIVM uploads a dataset with all cases of confirmed COVID-19 patients. The data reports Date statistics, Date statistics type, Agegroup, Sex, Province, Hospital admission, Deceased, Week of death, and Municipal health service.

## Calculation of cases

Each day, the dataset is downloaded and a table is parsed that calculates the cumulative number of reported cases on each calendar day. The [table](corrections/cases_perday.csv) is therefore a matrix that shows each calendar date with reported cases in the rows and the cumulative count of cases for each reporting day in the columns.

## Calculation of corrections

To calculate the number of corrections, we subtract the values in this [table](corrections/cases_perday.csv) from the values of the table of yesterday. These difference values get added to the [table](corrections/cases_perday.csv) that shows the reported cases per day. These corrections, in technical jargon, are mutations of the reported number of cases on each calendar day. The mutations are the net difference between the cumulative number of cases of today and yesterday for each calendar day. This implies that we cannot differentiate between the gross increase and decrease of mutations on each day: we can only see the net mutation of cases per reporting day.
An example: The cumulative count of cases on calendar day 22-07 is reported to be 10 on reporting day 24-07. The next day (reporting day 25-07), we observe that the cumulative count of cases has increased to 12 and we therefore calculate a net increase of +2. Whether the +2 is only made up of two new cases or whether this is the result of 3 new cases being added and 1 being removed, is unknown.

 