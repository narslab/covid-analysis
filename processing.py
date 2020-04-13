import csv

with open(r"C:\Users\nasko\dev\covid\covid-analysis\JHU\csse_covid_19_data\csse_covid_19_daily_reports\04-12-2020.csv", 'r') as f:
    reader = csv.reader(f)
    next(reader)
    maxnum = max(reader, key=lambda row: int(row[8]))
print(maxnum)

with open(r"C:\Users\nasko\dev\covid\covid-analysis\JHU\csse_covid_19_data\csse_covid_19_daily_reports\04-12-2020.csv", 'r') as f:
    reader = csv.reader(f)
    next(reader)
    minnum = min(reader, key=lambda row: int(row[8]))
print(minnum)
