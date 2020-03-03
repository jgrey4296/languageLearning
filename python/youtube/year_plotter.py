"""
Plotting youtube videos published per date by my subscriptions,
adapted from:
https://matplotlib.org/gallery/text_labels_and_annotations/date.html#sphx-glr-gallery-text-labels-and-annotations-date-py
"""
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
import matplotlib.cbook as cbook
from matplotlib.dates import date2num, num2date
import json
import datetime

years = mdates.YearLocator()   # every year
months = mdates.MonthLocator()  # every month
years_fmt = mdates.DateFormatter('%Y')


with open('.dates.json','r') as f:
    dates = json.load(f)

data = np.zeros(2)

for year_num,year_data in dates.items():
    if year_num == "__counts":
        continue
    for month_num, month_data in year_data.items():
        if month_num == "__counts":
            continue
        for day_num, day_data in month_data.items():
            if day_num == "__counts":
                continue
            date = date2num(datetime.datetime(int(year_num),int(month_num),int(day_num)))
            data = np.row_stack((data, np.array([date, day_data['__counts']])))

data = data[1:,:]
sorted_data = data[np.argsort(data[:,0])]

fig, ax = plt.subplots()
ax.plot(sorted_data[0:,0],sorted_data[0:,1])


# format the ticks
ax.xaxis.set_major_locator(years)
ax.xaxis.set_major_formatter(years_fmt)
ax.xaxis.set_minor_locator(months)

# round to nearest years.
datemin = np.datetime64(num2date(sorted_data[0:,0])[0], 'Y')
datemax = np.datetime64(num2date(sorted_data[0:,0])[-1], 'Y') + np.timedelta64(1, 'Y')
ax.set_xlim(datemin, datemax)

# format the coords message box
ax.format_xdata = mdates.DateFormatter('%Y-%m-%d')
ax.format_ydata = lambda x: int(x)  # format the price.
ax.grid(True)

# rotates and right aligns the x labels, and moves the bottom of the
# axes up to make room for them
fig.autofmt_xdate()

plt.show()
