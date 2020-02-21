"""
Python Coding Sample - Web Scraping

Created on Fri Jan 01 12:00:00 2020

author: Bahar Zafer

Text analysis code which estimates time difference between 
dates of criminal incidents and courts.

Online data base is https://www.oldbaileyonline.org/, 
London's Central Criminal Court 1674-1913

"""
import os
import re
import urllib.request
import matplotlib.pylab as plt
import numpy as np
import time
request = 'all'

# Creating arrays for years and months
ok_years = np.zeros((10000, 1, 2), dtype='int')
ok_months = np.zeros((10000, 1, 2), dtype='int')
start_value = np.zeros((10000, 1, 1), dtype='int')
month = ["January", "February", "March", "April", "May", "June", "July", 
         "August", "September", "October", "November", "December"]

# Defining a function that 
## randomly chooses web pages and
## stores dates of courts and incidents. 
def court_date_analyze(query, n_query):
    clean_query = re.sub(r'\W+', '', query)
    if not os.path.exists(clean_query):
        os.makedirs(clean_query)

    from random import seed
    from random import choice
    # Seed random number generator
    seed(1)
    sequence = [i for i in range(0, 202790, 10)]  # Range of web pages
    # 202789 is the total number of cases. 
    # Each page contains 10 court cases.

    for m in range(0, n_query+1):
        temp_value = choice(sequence)
        if temp_value in start_value and m > 1:  
            # Checking if new value already exists in the array
            m = m - 1
        else:
            start_value[m] = temp_value

    ciy = 0  # Counter

    for k in range(0, m):
        # url
        url = 'https://www.oldbaileyonline.org/search.jsp?gen=1&form=custom&count=202789&start='

        url += str(int(start_value[k]))
        print(url)
        response = urllib.request.urlopen(url)
        web_content = response.read()
        # Converting text to utf-8
        text = web_content.decode("utf-8").split(" ")
        urls = []
        # Finding trial IDs
        for words in text:
            if words.find("browse.jsp?id=") != -1:
                # isolate the id
                urls.append(words[words.find("id=") + 3: words.find("&")])
        for items in urls:
            j = ciy  # Inner loop counter
            url = "http://www.oldbaileyonline.org/print.jsp?div=" + items

            # Opening web-page
            response = urllib.request.urlopen(url)
            web_content = response.read()
            # Filtering out punctuations and retrieving numbers and months
            numbers = [int(s) for s in re.findall(r"[\w']+|[.,!?;]", web_content.decode("utf-8")) if s.isdigit()]
            months = list(set(re.findall(r"[\w']+|[.,!?;]", web_content.decode("utf-8"))).intersection(month))
            # b = 0 for the date of court cases, b = 1 for the date of incidents
            b = 0
            for i in range(0, len(numbers)):
                if 1673 < numbers[i] < 1914:  # All court cases are between the years 1673 and 1914.
                    ok_years[j, 0, b] = numbers[i]
                    if set(month).intersection([re.findall(r"[\w']+|[.,!?;]", web_content.decode("utf-8"))[
                                                    re.findall(r"[\w']+|[.,!?;]", web_content.decode("utf-8")).index(
                                                        str(ok_years[j, 0, b])) - 1]]) != []:
                        ok_months[j, 0, 0] = month.index(re.findall(r"[\w']+|[.,!?;]", web_content.decode("utf-8"))[
                                                             re.findall(r"[\w']+|[.,!?;]",
                                                                        web_content.decode("utf-8")).index(
                                                                 str(ok_years[j, 0, 0])) - 1]) + 1
                    if len(months) > 2 and len(
                            list(set(months) - set(['January', month[ok_months[0, 0, 0]]]))) != 0 and b == 1:
                        # The first month in text is the date when the code is executed, which is why it is filtered out.
                        # The code is executed in January, this section is adjusted accordingly.
                        temp_1 = list(set(months) - set(['January', month[ok_months[0, 0, 0]]]))
                        temp_month = month.index(temp_1[0])
                        if temp_month > ok_months[j, 0, 0] and ok_years[j, 0, b] != 0:
                            ok_months[j, 0, 1] = temp_month
                        elif temp_month <= ok_months[j, 0, 0] and ok_years[j, 0, b] == 0:
                            ok_months[j, 0, 1] = temp_month
                    if ok_years[j, 0, b] != ok_years[j, 0, b - 1]: 
                        # Assuming the first year in the text following the court date is the year that incident hapenned.
                        b = b + 1
                    if b == 2:
                        break
                # If no month or year is mentioned after the court date
                # Assuming the incident happened in the same month and year with the court      
                if ok_years[j, 0, 1] == 0:
                    ok_years[j, 0, 1] = ok_years[j, 0, 0]
                if ok_months[j, 0, 1] == 0:
                    ok_months[j, 0, 1] = ok_months[j, 0, 0]
            ciy = j + 1
        time.sleep(1)  # Pause added in order to solve possible connection problems due to successive page requests

    return

court_date_analyze(request, 1000) 

# Calculating date differences
date_dif = (ok_years[:, 0, 0] - ok_years[:, 0, 1]) * 12 + (ok_months[:, 0, 0] - ok_months[:, 0, 1])
date_dif[date_dif < -12] = -1  # Cleaning possibly wrong logged dates
date_dif[date_dif > 120] = -1  # Cleaning possible data reading errors
date_dif[date_dif < 0] = date_dif[date_dif < 0] + 12  # Correcting month difference calculation
names = ['No difference', 'Difference', 'Wrong Cases']
no_disc = len(date_dif[date_dif == 0])/len(date_dif)*100
wr_cas = len(date_dif[date_dif == -1])/len(date_dif)*100
# Wrong cases consist of possibly wrong logged dates and suspected data reading errors.
disc = 100 - (no_disc + wr_cas)
values = [no_disc, disc, wr_cas]

# Plots
plt.subplot(211)
plt.plot(ok_years[:, 0, 0], date_dif, 'bs')
plt.axis([1673, 1914, 1, 120])
plt.ylabel('Month difference')
plt.xlabel('Years')
plt.grid(True)
plt.title('Difference between dates of incidents and courts')
plt.subplot(212)
plt.bar(names, values)
plt.xlabel('Categories')
plt.ylabel('Ratio [%]')