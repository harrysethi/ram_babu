# ram_babu

<b>ram_babu</b> is a program written in sml(standard ML) which functions to parse iCalender and CSV records, and consequently facilitates translation between them.

It provides the following functions:

<ul>
<li><i>ical2reclist</i>
</t>: Parses events from the input iCalender file. Output is a list of records, where each record is a calender event.
<li><i>csv2reclist</i>
</t>: Parses events from the input CSV file. Output is a list of records, where each record is a calender event.
<li><i>reclist2ical</i>
</t>: Given an input list of event records, writes the events in iCalender format to an output file.
<li><i>reclist2csv</i>
</t>: Given an input list of event records, writes them in CSV format to an output file.
<li><i>ical2csv</i>
</t>: Converts the events given in iCalender format from the input file to CSV format and writes the result to an output file.
</ul>

RUNNING <i>ram_babu</i>
---

1. you need to have 'sml' installed. 
2. go to 'sml' shell & type: use "my_ical.sml";
3. Use the desired function passing necessary arguments
