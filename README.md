# ram_babu

ram_babu is a program written in sml(standard ML) which functions to parse iCalender and CSV records, and consequently facilitates translation between them.

It provides the following functions:
1. ical2reclist
</t>=>Parses events from the input iCalender file. Output is a list of records, where each record is a calender event.
2. csv2reclist
</t>=>Parses events from the input CSV file. Output is a list of records, where each record is a calender event.
3. reclist2ical
</t>=>Given an input list of event records, writes the events in iCalender format to an output file.
4. reclist2csv
</t>=>Given an input list of event records, writes them in CSV format to an output file.
5. ical2csv
</t>=>Converts the events given in iCalender format from the input file to CSV format and writes the result to an output file.

RUNNING <i>ram_babu</i>
---

1. you need to have 'sml' installed. 
2. go to 'sml' shell & type: use "my_ical.sml";
3. Use the desired function passing necessary arguments
