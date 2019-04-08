
import csv
import os
if os.path.exists('voting_data' + '.csv'):
  os.remove('voting_data' + '.csv')

row = ['Party','Name','Constituency','Voting 1','Voting 2','Voting 3','Voting 4','Voting 5','Voting 6','Voting 7','Voting 8']

def write_csv(filename, row):
    with open(filename, 'a') as file:
        filewriter = csv.writer(file, delimiter=',')
        filewriter.writerow(row)
        file.close()
write_csv('voting_data' + '.csv', row)
row = []
with open("/Users/Abtin/Desktop/Guardian_files/table.html", "r") as f:
    i = 1
    for line in f.readlines():

        #selecting the party
        if(i == 3):
            row.append(line)

        #selecting name
        if(i == 6):
            row.append(line)

        #getting the contingeuncy
        #percentages are written in terms of Remain
        if(i == 11):
            st = line.split('>')[1]
            if('--' in st):
                row.append(st)
                print(row)
            if 'Leave'  in st:
                st = st[6:8]
                st= 100-int(st)
                row.append(st)
            if 'Remain' in str(st):
                st = int(st[7:9])
                row.append(st)

        #getting vote 1
        if(i ==16):
            st =  line.split('gv-vote-blob ')[1]
            st = st[:-2]
            row.append(st)

        #getting vote 2
        if(i ==19):
            st =  line.split('gv-vote-blob ')[1]
            st = st[:-2]
            row.append(st)

        #getting vote 3
        if(i ==22):
            st =  line.split('gv-vote-blob ')[1]
            st = st[:-2]
            row.append(st)

        #getting vote 4
        if(i ==25):
            st =  line.split('gv-vote-blob ')[1]
            st = st[:-2]
            row.append(st)

        #getting vote 5
        if(i ==28):
            st =  line.split('gv-vote-blob ')[1]
            st = st[:-2]
            row.append(st)

        #getting vote 6
        if(i ==31):
            st =  line.split('gv-vote-blob ')[1]
            st = st[:-2]
            row.append(st)

        #getting vote 7
        if(i ==34):
            st =  line.split('gv-vote-blob ')[1]
            st = st[:-2]
            row.append(st)

        #getting vote 8
        if(i ==37):
            st =  line.split('gv-vote-blob ')[1]
            st = st[:-2]
            row.append(st)

        #reseting the counter to reed the next row
        if (i==42):
            i = 0
            write_csv('voting_data' + '.csv', row)
            row=[]

        i= i+1
