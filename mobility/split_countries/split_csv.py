import csv
import pandas as pd
import numpy as np

with open('cases_mobility_activity.csv') as fin:    
    csvin = csv.DictReader(fin)
    # Category -> open file lookup
    outputs = {}
    for row in csvin:
        cat = row['region']
        # Open a new file and write the header
        if cat not in outputs:
            fout = open('{}.csv'.format(cat), 'w')
            dw = csv.DictWriter(fout, fieldnames=csvin.fieldnames)
            dw.writeheader()
            outputs[cat] = fout, dw
        # Always write the row
        outputs[cat][1].writerow(row)
    # Close all the files
    for fout, _ in outputs.values():
        fout.close()
        
df = pd.DataFrame(outputs)
df = df.T
