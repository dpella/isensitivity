#!/usr/bin/env python3
import sys
import csv
from pathlib import Path
import matplotlib.pyplot as plt
import re

# reference_data :: [(query, distribution)]
# synthetic_data :: [(query, distribution)]
def plot(reference_data, old_synthetic_data, new_synthetic_data, title=""):
    plt.title(title)
    plt.xlabel('Query')
    plt.ylabel('Score')

    dim = range(1,len(reference_data)+1)
    plt.xticks([1,10,20,30,40,50])

    reference_data_distribution = [x[1] for x in reference_data]
    old_synthetic_data_distribution = [x[1] for x in old_synthetic_data]
    new_synthetic_data_distribution = [x[1] for x in new_synthetic_data]

    # The line representing the raw reference data
    praw = plt.plot(dim,reference_data_distribution, 'bo')
    # The line representing the synthetic data
    opsyn = plt.plot(dim,old_synthetic_data_distribution, 'ro', alpha=0.5)
    npsyn = plt.plot(dim,new_synthetic_data_distribution, 'gx', color='black', alpha=0.7)

    # Make the plot look nicer
    plt.legend((praw[0], opsyn[0], npsyn[0]), ('Raw', 'Synthetic (OG: hardcoded sensitivity)', 'Synthetic (NEW: computed sensitivity)'))
    plt.ylim(0, max(reference_data_distribution)+ 0.1*max(reference_data_distribution))
    plt.grid(True)
    plt.tight_layout()
    return plt

def main():
    reference_data = []
    with open('reference.csv') as csvfile:
        reference_data = [(row[0:-1], float(row[-1])) for row in list(csv.reader(csvfile))[1:]]  # Chop of the header
    # reference_data.sort(key=lambda o: o[1])
    reference_data_attributes = [ref[0] for ref in reference_data]

    old_paths = Path('results/old').glob('*.csv')
    new_paths = Path('results/new').glob('*.csv')
    for (old,new) in zip(old_paths,new_paths):
        print(f'plotting {old} / {new}')
        old_synthetic_data = []
        with open(old) as csvfile:
            csv_reader = csv.reader(csvfile)
            old_synthetic_data = [(row[0:-1], float(row[-1])) for row in list(csv_reader)[1:]]  # synthetic_data :: [(attributes, distribution)]
            # old_synthetic_data.sort(key=lambda syn: reference_data_attributes.index(syn[0]))  # Sort by order of reference data

        new_synthetic_data = []
        with open(new) as csvfile:
            csv_reader = csv.reader(csvfile)
            new_synthetic_data = [(row[0:-1], float(row[-1])) for row in list(csv_reader)[1:]]  # synthetic_data :: [(attributes, distribution)]
            # new_synthetic_data.sort(key=lambda syn: reference_data_attributes.index(syn[0]))  # Sort by order of reference data

        # Plot the real data in relation to the synthetic data
        new_regexp     = re.search(r'adult-(.*)e-(.*)i', new.stem)
        new_epsilon    = new_regexp.group(1)
        new_iterations = new_regexp.group(2)
        old_regexp     = re.search(r'adult-(.*)e-(.*)i', old.stem)
        old_epsilon    = old_regexp.group(1)
        old_iterations = old_regexp.group(2)
        fancy_name = f'MWEM - ε {new_epsilon}\n{new_iterations} iterations'

        # Pick the top X data points
        final_plot = plot(reference_data, old_synthetic_data, new_synthetic_data, title=fancy_name)
        final_plot.savefig(f'figures/{new.stem}.png', bbox_inches='tight')
        final_plot.clf()
    return 0

if __name__ == '__main__':
    sys.exit(main())
