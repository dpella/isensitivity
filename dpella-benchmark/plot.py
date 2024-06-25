#!/usr/bin/env python3
import sys
import csv
from pathlib import Path
import matplotlib.pyplot as plt
import re

# reference_data :: [(query, distribution)]
# synthetic_data :: [(query, distribution)]
def plot(reference_data, synthetic_data, title=""):
    plt.title(title)
    plt.xlabel('Query')
    plt.ylabel('Score')

    dim = range(1,len(reference_data)+1)
    plt.xticks([1,10,20,30,40,50])

    reference_data_distribution = [x[1] for x in reference_data]
    synthetic_data_distribution = [x[1] for x in synthetic_data]

    # The line representing the raw reference data
    praw = plt.plot(dim,reference_data_distribution, 'bo',linewidth=3)
    # The line representing the synthetic data
    psyn = plt.plot(dim,synthetic_data_distribution, 'ro', linewidth=3, alpha=0.5)

    # Make the plot look nicer
    # plt.legend((praw[0], psyn[0]), ('Raw', 'Synthetic (OG: hardcoded sensitivity)'))
    plt.legend((praw[0], psyn[0]), ('Raw', 'Synthetic (NEW: computed sensitivity)'))
    plt.grid(True)
    plt.tight_layout()
    return plt

def main():
    reference_data = []
    with open('reference.csv') as csvfile:
        reference_data = [(row[0:-1], float(row[-1])) for row in list(csv.reader(csvfile))[1:]]  # Chop of the header
    # reference_data.sort(key=lambda o: o[1])
    reference_data_attributes = [ref[0] for ref in reference_data]

    paths = Path('results/').glob('*.csv')
    for path in paths:
        synthetic_data = []
        with open(path) as csvfile:
            csv_reader = csv.reader(csvfile)
            synthetic_data = [(row[0:-1], float(row[-1])) for row in list(csv_reader)[1:]]  # synthetic_data :: [(attributes, distribution)]
            # synthetic_data.sort(key=lambda syn: reference_data_attributes.index(syn[0]))  # Sort by order of reference data
            # print(sum([float(o[1]) for o in synthetic_data]))  # Just to check that the generated data is as large as the reference. Should print out ~32 560

        # Plot the real data in relation to the synthetic data
        regexp     = re.search(r'adult-(.*)e-(.*)i', path.stem)
        epsilon    = regexp.group(1)
        iterations = regexp.group(2)
        fancy_name = f'MWEM - ε {epsilon}\n{iterations} iterations'

        # Pick the top X data points
        final_plot = plot(reference_data, synthetic_data, title=fancy_name)
        final_plot.savefig(f'figures/{path.stem}.png', bbox_inches='tight')
        final_plot.clf()
        # Calculate some statistics on the data points
        abs_difference = [abs(ref[1] - syn[1]) for (ref, syn) in zip(reference_data, synthetic_data)]
        print(f'Absolute difference: {abs_difference}')
        print(f'Largest difference: {max(abs_difference)}')
    return 0

if __name__ == '__main__':
    sys.exit(main())
