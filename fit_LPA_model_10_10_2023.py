import pandas as pd
import pickle
from vbayesfa.fit_lpa import fit_lpa

# The prior on alpha is not effectively fixed at any value (the prior is weak).
# The random seed is specified, so this should be replicable.

# This only includes people with no missing data.

if __name__ == "__main__":

    path = '/Users/sampaskewitz/Documents/ABCD data processing/complete data sets (z-scored etc)/'
    x = pd.read_csv(path + 'task behavioral data (2-year z-scored full data).csv',
                    index_col = 0)
    keep = x.isna().sum(axis = 1) == 0 # people with 0 missing values
    x = x.loc[keep] # only keep those people
    
    results = fit_lpa(x = x, 
                      n_workers = 4,
                      restarts = 100,
                      T = 20,
                      seed = 11413,
                      min_iter = 10,
                      max_iter = 1000,
                      tolerance = 1e-08)

    model = results['best_model']

    file = open('10-10-2023 (best infinite LPA model, no missing data).pkl', "wb")
    pickle.dump(model, file)
    file.close()