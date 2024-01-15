import numpy as np
import pandas as pd

if __name__ == "__main__":

    path = '/Users/sampaskewitz/Documents/ABCD data processing/processed data (release 5.0)/'

    task_behav_data = pd.read_csv(path + 'ABCD 2-year task behavioral data.csv',
                                  index_col = 0)
    info_data = pd.read_csv(path + 'ABCD 2-year demographic info etc.csv',
                            index_col = 0)
    outcome_2year_data = pd.read_csv(path + 'ABCD 2-year outcome measures.csv',
                                     index_col = 0)
    outcome_3year_data = pd.read_csv(path + 'ABCD 3-year outcome measures.csv',
                                     index_col = 0)

    at_most_2missing = info_data.index[info_data['number_behav_missing'] <= 2].values
    training_set = task_behav_data.loc[at_most_2missing].sample(frac = 0.8, random_state = 12345)
    training_index = training_set.index
    test_index = task_behav_data.loc[at_most_2missing].index.difference(training_index) # people not in the training set

    def standardize(data):
        '''
        Standardize the columns of a data frame, i.e. turn them into z scores.
        '''
        z_data = data.copy()
        z_data = (z_data - z_data.mean(axis = 0))/z_data.std(axis = 0)
        return z_data
    
    def log_standardize(data):
        '''
        Log-transform the columns of a data frame, then standardize them.
        '''
        log_data = np.log(data.copy())
        z_log_data = standardize(log_data)
        return z_log_data

    zvars = ['extern', 'rbreak', 'agrs', 'intern', 'somatic', 'anx_dep', 'with_dep', 'extern_youth', 'intern_youth', 'atten_youth', 'pos_urg', 'neg_urg', 'alc_pos_expect', 'alc_neg_expect'] # outcome variables for normal analysis (as opposed to Bernoulli analysis)
    bvars_2year = ['any_substance_use', 'conduct_disorder', 'curiosity_tobacco', 'curiosity_alcohol', 'curiosity_marijuana', 'intent_tobacco', 'intent_alcohol', 'intent_marijuana'] # binary variables (for Bernoulli analysis), two-year follow up
    bvars_later = ['any_substance_use', 'conduct_disorder', 'curiosity_alcohol', 'curiosity_marijuana', 'curiosity_cigarette', 'curiosity_vaping', 'intent_alcohol', 'intent_marijuana', 'intent_cigarette', 'intent_vaping'] # binary variables (for Bernoulli analysis), after two-year follow up

    output_path = '/Users/sampaskewitz/Documents/ABCD data processing/properly (separately) z-scored training and test set data/'

    # export data that is split into training and test sets (z-scored, i.e. standardized)
    standardize(task_behav_data.loc[training_index]).to_csv(output_path + 'task behavioral data (2-year z-scored training set).csv')
    standardize(task_behav_data.loc[test_index]).to_csv(output_path + 'task behavioral data (2-year z-scored test set).csv')
    standardize(outcome_2year_data.loc[training_index, zvars]).to_csv(output_path + 'outcome data (2-year z-scored training set).csv')
    standardize(outcome_2year_data.loc[test_index, zvars]).to_csv(output_path + 'outcome data (2-year z-scored test set).csv')
    standardize(outcome_3year_data.loc[training_index, zvars]).to_csv(output_path + 'outcome data (3-year z-scored training set).csv')
    standardize(outcome_3year_data.loc[test_index, zvars]).to_csv(output_path + 'outcome data (3-year z-scored test set).csv')
    
    # export outcome data that is split into training and test sets (log-transformed, then z-scored)
    log_standardize(outcome_2year_data.loc[training_index, zvars]).to_csv(output_path + 'outcome data (2-year z-scored log training set).csv')
    log_standardize(outcome_2year_data.loc[test_index, zvars]).to_csv(output_path + 'outcome data (2-year z-scored log test set).csv')
    log_standardize(outcome_3year_data.loc[training_index, zvars]).to_csv(output_path + 'outcome data (3-year z-scored log training set).csv')
    log_standardize(outcome_3year_data.loc[test_index, zvars]).to_csv(output_path + 'outcome data (3-year z-scored log test set).csv')

    # export data that is split into training and test sets (binary data for Bernoulli analysis)
    outcome_2year_data.loc[training_index, bvars_2year].to_csv(output_path + 'outcome data (2-year binary vars training set).csv')
    outcome_2year_data.loc[test_index, bvars_2year].to_csv(output_path + 'outcome data (2-year binary vars test set).csv')
    outcome_3year_data.loc[training_index, bvars_later].to_csv(output_path + 'outcome data (3-year binary vars training set).csv')
    outcome_3year_data.loc[test_index, bvars_later].to_csv(output_path + 'outcome data (3-year binary vars test set).csv')
    
    # export data that is not split into training and test sets (z-scored, i.e. standardized)
    output_path = '/Users/sampaskewitz/Documents/ABCD data processing/complete data sets (z-scored etc)/'
    standardize(task_behav_data).to_csv(output_path + 'task behavioral data (2-year z-scored full data).csv')
    standardize(outcome_2year_data.loc[:, zvars]).to_csv(output_path + 'outcome data (2-year z-scored full data).csv')
    standardize(outcome_3year_data.loc[:, zvars]).to_csv(output_path + 'outcome data (3-year z-scored full data).csv')
    outcome_2year_data[bvars_2year].to_csv(output_path + 'outcome data (2-year binary vars full data).csv')
    outcome_3year_data[bvars_later].to_csv(output_path + 'outcome data (3-year binary vars full data).csv')
    