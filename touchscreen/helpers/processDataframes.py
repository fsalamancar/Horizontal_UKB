import pandas as pd
import numpy as np
import re

def na_elimination(dataframe , percentage=0.5):

    threshold = percentage * len(dataframe)
    dataframe = dataframe.dropna(thresh=threshold, axis=1)
    return dataframe

def impute_by_disease(df):
    df = df.copy()
    for col in df.columns:
        if col not in ['eid', 'Disease']:
            modas = df.groupby('Disease')[col].agg(lambda x: x.mode().iloc[0] if not x.mode().empty else np.nan)
            df[col] = df.apply(
                lambda row: modas[row['Disease']] if pd.isna(row[col]) else row[col], axis=1
            )
    return df

def extract_field_id(colname):
    match = re.match(r'f_(\d+)_\d+_\d+', colname)
    if match:
        return int(match.group(1))
    return None

def get_fieldid_to_name_map(chars_df):
    df = chars_df.dropna(subset=['FieldID', 'Field']).copy()
    df['FieldID'] = df['FieldID'].astype(int)
    df['Field'] = df['Field'].str.strip()
    return dict(zip(df['FieldID'], df['Field']))

def get_highly_correlated_columns_from_matrix(corr_matrix, chars_df, threshold=0.75, show=True):
    # Map FieldID to readable names
    fieldid_to_name = get_fieldid_to_name_map(chars_df)
    # Extract upper triangle to avoid duplicates
    upper = corr_matrix.where(np.triu(np.ones(corr_matrix.shape), k=1).astype(bool))

    # Extract strongly correlated pairs
    high_corr_pairs = (
        upper.stack()
        .reset_index()
        .rename(columns={0: 'correlation', 'level_0': 'Variable1', 'level_1': 'Variable2'})
    )

    high_corr_pairs = high_corr_pairs[high_corr_pairs['correlation'] > threshold]

    # Extract FieldIDs
    high_corr_pairs['FieldID1'] = high_corr_pairs['Variable1'].apply(extract_field_id)
    high_corr_pairs['FieldID2'] = high_corr_pairs['Variable2'].apply(extract_field_id)

    # Map to readable names (if available)
    high_corr_pairs['Name1'] = high_corr_pairs['FieldID1'].apply(lambda fid: fieldid_to_name.get(fid, f'f_{fid}' if fid else 'Unknown'))
    high_corr_pairs['Name2'] = high_corr_pairs['FieldID2'].apply(lambda fid: fieldid_to_name.get(fid, f'f_{fid}' if fid else 'Unknown'))

    # Order by correlation
    high_corr_pairs = high_corr_pairs.sort_values(by='correlation', ascending=False)

    if show:
        print(f"Highly correlated pairs (correlation > {threshold}):")
        display(high_corr_pairs[['Variable1', 'Variable2', 'Name1', 'Name2', 'correlation']])

    return high_corr_pairs[['Variable1', 'Variable2', 'Name1', 'Name2', 'correlation']]

def eliminate_fields_by_fieldID (irrelevant_ids, df) :
    pattern = re.compile(rf"^f_({'|'.join(irrelevant_ids)})_\d+_\d+$")
    columns_to_drop = [col for col in df.columns if pattern.match(col)]
    return df.drop(columns=columns_to_drop)