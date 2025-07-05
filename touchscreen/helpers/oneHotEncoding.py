import re
import numpy as np
import pandas as pd
from collections import defaultdict
from functools import reduce
from joblib import Parallel, delayed


# -------------------- Utilidades auxiliares --------------------

def extract_fieldid_mapping(df, pattern=r'f_(\d+)_\d+_\d+'):
    """
    Extract FieldID from UKB-style column names.

    Parameters:
    - df: DataFrame with UKB-style columns (e.g., 'f_12345_0_0').
    - pattern: Regex to extract FieldID.

    Returns:
    - Dict mapping column name to FieldID.
    """
    compiled = re.compile(pattern)
    return {
        col: int(compiled.search(col).group(1))
        for col in df.columns
        if col != 'eid' and compiled.search(col)
    }

def extract_fieldid_to_coding(touchscreen_chars_df):
    """
    Create a FieldID → Coding dictionary from the touchscreen characteristics table.
    """
    df = touchscreen_chars_df.dropna(subset=['Coding']).copy()
    df['FieldID'] = df['FieldID'].astype(int)
    df['Coding'] = df['Coding'].astype(int)
    return df.set_index('FieldID')['Coding'].to_dict()

def get_valid_codes(coding, touchscreen_cbkeys_df):
    """
    Return valid codes for a given coding from touchscreen_cbkeys_df.
    """
    return touchscreen_cbkeys_df[
        touchscreen_cbkeys_df['coding_name'] == coding
    ]['code'].unique()


# -------------------- Codificación one-hot simple --------------------

def process_column_simple(col, fieldid, coding, col_values, valid_codes):
    """
    One-hot encode a single-column (simple nominal) based on valid codes.
    """
    if coding is None:
        return []

    present_values = col_values.dropna().unique()
    used_codes = [code for code in valid_codes if code in present_values]

    result = []
    for code in used_codes:
        new_col_name = f'f_{fieldid}_{coding}_{code}'
        mask_valid = col_values.notna()
        encoded_col = np.where(mask_valid, (col_values == code).astype(int), pd.NA)
        result.append((new_col_name, encoded_col))

    return result

def onehot_encode_simple_nominal(df, touchscreen_chars_df, touchscreen_cbkeys_df):
    """
    One-hot encode UK Biobank simple nominal categorical fields.

    Parameters:
    - df: DataFrame with columns like 'f_<FieldID>_<Instance>_<Array>' and 'eid'.
    - touchscreen_chars_df: DataFrame mapping FieldID → Coding.
    - touchscreen_cbkeys_df: DataFrame with valid codes per Coding.

    Returns:
    - A new DataFrame with one-hot encoded columns.
    """
    eid = df['eid']
    encoded_data = {'eid': eid}

    column_fieldid_map = extract_fieldid_mapping(df)
    fieldid_to_coding = extract_fieldid_to_coding(touchscreen_chars_df)

    tasks = []
    for col, fieldid in column_fieldid_map.items():
        if col not in df.columns:
            continue
        coding = fieldid_to_coding.get(fieldid)
        valid_codes = get_valid_codes(coding, touchscreen_cbkeys_df)
        col_values = df[col]
        tasks.append((col, fieldid, coding, col_values, valid_codes))

    results = Parallel(n_jobs=-1)(
        delayed(process_column_simple)(col, fid, cod, vals, vcodes)
        for col, fid, cod, vals, vcodes in tasks
    )

    for col_results in results:
        for new_col_name, values in col_results:
            encoded_data[new_col_name] = values

    return pd.DataFrame(encoded_data)


# -------------------- Codificación one-hot múltiple --------------------

def onehot_encode_multiple_nominal(df, touchscreen_chars_df, touchscreen_cbkeys_df):
    """
    One-hot encode UK Biobank multiple-choice categorical fields.

    Parameters:
    - df: DataFrame with columns like 'f_<FieldID>_0_<Array>' and 'eid'.
    - touchscreen_chars_df: DataFrame mapping FieldID → Coding.
    - touchscreen_cbkeys_df: DataFrame with valid codes per Coding.

    Returns:
    - A new DataFrame with one-hot encoded columns for multiple-choice fields.
    """
    # Convert columns to efficient nullable integer type
    for col in df.columns:
        if col != 'eid':
            df.loc[:, col] = df[col].astype("Int32")

    # Extract FieldID mappings and coding dictionary
    column_fieldid_map = extract_fieldid_mapping(df, pattern=r'f_(\d+)_0_\d+')
    fieldid_to_coding = extract_fieldid_to_coding(touchscreen_chars_df)

    # Group columns by FieldID
    fieldid_to_columns = defaultdict(list)
    for col, fieldid in column_fieldid_map.items():
        fieldid_to_columns[fieldid].append(col)

    def process_field_multiple(fieldid, cols):
        coding = fieldid_to_coding.get(fieldid)
        if coding is None:
            return None

        valid_codes = get_valid_codes(coding, touchscreen_cbkeys_df)
        sub_df = df[cols].copy()
        sub_df['eid'] = df['eid']

        # Wide to long format
        long_df = sub_df.melt(id_vars='eid', value_name='code')
        long_df = long_df.dropna()
        long_df['code'] = long_df['code'].astype(int)
        long_df = long_df[long_df['code'].isin(valid_codes)]

        # One-hot encoding via pivot
        onehot = (
            long_df
            .assign(dummy=1)
            .pivot_table(index='eid', columns='code', values='dummy', fill_value=0)
            .astype("Int8")
        )

        onehot.columns = [f'f_{fieldid}_{coding}_{code}' for code in onehot.columns]
        return onehot

    # Parallel processing of each FieldID group
    results = Parallel(n_jobs=-1)(
        delayed(process_field_multiple)(fieldid, cols)
        for fieldid, cols in fieldid_to_columns.items()
    )

    valid_results = [df for df in results if df is not None]

    if valid_results:
        encoded_df = reduce(lambda left, right: left.join(right, how='outer'), valid_results)
        encoded_df = encoded_df.reset_index()
        cols = ['eid'] + [col for col in encoded_df.columns if col != 'eid']
        encoded_df = encoded_df[cols]
    else:
        encoded_df = df[['eid']].copy()

    return encoded_df
