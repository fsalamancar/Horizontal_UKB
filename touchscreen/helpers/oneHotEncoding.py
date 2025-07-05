import pandas as pd
import numpy as np
import re
from collections import defaultdict
from functools import reduce
from joblib import Parallel, delayed

def get_fieldid_to_coding_map(touchscreen_chars_df):
    df = touchscreen_chars_df.dropna(subset=['Coding']).copy()
    df['FieldID'] = df['FieldID'].astype(int)
    df['Coding'] = df['Coding'].astype(int)
    return df.set_index('FieldID')['Coding'].to_dict()

def get_valid_codes(coding, touchscreen_cbkeys_df):
    return touchscreen_cbkeys_df[
        (touchscreen_cbkeys_df['coding_name'] == coding)
    ]['code'].unique()

def extract_column_fieldid_map(df, pattern=r'f_(\d+)_\d+_\d+'):
    return {
        col: int(re.search(pattern, col).group(1))
        for col in df.columns
        if col != 'eid' and re.search(pattern, col)
    }

def extract_fieldid_to_columns_map(df, pattern=r'f_(\d+)_0_\d+'):
    fieldid_to_columns = defaultdict(list)
    for col in df.columns:
        if col != 'eid':
            match = re.search(pattern, col)
            if match:
                fieldid = int(match.group(1))
                fieldid_to_columns[fieldid].append(col)
    return fieldid_to_columns

def one_hot_encode_nominal_df(df, fieldid_to_coding, touchscreen_cbkeys_df, n_jobs=-1):
    eid = df['eid']
    encoded_data = {'eid': eid}
    column_fieldid_map = extract_column_fieldid_map(df)

    tasks = []
    for col, fieldid in column_fieldid_map.items():
        coding = fieldid_to_coding.get(fieldid)
        if coding is None:
            continue
        valid_codes = get_valid_codes(coding, touchscreen_cbkeys_df)
        col_values = df[col]
        tasks.append((col, fieldid, coding, col_values, valid_codes))

    def process_nominal_column(col, fieldid, coding, col_values, valid_codes):
        result = []
        present_values = col_values.dropna().unique()
        used_codes = [code for code in valid_codes if code in present_values]
        for code in used_codes:
            new_col = f'f_{fieldid}_{coding}_{code}'
            values = np.where(col_values.notna(), (col_values == code).astype(int), pd.NA)
            result.append((new_col, values))
        return result

    results = Parallel(n_jobs=n_jobs)(
        delayed(process_nominal_column)(col, fid, cod, vals, vcodes)
        for col, fid, cod, vals, vcodes in tasks
    )

    for col_results in results:
        for new_col, values in col_results:
            encoded_data[new_col] = values

    return pd.DataFrame(encoded_data)

def one_hot_encode_multiple_df(df, fieldid_to_coding, touchscreen_cbkeys_df, n_jobs=-1):
    for col in df.columns:
        if col != 'eid':
            df[col] = df[col].astype("Int32")

    fieldid_to_columns = extract_fieldid_to_columns_map(df)

    def process_multiple_field(fieldid, cols):
        coding = fieldid_to_coding.get(fieldid)
        if coding is None:
            return None

        valid_codes = get_valid_codes(coding, touchscreen_cbkeys_df)
        sub_df = df[cols + ['eid']].copy()

        long_df = sub_df.melt(id_vars='eid', value_name='code').dropna()
        long_df['code'] = long_df['code'].astype(int)
        long_df = long_df[long_df['code'].isin(valid_codes)]

        onehot = (
            long_df.assign(dummy=1)
            .pivot_table(index='eid', columns='code', values='dummy', fill_value=0)
            .astype("Int8")
        )

        onehot.columns = [f'f_{fieldid}_{coding}_{code}' for code in onehot.columns]
        return onehot

    results = Parallel(n_jobs=n_jobs)(
        delayed(process_multiple_field)(fid, cols)
        for fid, cols in fieldid_to_columns.items()
    )

    valid_results = [df for df in results if df is not None]
    if valid_results:
        encoded_df = reduce(lambda left, right: left.join(right, how='outer'), valid_results)
        encoded_df = encoded_df.reset_index()
        return encoded_df[['eid'] + [col for col in encoded_df.columns if col != 'eid']]
    else:
        return df[['eid']].copy()
