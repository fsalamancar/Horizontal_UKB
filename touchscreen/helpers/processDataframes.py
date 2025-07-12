import pandas as pd
import numpy as np
import re
from functools import reduce
from sklearn.ensemble import IsolationForest
from sklearn.preprocessing import StandardScaler

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
    # Create FieldID mapping → readable name
    fieldid_to_name = get_fieldid_to_name_map(chars_df)

    # Extract upper triangle (no duplicates or diagonal)
    upper = corr_matrix.where(np.triu(np.ones(corr_matrix.shape), k=1).astype(bool))

    # Extract pairs with high correlation
    high_corr_pairs = (
        upper.stack()
        .reset_index()
        .rename(columns={0: 'correlation', 'level_0': 'Variable1', 'level_1': 'Variable2'})
    )

    high_corr_pairs = high_corr_pairs[high_corr_pairs['correlation'] > threshold]

    high_corr_pairs['FieldID1'] = high_corr_pairs['Variable1'].apply(extract_field_id)
    high_corr_pairs['FieldID2'] = high_corr_pairs['Variable2'].apply(extract_field_id)

    high_corr_pairs['Name1'] = high_corr_pairs['FieldID1'].apply(lambda fid: fieldid_to_name.get(fid, f'f_{fid}' if fid else 'Unknown'))
    high_corr_pairs['Name2'] = high_corr_pairs['FieldID2'].apply(lambda fid: fieldid_to_name.get(fid, f'f_{fid}' if fid else 'Unknown'))

    high_corr_pairs = high_corr_pairs.sort_values(by='correlation', ascending=False)

    if show:
        print(f"\nPairs found with correlation > {threshold}: {len(high_corr_pairs)}")
        if not high_corr_pairs.empty:
            display(high_corr_pairs[['Variable1', 'Variable2', 'FieldID1', 'FieldID2', 'Name1', 'Name2', 'correlation']])
        else:
            print("No pairs were found with correlation above the threshold.")


def eliminate_fields_by_fieldID (irrelevant_ids, df) :
    pattern = re.compile(rf"^f_({'|'.join(irrelevant_ids)})_\d+_\d+$")
    columns_to_drop = [col for col in df.columns if pattern.match(col)]
    return df.drop(columns=columns_to_drop)

def remove_outliers_and_align_datasets(
    numerical_df, *other_dfs, id_col="eid",
    contamination=0.05,  # usa float si tu sklearn < 1.4
    n_estimators=300,
    random_state=42
):
    # 1) EIDs comunes
    common_eids = set.intersection(*(set(df[id_col]) for df in (numerical_df, *other_dfs)))
    numerical_df = numerical_df[numerical_df[id_col].isin(common_eids)].copy()
    aligned_other_dfs = [df[df[id_col].isin(common_eids)].copy() for df in other_dfs]

    # 2) AJUSTE Isolation Forest (sin escalado, opcional)
    features = numerical_df.drop(columns=[id_col])
    iso = IsolationForest(
        contamination=contamination,
        n_estimators=n_estimators,
        n_jobs=-1,
        random_state=random_state
    )
    inlier_mask = iso.fit_predict(features) == 1  # booleano

    # 3) Filtrado
    numeric_df_clean = numerical_df.loc[inlier_mask].reset_index(drop=True)
    cleaned_other_dfs = [
        df.loc[df[id_col].isin(numeric_df_clean[id_col])].reset_index(drop=True)
        for df in aligned_other_dfs
    ]
    valid_eids = numeric_df_clean[id_col].copy()

    return (numeric_df_clean, *cleaned_other_dfs)

def rename_columns_with_field_names(df, touchscreen_chars_df, exclude_cols=['eid', 'Disease']):
    touchscreen_chars_df = touchscreen_chars_df.dropna(subset=['FieldID', 'Field']).copy()
    touchscreen_chars_df['FieldID'] = touchscreen_chars_df['FieldID'].astype(int)
    touchscreen_chars_df['Field'] = touchscreen_chars_df['Field'].str.strip()

    fieldid_to_name = dict(zip(touchscreen_chars_df['FieldID'], touchscreen_chars_df['Field']))

    rename_dict = {}
    for col in df.columns:
        if col in exclude_cols:
            continue

        match = re.match(r'f_(\d+)_\d+_\d+', col)
        if match:
            fieldid = int(match.group(1))
            new_name = fieldid_to_name.get(fieldid)
            if new_name:
                rename_dict[col] = new_name
            else:
                print(f"FieldID {fieldid} encontrado en columna '{col}' no está en touchscreen_chars_df")
        else:
            print(f"No coincide con patrón: {col}")

    return df.rename(columns=rename_dict)

def rename_onehot_columns(df, touchscreen_chars_df, touchscreen_cbkeys_df, exclude_cols=['eid']):
    """
    Renombra columnas one-hot codificadas con formato f_<FieldID>_<Coding>_<Code> 
    usando nombres legibles de preguntas y significados de códigos.

    Parámetros:
    - df: DataFrame con columnas one-hot.
    - touchscreen_chars_df: contiene 'FieldID', 'Field', y 'Coding'.
    - touchscreen_cbkeys_df: contiene 'coding_name', 'code', 'meaning'.
    - exclude_cols: columnas a dejar sin cambio, como 'eid'.

    Retorna:
    - Un nuevo DataFrame con las columnas renombradas.
    """

    # Diccionario FieldID → Field (nombre de la pregunta)
    fieldid_to_name = {
        int(row['FieldID']): row['Field']
        for _, row in touchscreen_chars_df.dropna(subset=['FieldID', 'Field']).iterrows()
    }

    # Diccionario (coding_name, code) → meaning
    coding_code_to_meaning = {
        (int(row['coding_name']), int(row['code'])): str(row['meaning'])
        for _, row in touchscreen_cbkeys_df.dropna(subset=['coding_name', 'code', 'meaning']).iterrows()
    }

    rename_dict = {}

    for col in df.columns:
        if col in exclude_cols:
            continue

        match = re.match(r'f_(\d+)_(\d+)_(-?\d+)', col)
        if not match:
            continue

        fieldid, coding, code = map(int, match.groups())
        question = fieldid_to_name.get(fieldid, f"Field_{fieldid}")
        meaning = coding_code_to_meaning.get((coding, code), f"Code_{code}")

        # Formato final: Pregunta: Respuesta
        new_col = f"{question}: {meaning}"
        rename_dict[col] = new_col

    return df.rename(columns=rename_dict)

def merge_dataframes_on_eid(dataframes, disease_source_df=None):
    """
    Une múltiples DataFrames por 'eid' usando outer join.
    Si disease_source_df se proporciona, añade la columna 'Disease' después del merge.
    """

    for i, df in enumerate(dataframes):
        if 'eid' not in df.columns:
            raise ValueError(f"DataFrame {i} no contiene la columna 'eid'.")
        df['eid'] = df['eid'].astype('int64')

    if disease_source_df is not None:
        disease_source_df['eid'] = disease_source_df['eid'].astype('int64')

    merged_df = reduce(
        lambda left, right: pd.merge(left, right, on='eid', how='outer'),
        dataframes
    )

    if disease_source_df is not None:
        merged_df = merged_df.merge(
            disease_source_df[['eid', 'Disease']],
            on='eid',
            how='left'
        )

    return merged_df