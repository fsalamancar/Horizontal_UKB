from sklearn.preprocessing import StandardScaler
from sklearn.preprocessing import MinMaxScaler
import pandas as pd
import numpy as np

def scale_numeric_columns(df, id_col='eid'):
    """
    Estandariza las columnas numéricas del DataFrame (media=0, std=1), excluyendo la columna id_col.

    Parámetros:
        df (pd.DataFrame): DataFrame con los datos.
        id_col (str): Columna identificadora (no se escala).

    Retorna:
        pd.DataFrame: DataFrame con columnas escaladas y columna id_col intacta.
    """
    df = df.copy()

    if id_col not in df.columns:
        raise ValueError(f"The column '{id_col}' is not in the DataFrame.")

    id_series = df[[id_col]].astype(str)
    numeric_df = df.drop(columns=[id_col]).select_dtypes(include=[np.number])

    scaler = StandardScaler()
    scaled_array = scaler.fit_transform(numeric_df)
    scaled_df = pd.DataFrame(scaled_array, columns=numeric_df.columns, index=numeric_df.index)

    return pd.concat([id_series, scaled_df], axis=1)

def normalize_numeric_columns(df, id_col='eid'):
    """
    Normaliza las columnas numéricas del DataFrame al rango [0, 1], excluyendo la columna id_col.

    Parámetros:
        df (pd.DataFrame): DataFrame con los datos.
        id_col (str): Columna identificadora (no se normaliza).

    Retorna:
        pd.DataFrame: DataFrame con columnas normalizadas y columna id_col intacta.
    """
    df = df.copy()

    if id_col not in df.columns:
        raise ValueError(f"The column '{id_col}' is not in the DataFrame.")

    id_series = df[[id_col]].astype(str)
    numeric_df = df.drop(columns=[id_col]).select_dtypes(include=[np.number])

    scaler = MinMaxScaler()
    normalized_array = scaler.fit_transform(numeric_df)
    normalized_df = pd.DataFrame(normalized_array, columns=numeric_df.columns, index=numeric_df.index)

    return pd.concat([id_series, normalized_df], axis=1)