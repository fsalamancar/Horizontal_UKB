from sklearn.preprocessing import StandardScaler
import pandas as pd
import numpy as np

def scale_numeric_columns(df, id_col='eid'):
    """
    Escala las columnas numéricas del DataFrame, excluyendo la columna id_col.

    Parámetros:
        df (pd.DataFrame): DataFrame a escalar.
        id_col (str): Nombre de la columna de identificación (no se escala).

    Retorna:
        pd.DataFrame: DataFrame con columnas numéricas escaladas y id_col intacta.
    """
    df = df.copy()

    if id_col not in df.columns:
        raise ValueError(f"La columna '{id_col}' no está en el DataFrame.")

    # Separar columna ID
    id_series = df[[id_col]].astype(str)

    # Seleccionar columnas numéricas
    numeric_df = df.drop(columns=[id_col]).select_dtypes(include=[np.number])

    # Escalar
    scaler = StandardScaler()
    scaled_array = scaler.fit_transform(numeric_df)
    scaled_df = pd.DataFrame(scaled_array, columns=numeric_df.columns, index=numeric_df.index)

    # Concatenar columna ID y columnas escaladas
    return pd.concat([id_series, scaled_df], axis=1)