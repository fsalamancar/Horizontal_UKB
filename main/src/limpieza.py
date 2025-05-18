 # funciones para quitar NAs, imputaciones y outliers

# Eliminar Outliers columna por columna, usando IQR

def eliminar_outliers_iqr(df):
    """Elimina outliers de un DataFrame usando el método IQR (Rango Intercuartílico).
    Args:
        df (pd.DataFrame): DataFrame del cual se eliminarán los outliers.
    Returns:
        pd.DataFrame: DataFrame sin outliers.
    """
    from scipy import stats
    import numpy as np
    import pandas as pd
    
    df_clean = df.copy()
    
    # Seleccionar solo columnas numéricas
    numeric_cols = df_clean.select_dtypes(include=[np.number]).columns
    
    for col in numeric_cols:
        Q1 = df_clean[col].quantile(0.25)
        Q3 = df_clean[col].quantile(0.75)
        IQR = Q3 - Q1
        
        lower_bound = Q1 - 1.5 * IQR
        upper_bound = Q3 + 1.5 * IQR
        
        # Reemplazar outliers por NaN (o puedes eliminar filas, pero cuidado con perder datos)
        df_clean[col] = df_clean[col].where(
            (df_clean[col] >= lower_bound) & (df_clean[col] <= upper_bound),
            np.nan
        )
    return df_clean


def eliminar_nas_col(df,threshold):
    """Elimina columnas con un >80% de NaN de un DataFrame.
    Args:
        df (pd.DataFrame): DataFrame del cual se eliminarán las columnas con NaN.
        threshold (float): Porcentaje de NaN permitido en las columnas. 
                           Si el porcentaje de NaN es mayor a este valor, la columna se eliminará.
    Returns:
        pd.DataFrame: DataFrame sin columnas con NaN.
    """
    import pandas as pd
    # Asegurarse de que el threshold esté entre 0 y 1
    if not (0 <= threshold <= 1):
        raise ValueError("El threshold debe estar entre 0 y 1.")
    
    # Calcular porcentaje de NAs por columna
    percent_nas = df.isna().mean()

    # Columnas a eliminar
    cols_to_drop = percent_nas[percent_nas > threshold].index

    print(f"Columnas a eliminar por tener más del {threshold*100}% de NAs:")
    print(cols_to_drop)

    # Eliminar columnas
    df = df.drop(columns=cols_to_drop)

    return df
    