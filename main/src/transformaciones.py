# funciones para modificar los datos, corregir skewness(sesgo), escalado, 

def normalizar_proteinas(df, clase):
    """Normalizar cada proteína dependiendo de su asimetria,
     
    1. Criterio para elegir transformación según su asimetria:
    Asimetria	    Transformación 
    < -1	        Log(x * -1 + c), si todos son negativos
    -1 a -0.5	    Raíz cuadrada inversa
    -0.5 a 0.5	    Ninguna (ya casi normal)
    0.5 a 1	        Raíz cuadrada (np.sqrt(x))
    > 1	            Logarítmica (np.log1p(x))
    
    Args:
        df (pd.DataFrame): DataFrame a normalizar.
        clase (str): Nombre de la columna categórica que se quiere usar como referencia para la normalización.
    Returns:
        pd.DataFrame: DataFrame normalizado.
    """
    
    import numpy as np
    from scipy.stats import skew
    
    for col in df.columns:
        if col == clase or not np.issubdtype(df[col].dtype, np.number):
            continue  # saltar columnas no numéricas o la clase
        
        x = df[col]
        s = skew(x.dropna())
        
        if s < -1:
            x_trans = np.log1p(-x + abs(x.min()) + 1)
        elif -1 <= s < -0.5:
            x_trans = 1 / np.sqrt(x - x.min() + 1)
        elif -0.5 <= s <= 0.5:
            x_trans = x  # no se transforma
        elif 0.5 < s <= 1:
            x_trans = np.sqrt(x - x.min() + 1)
        elif s > 1:
            x_trans = np.log1p(x - x.min() + 1)
        
        df[col] = x_trans

    return df