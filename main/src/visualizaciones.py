# funciones para realizar graficos, KDE, heatmaps, etc.


def graficar_distribucion_proteinas(proteinas , clase, path, nombre, mostrar):
    """
    Función para graficar la distribución de las proteínas transformadas según la clase de enfermedad.
    Args:
        proteinas: DataFrame que contiene los datos filtrados.
        clase (str): Variable por la cual se quiere agrupar, tiene que ser categorica.
        path (str): Ruta donde se guardará el gráfico.
        nombre (str): Nombre del gráfico.pdf
        mostrar (bool): Si True, muestra el gráfico. Si False, solo lo guarda.
    """
    import os
    import math
    import matplotlib.pyplot as plt
    import seaborn as sns

    # Graficar la distribución de las proteínas transformadas segun clase 
    # Asegúrate de tener la columna 'Disease' en el DataFrame
    assert clase in proteinas.columns

    # Filtrar solo columnas numéricas para graficar (excluyendo 'Disease')
    cols_a_graficar = proteinas.select_dtypes(include='number').columns.tolist()

    # Número total de proteínas (columnas numéricas)
    n_proteinas = len(cols_a_graficar)

    # Definir layout del grid
    n_cols = 10
    n_rows = math.ceil(n_proteinas / n_cols)

    # Crear figura y ejes
    fig, axs = plt.subplots(n_rows, n_cols, figsize=(n_cols * 4, n_rows * 3), squeeze=False)

    # Loop para graficar KDE de cada proteína por grupo de Disease
    for i, col in enumerate(cols_a_graficar):
        r = i // n_cols
        c = i % n_cols
        ax = axs[r, c]
        
        for grupo, datos in proteinas.groupby(clase):
            sns.kdeplot(datos[col].dropna(), ax=ax, label=str(grupo), fill=True, alpha=0.4)
        
        ax.set_title(col, fontsize=8)
        ax.set_xticks([])
        ax.set_yticks([])

    # Quitar subplots vacíos si hay
    for j in range(i + 1, n_rows * n_cols):
        fig.delaxes(axs.flatten()[j])

    # Añadir leyenda general en la figura
    handles, labels = axs[0][0].get_legend_handles_labels()
    fig.legend(handles, labels, loc='upper right', fontsize='small')

    plt.tight_layout(rect=[0, 0, 0.97, 1])  # Deja espacio para la leyenda

    # Guardar el gráfico en PDF
    plt.savefig(os.path.join(path, nombre), bbox_inches='tight')

    # Mostrar el gráfico
    if mostrar:
        plt.show()
    else:
        plt.close(fig)  # Cierra la figura si no se muestra


def graficar_boxplots_proteinas(df, clase_x, hue, path, nombre, mostrar):
    """
    Genera un grid de boxplots para todas las proteínas numéricas y lo guarda como un único PDF.

    Args:
        df (pd.DataFrame): DataFrame con las proteínas y columnas de clase y hue.
        clase_x (str): Columna categórica para el eje X (ej. 'Disease').
        hue (str): Columna para usar como hue (ej. 'Time_Category').
        path (str): Carpeta donde guardar el gráfico.
        nombre (str): Nombre del archivo PDF a guardar.
        mostrar (bool): Si True, muestra el gráfico. Si False, solo lo guarda.
    """
    assert clase_x in df.columns
    assert hue in df.columns
    import matplotlib.pyplot as plt
    import seaborn as sns
    import math
    import os

    # Crear la carpeta si no existe
    os.makedirs(path, exist_ok=True)

    # Seleccionar solo columnas numéricas (proteínas)
    columnas = df.select_dtypes(include='number').columns.tolist()
    n = len(columnas)

    n_cols = 5
    n_rows = math.ceil(n / n_cols)

    fig, axs = plt.subplots(n_rows, n_cols, figsize=(n_cols * 5, n_rows * 4), squeeze=False)

    for i, col in enumerate(columnas):
        r = i // n_cols
        c = i % n_cols
        ax = axs[r, c]

        sns.boxplot(data=df, x=clase_x, y=col, hue=hue, palette='Set2', ax=ax)
        ax.set_title(col, fontsize=10)
        ax.set_xlabel('')
        ax.set_ylabel('')
        ax.tick_params(axis='x', labelrotation=45)

        if i != 0:
            ax.get_legend().remove()

    # Eliminar subplots vacíos
    for j in range(n, n_rows * n_cols):
        fig.delaxes(axs[j // n_cols, j % n_cols])

    # Leyenda general
    handles, labels = axs[0][0].get_legend_handles_labels()
    fig.legend(handles, labels, loc='upper right', fontsize='small')

    plt.tight_layout(rect=[0, 0, 0.97, 1])
    
    # Guardar el gráfico en PDF
    plt.savefig(os.path.join(path, nombre), bbox_inches='tight')

    # Mostrar el gráfico
    if mostrar:
        plt.show()
    else:
        plt.close(fig)  # Cierra la figura si no se muestra



