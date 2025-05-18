# funciones para realizar graficos, KDE, heatmaps, etc.


def graficar_distribucion_proteinas(proteinas , clase, path,  nombre):
    """
    Función para graficar la distribución de las proteínas transformadas según la clase de enfermedad.
    Args:
        proteinas: DataFrame que contiene los datos filtrados.
        clase (str): Variable por la cual se quiere agrupar, tiene que ser categorica.
        path (str): Ruta donde se guardará el gráfico.
        nombre (str): Nombre del gráfico.pdf
    """
    import os
    import math
    import matplotlib.pyplot as plt
    import seaborn as sns

    # Graficar la distribución de las proteínas transformadas segun clase 
    # Asegúrate de tener la columna 'Disease' en el DataFrame
    assert 'Disease' in proteinas.columns

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
    plt.show()


