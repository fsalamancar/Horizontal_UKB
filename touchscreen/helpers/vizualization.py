import math
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
from scipy import stats
import pandas as pd

def correlation_matrix_spearman(df):
    ordinal_df = df.drop(columns=['Disease'])
    corr_matrix = ordinal_df.corr(method='spearman')
    return corr_matrix

def visualizate_correlation_matrix_spearman(corr_matrix):
    # Visualizarla
    plt.figure(figsize=(12, 10))
    sns.heatmap(corr_matrix, annot=False, cmap='coolwarm', center=0)
    plt.title('Correlation matrix (Spearman) between ordinal variables')
    plt.show()

    return corr_matrix

def cramers_v(x, y):
    confusion_matrix = pd.crosstab(x, y)
    chi2 = stats.chi2_contingency(confusion_matrix)[0]
    n = confusion_matrix.sum().sum()
    phi2 = chi2 / n
    r, k = confusion_matrix.shape
    # Corrección por sesgo
    phi2corr = max(0, phi2 - ((k - 1)*(r - 1)) / (n - 1))
    rcorr = r - ((r - 1)**2) / (n - 1)
    kcorr = k - ((k - 1)**2) / (n - 1)
    return np.sqrt(phi2corr / min((kcorr - 1), (rcorr - 1)))

def correlation_matrix_crammer(df):
    excluded_cols = ['eid', 'Disease']
    nominal_df = df.drop(columns=excluded_cols)

    # ---------- 3. Calcular matriz de Cramér's V ----------
    columns = nominal_df.columns
    n = len(columns)
    cramers_matrix = pd.DataFrame(np.zeros((n, n)), columns=columns, index=columns)

    for col1 in columns:
        for col2 in columns:
            if col1 != col2:
                cramers_matrix.loc[col1, col2] = cramers_v(nominal_df[col1], nominal_df[col2])
            else:
                cramers_matrix.loc[col1, col2] = 1.0

        return cramers_matrix
    
def visualizate_correlation_matrix_crammer(cramers_matrix):
    # ---------- 4. Visualizar matriz de asociación ----------
    plt.figure(figsize=(12, 10))
    sns.heatmap(cramers_matrix, cmap='YlOrRd', center=0.5)
    plt.title("Matriz de Cramér's V (variables nominales)")
    plt.tight_layout()
    plt.show()

def create_histograms(df, ncols=3):
    numeric_columns = df.drop('eid')
    num_plots = len(numeric_columns)

    nrows = math.ceil(num_plots / ncols)

    fig, axes = plt.subplots(nrows=nrows, ncols=ncols, figsize=(15, nrows * 3))
    axes = axes.flatten()

    for i, col in enumerate(numeric_columns):
        sns.histplot(df[col], kde=True, ax=axes[i], bins=30)
        axes[i].set_title(f'{col} Distribution')
        axes[i].set_xlabel('')
        axes[i].set_ylabel('')

    # Si hay más subplots que columnas, apaga los sobrantes
    for j in range(i + 1, len(axes)):
        axes[j].axis('off')

    plt.tight_layout()
    plt.show()

def create_boxplots(df, ncols=3):
    cols = df.drop('eid')  # Quitamos la columna 'eid'
    n = len(cols)
    ncols = 3                              # 3 columnas
    nrows = math.ceil(n / ncols)          # Número de filas necesario

    # Crear una sola figura
    fig, axes = plt.subplots(nrows=nrows, ncols=ncols, figsize=(15, nrows * 3))
    axes = axes.flatten()  # Para recorrerlos fácilmente

    # Crear los boxplots
    for i, col in enumerate(cols):
        sns.boxplot(x=df[col].dropna(), ax=axes[i])
        axes[i].set_title(col)
        axes[i].set_xlabel('')
        axes[i].set_ylabel('')

    # Apagar los ejes sobrantes si hay menos gráficos que subplots
    for j in range(i + 1, len(axes)):
        axes[j].axis('off')

    plt.tight_layout()
    plt.show()