import math
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
from scipy import stats
import pandas as pd

def correlation_matrix_spearman(df):
    # Drop non-numeric columns like 'Disease' and 'sex' if present
    columns_to_drop = [col for col in ['Disease', 'sex'] if col in df.columns]
    numeric_df = df.drop(columns=columns_to_drop)

    # Keep only numeric columns
    numeric_df = numeric_df.select_dtypes(include=['number'])

    # Compute Spearman correlation
    corr_matrix = numeric_df.corr(method='spearman')
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

    if confusion_matrix.shape[0] < 2 or confusion_matrix.shape[1] < 2:
        # Cramér's V no está definido para variables constantes
        return np.nan

    chi2 = stats.chi2_contingency(confusion_matrix, correction=False)[0]
    n = confusion_matrix.sum().sum()
    phi2 = chi2 / n
    r, k = confusion_matrix.shape

    # Corrección por sesgo (Bergsma 2013)
    phi2corr = max(0, phi2 - ((k - 1)*(r - 1)) / (n - 1))
    rcorr = max(1, r - ((r - 1)**2) / (n - 1))
    kcorr = max(1, k - ((k - 1)**2) / (n - 1))

    denom = min((kcorr - 1), (rcorr - 1))
    if denom <= 0:
        return np.nan

    return np.sqrt(phi2corr / denom)

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
    
def visualize_correlation_matrix(matrix, title="Correlation Matrix", center=None):
    """
    Generic heatmap visualization for any correlation or association matrix.

    Parameters:
    - matrix: pandas DataFrame representing the matrix (square).
    - title: title of the plot.
    - center: optional value to center the colormap (e.g., 0 for Pearson).
    """
    plt.figure(figsize=(12, 10))
    sns.heatmap(matrix, cmap='coolwarm', center=center, annot=False)
    plt.title(title)
    plt.tight_layout()
    plt.show()

def visualize_correlation_matrix(matrix, title="Correlation Matrix", center=None):
    """
    Generic heatmap visualization for any correlation or association matrix.

    Parameters:
    - matrix: pandas DataFrame representing the matrix (square).
    - title: title of the plot.
    - center: optional value to center the colormap (e.g., 0 for Pearson).
    """
    plt.figure(figsize=(12, 10))
    sns.heatmap(matrix, cmap='coolwarm', center=center, annot=False)
    plt.title(title)
    plt.tight_layout()
    plt.show()
    
def create_histograms(df, ncols=3):
    """
    Create histograms with KDE for all numeric columns in a DataFrame.

    Parameters:
    - df: DataFrame containing numeric values (and possibly 'eid')
    - ncols: number of columns per row in the subplot grid
    """
    # Drop 'eid' safely if it exists
    numeric_columns = df.drop(columns='eid', errors='ignore')
    num_plots = len(numeric_columns.columns)

    nrows = math.ceil(num_plots / ncols)
    fig, axes = plt.subplots(nrows=nrows, ncols=ncols, figsize=(5 * ncols, 3 * nrows))
    axes = axes.flatten()

    for i, col in enumerate(numeric_columns.columns):
        sns.histplot(numeric_columns[col], kde=True, ax=axes[i], bins=30)
        axes[i].set_title(f'{col} Distribution')
        axes[i].set_xlabel('')
        axes[i].set_ylabel('')

    # Turn off extra axes
    for j in range(i + 1, len(axes)):
        axes[j].axis('off')

    plt.tight_layout()
    plt.show()

def create_boxplots(df, ncols=3):
    """
    Create horizontal boxplots for all numeric columns in a DataFrame.

    Parameters:
    - df: DataFrame containing numeric values (optionally includes 'eid')
    - ncols: number of columns per row in the subplot grid
    """
    cols = df.drop(columns='eid', errors='ignore')
    numeric_columns = cols.select_dtypes(include=[np.number])
    n = len(numeric_columns.columns)

    if n == 0:
        print("No numeric columns to plot.")
        return

    nrows = math.ceil(n / ncols)
    fig, axes = plt.subplots(nrows=nrows, ncols=ncols, figsize=(5 * ncols, 3 * nrows))
    axes = axes.flatten()

    for i, col in enumerate(numeric_columns.columns):
        sns.boxplot(x=numeric_columns[col].dropna(), ax=axes[i])
        axes[i].set_title(f'{col} Boxplot')
        axes[i].set_xlabel('')
        axes[i].set_ylabel('')

    # Desactivar ejes vacíos
    for j in range(i + 1, len(axes)):
        axes[j].axis('off')

    plt.tight_layout()
    plt.show()
