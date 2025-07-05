import pandas as pd
import os

def load_tsv(filename, path, **kwargs):
    """
    Load a TSV file with flexible read_csv parameters.
    """
    filepath = os.path.normpath(os.path.join(path, filename))
    print(f"Looking for file at: {filepath}")

    if not os.path.exists(filepath):
        raise FileNotFoundError(f"File not found: '{filepath}'")

    try:
        df = pd.read_csv(filepath, sep="\t", low_memory=False, encoding='utf-8', **kwargs)
        print(f"File loaded: {df.shape[0]} rows × {df.shape[1]} columns")
        return df
    except pd.errors.ParserError as e:
        print("Parser error: Could not parse the TSV file.")
        raise e
    except Exception as e:
        print("Unexpected error while loading the file.")
        raise e

def load_csv(filename, path, **kwargs):
    """
    Load a TSV file with flexible read_csv parameters.
    """
    filepath = os.path.normpath(os.path.join(path, filename))
    print(f"Looking for file at: {filepath}")

    if not os.path.exists(filepath):
        raise FileNotFoundError(f"File not found: '{filepath}'")

    try:
        df = pd.read_csv(filepath, low_memory=False, encoding='utf-8', **kwargs)
        print(f"File loaded: {df.shape[0]} rows × {df.shape[1]} columns")
        return df
    except pd.errors.ParserError as e:
        print("Parser error: Could not parse the TSV file.")
        raise e
    except Exception as e:
        print("Unexpected error while loading the file.")
        raise e
