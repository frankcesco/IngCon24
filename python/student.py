import numpy as np
from scipy import stats
import pandas as pd

# Carica i dati dal file CSV
data_path = '../data/model_accuracies.csv'
model_data = pd.read_csv(data_path)

# Crea un dizionario dai dati
models = {row['model']: {'mean_accuracy': row['mean_accuracy'], 'std_accuracy': row['std_accuracy'], 'n': row['n']} for index, row in model_data.iterrows()}

def compute_t_test(model1, model2):
    """Esegui il test t di Student tra due modelli."""
    mean1, std1, n1 = models[model1]['mean_accuracy'], models[model1]['std_accuracy'], models[model1]['n']
    mean2, std2, n2 = models[model2]['mean_accuracy'], models[model2]['std_accuracy'], models[model2]['n']

    # Calcolo della statistica t e del p-value
    pooled_std = np.sqrt(((std1 ** 2 / n1) + (std2 ** 2 / n2)) / (1 / (n1 + n2 - 2)))
    t_statistic = (mean1 - mean2) / pooled_std
    df = n1 + n2 - 2
    p_value = 2 * (1 - stats.norm.cdf(np.abs(t_statistic)))

    return t_statistic, p_value


def main():
    results = []

    # Calcolare il test t per ogni coppia di modelli
    models_list = list(models.keys())
    best_model = max(models_list, key=lambda m: models[m]['mean_accuracy'])
    best_accuracy = models[best_model]['mean_accuracy']

    for i in range(len(models_list)):
        for j in range(i + 1, len(models_list)):
            model1, model2 = models_list[i], models_list[j]
            t_statistic, p_value = compute_t_test(model1, model2)
            results.append((model1, model2, t_statistic, p_value))

    # Stampa i risultati
    print("Test t di Student tra le accuratezze medie dei modelli:")
    for model1, model2, t_statistic, p_value in results:
        print(f"Confronto tra {model1} e {model2}:")
        print(f"  Statistica t: {t_statistic:.4f}")
        print(f"  p-value: {p_value:.4f}")
        if p_value < 0.05:
            print(f"  Risultato significativo: le medie sono significativamente diverse.")
        else:
            print(f"  Risultato non significativo: le medie non sono significativamente diverse.")
        print()

    # Stampa il miglior modello
    print(f"Il miglior modello basato sull'accuratezza media e': {best_model} con un'accuratezza media di {best_accuracy:.4f}")


if __name__ == "__main__":
    main()