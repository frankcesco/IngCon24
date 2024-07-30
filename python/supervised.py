import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import train_test_split, GridSearchCV
from sklearn.metrics import accuracy_score, classification_report, roc_curve, auc
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.svm import SVC
from sklearn.neighbors import KNeighborsClassifier
from sklearn.linear_model import LogisticRegression

# Crea la cartella per salvare le curve ROC, se non esiste
plots_directory = '../plots/'
if not os.path.exists(plots_directory):
    os.makedirs(plots_directory)

# Carica il dataset ridotto
reduced_output_path = '../data/merged_data_reduced.csv'
df_reduced = pd.read_csv(reduced_output_path)

# Seleziona le feature indicate da Boruta salvate in ../data/selected_features.csv
selected_features_path = '../data/selected_features.csv'
selected_features = pd.read_csv(selected_features_path, header=0).values.flatten()
print(f"Selected Features: {selected_features}")
print(f"Total Selected Features: {len(selected_features)}\n")

# Convertire le feature categoriche in numeriche utilizzando get_dummies per one-hot encoding
df_reduced = pd.get_dummies(df_reduced)

# Separare le feature (X) e il target (y)
X = df_reduced[selected_features]
y = df_reduced['Lesioni']

# Dividere il dataset in training e test set
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Modelli da confrontare
models = {
    'RandomForest': RandomForestClassifier(n_jobs=-1, class_weight='balanced', random_state=42),
    'SVC': SVC(probability=True),
    'KNN': KNeighborsClassifier(),
    'LogisticRegression': LogisticRegression(max_iter=1000)
}

# Griglia degli iperparametri per ogni modello
param_grid = {
    'RandomForest': {
        'randomforest__n_estimators': [100, 200, 500],
        'randomforest__max_features': ['sqrt', 'log2']
    },
    'SVC': {
        'svc__C': [0.1, 1, 10],
        'svc__kernel': ['linear', 'rbf']
    },
    'KNN': {
        'knn__n_neighbors': [3, 5, 7]
    },
    'LogisticRegression': {
        'logisticregression__C': [0.1, 1, 10]
    }
}

# Esegui la GridSearchCV per ogni modello per selezionare i migliori iperparametri
for model_name in models:
    print(f"Training {model_name}...")
    pipe = Pipeline(steps=[('scaler', StandardScaler()), (model_name.lower(), models[model_name])])
    search = GridSearchCV(pipe, param_grid[model_name], cv=5, scoring='accuracy') # Si può mettere repeated_kfold al posto di cv=5
    search.fit(X_train, y_train)
    print(f"Best parameters for {model_name}: {search.best_params_}")
    print(f"Best cross-validation accuracy for {model_name}: {search.best_score_}")
    test_accuracy = search.score(X_test, y_test)
    print(f"Test set accuracy for {model_name}: {test_accuracy}")

    # Calcola la curva ROC e l'AUC
    y_pred_proba = search.predict_proba(X_test)[:, 1] if hasattr(search, "predict_proba") else search.decision_function(
        X_test)
    fpr, tpr, _ = roc_curve(y_test, y_pred_proba)
    roc_auc = auc(fpr, tpr)

    # Stampa il valore ROC AUC
    print(f"ROC AUC for {model_name}: {roc_auc} \n")

    # Plot ROC curve
    plt.figure()
    plt.plot(fpr, tpr, color='darkorange', lw=2, label=f'ROC curve (area = {roc_auc:.2f})')
    plt.plot([0, 1], [0, 1], color='navy', lw=2, linestyle='--')
    plt.xlim([0.0, 1.0])
    plt.ylim([0.0, 1.05])
    plt.xlabel('False Positive Rate')
    plt.ylabel('True Positive Rate')
    plt.title(f'Receiver Operating Characteristic - {model_name}')
    plt.legend(loc="lower right")

    # Salva la figura
    plot_path = os.path.join(plots_directory, f'ROC_{model_name}.png')
    plt.savefig(plot_path)
    plt.close()
