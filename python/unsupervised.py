import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler
from sklearn.cluster import KMeans
from sklearn.metrics import silhouette_score

# Carica il dataset
data_path = '../data/merged_data_reduced.csv'
df = pd.read_csv(data_path)

# Seleziona le feature desiderate (escludi la variabile target 'Lesioni' se esiste)
features = df.drop(columns=['Lesioni']) if 'Lesioni' in df.columns else df

# Converti le feature categoriali in variabili dummy
features = pd.get_dummies(features)

# Standardizza le feature
scaler = StandardScaler()
scaled_features = scaler.fit_transform(features)

# Applica PCA con un numero maggiore di componenti per catturare l'80% di varianza
pca = PCA(n_components=0.8)  # 0.8 indica che vogliamo spiegare almeno l'80% della varianza
principal_components = pca.fit_transform(scaled_features)
explained_variance = pca.explained_variance_ratio_

# Verifica la varianza spiegata
cumulative_variance = np.cumsum(explained_variance)
print(f"Cumulative Variance: {cumulative_variance}")

# Plot delle componenti principali
plt.figure(figsize=(8, 6))
plt.scatter(principal_components[:, 0], principal_components[:, 1], s=50)
plt.xlabel('Principal Component 1')
plt.ylabel('Principal Component 2')
plt.title('PCA - Principal Components')
plt.grid(True)
plt.show()

# Se la varianza cumulativa è superiore all'80%, esegui clustering
if cumulative_variance[-1] >= 0.8:
    # Applica KMeans clustering
    kmeans = KMeans(n_clusters=3, random_state=42)  # Scegli il numero di cluster, es. 3
    clusters = kmeans.fit_predict(principal_components)

    # Calcola il silhouette score
    silhouette_avg = silhouette_score(principal_components, clusters)
    print(f"Silhouette Score: {silhouette_avg}")

    # Plot dei cluster
    plt.figure(figsize=(8, 6))
    plt.scatter(principal_components[:, 0], principal_components[:, 1], c=clusters, cmap='viridis', s=50)
    plt.xlabel('Principal Component 1')
    plt.ylabel('Principal Component 2')
    plt.title(f'KMeans Clustering (Silhouette Score: {silhouette_avg:.2f})')
    plt.grid(True)
    plt.show()
else:
    print("La varianza cumulativa è ancora inferiore all'80%.")
