# CONCEPTUAL Q: 
# Network representation choices shape results. For example, treating a directed network as undirected can misrepresent influence in centrality rankings, while ignoring edge weights can obscure tie strength and lead to weaker or less accurate community detection.
# 
import os
from datetime import date

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import networkx as nx
import scipy.sparse as sp

from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score, adjusted_rand_score, confusion_matrix

import torch
import torch.nn as nn
import torch.nn.functional as F

# -----------------------------------------------------------------------------
# Setup
# -----------------------------------------------------------------------------
np.random.seed(123)
torch.manual_seed(123)

# Save everything to a local outputs folder next to this script
OUTPUT_DIR = "network_data_outputs"
import os

OUTPUT_DIR = os.path.expanduser("~/Desktop/network_data_outputs")
os.makedirs(OUTPUT_DIR, exist_ok=True)
# -----------------------------------------------------------------------------
# Part 1: Generate a Synthetic Network (Stochastic Block Model)
# -----------------------------------------------------------------------------
block_sizes = [400, 350, 250]
num_blocks = len(block_sizes)
num_nodes = sum(block_sizes)

P = np.array([
    [0.06, 0.01, 0.005],
    [0.01, 0.05, 0.008],
    [0.005, 0.008, 0.04]
])

G = nx.stochastic_block_model(block_sizes, P, seed=123)
num_edges = G.number_of_edges()
density = nx.density(G)

print("Synthetic graph summary:")
print("  Nodes:", num_nodes)
print("  Edges:", num_edges)
print("  Density:", round(density, 6))

true_labels = np.concatenate([
    np.zeros(block_sizes[0], dtype=int),
    np.ones(block_sizes[1], dtype=int),
    2 * np.ones(block_sizes[2], dtype=int)
])

# -----------------------------------------------------------------------------
# Part 2: Synthetic Node Features
# -----------------------------------------------------------------------------
num_features = 8

centers = np.array([
    [2, 0, 0, 1, 0, 0, 1, 0],
    [0, 2, 0, 0, 1, 0, 0, 1],
    [0, 0, 2, 0, 0, 1, 1, 0]
], dtype=float)

noise_sd = 0.8

X = np.zeros((num_nodes, num_features), dtype=float)
X[true_labels == 0] = centers[0] + np.random.normal(0, noise_sd, size=(np.sum(true_labels == 0), num_features))
X[true_labels == 1] = centers[1] + np.random.normal(0, noise_sd, size=(np.sum(true_labels == 1), num_features))
X[true_labels == 2] = centers[2] + np.random.normal(0, noise_sd, size=(np.sum(true_labels == 2), num_features))

# -----------------------------------------------------------------------------
# Part 3: Graph Construction + Centrality
# -----------------------------------------------------------------------------
edge_list = pd.DataFrame(list(G.edges()), columns=["u", "v"])
edge_list.to_csv(os.path.join(OUTPUT_DIR, "edge_list.csv"), index=False)

print("\nEdge list (first 10 rows):")
print(edge_list.head(10))

G2 = nx.from_pandas_edgelist(edge_list, source="u", target="v", create_using=nx.Graph())

print("\nReconstructed graph summary:")
print("  Nodes:", G2.number_of_nodes())
print("  Edges:", G2.number_of_edges())

A = nx.to_scipy_sparse_array(G2, format="csr", dtype=np.float32)

print("\nAdjacency matrix:")
print("  Shape:", A.shape)
print("  Nonzeros:", A.nnz)

# Centrality measures
deg = np.array([d for _, d in G2.degree()])
deg_cent = nx.degree_centrality(G2)

betw = nx.betweenness_centrality(G2, k=200, seed=123)
betw_values = np.array(list(betw.values()))

eig = nx.eigenvector_centrality_numpy(G2)
eig_values = np.array(list(eig.values()))

# Top 10 nodes for each measure
top10_degree = pd.DataFrame(sorted(deg_cent.items(), key=lambda x: x[1], reverse=True)[:10],
                            columns=["node", "degree_centrality"])
top10_betw = pd.DataFrame(sorted(betw.items(), key=lambda x: x[1], reverse=True)[:10],
                          columns=["node", "betweenness_centrality"])
top10_eig = pd.DataFrame(sorted(eig.items(), key=lambda x: x[1], reverse=True)[:10],
                         columns=["node", "eigenvector_centrality"])

print("\nTop 10 nodes by degree centrality:")
print(top10_degree)
print("\nTop 10 nodes by approximate betweenness centrality:")
print(top10_betw)
print("\nTop 10 nodes by eigenvector centrality:")
print(top10_eig)

top10_degree.to_csv(os.path.join(OUTPUT_DIR, "top10_degree_centrality.csv"), index=False)
top10_betw.to_csv(os.path.join(OUTPUT_DIR, "top10_betweenness_centrality.csv"), index=False)
top10_eig.to_csv(os.path.join(OUTPUT_DIR, "top10_eigenvector_centrality.csv"), index=False)

# One figure with three histograms
fig, axes = plt.subplots(1, 3, figsize=(15, 4))
axes[0].hist(list(deg_cent.values()), bins=40)
axes[0].set_title("Degree centrality")
axes[0].set_xlabel("Value")
axes[0].set_ylabel("Count")

axes[1].hist(betw_values, bins=40)
axes[1].set_title("Approx. betweenness")
axes[1].set_xlabel("Value")
axes[1].set_ylabel("Count")

axes[2].hist(eig_values, bins=40)
axes[2].set_title("Eigenvector centrality")
axes[2].set_xlabel("Value")
axes[2].set_ylabel("Count")

plt.tight_layout()
plt.savefig(os.path.join(OUTPUT_DIR, "centrality_histograms.png"), dpi=300, bbox_inches="tight")
plt.close()

centrality_discussion = (
    "The rankings overlap somewhat, but they do not fully agree because each measure captures a different kind of importance. "
    "Degree centrality highlights nodes with many direct ties, so it reflects local connectivity. "
    "Approximate betweenness centrality instead emphasizes brokerage, meaning nodes that sit on many shortest paths between others. "
    "Eigenvector centrality rewards nodes that are connected to other well-connected nodes, so it reflects embeddedness in influential parts of the network. "
    "In this synthetic SBM, some nodes rank highly on multiple measures because they are both well connected and well positioned inside dense blocks. "
    "However, a node can have many ties without serving as a bridge, and a bridge node can matter structurally even if its degree is not the highest."
)

# -----------------------------------------------------------------------------
# Part 4: Community Detection + Evaluation
# -----------------------------------------------------------------------------
louvain_comms = nx.algorithms.community.louvain_communities(G2, seed=123)
louvain_labels = np.zeros(num_nodes, dtype=int)

for comm_id, comm in enumerate(louvain_comms):
    for node in comm:
        louvain_labels[node] = comm_id

louvain_sizes = [len(c) for c in louvain_comms]
community_sizes_df = pd.DataFrame({
    "community_id": list(range(len(louvain_sizes))),
    "size": louvain_sizes
}).sort_values("community_id")

ari = adjusted_rand_score(true_labels, louvain_labels)

print("\nCommunity detection summary:")
print("  Number of Louvain communities:", len(louvain_comms))
print("\nCommunity sizes:")
print(community_sizes_df)
print("\nAdjusted Rand Index (Louvain vs truth):", ari)

community_sizes_df.to_csv(os.path.join(OUTPUT_DIR, "community_sizes.csv"), index=False)

plt.figure(figsize=(8, 5))
plt.bar(community_sizes_df["community_id"].astype(str), community_sizes_df["size"])
plt.title("Louvain community sizes")
plt.xlabel("Community ID")
plt.ylabel("Number of nodes")
plt.tight_layout()
plt.savefig(os.path.join(OUTPUT_DIR, "community_sizes_barplot.png"), dpi=300, bbox_inches="tight")
plt.close()

community_discussion = (
    f"The ARI is {ari:.3f}, which indicates how closely the Louvain partition matches the known SBM block labels after correcting for chance agreement. "
    "Values closer to 1 indicate stronger agreement, whereas values near 0 would suggest random-like correspondence. "
    "In this case, the result suggests that Louvain recovers much of the planted community structure, though not necessarily perfectly. "
    "That is reasonable because the SBM was generated with stronger within-block ties than between-block ties. "
    "Even so, community detection can still split a true block when a block contains internally uneven regions or a few sparse bridges. "
    "It can also merge true blocks when between-block ties are strong enough to make two groups look like one larger community under the modularity objective."
)

# -----------------------------------------------------------------------------
# Part 5: Node Classification — Logistic Regression Baseline
# -----------------------------------------------------------------------------
perm = np.random.permutation(num_nodes)
train_size = int(0.60 * num_nodes)
val_size = int(0.20 * num_nodes)

train_idx = perm[:train_size]
val_idx = perm[train_size:train_size + val_size]
test_idx = perm[train_size + val_size:]

y = true_labels.copy()

lr = LogisticRegression(max_iter=200)
lr.fit(X[train_idx], y[train_idx])

yhat_val_lr = lr.predict(X[val_idx])
yhat_test_lr = lr.predict(X[test_idx])

val_acc_lr = accuracy_score(y[val_idx], yhat_val_lr)
test_acc_lr = accuracy_score(y[test_idx], yhat_test_lr)

print("\nBaseline (features-only) logistic regression:")
print("  Validation accuracy:", val_acc_lr)
print("  Test accuracy:", test_acc_lr)

# -----------------------------------------------------------------------------
# Part 6: 2-layer GCN-style model
# -----------------------------------------------------------------------------
I = sp.eye(num_nodes, format="csr", dtype=np.float32)
A_tilde = A + I

deg_tilde = np.array(A_tilde.sum(axis=1)).flatten()
deg_inv_sqrt = 1.0 / np.sqrt(deg_tilde)
D_inv_sqrt = sp.diags(deg_inv_sqrt.astype(np.float32), format="csr")
A_norm = D_inv_sqrt @ A_tilde @ D_inv_sqrt

X_t = torch.tensor(X, dtype=torch.float32)
y_t = torch.tensor(y, dtype=torch.long)

A_norm_coo = A_norm.tocoo()
A_indices = torch.tensor(np.vstack((A_norm_coo.row, A_norm_coo.col)), dtype=torch.long)
A_values = torch.tensor(A_norm_coo.data, dtype=torch.float32)
A_norm_t = torch.sparse_coo_tensor(A_indices, A_values, size=(num_nodes, num_nodes)).coalesce()

train_idx_t = torch.tensor(train_idx, dtype=torch.long)
val_idx_t = torch.tensor(val_idx, dtype=torch.long)
test_idx_t = torch.tensor(test_idx, dtype=torch.long)

hidden_dim = 16
num_classes = num_blocks

lin1 = nn.Linear(num_features, hidden_dim)
lin2 = nn.Linear(hidden_dim, num_classes)
optimizer = torch.optim.Adam(list(lin1.parameters()) + list(lin2.parameters()), lr=0.01, weight_decay=5e-4)

epochs = 30
training_log = []

for epoch in range(1, epochs + 1):
    H0 = lin1(X_t)
    H1 = torch.sparse.mm(A_norm_t, H0)
    H1 = torch.relu(H1)

    Z0 = lin2(H1)
    logits = torch.sparse.mm(A_norm_t, Z0)

    loss = F.cross_entropy(logits[train_idx_t], y_t[train_idx_t])

    optimizer.zero_grad()
    loss.backward()
    optimizer.step()

    preds = torch.argmax(logits, dim=1)
    train_acc = (preds[train_idx_t] == y_t[train_idx_t]).float().mean().item()
    val_acc = (preds[val_idx_t] == y_t[val_idx_t]).float().mean().item()
    test_acc = (preds[test_idx_t] == y_t[test_idx_t]).float().mean().item()

    training_log.append({
        "epoch": epoch,
        "loss": float(loss.detach().cpu().numpy()),
        "train_acc": train_acc,
        "val_acc": val_acc,
        "test_acc": test_acc
    })

    print("Epoch", epoch,
          "| loss:", float(loss.detach().cpu().numpy()),
          "| train_acc:", train_acc,
          "| val_acc:", val_acc,
          "| test_acc:", test_acc)

training_log_df = pd.DataFrame(training_log)
training_log_df.to_csv(os.path.join(OUTPUT_DIR, "gcn_training_log.csv"), index=False)

preds_np = preds.detach().cpu().numpy()
test_truth = y[test_idx]
test_pred_gcn = preds_np[test_idx]

val_acc_gcn = accuracy_score(y[val_idx], preds_np[val_idx])
test_acc_gcn = accuracy_score(test_truth, test_pred_gcn)

confusion_like = pd.DataFrame(
    confusion_matrix(test_truth, test_pred_gcn),
    index=["True_0", "True_1", "True_2"],
    columns=["Pred_0", "Pred_1", "Pred_2"]
)

print("\nModel comparison:")
print("  Baseline LR validation accuracy:", val_acc_lr)
print("  Baseline LR test accuracy:", test_acc_lr)
print("  GCN validation accuracy:", val_acc_gcn)
print("  GCN test accuracy:", test_acc_gcn)

print("\nGCN confusion table (test set):")
print(confusion_like)

confusion_like.to_csv(os.path.join(OUTPUT_DIR, "gcn_confusion_matrix.csv"))

classification_discussion = (
    f"The features-only baseline reached validation accuracy of {val_acc_lr:.3f} and test accuracy of {test_acc_lr:.3f}, "
    f"whereas the GCN reached validation accuracy of {val_acc_gcn:.3f} and test accuracy of {test_acc_gcn:.3f}. "
    "The GCN can outperform the baseline here because it uses both node features and network structure, allowing each node representation to incorporate information from its neighbors. "
    "That is especially useful in this SBM because nodes in the same community tend to be connected and also share similar feature patterns. "
    "At the same time, a baseline can still be competitive, or even better, when node features are already highly informative or when network ties are noisy, sparse, or unrelated to the target labels. "
    "A simpler baseline may also generalize better when the graph model over-smooths node differences or when there are only limited labeled data. "
    "One important caution is that high predictive accuracy does not automatically imply scientific validity in social network settings. "
    "Strong performance may reflect label leakage, homophily, or dataset-specific structure rather than a substantively meaningful causal process."
)

# -----------------------------------------------------------------------------
# Save a concise report for the written questions
# -----------------------------------------------------------------------------
summary_lines = []
summary_lines.append("Applied Exercises Report")
summary_lines.append(f"Date: {date.today()}")
summary_lines.append("\n3. Graph construction + centrality")
summary_lines.append(f"Nodes: {num_nodes}")
summary_lines.append(f"Edges: {num_edges}")
summary_lines.append(f"Density: {density:.6f}")
summary_lines.append("\nTop 10 nodes by degree centrality:")
summary_lines.append(top10_degree.to_string(index=False))
summary_lines.append("\nTop 10 nodes by approximate betweenness centrality:")
summary_lines.append(top10_betw.to_string(index=False))
summary_lines.append("\nTop 10 nodes by eigenvector centrality:")
summary_lines.append(top10_eig.to_string(index=False))
summary_lines.append("\nDiscussion:")
summary_lines.append(centrality_discussion)

summary_lines.append("\n4. Community detection + evaluation")
summary_lines.append(f"Number of detected communities: {len(louvain_comms)}")
summary_lines.append("Community sizes:")
summary_lines.append(community_sizes_df.to_string(index=False))
summary_lines.append(f"Adjusted Rand Index (ARI): {ari:.6f}")
summary_lines.append("\nInterpretation:")
summary_lines.append(community_discussion)

summary_lines.append("\n5. Node classification")
summary_lines.append(f"Baseline LR validation accuracy: {val_acc_lr:.6f}")
summary_lines.append(f"Baseline LR test accuracy: {test_acc_lr:.6f}")
summary_lines.append(f"GCN validation accuracy: {val_acc_gcn:.6f}")
summary_lines.append(f"GCN test accuracy: {test_acc_gcn:.6f}")
summary_lines.append("\nGCN confusion matrix:")
summary_lines.append(confusion_like.to_string())
summary_lines.append("\nComparison:")
summary_lines.append(classification_discussion)

report_path = os.path.join(OUTPUT_DIR, "applied_exercises_report.txt")
with open(report_path, "w", encoding="utf-8") as f:
    f.write("\n".join(summary_lines))

print(f"\nSaved outputs to: {os.path.abspath(OUTPUT_DIR)}")
print(f"Saved report to: {os.path.abspath(report_path)}")
