from time import time

import numpy as np
import numpy.linalg as la
from scipy import sparse
from scipy.sparse.linalg import eigsh
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib import cm
from mpl_toolkits.mplot3d import Axes3D
from sklearn.metrics import pairwise_distances
from sklearn.preprocessing import normalize
from sklearn.cluster import SpectralClustering

def save_sparse_csr(filename, array):
    np.savez(filename, data=array.data, indices=array.indices,
             indptr=array.indptr, shape=array.shape)

def load_sparse_csr(filename):
    with np.load(filename) as loader:
        res = sparse.csr_matrix((
            loader['data'], loader['indices'], loader['indptr']),
            shape=loader['shape'])
    return res


# data = np.loadtxt('../data/dtm_tfidf.csv')

# sdata = sparse.csr_matrix(data)
# save_sparse_csr('../data/sdtm_tfidf.npz', sdata)


# data = np.loadtxt('../data/dtm_tf.csv')

# sdata = sparse.csr_matrix(data)
# save_sparse_csr('../data/sdtm_tf.npz', sdata)


keep = pd.read_csv('../data/ids_kept.csv')
keep = keep['kept'].astype(bool)

emails = pd.read_csv('../data/emails.csv')
emails = emails.reset_index()
emails = emails.ix[keep]
emails = emails.reset_index()
hill = np.array(emails['SenderPersonId'] == 80)

# sdata = load_sparse_csr('../data/sdtm_tfidf.npz')
sdata = load_sparse_csr('../data/sdtm_tf.npz')


np.random.seed(2634)
inds = np.random.choice(sdata.shape[0], 8000, replace=False)
sd = sdata[inds]
hill = hill[inds]


dist = pairwise_distances(sd, metric='cosine')
dist[dist < 0.] = 0.
dist2 = dist**2

n = dist2.shape[0]
H = (np.eye(n) - np.ones((n, n))/n)
G = -(1/2) * H @ dist2 @ H

eig = eigsh(G, k=3)
z1 = np.sqrt(eig[0][-1]) * eig[1][:,-1]
z2 = np.sqrt(eig[0][-2]) * eig[1][:,-2]
z3 = np.sqrt(eig[0][-3]) * eig[1][:,-3]
z = np.column_stack((z1, z2, z3))


start = time()
sim = 1 - dist
spectral = SpectralClustering(
    n_clusters=10, eigen_solver='arpack', affinity="precomputed",
    random_state=5373, n_init=20)
spectral.fit(sim)
print(time() - start)
y_pred = y_pred = spectral.labels_.astype(np.int)


colors = cm.Paired(np.linspace(0, 1, 10))

fig = plt.figure()
ax = fig.add_subplot(111)
ax.scatter(z1, z2, s=10, lw=0.2)
fig.savefig('../images/mds_2d.pdf')

fig = plt.figure()
ax = fig.add_subplot(111)
for g, c in zip(range(10), colors):
    ax.scatter(z1[y_pred==g], z2[y_pred==g],
               c=c, s=10, lw=0.2, label=str(g))
ax.legend(scatterpoints=1, markerscale=2)
fig.savefig('../images/mds_2d_cluster.pdf')


fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
ax.scatter(z1, z2, z3, s=10, lw=0.2)
ax.view_init(29, 46)
fig.savefig('../images/mds_3d_1.pdf')

ax.view_init(-10, 227)
fig.savefig('../images/mds_3d_2.pdf')

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
for g, c in zip(range(10), colors):
    ax.scatter(z1[y_pred==g], z2[y_pred==g], z3[y_pred==g],
               c=c, s=10, lw=0.2, label=str(g))
ax.legend(scatterpoints=1, markerscale=2)
ax.view_init(29, 46)
fig.savefig('../images/mds_3d_cluster_1.pdf')

ax.view_init(-10, 227)
fig.savefig('../images/mds_3d_cluster_2.pdf')


np.savetxt('../data/clusters.txt', np.column_stack((inds, y_pred)))


for a in range(10):
    print(a, np.sum(y_pred==a))

for a in np.random.choice(np.where(y_pred == 0)[0], size=5):
    print()
    print(a)
    print(z[a])
    print(emails['AllText'][inds].iloc[a])


# Cluster=10
# 0 lt blue: FYI + something
# 1 dk blue: misc
# 2 green  : call, talk, short
# 3 dk grey: schedules
# 4 red    : call, talk, with some substance
# 5 lt oran: print requests
# 6 dk oran: news headlines
# 7 purple : misc, staff/scheduling/meeting
# 8 tan    : misc
# 9 brown  : FYI short

# 3.5k emails in 1, 1.8k emails in 8
