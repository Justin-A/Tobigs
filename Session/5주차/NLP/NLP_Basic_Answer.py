import pandas as pd
import numpy as np
import re
from konlpy.tag import *
from gensim.models import Word2Vec
from sklearn import cluster

data = pd.read_csv("tw_Cr.csv")
data = data[['x', 'Keyword']]

sentence = list(data['x'])

regEx = re.compile(r"@+[A-z0-9]+:")

result = []
for i in range(len(sentence)):
	result2 = re.split(regEx, sentence[i])
	result.append(result2[-1])

result2 = []
for i in range(len(result)):
	parse = re.sub('[s://t.co0-9a-zA-Z()&]', '', result[i])
	result2.append(parse)

data['x_regex'] = result2

kkma = Kkma()
token = []
for i in result2:
	token.append(kkma.nouns(i))

data['x_token'] = token

model = Word2Vec(token,
	sg = 1,
	window = 5,
	size = 10,
	min_count = 5)

data['x_embedding'] = model.wv.vectors

km = cluster.Kmeans(n_cluster = 14)
km.fit(data['x_embedding'])

num_clust = km.labels_
data['x_Kmeans'] = num_clust

# 이후는 자유 서술
# 군집 간 비교는 개인의 생각하는 역량을 보기 위함이지만 대부분 코드만 쓰고 제출하지않을까?