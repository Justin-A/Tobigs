import torch
import torch.nn as nn
import torch.nn.functional as F

class CNNClassifier(nn.Module):
    
    def __init__(self, vocab_size, embedding_dim, output_size, kernel_dim=100, kernel_sizes=(3, 4, 5), dropout=0.5):
        super(CNNClassifier,self).__init__()
        
        self.embedding_dim = embedding_dim
        self.embedding = nn.Embedding(vocab_size, embedding_dim)
        
        self.convs = nn.ModuleList([nn.Conv1d(1, kernel_dim, embedding_dim * K, stride=embedding_dim) for K in kernel_sizes])
        # in, out, kernel_size, stride

        # kernal_size = (K,D) 
        self.dropout = nn.Dropout(dropout)
        self.fc = nn.Linear(len(kernel_sizes) * kernel_dim, output_size)

    def forward(self, inputs):
        inputs = self.embedding(inputs) # B,T,D
        inputs = inputs.view(-1, 1,self.embedding_dim*inputs.size(1)) # (B,1,T*D)
        print (inputs.size())
        inputs = [F.relu(conv(inputs)) for conv in self.convs] #[(B,kernel_dim,L_out), ...]*len(Ks)
        print (inputs[0].size(), inputs[1].size(), inputs[2].size())
        inputs = [F.max_pool1d(i, i.size(2)).squeeze(2) for i in inputs] #[(B,kernel_dim), ...]*len(Ks)
        print (inputs[0].size(), inputs[1].size(), inputs[2].size())

        concated = torch.cat(inputs, 1) # B, kernel_dim*len(Ks)
        print (concated.size())
        concated = self.dropout(concated) 
        out = self.fc(concated)
        
        return F.sigmoid(out)