import torch.nn as nn
import torch.nn.functional as F
import torch
from torch.autograd import Variable

class RNN(nn.Module):
    def __init__(self,input_size,embed_size,hidden_size,output_size,num_layers=1,bidirec=False):
        super(RNN,self).__init__()
        
        self.hidden_size = hidden_size
        self.num_layers = num_layers
        if bidirec:
            self.num_directions = 2
        else:
            self.num_directions = 1
            
        self.embed = nn.Embedding(input_size,embed_size)
        self.lstm = nn.LSTM(embed_size,hidden_size,num_layers,batch_first=True,bidirectional=bidirec)
        self.linear = nn.Linear(hidden_size*self.num_directions,output_size)
        
    def init_hidden(self,batch_size):
        # (num_layers * num_directions, batch_size, hidden_size)
        
        hidden = Variable(torch.zeros(self.num_layers*self.num_directions,batch_size,self.hidden_size)).cuda()
        cell = Variable(torch.zeros(self.num_layers*self.num_directions,batch_size,self.hidden_size)).cuda()
        return hidden, cell

    def forward(self,inputs):
        """
        inputs : B,T
        """
        embed = self.embed(inputs) # word vector indexing
        hidden, cell = self.init_hidden(inputs.size(0)) # initial hidden,cell
        
        output, h = self.lstm(embed,(hidden,cell))
        hidden, cell = h
        # Many-to-One
        hidden = hidden[-self.num_directions:] # (num_directions,B,H)
        hidden = torch.cat([h for h in hidden],1)
        output = self.linear(hidden) # last hidden
        
        return F.sigmoid(output)
