import keras

from keras.models import Sequential
from keras.layers import Conv2D, MaxPooling2D, Flatten, Dense, Dropout
from keras.callbacks import EarlyStopping
from keras import optimizers

Alexnet = Sequential()
early_stopping = EarlyStopping()

# CONV1
Alexnet.add(Conv2D(
	filters = 96,
	kernel_size = (11, 11),
	input_shape = (227, 227, 3),
	strides = (4, 4),
	padding = 'valid',
	activation = 'relu'))

# MAX POOL1
Alexnet.add(MaxPooling2D(
	pool_size = (3,3),
	strides = (2,2)))

# CONV2
Alexnet.add(Conv2D(
	filters = 256,
	kernel_size = (5, 5),
	strides = (1,1),
	padding = 'same',
	activation = 'relu'))

# Max POOL2
Alexnet.add(MaxPooling2D(
	pool_size = (3,3), 
	strides = (2,2)))

# CONV3
Alexnet.add(Conv2D(
	filters = 384,
	kernel_size = (3,3),
	strides = (1,1),
	padding = 'same',
	activation = 'relu'))

# CONV4
Alexnet.add(Conv2D(
	filters = 384,
	kernel_size = (3,3),
	strides = (1,1),
	padding = 'same',
	activation = 'relu'))

# CONV5
Alexnet.add(Conv2D(
	filters = 256,
	kernel_size = (3,3),
	strides = (1,1),
	padding = 'same',
	activation = 'relu'))

# MAX POOL3
Alexnet.add(MaxPooling2D(
	pool_size = (3,3), 
	strides = (2,2)))

# Flatten for FC Input
Alexnet.add(Flatten())

# FC6
Alexnet.add(Dense(
	units = 4096,
	activation = 'relu'))

# FC7
Alexnet.add(Dense(
	units = 4096,
	activation = 'relu'))

# FC8
Alexnet.add(Dense(
	units = 1000, 
	activation = 'softmax'))

# SGD
sgd = optimizers.SGD(
	lr = 0.01, 
	decay = 5e-4, 
	momentum = 0.9)

# Compile
Alexnet.compile(
	optimizer = sgd, 
	loss = 'categorical_crossentropy',
	metrics = ['accuracy'])

# Architecture
Alexnet.summary()
# Training
Alexnet.fit(
	x = training_set,
	y = test_set,
	batch_size = 128,
	epochs = 300,
	callbacks = [early_stopping],
	validation_split = 0.2)