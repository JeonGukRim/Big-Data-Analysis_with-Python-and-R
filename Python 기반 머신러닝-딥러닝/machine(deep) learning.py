import tensorflow as tf
import random
import numpy as np
r = [] #귤 1
b = [] #오렌지 0
for i in range(50):
    r.append([random.randint(1,10),random.randint(50,100),1])
    b.append([random.randint(7,20),random.randint(80, 130),0])

def distance(x,y):
    #두 점 사이의 거리를 구하는 함수
    return np.sqrt(pow((x[0]-y[0]),2)+pow((x[1]-y[1]),2))

def knn(x,y,k):
    result =[]
    cnt = 0
    for i in range(len(y)):
        result.append([distance(x,y[i]),y[i][2]])
    result.sort()
    for i in range(k):
        if (result[i][1] == 1):
            cnt += 1
    if (cnt > (k/2)) :
        print('이것은 귤입니다,')
    else:
        print('이것은 오렌지 입니다')

weight = input('무게를 입력해주세요')
size = input('크기를 입력해주세요')
num = input('k를 입력해주세요')
new =[int(size),int(weight)]

knn(new,r+b,int(num))

import matplotlib.pyplot as plt
# %matplotlib inline
rr = np.array(r)
bb = np.array(b)
for i,j in rr[:,:2]:
    plt.plot(i,j,'or')
for i,j in bb[:,:2]:
    plt.plot(i,j,'ob')
plt.plot(int(size),int(weight),'og')
plt.show()



import tensorflow as tf
import tensorflow.keras.utils as utils
from tensorflow.keras.datasets import mnist
from tensorflow.keras.layers import Dense,Activation
import numpy as np
import matplotlib.pyplot as plt
import random
%matplotlib inline
x_data =[]
for i in range(100):
    x_data.append([random.randint(7,20),random.randint(80,130)])
    x_data.append([random.randint(1,10),random.randint(50,100)])
y_data =[]
for i in range(100):
    y_data.append(1) #오렌지
    y_data.append(0)#귤

x_train = np.array(x_data)
x_train =x_train.reshape(200,2)
y_train = np.array(y_data)
y_train = y_train.reshape(200,)
x_data = []
y_data = []
for i in range(10):
    x_data.append([random.randint(7,20),random.randint(80,130)])
    x_data.append([random.randint(1,10),random.randint(50,100)])
y_data =[]
for i in range(10):
    y_data.append(1) #오렌지
    y_data.append(0)#귤
x_test = np.array([x_data])
x_test =x_test.reshape(20,2)
y_test =np.array(y_data)
y_test =y_test.reshape(20,)
from tensorflow.keras.models import Sequential
model = Sequential()
model.add(Dense(20,input_dim=2,activation='relu'))
model.add(Dense(10,activation='relu'))
model.add(Dense(1,activation='sigmoid'))

model.compile(loss='binary_crossentropy',optimizer='adam',metrics=['accuracy'])
hist = model.fit(x_train,y_train,epochs = 200,batch_size =10,validation_data=(x_train,y_train))
loss_and_metrics= model.evaluate(x_test,y_test,batch_size = 1)

fig,loss_ax =plt.subplots()
acc_ax = loss_ax.twinx()

loss_ax.plot(hist.history['loss'],'y',label='train loss')
loss_ax.plot(hist.history['val_loss'],'r',label='val loss')
acc_ax.plot(hist.history['accuracy'],'b',label='train acc')
acc_ax.plot(hist.history['val_accuracy'],'g',label='val acc')

loss_ax.set_xlabel('epoch')
loss_ax.set_ylabel('loss')
acc_ax.set_ylabel('accuray')

loss_ax.legend(loc='upper left')
acc_ax.legend(loc='lower left')
plt.show()


x_train,y_train= []
(x_train,y_train),(x_test,y_test) = mnist.load_data()
x_val = x_train[50000:]
y_val = y_train[50000:]
x_train = x_train[:50000]
y_train = y_train[:50000]
x_train = x_train.reshape(50000,784)

#---------------------------mnist---------------------------손 글씨 데이터셋(7만개)으로 딥러닝
import tensorflow as tf
import tensorflow.keras.utils as utils
from tensorflow.keras.datasets import mnist
from tensorflow.keras.layers import Dense,Activation
from tensorflow.keras.models import Sequential
import numpy as np
import matplotlib.pyplot as plt
import random
from tensorflow.keras.datasets import mnist
(x_train,y_train),(x_test,y_test) = mnist.load_data()
x_val = x_train[50000:]
y_val = y_train[50000:]
x_train = x_train[:50000]
y_train = y_train[:50000]
x_train = x_train.reshape(50000,784).astype('float32')
x_val = x_val.reshape(10000,784).astype('float32')
x_test = x_test.reshape(10000,784).astype('float32')

y_train = utils.to_categorical(y_train)
y_val= utils.to_categorical(y_val)
y_test= utils.to_categorical(y_test)

model = Sequential()
model.add(Dense(units=512,input_dim =28*28,activation='relu'))
model.add(Dense(units=256,activation='relu'))
model.add(Dense(units=128,activation='relu'))
model.add(Dense(units=10,activation='softmax'))
model.compile(loss='categorical_crossentropy',optimizer='adam',metrics=['accuracy'])
hist = model.fit(x_train,y_train,epochs = 10, batch_size=50,
                 validation_data =(x_val,y_val))
loss_and_metrics= model.evaluate(x_test,y_test,batch_size = 32)
print('')
print('lose:'+str(loss_and_metrics[0]))
print('accuray:'+str(loss_and_metrics[1]))

fig,loss_ax =plt.subplots()
acc_ax = loss_ax.twinx()

loss_ax.plot(hist.history['loss'],'y',label='train loss')
loss_ax.plot(hist.history['val_loss'],'r',label='val loss')
acc_ax.plot(hist.history['accuracy'],'b',label='train acc')
acc_ax.plot(hist.history['val_accuracy'],'g',label='val acc')

loss_ax.set_xlabel('epoch')
loss_ax.set_ylabel('loss')
acc_ax.set_ylabel('accuray')

loss_ax.legend(loc='upper left')
acc_ax.legend(loc='lower left')
plt.show()

#cifar-10 사이파 데이터셋-----------
#조기종료,
#조기종료 언더핏 방지 함수- loss 값이 불안정함을 방지
from tensorflow.keras.datasets import cifar10
from tensorflow.keras.callbacks import EarlyStopping
early_stopping = EarlyStopping(patience = 10) # loss 불안정 방지
cifar10_data = tf.keras.datasets.cifar10.load_data()
x_train,y_train= [] #초기화
x_test,y_test = []
(x_train,y_train),(x_test,y_test) = cifar10_data
x_val = x_train[40000:]
y_val = y_train[40000:]
x_train = x_train[:40000]
y_train = y_train[:40000]
# 그림 확인
print(x_train[0].shape)
plt.imshow(x_train[0])

x_train = x_train.reshape(40000,32*32*3).astype('float32')
x_val = x_val.reshape(10000,32*32*3).astype('float32')
x_test = x_test.reshape(10000,32*32*3).astype('float32')

# 0과 1사이 값으로 변경
y_train = utils.to_categorical(y_train)
y_val= utils.to_categorical(y_val)
y_test= utils.to_categorical(y_test)

model = Sequential()
#모델생성
model.add(Dense(units=512,input_dim =32*32*3,activation='relu'))
model.add(Dense(units=256,activation='relu'))
model.add(Dense(units=256,activation='relu'))
model.add(Dense(units=128,activation='relu'))
model.add(Dense(units=128,activation='relu'))
model.add(Dense(units=64,activation='relu'))
model.add(Dense(units=64,activation='relu'))
model.add(Dense(units=10,activation='softmax'))
model.compile(loss='categorical_crossentropy',optimizer='adam',metrics=['accuracy'])
#모델 훈련
hist = model.fit(x_train,y_train,epochs = 30, batch_size=32,
                 validation_data =(x_val,y_val))
#모델 평가하기
loss_and_metrics= model.evaluate(x_test,y_test,batch_size = 32)
print('')
print('lose:'+str(loss_and_metrics[0]))
print('accuray:'+str(loss_and_metrics[1]))

fig,loss_ax =plt.subplots()
acc_ax = loss_ax.twinx()

loss_ax.plot(hist.history['loss'],'y',label='train loss')
loss_ax.plot(hist.history['val_loss'],'r',label='val loss')
acc_ax.plot(hist.history['accuracy'],'b',label='train acc')
acc_ax.plot(hist.history['val_accuracy'],'g',label='val acc')

loss_ax.set_xlabel('epoch')
loss_ax.set_ylabel('loss')
acc_ax.set_ylabel('accuray')

loss_ax.legend(loc='upper left')
acc_ax.legend(loc='lower left')
plt.show()
