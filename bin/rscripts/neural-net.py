#!/usr/bin/env python
# coding: utf-8

# In[1]:


import numpy as np
import pandas as pd
from sklearn.model_selection import train_test_split, KFold
from sklearn.ensemble import GradientBoostingClassifier
from sklearn.metrics import classification_report, accuracy_score, f1_score, precision_score, recall_score, confusion_matrix
import tensorflow as tf
import matplotlib.pyplot as plt


# ## Data read in

# In[2]:


df = pd.read_excel('Dataset/dataset.xlsx', skiprows= 5)
df = df.replace(to_replace = 1, value = 0)


# In[32]:


df.head()


# ## Categories
# - M: testimony
# - D: potential
# - A: proven
# 
# In the manuscript, `M` and `D` were grouped together as "possible accidents characteristics" (PAC), while `A` was categorized as "accidents characteristics" (AC).

# In[3]:


df.loc[df['ta2'] == 'D', 'category'] = 'potential'
df.loc[df['ta2'] == 'M', 'category'] = 'testimony'
df.loc[df['ta2'] == 'A', 'category'] = 'proven'


# In[4]:


df.loc[df['category'] == 'potential', 'label2'] = 0 #'PAC'
df.loc[df['category'] == 'testimony', 'label2'] = 0 #'PAC' # Possible accident characteristics
df.loc[df['category'] == 'proven', 'label2'] = 1 #'AC' # ACCident characteristics


# In[5]:


df = df.dropna()


# In[6]:


X = df.iloc[:,0:66] #a to bn
y = df.label2


# In[7]:


X.head()


# In[8]:


y.unique()


# In[9]:


X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=9)


# In[10]:


clf_final = GradientBoostingClassifier(n_estimators=800, learning_rate=.01, 
                                 max_depth=5, random_state=0).fit(X_train, y_train)
y_pred = clf_final.predict(X_test)
print(confusion_matrix(y_test, y_pred))
print(classification_report(y_test,y_pred))


# In[60]:


def plot_confusion_matrix(validation_labels, y_pred):
    con_mat = tf.math.confusion_matrix(labels=np.argmax(validation_labels, axis=-1), predictions=y_pred).numpy()
#     con_mat_norm = np.around(con_mat.astype('float') / con_mat.sum(axis=1)[:, np.newaxis], decimals=2)
#     con_mat_df = pd.DataFrame(con_mat_norm, index = y.unique(), columns = class_labels)
#     #print("Confusion matrix for scenario " + scenario + ", resolution: " + str(image_size) + ":")
#     #print(con_mat_df)
#     figure = plt.figure()#figsize=(4, 4))    ## Confusion matrix heatmap
#     ax = sns.heatmap(con_mat_df, annot=True, cmap=plt.cm.Blues, fmt='g', cbar = False, annot_kws={"size": 16})
#     #figure.tight_layout()
#     plt.ylabel('True',fontsize=16)
#     ax.set_yticklabels(class_labels,va='center',fontsize=14)
#     ax.set_xticklabels(class_labels, ha='center',fontsize=14)
#     plt.xlabel('Predicted',fontsize=16)
    return


# In[164]:


# Fit classifier with out-of-bag estimates
params = {
    "n_estimators": 1200,
    "max_depth": 3,
    "subsample": 0.5,
    "learning_rate": 0.01,
    "min_samples_leaf": 1,
    "random_state": 3,
}

n_estimators = params["n_estimators"]
x = np.arange(n_estimators) + 1


def heldout_score(clf, X_test, y_test):
    """compute deviance scores on ``X_test`` and ``y_test``."""
    score = np.zeros((n_estimators,), dtype=np.float64)
    for i, y_pred in enumerate(clf.staged_decision_function(X_test)):
        #print(y_pred, y_test)
        score[i] = clf.loss_(y_test, y_pred)
    return score


def cv_estimate(n_splits=None):
    cv = KFold(n_splits=n_splits)
    cv_clf = GradientBoostingClassifier(**params)
    val_scores = np.zeros((n_estimators,), dtype=np.float64)
    for train, test in cv.split(X_train, y_train):
        print(train)
        cv_clf.fit(X_train.iloc[train], y_train.iloc[train])
        val_scores += heldout_score(cv_clf, X_train.iloc[test], y_train.iloc[test])
    val_scores /= n_splits
    return val_scores


# Estimate best n_estimator using cross-validation
cv_score = cv_estimate(5)

# Compute best n_estimator for test data
test_score = heldout_score(clf, X_test, y_test)


# In[ ]:





# In[168]:


clf = GradientBoostingClassifier(**params)

clf.fit(X_train, y_train)
acc = clf.score(X_test, y_test)
print("Accuracy: {:.4f}".format(acc))

# negative cumulative sum of oob improvements
cumsum = -np.cumsum(clf.oob_improvement_)

# min loss according to OOB
oob_best_iter = x[np.argmin(cumsum)]

# min loss according to test (normalize such that first loss is 0)
test_score -= test_score[0]
test_best_iter = x[np.argmin(test_score)]

# min loss according to cv (normalize such that first loss is 0)
cv_score -= cv_score[0]
cv_best_iter = x[np.argmin(cv_score)]


# In[169]:


plt.figure(figsize=(10,6))
# plot curves and vertical lines for best iterations
plt.plot(x, cumsum, label="OOB loss", color='red')
plt.plot(x, test_score, label="Test loss", color='blue')
plt.plot(x, cv_score, label="CV loss", color='orange')
plt.axvline(x=oob_best_iter, color='red')
plt.axvline(x=test_best_iter, color='blue')
plt.axvline(x=cv_best_iter, color='orange')

# add three vertical lines to xticks
xticks = plt.xticks()
xticks_pos = np.array(
    xticks[0].tolist() + [oob_best_iter, cv_best_iter, test_best_iter]
)
xticks_label = np.array(list(map(lambda t: int(t), xticks[0])) + ["OOB", "CV", "Test"])
ind = np.argsort(xticks_pos)
xticks_pos = xticks_pos[ind]
xticks_label = xticks_label[ind]
plt.xticks(xticks_pos, xticks_label)

plt.legend(loc="upper right")
plt.ylabel("normalized loss")
plt.xlabel("number of iterations")

plt.show()


# ## Neural Network

# In[98]:


from keras.models import Sequential
from keras.layers import Dense, Dropout, BatchNormalization
from tensorflow.keras.callbacks import Callback, EarlyStopping
from tensorflow.keras import backend as K


# In[37]:


class Metrics(Callback):
    def __init__(self, val_data):#, batch_size = 64):
        super().__init__()
        self.validation_data = val_data

    def on_train_begin(self, logs={}):
        self.val_f1s = []
        self.val_recalls = []
        self.val_precisions = []

    def on_epoch_end(self, epoch, logs={}):
        xVal, yVal = self.validation_data
        val_pred = np.argmax(np.asarray(self.model.predict(xVal)), axis=1)
        val_true = np.argmax(yVal, axis=1)        
        _val_f1 = f1_score(val_true, val_pred, average='macro', zero_division = 0)
        _val_precision = precision_score(val_true, val_pred, average='macro', zero_division = 0)
        _val_recall = recall_score(val_true, val_pred, average='macro', zero_division = 0)

        self.val_f1s.append(_val_f1)
        self.val_recalls.append(_val_recall)
        self.val_precisions.append(_val_precision)
        logs["val_f1"] = _val_f1
        logs["val_recall"] = _val_recall
        logs["val_precision"] = _val_precision
        print('— val_f1: %f — val_precision: %f — val_recall %f' %(_val_f1, _val_precision, _val_recall))
        return
model_metrics = Metrics(val_data=(X_test, y_test))


# In[100]:


# define the keras model
early_stopping = EarlyStopping(monitor='val_accuracy', patience=3, restore_best_weights=True)
K.clear_session()
model = Sequential()
model.add(Dense(128, input_dim=66, activation='relu'))
model.add(Dense(128, activation='relu'))
model.add(Dense(64, activation='relu'))
model.add(Dense(16, activation='relu'))
#model.add(Dropout(0.1))
model.add(Dense(8, activation='relu'))
#model.add(Dense(4, activation='relu'))
model.add(Dense(1, activation='sigmoid'))


# In[101]:


# compile the keras model
opt = tf.keras.optimizers.Adam(learning_rate = 0.01)    
model.compile(loss='binary_crossentropy', optimizer = opt, metrics=['accuracy'])
# fit the keras model on the dataset
model.fit(X_train, y_train, epochs=15, batch_size=10, verbose=1, validation_split = .2)#, callbacks = [model_metrics])
# make class predictions with the model


# In[94]:


#y_pred = np.argmax(model.predict(validation_images), axis=-1)     ## Params
y_pred = (model.predict(X_test) > 0.5).astype(int)
## Classification report
report = classification_report(y_test, y_pred, zero_division=0)
                               #labels = np.arange(len(class_labels)), target_names=class_labels, output_dict=True)
print(report)


# In[63]:


K.clear_session()
model = Sequential()
model.add(Dense(32, input_dim=66, activation='relu'))
model.add(Dense(16, activation='relu'))
model.add(Dropout(0.3))
model.add(Dense(16, activation='relu'))
#model.add(Dense(16, activation='relu'))
#model.add(Dense(4, activation='relu'))
model.add(Dense(1, activation='sigmoid'))
# compile the keras model
model.compile(loss='binary_crossentropy', optimizer='adam', metrics=['accuracy'])
# fit the keras model on the dataset
model.fit(X_train, y_train, epochs=10, batch_size=10, verbose=1)#, callbacks = [model_metrics])
# make class predictions with the model
#y_pred = np.argmax(model.predict(validation_images), axis=-1)     ## Params
y_pred = (model.predict(X_test) > 0.4).astype(int)
## Classification report
report = classification_report(y_test, y_pred, zero_division=0)
                               #labels = np.arange(len(class_labels)), target_names=class_labels, output_dict=True)
print(report)


# In[ ]:


K.clear_session()
model = Sequential()
model.add(Dense(32, input_dim=66, activation='relu'))
model.add(Dense(16, activation='relu'))
model.add(Dropout(0.3))
model.add(Dense(16, activation='relu'))
#model.add(Dense(16, activation='relu'))
#model.add(Dense(4, activation='relu'))
model.add(Dense(1, activation='sigmoid'))
# compile the keras model
model.compile(loss='binary_crossentropy', optimizer='adam', metrics=['accuracy'])
# fit the keras model on the dataset
model.fit(X_train, y_train, epochs=10, batch_size=10, verbose=1)#, callbacks = [model_metrics])
# make class predictions with the model
#y_pred = np.argmax(model.predict(validation_images), axis=-1)     ## Params
y_pred = (model.predict(X_test) > 0.5).astype(int)
## Classification report
report = classification_report(y_test, y_pred, zero_division=0)
                               #labels = np.arange(len(class_labels)), target_names=class_labels, output_dict=True)
print(report) 

