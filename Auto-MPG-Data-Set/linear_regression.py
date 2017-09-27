import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import math

data = pd.DataFrame.from_csv('data/auto-mpg-with-header.data')

# create another data frame of log values
dict = data.to_dict()
del(dict['car_name'])
data_log = np.log(pd.DataFrame(dict))

lm_original = np.polyfit(data.acceleration, data.mpg, 2)

# calculate the y values based on the co-efficients from the model

r_x, r_y = zip(*((i,(i ** 2) * lm_original[0] + i * lm_original[1] + lm_original[2]) for i in sorted(data.acceleration)))

# Put in to a data frame, to keep is all nice
lm_original_plot = pd.DataFrame({
    'acceleration': r_x,
    'mpg': r_y
})

# ========================
# Model for Log Data
# ========================

# Get the linear models
lm_log = np.polyfit(data_log.acceleration, data_log.mpg, 2)

# calculate the y values based on the co-efficients from the model
r_x, r_y = zip(*((i,(i ** 2) * lm_log[0] + i * lm_log[1] + lm_log[2]) for i in sorted(data_log.acceleration)))

# Put in to a data frame, to keep is all nice
lm_log_plot = pd.DataFrame({
    'acceleration': r_x,
    'mpg': r_y
})

fig, axes = plt.subplots(nrows=1, ncols=2)

# Plot the original data and model
data.plot(kind='scatter', color='Blue', x='acceleration', y='mpg', ax=axes[0], title='Original Values')
lm_original_plot.plot(kind='line', color='Red', x='acceleration', y='mpg', ax=axes[0])

# Plot the log transformed data and model
data_log.plot(kind='scatter', color='Blue', x='acceleration', y='mpg', ax=axes[1], title='Log Values')
lm_log_plot.plot(kind='line', color='Red',x='acceleration', y='mpg', ax=axes[1])

plt.show()