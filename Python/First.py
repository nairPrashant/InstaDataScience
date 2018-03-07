import numpy as np # linear algebra
import pandas as pd # data processing, CSV file I/O (e.g. pd.read_csv)
import matplotlib.pyplot as plt
import seaborn as sns
color = sns.color_palette()
pd.set_option('display.float_format', lambda x: '%.3f' % x)

%matplotlib inline
pd.options.mode.chained_assignment = None  # default='warn'

#changing  to current working directory
import os
