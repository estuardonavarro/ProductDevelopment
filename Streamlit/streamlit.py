# Desarrollado por: Javier Navarro


import streamlit as st
import numpy as np
import pandas as pd

# título de la página
st.title('This is my first streamlit app, for Galileo Master')

x = 4
st.write(x, 'square is', x * x)

x, 'square is', x * x

st.write('Now using dataframes:')

df = pd.DataFrame({'Column A': [1, 2, 3, 4, 5],
                   'Column B': ['A', 'B', 'C', 'D', 'E']})

st.write(df)

'''
# Title
## Subtitle
'''

df

'''
## Let's use some graphs'
'''

chart_df = pd.DataFrame(
    np.random.randn(20, 3),
    columns=['A', 'B', 'C'])

st.line_chart(chart_df)

'''
## How about a map
'''

map_df = pd.DataFrame(
    np.random.randn(1000, 2) / [50, 50] + [37.76, -122.4],
    columns=['lat', 'lon'])

st.map(map_df)

if st.checkbox('Show me the dataframe'):
    map_df

'''
### Slider test
'''

x = st.slider('Select value for x')
st.write(x, 'square is', x * x)

'''
### Options
'''

option = st.selectbox(
    'Which number do you like best?',
    [1, 2, 3, 4])

'You selected the option', option

'''
### Progressbar
'''
import time

progress_bar_label = st.empty()
progress_bar = st.progress(0)
progress_bar2 = st.sidebar.progress(0)

for i in range(101):
    progress_bar_label.text(f'iteración {i}')
    progress_bar.progress(i)
    time.sleep(0.5)

for i in range(101):
    progress_bar2.progress(i)
    time.sleep(0.5)

option_side = st.sidebar.selectbox('Choose your weapon?',
                                   ['handgun', 'machinegun', 'knife'])

st.sidebar.write('Your weapon is:', option_side)

another_slider = st.sidebar.slider('Select the range',
                                   0.0, 100.0, (25.0, 75.0))

st.sidebar.write('The range selected is:', another_slider)
