"""Original file is located at
    https://colab.research.google.com/drive/14xKpxxT9lsfRh9cu8HkVSQom9eysR6rl?usp=sharing
"""

#pip install torchmetrics

# Python 3.10.12
import torchmetrics as tm
import pandas as pd
import numpy as np

print(tm.__version__)
# 1.3.1

print(pd.__version__)
# 1.5.3

print(np.__version__)
# 1.25.2

df = pd.read_csv("data/clean_wordified_transcripts_v062923.csv", index_col="X")

google_trans = list(df['clean_google_trans'])
hand_trans = list(df['clean_hand_trans'])

# Calculate word error rates (using Levinshtein distance metric)
wers = []
word_error_rate = tm.WordErrorRate()
for i in range(0, len(google_trans)):
  asr_pred = google_trans[i]
  reference = hand_trans[i]
  wer = word_error_rate(asr_pred, reference)
  wers.append(wer.item())

# Join into original dataframe and save
df['wer'] = wers

df.to_csv("data/transcript_wers_v062923.csv")


## pip list installed 

# Package                          Version
# -------------------------------- ---------------------
# absl-py                          1.4.0
# aiohttp                          3.8.4
# aiosignal                        1.3.1
# alabaster                        0.7.13
# albumentations                   1.2.1
# altair                           4.2.2
# anyio                            3.7.0
# appdirs                          1.4.4
# argon2-cffi                      21.3.0
# argon2-cffi-bindings             21.2.0
# array-record                     0.4.0
# arviz                            0.15.1
# astropy                          5.2.2
# astunparse                       1.6.3
# async-timeout                    4.0.2
# attrs                            23.1.0
# audioread                        3.0.0
# autograd                         1.6.1
# Babel                            2.12.1
# backcall                         0.2.0
# beautifulsoup4                   4.11.2
# bleach                           6.0.0
# blis                             0.7.9
# blosc2                           2.0.0
# bokeh                            2.4.3
# branca                           0.6.0
# build                            0.10.0
# CacheControl                     0.13.1
# cached-property                  1.5.2
# cachetools                       5.3.1
# catalogue                        2.0.8
# certifi                          2023.5.7
# cffi                             1.15.1
# chardet                          4.0.0
# charset-normalizer               2.0.12
# chex                             0.1.7
# click                            8.1.3
# click-plugins                    1.1.1
# cligj                            0.7.2
# cloudpickle                      2.2.1
# cmake                            3.25.2
# cmdstanpy                        1.1.0
# colorcet                         3.0.1
# colorlover                       0.3.0
# community                        1.0.0b1
# confection                       0.0.4
# cons                             0.4.6
# contextlib2                      0.6.0.post1
# contourpy                        1.1.0
# convertdate                      2.4.0
# cufflinks                        0.17.3
# cvxopt                           1.3.1
# cvxpy                            1.3.1
# cycler                           0.11.0
# cymem                            2.0.7
# Cython                           0.29.35
# dask                             2022.12.1
# datascience                      0.17.6
# db-dtypes                        1.1.1
# dbus-python                      1.2.16
# debugpy                          1.6.6
# decorator                        4.4.2
# defusedxml                       0.7.1
# distributed                      2022.12.1
# dlib                             19.24.2
# dm-tree                          0.1.8
# docutils                         0.16
# dopamine-rl                      4.0.6
# duckdb                           0.8.1
# earthengine-api                  0.1.357
# easydict                         1.10
# ecos                             2.0.12
# editdistance                     0.6.2
# en-core-web-sm                   3.5.0
# entrypoints                      0.4
# ephem                            4.1.4
# et-xmlfile                       1.1.0
# etils                            1.3.0
# etuples                          0.3.9
# exceptiongroup                   1.1.1
# fastai                           2.7.12
# fastcore                         1.5.29
# fastdownload                     0.0.7
# fastjsonschema                   2.17.1
# fastprogress                     1.0.3
# fastrlock                        0.8.1
# filelock                         3.12.2
# Fiona                            1.9.4.post1
# firebase-admin                   5.3.0
# Flask                            2.2.5
# flatbuffers                      23.5.26
# flax                             0.6.11
# folium                           0.14.0
# fonttools                        4.40.0
# frozendict                       2.3.8
# frozenlist                       1.3.3
# fsspec                           2023.6.0
# future                           0.18.3
# gast                             0.4.0
# gcsfs                            2023.6.0
# GDAL                             3.3.2
# gdown                            4.6.6
# gensim                           4.3.1
# geographiclib                    2.0
# geopandas                        0.13.2
# geopy                            2.3.0
# gin-config                       0.5.0
# glob2                            0.7
# google                           2.0.3
# google-api-core                  2.11.1
# google-api-python-client         2.84.0
# google-auth                      2.17.3
# google-auth-httplib2             0.1.0
# google-auth-oauthlib             1.0.0
# google-cloud-bigquery            3.10.0
# google-cloud-bigquery-connection 1.12.0
# google-cloud-bigquery-storage    2.20.0
# google-cloud-core                2.3.2
# google-cloud-datastore           2.15.2
# google-cloud-firestore           2.11.1
# google-cloud-functions           1.13.0
# google-cloud-language            2.9.1
# google-cloud-storage             2.8.0
# google-cloud-translate           3.11.1
# google-colab                     1.0.0
# google-crc32c                    1.5.0
# google-pasta                     0.2.0
# google-resumable-media           2.5.0
# googleapis-common-protos         1.59.1
# googledrivedownloader            0.4
# graphviz                         0.20.1
# greenlet                         2.0.2
# grpc-google-iam-v1               0.12.6
# grpcio                           1.56.0
# grpcio-status                    1.48.2
# gspread                          3.4.2
# gspread-dataframe                3.0.8
# gym                              0.25.2
# gym-notices                      0.0.8
# h5netcdf                         1.2.0
# h5py                             3.8.0
# holidays                         0.27.1
# holoviews                        1.15.4
# html5lib                         1.1
# httpimport                       1.3.0
# httplib2                         0.21.0
# humanize                         4.6.0
# hyperopt                         0.2.7
# idna                             3.4
# imageio                          2.25.1
# imageio-ffmpeg                   0.4.8
# imagesize                        1.4.1
# imbalanced-learn                 0.10.1
# imgaug                           0.4.0
# importlib-resources              5.12.0
# imutils                          0.5.4
# inflect                          6.0.4
# iniconfig                        2.0.0
# intel-openmp                     2023.1.0
# ipykernel                        5.5.6
# ipython                          7.34.0
# ipython-genutils                 0.2.0
# ipython-sql                      0.4.1
# ipywidgets                       7.7.1
# itsdangerous                     2.1.2
# jax                              0.4.10
# jaxlib                           0.4.10+cuda11.cudnn86
# jieba                            0.42.1
# Jinja2                           3.1.2
# joblib                           1.2.0
# jsonpickle                       3.0.1
# jsonschema                       4.3.3
# jupyter-client                   6.1.12
# jupyter-console                  6.1.0
# jupyter_core                     5.3.1
# jupyter-server                   1.24.0
# jupyterlab-pygments              0.2.2
# jupyterlab-widgets               3.0.7
# kaggle                           1.5.13
# keras                            2.12.0
# kiwisolver                       1.4.4
# langcodes                        3.3.0
# lazy_loader                      0.2
# libclang                         16.0.0
# librosa                          0.10.0.post2
# lightgbm                         3.3.5
# lit                              16.0.6
# llvmlite                         0.39.1
# locket                           1.0.0
# logical-unification              0.4.6
# LunarCalendar                    0.0.9
# lxml                             4.9.2
# Markdown                         3.4.3
# markdown-it-py                   3.0.0
# MarkupSafe                       2.1.3
# matplotlib                       3.7.1
# matplotlib-inline                0.1.6
# matplotlib-venn                  0.11.9
# mdurl                            0.1.2
# miniKanren                       1.0.3
# missingno                        0.5.2
# mistune                          0.8.4
# mizani                           0.8.1
# mkl                              2019.0
# ml-dtypes                        0.2.0
# mlxtend                          0.14.0
# more-itertools                   9.1.0
# moviepy                          1.0.3
# mpmath                           1.3.0
# msgpack                          1.0.5
# multidict                        6.0.4
# multipledispatch                 0.6.0
# multitasking                     0.0.11
# murmurhash                       1.0.9
# music21                          8.1.0
# natsort                          8.3.1
# nbclient                         0.8.0
# nbconvert                        6.5.4
# nbformat                         5.9.0
# nest-asyncio                     1.5.6
# networkx                         3.1
# nibabel                          3.0.2
# nltk                             3.8.1
# notebook                         6.4.8
# numba                            0.56.4
# numexpr                          2.8.4
# numpy                            1.22.4
# oauth2client                     4.1.3
# oauthlib                         3.2.2
# opencv-contrib-python            4.7.0.72
# opencv-python                    4.7.0.72
# opencv-python-headless           4.7.0.72
# openpyxl                         3.0.10
# opt-einsum                       3.3.0
# optax                            0.1.5
# orbax-checkpoint                 0.2.6
# osqp                             0.6.2.post8
# packaging                        23.1
# palettable                       3.3.3
# pandas                           1.5.3
# pandas-datareader                0.10.0
# pandas-gbq                       0.17.9
# pandocfilters                    1.5.0
# panel                            0.14.4
# param                            1.13.0
# parso                            0.8.3
# partd                            1.4.0
# pathlib                          1.0.1
# pathy                            0.10.2
# patsy                            0.5.3
# pexpect                          4.8.0
# pickleshare                      0.7.5
# Pillow                           8.4.0
# pip                              23.1.2
# pip-tools                        6.13.0
# platformdirs                     3.7.0
# plotly                           5.13.1
# plotnine                         0.10.1
# pluggy                           1.2.0
# polars                           0.17.3
# pooch                            1.6.0
# portpicker                       1.5.2
# prefetch-generator               1.0.3
# preshed                          3.0.8
# prettytable                      0.7.2
# proglog                          0.1.10
# progressbar2                     4.2.0
# prometheus-client                0.17.0
# promise                          2.3
# prompt-toolkit                   3.0.38
# prophet                          1.1.4
# proto-plus                       1.22.3
# protobuf                         3.20.3
# psutil                           5.9.5
# psycopg2                         2.9.6
# ptyprocess                       0.7.0
# py-cpuinfo                       9.0.0
# py4j                             0.10.9.7
# pyarrow                          9.0.0
# pyasn1                           0.5.0
# pyasn1-modules                   0.3.0
# pycocotools                      2.0.6
# pycparser                        2.21
# pyct                             0.5.0
# pydantic                         1.10.9
# pydata-google-auth               1.8.0
# pydot                            1.4.2
# pydot-ng                         2.0.0
# pydotplus                        2.0.2
# PyDrive                          1.3.1
# pyerfa                           2.0.0.3
# pygame                           2.4.0
# Pygments                         2.14.0
# PyGObject                        3.36.0
# pymc                             5.1.2
# PyMeeus                          0.5.12
# pymystem3                        0.2.0
# PyOpenGL                         3.1.7
# pyparsing                        3.1.0
# pyproj                           3.6.0
# pyproject_hooks                  1.0.0
# pyrsistent                       0.19.3
# PySocks                          1.7.1
# pytensor                         2.10.1
# pytest                           7.2.2
# python-apt                       0.0.0
# python-dateutil                  2.8.2
# python-louvain                   0.16
# python-slugify                   8.0.1
# python-utils                     3.7.0
# pytz                             2022.7.1
# pyviz-comms                      2.3.2
# PyWavelets                       1.4.1
# PyYAML                           6.0
# pyzmq                            23.2.1
# qdldl                            0.1.7
# qudida                           0.0.4
# regex                            2022.10.31
# requests                         2.27.1
# requests-oauthlib                1.3.1
# requests-unixsocket              0.2.0
# requirements-parser              0.5.0
# rich                             13.4.2
# rpy2                             3.5.5
# rsa                              4.9
# scikit-image                     0.19.3
# scikit-learn                     1.2.2
# scipy                            1.10.1
# scs                              3.2.3
# seaborn                          0.12.2
# Send2Trash                       1.8.2
# setuptools                       67.7.2
# shapely                          2.0.1
# six                              1.16.0
# sklearn-pandas                   2.2.0
# smart-open                       6.3.0
# sniffio                          1.3.0
# snowballstemmer                  2.2.0
# sortedcontainers                 2.4.0
# soundfile                        0.12.1
# soupsieve                        2.4.1
# soxr                             0.3.5
# spacy                            3.5.3
# spacy-legacy                     3.0.12
# spacy-loggers                    1.0.4
# Sphinx                           3.5.4
# sphinxcontrib-applehelp          1.0.4
# sphinxcontrib-devhelp            1.0.2
# sphinxcontrib-htmlhelp           2.0.1
# sphinxcontrib-jsmath             1.0.1
# sphinxcontrib-qthelp             1.0.3
# sphinxcontrib-serializinghtml    1.1.5
# SQLAlchemy                       2.0.16
# sqlparse                         0.4.4
# srsly                            2.4.6
# statsmodels                      0.13.5
# sympy                            1.11.1
# tables                           3.8.0
# tabulate                         0.8.10
# tblib                            2.0.0
# tenacity                         8.2.2
# tensorboard                      2.12.3
# tensorboard-data-server          0.7.1
# tensorflow                       2.12.0
# tensorflow-datasets              4.9.2
# tensorflow-estimator             2.12.0
# tensorflow-gcs-config            2.12.0
# tensorflow-hub                   0.13.0
# tensorflow-io-gcs-filesystem     0.32.0
# tensorflow-metadata              1.13.1
# tensorflow-probability           0.20.1
# tensorstore                      0.1.38
# termcolor                        2.3.0
# terminado                        0.17.1
# text-unidecode                   1.3
# textblob                         0.17.1
# tf-slim                          1.1.0
# thinc                            8.1.10
# threadpoolctl                    3.1.0
# tifffile                         2023.4.12
# tinycss2                         1.2.1
# toml                             0.10.2
# tomli                            2.0.1
# toolz                            0.12.0
# torch                            2.0.1+cu118
# torchaudio                       2.0.2+cu118
# torchdata                        0.6.1
# torchmetrics                     0.11.4
# torchsummary                     1.5.1
# torchtext                        0.15.2
# torchvision                      0.15.2+cu118
# tornado                          6.3.1
# tqdm                             4.65.0
# traitlets                        5.7.1
# triton                           2.0.0
# tweepy                           4.13.0
# typer                            0.7.0
# types-setuptools                 68.0.0.0
# typing_extensions                4.6.3
# tzlocal                          5.0.1
# uritemplate                      4.1.1
# urllib3                          1.26.16
# vega-datasets                    0.9.0
# wasabi                           1.1.2
# wcwidth                          0.2.6
# webcolors                        1.13
# webencodings                     0.5.1
# websocket-client                 1.6.0
# Werkzeug                         2.3.6
# wheel                            0.40.0
# widgetsnbextension               3.6.4
# wordcloud                        1.8.2.2
# wrapt                            1.14.1
# xarray                           2022.12.0
# xarray-einstats                  0.5.1
# xgboost                          1.7.6
# xlrd                             2.0.1
# yarl                             1.9.2
# yellowbrick                      1.5
# yfinance                         0.2.21
# zict                             3.0.0
# zipp                             3.15.0

## ! apt list --installed | grep python

# libboost-mpi-python-dev/focal,now 1.71.0.0ubuntu2 amd64 [installed,automatic]
# libboost-mpi-python1.71-dev/focal,now 1.71.0-6ubuntu6 amd64 [installed,automatic]
# libboost-mpi-python1.71.0/focal,now 1.71.0-6ubuntu6 amd64 [installed,automatic]
# libboost-python-dev/focal,now 1.71.0.0ubuntu2 amd64 [installed,automatic]
# libboost-python1.71-dev/focal,now 1.71.0-6ubuntu6 amd64 [installed,automatic]
# libboost-python1.71.0/focal,now 1.71.0-6ubuntu6 amd64 [installed,automatic]
# libpython2.7-minimal/focal-updates,focal-security,now 2.7.18-1~20.04.3 amd64 [installed,automatic]
# libpython2.7-stdlib/focal-updates,focal-security,now 2.7.18-1~20.04.3 amd64 [installed,automatic]
# libpython3-dev/focal,now 3.8.2-0ubuntu2 amd64 [installed]
# libpython3-stdlib/focal,now 3.8.2-0ubuntu2 amd64 [installed,automatic]
# libpython3.10-dev/focal,now 3.10.12-1+focal1 amd64 [installed,automatic]
# libpython3.10-minimal/focal,now 3.10.12-1+focal1 amd64 [installed,automatic]
# libpython3.10-stdlib/focal,now 3.10.12-1+focal1 amd64 [installed,automatic]
# libpython3.10/focal,now 3.10.12-1+focal1 amd64 [installed,automatic]
# libpython3.8-dev/focal-updates,focal-security,now 3.8.10-0ubuntu1~20.04.8 amd64 [installed,automatic]
# libpython3.8-minimal/focal-updates,focal-security,now 3.8.10-0ubuntu1~20.04.8 amd64 [installed,automatic]
# libpython3.8-stdlib/focal-updates,focal-security,now 3.8.10-0ubuntu1~20.04.8 amd64 [installed,automatic]
# libpython3.8/focal-updates,focal-security,now 3.8.10-0ubuntu1~20.04.8 amd64 [installed,automatic]
# python-apt-common/focal-updates,now 2.0.1ubuntu0.20.04.1 all [installed,automatic]
# python2.7-minimal/focal-updates,focal-security,now 2.7.18-1~20.04.3 amd64 [installed,automatic]
# python2.7/focal-updates,focal-security,now 2.7.18-1~20.04.3 amd64 [installed,automatic]
# python3-apt/focal-updates,now 2.0.1ubuntu0.20.04.1 amd64 [installed,automatic]
# python3-certifi/focal,now 2019.11.28-1 all [installed,automatic]
# python3-chardet/focal,now 3.0.4-4build1 all [installed,automatic]
# python3-dbus/focal,now 1.2.16-1build1 amd64 [installed,automatic]
# python3-dev/focal,now 3.8.2-0ubuntu2 amd64 [installed,automatic]
# python3-distutils/focal-updates,focal-security,now 3.8.10-0ubuntu1~20.04 all [installed,automatic]
# python3-gi/focal,now 3.36.0-1 amd64 [installed,automatic]
# python3-idna/focal,now 2.8-1 all [installed,automatic]
# python3-lib2to3/focal-updates,focal-security,now 3.8.10-0ubuntu1~20.04 all [installed,automatic]
# python3-minimal/focal,now 3.8.2-0ubuntu2 amd64 [installed,automatic]
# python3-pkg-resources/focal-updates,focal-security,now 45.2.0-1ubuntu0.1 all [installed,automatic]
# python3-requests-unixsocket/focal,now 0.2.0-2 all [installed,automatic]
# python3-requests/focal-updates,focal-security,now 2.22.0-2ubuntu1.1 all [installed,automatic]
# python3-six/focal,now 1.14.0-2 all [installed,automatic]
# python3-software-properties/focal-updates,now 0.99.9.11 all [installed,automatic]
# python3-urllib3/focal-updates,focal-security,now 1.25.8-2ubuntu0.2 all [installed,automatic]
# python3.10-dev/focal,now 3.10.12-1+focal1 amd64 [installed]
# python3.10-distutils/focal,now 3.10.12-1+focal1 all [installed]
# python3.10-lib2to3/focal,now 3.10.12-1+focal1 all [installed,automatic]
# python3.10-minimal/focal,now 3.10.12-1+focal1 amd64 [installed,automatic]
# python3.10-tk/focal,now 3.10.12-1+focal1 amd64 [installed]
# python3.10/focal,now 3.10.12-1+focal1 amd64 [installed]
# python3.8-dev/focal-updates,focal-security,now 3.8.10-0ubuntu1~20.04.8 amd64 [installed,automatic]
# python3.8-minimal/focal-updates,focal-security,now 3.8.10-0ubuntu1~20.04.8 amd64 [installed,automatic]
# python3.8/focal-updates,focal-security,now 3.8.10-0ubuntu1~20.04.8 amd64 [installed,automatic]
# python3/focal,now 3.8.2-0ubuntu2 amd64 [installed,automatic]