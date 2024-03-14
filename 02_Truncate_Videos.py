# Trim videos into 2-min long
# Use the following bash code to get ffmpeg
# export PATH=/software/ffmpeg:/software/ffmpeg/bin:$PATH


# Python 3.9.7
import os
from tqdm import tqdm

# tqdm.__version__
# 4.64.0

path_mp4 = "./videos/"
path_mp4_truncated = "./videos_cut/"

errors_cut =[]
for filename in tqdm(os.listdir(path_mp4)[:]):
    try:
        id = os.path.basename(filename).split('.')[0]
        output = id +'_c.mp4'
        command = "ffmpeg -accurate_seek -ss 00:00:00 -i " + os.path.join(path_mp4, filename) + " -to 00:02:10  -c:v copy -c:a copy " + os.path.join(path_mp4_truncated, output)
        #print(command)
        os.system(command)
    except:
        errors_cut.append(filename)
        pass

# pip list installed
# Package                            Version
# ---------------------------------- --------------------
# absl-py                            1.0.0
# aiohttp                            3.8.1
# aiohttp-retry                      2.4.6
# aiosignal                          1.2.0
# alabaster                          0.7.12
# anaconda-client                    1.9.0
# anaconda-navigator                 2.1.1
# anaconda-project                   0.10.1
# anyio                              2.2.0
# appdirs                            1.4.4
# applaunchservices                  0.2.1
# appnope                            0.1.2
# appscript                          1.1.2
# argh                               0.26.2
# argon2-cffi                        20.1.0
# arrow                              0.13.1
# asgiref                            3.5.0
# asn1crypto                         1.4.0
# astroid                            2.6.6
# astropy                            4.3.1
# astunparse                         1.6.3
# async-generator                    1.10
# async-timeout                      4.0.2
# asyncssh                           2.10.1
# atomicwrites                       1.4.0
# atpublic                           3.0.1
# attrs                              21.2.0
# autopep8                           1.5.7
# Babel                              2.9.1
# backcall                           0.2.0
# backports.functools-lru-cache      1.6.4
# backports.shutil-get-terminal-size 1.0.0
# backports.tempfile                 1.0
# backports.weakref                  1.0.post1
# beautifulsoup4                     4.10.0
# binaryornot                        0.4.4
# bitarray                           2.3.0
# bkcharts                           0.2
# black                              19.10b0
# bleach                             4.0.0
# blis                               0.7.9
# bokeh                              2.4.1
# boto                               2.49.0
# boto3                              1.22.4
# botocore                           1.25.4
# Bottleneck                         1.3.2
# brotlipy                           0.7.0
# cached-property                    1.5.2
# cachetools                         4.2.1
# catalogue                          2.0.8
# certifi                            2021.10.8
# cffi                               1.14.6
# chardet                            4.0.0
# charset-normalizer                 2.0.4
# click                              8.0.3
# cloudpickle                        2.0.0
# clusim                             0.4
# clyent                             1.2.2
# colorama                           0.4.4
# commonmark                         0.9.1
# conda                              4.13.0
# conda-build                        3.21.5
# conda-content-trust                0+unknown
# conda-pack                         0.6.0
# conda-package-handling             1.8.1
# conda-repo-cli                     1.0.4
# conda-token                        0.3.0
# conda-verify                       3.4.2
# confection                         0.0.4
# configobj                          5.0.6
# contextlib2                        0.6.0.post1
# cookiecutter                       1.7.2
# cryptography                       3.4.8
# cycler                             0.10.0
# cymem                              2.0.7
# Cython                             0.29.24
# cytoolz                            0.11.0
# daal4py                            2021.3.0
# dask                               2021.10.0
# debugpy                            1.4.1
# decorator                          5.1.0
# defusedxml                         0.7.1
# dictdiffer                         0.9.0
# diff-match-patch                   20200713
# diskcache                          5.4.0
# distributed                        2021.10.0
# distro                             1.7.0
# Django                             4.0.3
# django-cors-headers                3.11.0
# djangorestframework                3.13.1
# docutils                           0.17.1
# dpath                              2.0.6
# dulwich                            0.20.35
# dvc                                2.10.2
# dvc-render                         0.0.5
# dvclive                            0.7.3
# en-core-web-trf                    3.5.0
# entrypoints                        0.3
# et-xmlfile                         1.1.0
# fastcache                          1.1.0
# filelock                           3.3.1
# flake8                             3.9.2
# Flask                              1.1.2
# flatbuffers                        2.0.7
# flatten-dict                       0.4.2
# flufl.lock                         7.0
# fonttools                          4.25.0
# frozenlist                         1.3.0
# fsspec                             2022.3.0
# ftfy                               6.1.1
# funcy                              1.17
# future                             0.18.2
# gast                               0.4.0
# gevent                             21.8.0
# gitdb                              4.0.9
# GitPython                          3.1.27
# glob2                              0.7
# gmpy2                              2.0.8
# google-api-core                    2.10.1
# google-auth                        1.29.0
# google-auth-oauthlib               0.4.4
# google-cloud-core                  2.3.2
# google-cloud-speech                2.15.1
# google-cloud-storage               2.5.0
# google-crc32c                      1.5.0
# google-pasta                       0.2.0
# google-resumable-media             2.3.3
# googleapis-common-protos           1.56.4
# grandalf                           0.6
# greenlet                           1.1.1
# grpcio                             1.49.1
# grpcio-status                      1.49.1
# h5py                               3.1.0
# HeapDict                           1.0.1
# html5lib                           1.1
# huggingface-hub                    0.15.1
# idna                               3.2
# igraph                             0.9.10
# imagecodecs                        2021.8.26
# imageio                            2.9.0
# imagesize                          1.2.0
# importlib-metadata                 4.8.1
# inflection                         0.5.1
# iniconfig                          1.1.1
# intervaltree                       3.1.0
# ipykernel                          6.4.1
# ipython                            7.29.0
# ipython-genutils                   0.2.0
# ipywidgets                         7.6.5
# isort                              5.9.3
# itsdangerous                       2.0.1
# jdcal                              1.4.1
# jedi                               0.18.0
# Jinja2                             2.11.3
# jinja2-time                        0.2.0
# jmespath                           1.0.0
# joblib                             1.1.0
# json5                              0.9.6
# jsonschema                         3.2.0
# jupyter                            1.0.0
# jupyter-client                     6.1.12
# jupyter-console                    6.4.0
# jupyter-core                       4.8.1
# jupyter-server                     1.4.1
# jupyterlab                         3.2.1
# jupyterlab-pygments                0.1.2
# jupyterlab-server                  2.8.2
# jupyterlab-widgets                 1.0.0
# keras                              2.10.0
# keras-nightly                      2.10.0.dev2022051007
# Keras-Preprocessing                1.1.2
# keyring                            23.1.0
# kiwisolver                         1.3.1
# langcodes                          3.3.0
# lazy-object-proxy                  1.6.0
# leidenalg                          0.8.10
# Levenshtein                        0.21.1
# libarchive-c                       2.9
# libclang                           14.0.1
# llvmlite                           0.37.0
# locket                             0.2.1
# lxml                               4.6.3
# mailchecker                        4.1.16
# Markdown                           3.3.4
# MarkupSafe                         1.1.1
# matplotlib                         3.4.3
# matplotlib-inline                  0.1.2
# mccabe                             0.6.1
# mistune                            0.8.4
# mkl-fft                            1.3.1
# mkl-random                         1.2.2
# mkl-service                        2.4.0
# mock                               4.0.3
# more-itertools                     8.10.0
# mpmath                             1.2.1
# msgpack                            1.0.2
# multidict                          6.0.2
# multipledispatch                   0.6.0
# munkres                            1.1.4
# murmurhash                         1.0.9
# mypy-extensions                    0.4.3
# nanotime                           0.5.2
# navigator-updater                  0.2.1
# nbclassic                          0.2.6
# nbclient                           0.5.3
# nbconvert                          6.1.0
# nbformat                           5.1.3
# nest-asyncio                       1.5.1
# networkx                           2.8.2
# nltk                               3.6.5
# nose                               1.3.7
# notebook                           6.4.5
# numba                              0.54.1
# numexpr                            2.7.3
# numpy                              1.20.3
# numpydoc                           1.1.0
# oauthlib                           3.1.0
# olefile                            0.46
# opencv-python                      4.5.5.64
# openpyxl                           3.0.9
# opt-einsum                         3.3.0
# packaging                          21.0
# pandas                             1.3.4
# pandocfilters                      1.4.3
# parso                              0.8.2
# partd                              1.2.0
# path                               16.0.0
# pathlib2                           2.3.6
# pathspec                           0.9.0
# pathy                              0.10.2
# patsy                              0.5.2
# pep8                               1.7.1
# pexpect                            4.8.0
# phonenumbers                       8.12.47
# pickleshare                        0.7.5
# Pillow                             8.4.0
# pip                                23.1.2
# pkginfo                            1.7.1
# pluggy                             0.13.1
# ply                                3.11
# poyo                               0.5.0
# preshed                            3.0.8
# prometheus-client                  0.11.0
# prompt-toolkit                     3.0.20
# proto-plus                         1.22.1
# protobuf                           3.20.1
# psutil                             5.9.0
# ptyprocess                         0.7.0
# py                                 1.10.0
# pyasn1                             0.4.8
# pyasn1-modules                     0.2.8
# pycodestyle                        2.7.0
# pycosat                            0.6.3
# pycparser                          2.20
# pycurl                             7.44.1
# pydantic                           1.10.9
# pydocstyle                         6.1.1
# pydot                              1.4.2
# pyerfa                             2.0.0
# pyflakes                           2.3.1
# pygit2                             1.9.1
# Pygments                           2.10.0
# pygtrie                            2.4.2
# PyJWT                              2.1.0
# pylint                             2.9.6
# pyls-spyder                        0.4.0
# pyodbc                             4.0.0-unsupported
# pyOpenSSL                          21.0.0
# pyparsing                          3.0.4
# pyrsistent                         0.18.0
# PySocks                            1.7.1
# pytest                             6.2.4
# python-benedict                    0.25.1
# python-dateutil                    2.8.2
# python-fsutil                      0.6.0
# python-igraph                      0.9.10
# python-Levenshtein                 0.21.1
# python-lsp-black                   1.0.0
# python-lsp-jsonrpc                 1.0.0
# python-lsp-server                  1.2.4
# python-slugify                     6.1.2
# pytz                               2021.3
# PyWavelets                         1.1.1
# PyYAML                             6.0
# pyzmq                              22.2.1
# QDarkStyle                         3.0.2
# qstylizer                          0.1.10
# QtAwesome                          1.0.2
# qtconsole                          5.1.1
# QtPy                               1.10.0
# rapidfuzz                          3.1.1
# regex                              2021.8.3
# requests                           2.26.0
# requests-oauthlib                  1.3.0
# rich                               12.3.0
# rope                               0.19.0
# rsa                                4.7.2
# Rtree                              0.9.7
# ruamel.yaml                        0.17.21
# ruamel.yaml.clib                   0.2.6
# ruamel-yaml-conda                  0.15.100
# s3transfer                         0.5.2
# safetensors                        0.3.1
# scikit-image                       0.18.3
# scikit-learn                       1.0.2
# scikit-learn-intelex               2021.20210714.100439
# scipy                              1.8.1
# scmrepo                            0.0.19
# seaborn                            0.11.2
# Send2Trash                         1.8.0
# setuptools                         68.0.0
# shortuuid                          1.0.8
# shtab                              1.5.4
# simplegeneric                      0.8.1
# singledispatch                     3.7.0
# sip                                4.19.13
# six                                1.15.0
# smart-open                         6.3.0
# smmap                              5.0.0
# sniffio                            1.2.0
# snowballstemmer                    2.1.0
# sortedcollections                  2.1.0
# sortedcontainers                   2.4.0
# soupsieve                          2.2.1
# sox                                1.4.1
# spacy                              3.5.4
# spacy-alignments                   0.9.0
# spacy-legacy                       3.0.12
# spacy-loggers                      1.0.4
# spacy-transformers                 1.2.5
# Sphinx                             4.2.0
# sphinxcontrib-applehelp            1.0.2
# sphinxcontrib-devhelp              1.0.2
# sphinxcontrib-htmlhelp             2.0.0
# sphinxcontrib-jsmath               1.0.1
# sphinxcontrib-qthelp               1.0.3
# sphinxcontrib-serializinghtml      1.1.5
# sphinxcontrib-websupport           1.2.4
# spyder                             5.1.5
# spyder-kernels                     2.1.3
# SQLAlchemy                         1.4.22
# sqlparse                           0.4.2
# srsly                              2.4.6
# statsmodels                        0.12.2
# sympy                              1.9
# tables                             3.6.1
# tabulate                           0.8.9
# tb-nightly                         2.10.0a20220510
# TBB                                0.2
# tblib                              1.7.0
# tensorboard                        2.10.0
# tensorboard-data-server            0.6.0
# tensorboard-plugin-wit             1.8.0
# tensorflow                         2.10.0
# tensorflow-estimator               2.10.0
# tensorflow-io-gcs-filesystem       0.25.0
# termcolor                          1.1.0
# terminado                          0.9.4
# testpath                           0.5.0
# text-unidecode                     1.3
# textdistance                       4.2.1
# texttable                          1.6.4
# tf-estimator-nightly               2.10.0.dev2022051008
# tf-nightly                         2.10.0.dev20220510
# thinc                              8.1.10
# threadpoolctl                      2.2.0
# three-merge                        0.1.1
# tifffile                           2021.7.2
# tinycss                            0.4
# tokenizers                         0.13.3
# toml                               0.10.2
# toolz                              0.11.1
# torch                              2.0.1
# tornado                            6.1
# tqdm                               4.64.0
# traitlets                          5.1.0
# transformers                       4.30.2
# typed-ast                          1.4.3
# typer                              0.9.0
# typing_extensions                  4.7.0
# ujson                              4.0.2
# unicodecsv                         0.14.1
# Unidecode                          1.2.0
# urllib3                            1.26.7
# voluptuous                         0.13.1
# wasabi                             1.1.2
# watchdog                           2.1.3
# wcwidth                            0.2.5
# webencodings                       0.5.1
# Werkzeug                           1.0.1
# wheel                              0.40.0
# whichcraft                         0.6.1
# widgetsnbextension                 3.5.1
# wrapt                              1.12.1
# wurlitzer                          2.1.1
# xlrd                               2.0.1
# XlsxWriter                         3.0.1
# xlwings                            0.24.9
# xlwt                               1.3.0
# xmltodict                          0.12.0
# yapf                               0.31.0
# yarl                               1.7.2
# zc.lockfile                        2.0
# zict                               2.0.0
# zipp                               3.6.0
# zope.event                         4.5.0
# zope.interface                     5.4.0