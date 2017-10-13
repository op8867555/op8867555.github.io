---
title: '[EN] Use your UNIX toolbox with pandas'
tags: python, pandas, data-analysis, note
---

I do analysis on log / csv / json files with a Python + Pandas + Jupyter Notebook stack at work.
These files are so large that I usually need to do some pre-processing before loading them into memory.

UNIX tools are powerful to stream processing files: concatenation, filtering, transforming...,
but it is annoying to "context-switch" between Jupyter Notebook and shell.
Also, It's generate immediate files that need to remove later manually, quite annoying.

After some stackoverflow-ing, I found a solution[^1] says you can read files in chunks:

~~~~python
df = pd.concat(process(chunk_df) # do processing in python
               for chunk_df in pd.read_csv('bigdata.csv', chunksize=10000))
~~~~

But there is a big overhead to create/process immediate data-frames.


Another solution[^2] says you can use `subprocess` with PIPE, a clever idea.
So I end up with this:


~~~~python
import pandas as pd
from subprocess import Popen, PIPE

with Popen('cat bigdata.csv | grep WHAT_I_CARE_ABOUT | some_transformations',
           shell=True,
           stdout=PIPE) as process:
    df = pd.read_csv(process.stdout)
~~~~

With this trick, I can now do some bash-fu with pandas in Jupyter-Notebook easily.

**Reading multiples files**

~~~~shell
cat stats/2017/*/*.csv
~~~~

**JSON query with jq**

~~~~shell
cat data.json | jq '{.loc = .loc}'
~~~~

**filter lines with awk or grep**

~~~~shell
zcat data.csv.gz | grep 'SOME_TAG' | awk '$1 == 2017'
~~~~

**pipe over files in .tar**

~~~~sh
tar -xOf bigdatas.tar.gz --wildcards "*.data.csv"
~~~~

[^1]: [pandas: filter lines on load in read_csv](https://stackoverflow.com/questions/13651117/pandas-filter-lines-on-load-in-read-csv/13653490#13653490)
[^2]: [How to use subprocess and 'cat' to read in data line by line?](https://stackoverflow.com/questions/39864304/how-to-use-subprocess-and-cat-to-read-in-data-line-by-line/39864368#39864368)
