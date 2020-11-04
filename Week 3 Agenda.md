# Week 3 Agenda

## 1. Details

### Timed Quiz

- Remember that it is worth 15%: doing well is a gift, doing badly does not mean you fail the class.
- You can Google away
- You can use JupyterLab or the Python Console
- We will not test you on Git or Unix utilities (or Vagrant/Docker!)
- We will only test you on Python
- The scheduled bits of class are only the core so that you know what to practice/look for; you need to help yourself: look for useful explainer videos, practice however you can (I realise that this has been harder than expected), and practice a little bit *regularly*
- Pace: yes, i2p is fast; however, from week 4 it will *start* to slow down as we see how these techniques all fit together. If we spent two weeks on LInux command line utilities you'd probably think it was all a waste of time. If we spent two weeks on Git you'd be just as confused.

### Linux Utilities

- Examinable? Not really, they are intended to be memorable and you will find it useful to memorise the following: `ls`, `gzip`, `du`, `grep`, `wc`, `pwd`, `cat`.

### Git

![](/Users/Shared/i2p/Week 3/Git_and_GitHub.png)

## 2. Techniques

### Deleting From Lists

```python
cities = {'London':1,'Paris':2,'Toronto':3,'Tokyo':4}
result = cities.pop('Birmingham',None)
print(result)
result = cities.pop('Toronto',None)
print(result)
print(cities)
del cities['Toronto']
del cities['London']
print(cities)
```

### Function Definitions

```python
def f(c1, c2, c3):
  print(f"The youngest is {c3}")
f('a','b','c')
f(c3='a', c2='b', c1='c')
f('a', c2='b', c3='c')
f(c1='a', 'b', 'c')
>>> SyntaxError: positional argument follows keyword argument
```

### Function Type *Hints*

```python
def f(c1:str='a', c2:str='b', c3:int=3) -> str:
  print(f"The youngest is {c3}")
```

But...

```python
def f(c1:str, c2:str, c3:str):
  print("The youngest is " + c3)
```

And...

```python
def f(c1:str="a", c2:str="b", c3:str="c") -> None:
  print("The youngest is " + c3)
```

### Logic

We can get `None` back if a `get()` fails to find a key in a dictionary:

```python
x = None
if x:
  print("True")
else:
  print("False")
```

But:

```python
x = None
if not x:
  print("True")
else:
  print("False")
```

## 3. Concepts

> Coming from a background outside of GIS, stats and math and also not having been in university for a little while it was funny to read how different areas felt like they were being under appreciated or devalued by data science. It was just funny to see that no matter what you do people mark their territory and don't like to collaborate with others even though wider sharing will get the best results. 

> the databases, software, and workflow management taught in a given data science masters program are unlikely to be the same as those used by one specific employer."

> Can really see the relation between this article and the structure of i2p from the concept of "flipping the lecture" to importance of anonymous feedback (this padlet!) and need to provide "scaffolding for student learning"

> reassured by Etherington's advocation for a "balance" between open and structured lectures. I think the balance approach is extremely important in order to accommodate all learning styles and individual strengths/weaknesses so that no students are left behind!

> Thought it was odd that Etherington advocated for open-source programming at the beginning of the article emphasizing the accessibility and affordability advantages as well as ability to analyze non-traditional data sets. BUT - then at the end of the article makes a plug for ensuring the importance that programming being taught so that it could be used with ArcGIS. Confusing.

> In making his distinction between predictive and generative modeling, Donoho notes that the former is "effectively silent" about how the data in question are produced. How can predictive models generate future data points without revealing the process by which this would occur? 

## 4. Looking Over at GIS & QM

- In GIS I see: 
  - Git & GitHub (you'll get this next week but it's the *same* process!)
  - Markdown and exporting to PDF (you see this this week and next week and it's the *same* process!)
  - Tidyverse + `sf` (the equivalent could be called the 'pandaverse')
  - Different ways to store Geodata (we will get to this over subsequent weeks)
    - Note that GeoJSON looks a *lot* like a Python dictionary (technically it's from a language that uses similar constructs)
    - Features and Attributes: Nominal, Ordinal, Interval, Ratio, Cyclic
    - Projection 
  - Cheatsheets (Will track down the Python equivalents)
  - R vs Python: really it's *both*/*and*, not *either*/*or* (you can actually have notebooks that mix R and Python but I didn't include this functionality: see `gds`)—I tend to prefer the syntax of Python, the somewhat greater control of Python ecosystem, and the key libs (sklearn, scipy, dumpy, and pandas); I don't see this as a positive:
    ![](/Users/Shared/R Packages.png)
- In QM I see:
  - Types of data (in Python!)
  - Describing centrality: mean, median, mode
  - Describing distributions: variance, standard deviation, IQR + outliers
  - Presenting data: 
    - Decimal places (see: f-strings)
    - Boxplots
    - Distributions and Density Plots (PDFs and CDFs)
    - Transformations: normal -> log-normal / log-linear / log-log
  - Tools:
    - `scipy`
    - More on the way...

