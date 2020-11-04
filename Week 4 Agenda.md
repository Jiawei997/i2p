# Week 4 Agenda

## Issue Tracker

Just a reminder: I am *far* from perfect. I make typos. I edit things and forget to fix the code because I don't re-run it or think it's to obvious that it *must* be right. I forget to check in a change. If you see something that doesn't make sense ask (ideally in a practical!). If you think something is just plain wrong and have the code to prove it, submit an Issue on GitHub: https://github.com/jreades/i2p/issues

Also, please do the 'early feedback' questionnaire: https://moodle.ucl.ac.uk/mod/questionnaire/view.php?id=2429914

## Assessment #1

Said this on Slack, but you will *only* be tested up to Week 3. Nothing on classes, inheritance, etc. Functions is the end of what you are expected to know.

## Technical Stuff

- Live coding: pace & review; importance of watching recordings if you're confused.

- Indentation: https://docs.python.org/2.0/ref/indentation.html

- Changing the data type to Booleans
  - I had thought (and thought I had tested!) that using `bool('False')` would lead to a `False` value being generated, but it yields a `bool` whose value is actually `True`. That's because the string is *still* defined and therefore evaluates to `True` (unlike `None`). So we had to change this to `val == 'True'` (this could aslo be written `val is 'True'`).
  - How did `col_data` work in my workshop file? `col_data` is defined in the _functions_ and therefore is accessible there. This is about scoping/namespaces. I've not lectured on this specifically, but it came up when I was talking about how some variables could be accessed only inside functions and you've seen it with class inheritance.
  
- Jupyter and Markdown
  - Let's look at what a notebook actually _looks_ like: https://raw.githubusercontent.com/jreades/i2p/master/practicals/Practical-01-Getting_Started.ipynb
  - And this is what raw Markdown looks like: https://raw.githubusercontent.com/jreades/i2p/master/lectures/1.1-Getting_Oriented.md
  - So Jupyter *uses* Markdown but it is *not* Markdown, which is why changing the extension doesn't produce a valid Jupyter notebook. But we *can* export/move between the two (up to a point) using `nbconvert`.
  
- Why can't you have `for k, x, v in dict.items()` to loop over a dict-of-dicts?
  - This is about generalisation: you *could* write your own function to do this if you really think it's important. It's even possible to 'overload' a function whereby you change the way an operator or function works in particular contexts (the `+` concatenating strings is an example of this). 
  - But as a general rule, supporting a dict-of-dicts is a special case: do you have to also support `for k, x, y, v in dict.items()` or `for k, x, y, z, v in dict.items()`? These are getting very hard to read when, in fact, nested `for` loops are cleaner and easier to read as well as being generalisable to any number of nested dictionaries. 

- More vs Less: https://www.tecmint.com/linux-more-command-and-less-command-examples/#:~:text=The%20main%20difference%20between%20more,using%20page%20up%2Fdown%20keys.&text=A%20useful%20feature%20of%20less,word%2Dto%2Dseach%20option. More loads the file. Less does not:

  > The main difference between **more** and **less** is that **less** command is faster because it does not load the entire file at once and allows navigation though file using page **up**/**down** keys.

## Conceptual Stuff

- More on classes: https://youtu.be/ZDa-Z5JzLYM (found by a student!)
  - How do you find that parents of a class... find out this week!
- Where do you put errors? At the top? Somewhere else?
- Unit tests: was really trying to just illustrate how Errors can be useful; hadn't quite expected people to try to run them! Should have thought this through. 🙄
- Error-handling: why do you get ZeroDivisionError even when using `except ZeroDivisionError`? 
  - As I would understand it, it would be because we hadn't finished the `except` block cleanly before the *next* error was raised.

A fuller example of a unit test:

```python
import unittest

def fun(x):
  return x + 1

class MyTest(unittest.TestCase):
  def test(self):
    self.assertEqual(fun(3), 4) # No error
    self.assertEqual(fun(3), 5) # Causes test to fail with AssertionError: 4 != 5

m = MyTest()
m.test()
```

With `doctest` you would have to do this as part of a `.py` executable (e.g. a package!).

- See `doc-test.py` created in `i2p/utils/`.
- `python doc-test.py` to execute.

These are really going to be relevant when you are *developing* software, not doing ad-hoc data analytics. 

## Theoretical Stuff

- **Why is Airbnb managing its data in this way?** Why do *you* think that they thought they could 'get away' with it?
- Were Airbnb fined? What do you think? If they were, do you think it would change their behaviour? 
- **How does it connect to the issues we've read around  in Data Feminism relating to cleaning, perspectives, and purposes?**
- Do you think that Data Feminism is arguing *against* data cleaning? Is there a tradeoff between scale and interpretability? Are there costs to making and keeping data open?
- Importance of standpoint theory and perspective: "You will see differences in our assignment outputs and chosen projects and the way we phrase and present our understandings."
- ***Can* a data analyst avoid being biased when analysing data?**
- **How do we know if the data provided is real and valid enough for analysis?**
- **What might a participatory, grounded process of data cleaning look like?** 
- It was also interesting to read the Data Feminism chapter after the Inside Airbnb report. **The Airbnb data appears to have been collected in quite a solitary manner**, and with a clear purpose and structure of information. Inside Airbnb's aim is to expose the impact of Airbnb on urban residential markets, but after reading D'Ignazio and Klein, I**'ve come to doubt that the data they collected goes far enough to achieve that goal.** 

## Other Stuff

- Couple of students have mentioned it would be nice to see more faces—I don't want to pressure anyone into doing this because I don't know what your situation is, but maybe take a photo of yourself as as default Zoom thing? Turn on video during practicals?

## Optional Extra Session?

Would there be interest in an extra ***optional*** session on **Monday 9 November from 1-3pm**?

Criteria:

- I would like it to be student-led and more discussion focussed (esp. around readings for assessment #2).
- There will be no coding *unless* students want to prepare and demonstrate (I am happy to comment).
- I would like it to be consensus-based.
- It should be recorded so that those who are unable to attend can benefit.
- It does *not* have to be about this class only. It could be about work-life balance or anything else.
- I have created a Padlet for you to use to plan and a Zoom session for you to use, though you may wish to do this all yourselves: https://padlet.com/jreades1/oid259dsyecshff2