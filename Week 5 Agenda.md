# Week 5 Collaborative Agenda

## Assessment #1

- Moodle is the worst quiz software... except for all the others. 
- All questions that have been marked as incorrect *and* have been flagged will be checked and regraded if necessary.
- I will then check through those *emails* that I have received looking for queries that are unlikely to have been regraded as they include unusual or unexpected interpretations of a question (that are *still* plausible!)
- Question from agenda: when will marks be received? 
  - I *do* understand the question, but it's the wrong one: we *will* get your marks right (this does not mean everyone will be happy), but the critical thing is whether you *know* what the answer was and *why*. If you focus on this, then it doesn't matter *when* you get the right marks. 
  - Scale challenge: let's imagine that it takes 1 minute for each flagged question and each student has flagged 3 questions. There are > 150 of you, so that's 8 hours of non-stop work on the quiz corrections.

## Assessment #2: Data Biography

A Markdown document submitted in Week 6 of teaching (after Reading Week). The word limit for this submission is 1,050 words (including the wording of the questions *without* the material in square brackets—**see the [template that I have provided](https://github.com/jreades/i2p/blob/master/assessments/Data_Biography.md)**). The data set for which the 'biography' will be written is the [Inside Airbnb data for London](https://github.com/jreades/i2p/raw/master/data/src/2020-08-24-listings.csv.gz).

This is a structured assessment that includes the following 9 sections:

1. Who collected the data? [25 words; 1 mark; *Consider the source of the data and its relation to the underlying data generating process.*]
2. Why did they collect it? [50 words; 2 marks;  *Consider the purposes for which the data was collected and how this might shape its form/content.*]
3. How was it collected? [75 words; 3 marks; *What was the method by which the data was collected and how might this shape its form/content.*]
4. What useful information does it contain? [100 words; 7 marks; *A general description of key fields and records identified by the student that might support subsequent analysis.*]
5. What useful information is it missing? [50 words; 7 marks; *A general description of key fields and records that a naive, but technically skilled, user might think that the data included but which it does not.*]
6. To what extent is the data ‘complete’? [150 words; 20 marks; *Reflecting on #4 and #5, to what extent is this data a ‘complete’ picture of the process that it claims to allow us to examine?*]
7. What kinds of analysis would this support? [150 words; 15 marks; *Given the limitations and opportunities identified above, what kinds of analysis would this data support?*]
8. What kinds of analysis would it *not* support? [100 words; 15 marks; *Given the limitations and opportunities identified above, what kinds of analysis cannot be undertaken that a naive, but technically skilled, user might think were possible?*]
9. Which of the uses listed in #7 are ethical? [300 words; 30 marks; *Justify the ethics of these analyses with reference to examples drawn from both #7 and #8 and to the literature.*]

In addition to looking for relevant content in [@dignazio:2020] ([URL](https://bookbook.pubpub.org/data-feminism)), you will also want to check consider:

- [@Elwood:2017] [DOI](https://doi.org/10.1080/13658816.2017.1334892)
- [@Elwood:2018] [DOI](https://doi.org/10.1080/0966369X.2018.1465396)
- [@Bemt:2018] [DOI](https://doi.org/10.1080/03098265.2018.1436534)
- [@Amoore:2019] [DOI](https://doi.org/10.1177%2F0263276419851846)
- [@Crawford:2015] [DOI](https://doi.org/10.1007/s10708-014-9597-z)

## Data Cleaning

You should view the work in Practical #5 as a *framework* for processing the full data set as will be called for in Assessment #3; however, these are not the only possible cleaning choices and you should look to *justify* the ones that you made: mine were largley arbitrary for the purposes of demonstrating features of `pandas`. This is why I termed the practical 'Easing into EDA'.

## Start/Stop/Continue

*n*=37

1. Do you understand what you are supposed to be learning?
   Yes: 100%
2. Do you understand how you'll be assessed?
   Yes: 82%
   No: 8%
3. Can you access the learning resources?
   Yes: 100%
4. Do you know who to contact for help?
   Yes: 97%
   No: 3%
5. Continue (*n*=13):
   1. Slack
   2. Demonstration/Live Coding
   3. Practical/Answers Later
   4. Recordings/Markdown/Media Mix
   5. Office Hours
6. Start (*n*=13):
   1. **More applied data analysis** [JR: coming soon... I hope you'll agree!]
   2. **Slow down a bit** [JR: I'm trying!]
   3. **Advice on what to prepare** [JR: see Slack message on: a) finishing up practical; b) videos; c) readings; d) agenda]
   4. **Workshop should be longer** [JR: would you want another 30 minutes?]
   5. **More structure to use of Slack** (or Moodle?) to avoid things being lost [JR: I agree, let's discus]
   6. Small challenges at the end of each practical [JR: will try to think of more options here!]
   7. **More feedback on practicals** [JR: I would *love* to do this, but there are 150 students so it's simply not possible.]
   8. **Encourage students to turn on cameras** [JR: can you please ensure that you add a photo of you doing something fun to your UCL Zoom profile?]
   9. Normal lectures [JR: Slack has an anonymous question/comment option if you'd like to expand on why this would be a benefit]
7. Stop (*n*=10)
   1. **Don't imply there is only one answer** [JR: I never meant to imply this, which is why I always put 'Answer' in single-quotes; if you can get the right result a different way then that is *also* an answer and we can discuss what might make one or other 'better'.]
   2. More time to practice
   3. git is hard [JR: I agree, but hopefully you now see it in the context of GIS too]
   4. **Information dispersed across too many channels (Slack, Moodle, GitHub)** [JR: I agree, let's talk usage.]
   5. Exam is too soon [JR: hopefully now you see it's not an exam]
   6. Live coding is too quick [JR: yes, I know. This is why I record it too but I hope that things will slow down a *bit* going forwards.]

## Practical #4 Follow Up

- What's a constructor?

## Techniques

- Hashing & Digests: how are they useful? 
  - Fundamental CS concept with relevance for random number generation, security, authentication, and encryption—you would be expected to know about this in a dev context and it's a useful extension of what's covered with random number generation.
  - Impacts performance of dictionaries and other data structures at scale
  - Check for changes to data files in processing pipelines (this is more common than you might think!) and validate row-level changes (also more common than you might think)
  - Points towards other issues is programming in general (and see *Triumphs* on understanding seeds!)
- Bit-wise operators
  - See a lot more of this today basically.
  - Pandas queries look like: `(df.<column1>=='foo') & (df.<column2>=='bar')` instead of `df.<column1>=='foo' and df.<column2>=='bar'`.
- Set difference:
  - Potato isn't printed because it's the elements of set 1 than *aren't* in set 2.
  - If you want what's missing from both you need `symmetric _difference`.
- Seeding
  - Seeding is just initialising a random number generator, nothing more.
  - If you want the state you have to ask for it (`get_state()`) just like `random`.
- Information Overload.
  - Have tried to reduce amount of reading, though you will need to do this for the final assessment. Hoping that re-watching a 15-minute video is *still* easier than reading 3 articles!

## Concepts

- *Invisible Women* is an excellent complement to *Data Feminism* and together can be used to contextualise two other questions today:
  1. Wachsmuth & Weisler's inferences about availability — how did they conclude that "whole-unit listings rented 60 days or more and available 120 days or more" represented units without full-time tenants? But also note link to 'rent-gap theory'.
  2. "This course is actually a campaign against airbnb" — I'm assuming this is sarcastic, but hard for me to tell on what level this is meant: as a joke (yay!) or as dismissal of a need for critical thinking about data and data analysis (boo!). 
  3. My argument: it's easy to find think-pieces that argue "This is disruptive innovation, regulation is for dinosaurs" and a lot harder to find critical pieces; so I've basically asked you to read three critical works to set out a different way of viewing their impact. We have two 'sides' of this story that are, apparently, looking at the 'same data' and coming to fundamentally different views of what that data means. 
  4. Please also look at the guidance for the final assessment.

- Examples of some research I've got going on around gender and ethnicity in doctoral research: problems around bias (in training and testing data); around inference (what, exactly, are we inferring?); etc.
- We are *always* dealing with unintended consequences: Airbnb's response to discrimination against African Americans (instant booking) is now being used to party through Covid: https://www.nytimes.com/2020/10/27/business/airbnb-party-house-coronavirus.html
  - Of course, the pandemic has rather changed the dynamic! Is it time for a rethink? Or has nature taken care of our housing challenges? <Note: this is intended to be provocative>
- Morozov's (2013) *To Save Everything Click Here!* is also a good (if slightly long-winded) critique of the tech industry and it's idea that technology is the solution to everything [Review](https://www.theguardian.com/global/2013/mar/20/save-everything-evgeny-morozov-review)