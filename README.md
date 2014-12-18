 Actual-Causation
================

Modeling tool for Actual Causation.

# Naive definition of causation

> *A* causes *B* if *B* counterfactually depends on *A*.

E.g., rain causes the streets to get wet, since if it hadn't rained the street would not have been wet.
It is easy to see that this naive definition has its flaw by considering someone emptying a bucket of water on the pavement.
Even if it would not have rained the street would still have been wet.

# Improved definition & formalization

The improved definition of causality reads along the following lines:
> *A* causes *B* if *B* counterfactually depends on *A* under some contingency.

This definition can be fleshed out a bit. The following is in line with [Halpern & Pearl 2005](http://bjps.oxfordjournals.org/content/56/4/843.short):

  1. Both the cause and what is caused must be the case.
  2. The endogenous variables can be partitioned into a causal path *Z* and the rest *W*, such that the counterfactual can be shown for some assignment of *W*. Furthermore, all the changing value of the caused in the counterfactual cannot be achieved by *W* in the actual case.
  3. The cause is minimal.

# A simple example of two causes

Let's take some examples from [Halpern & Hitchcock 2011](http://arxiv.org/abs/1106.2652): lightning may cause a forest to burn; and so does dropping a burning match stick. We represent that either cause suffices with the following structural equation:

> *F #= max(L,M)*

If both causes are needed, *max* would be replaced by *min* (i.e., conjunction as opposed to disjunction).

![](https://rawgit.com/wouterbeek/Actual-Causation/master/img/forest_fire.svg "Trumping example")

# The case of preemption

Suppose a patient has the bad habit of smoking and is developing emphysema in response.
This would eventually lead to chronic bronchitis which can lead to serious health problems that are also costly to treat.
While the patient in still in the early stages of emphysema, he visits the hospital for a bronchoscopy to look for the cause of his health problems.
Alas, the patient happens to be treated by a doctor who is quite negligent. Due to the doctor's negligence the bronchoscopy leads to complicates, damaging the patient's lungs.
Now the patient has chronic bronchitis immediately.
The doctor does not want to get sued, since the patient would have become sick eventually anyhow.
Intuitively one may agree that the doctor is the cause of the patient's chronic bronchitis, even though the patient would have had chronic bronchitis even if the doctor would not have been negligent.

Cases like these are called cases of **preemption** and they are quite difficult to model. However, using structural equations this is not so difficult after all:

> NegligenceDamages #= Negligent

> Emphysema #= min(Smokes,1-NegligenceDamages)

> Bronchitis #= max(Emphysema,NegligenceDamages)

![](https://rawgit.com/wouterbeek/Actual-Causation/master/img/billy_suzy_bottle.svg "Billy & Suzy & bottle")

# Authority in medicine

Suppose a doctor and a quack are both advising a patient as to whether she should undergo some form of treatment.
Intuitively, the doctor may be considered more authoritative than the quack, and therefore more responsible in case he is advising the patient in a wrong way (let's assume that it is good for the patient to undergo this particular treatment).

If the doctor advises to undergo the treatment the patient does so. The doctor's advise is causing her to make this decision. The same is true if the doctor advises her to abstain from treatment.

However, if the doctor does not give a specific verdict (e.g., he does not know what to advise), the patient will listen to the advise of the quack. Even though the quack may give the same advise in these situations, the quack will only become a cause of the patient's decision when the doctor gives no advice. In the latter case, both the doctor and the quack are causing the patient to decide as she does.

![](https://cdn.rawgit.com/wouterbeek/Actual-Causation/master/img/trumping.svg "Trumping example")
