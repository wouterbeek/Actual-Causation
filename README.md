 Actual-Causation
================

Modeling tool for Actual Causation.

# Naive definition of causation

> *A* causes *B* if *B* counterfactually depends on *A*.

E.g., rain causes the streets to get wet, since if it hadn't rained the street would not have been wet.
It is easy to see that this naive definition has its flaw by considering someone emptying a bucket of water on the pavement.
Even if it would not have rained the street would still have been wet.

# Improved definition & axiomatization

The improved definition of causality reads along the following lines:
> *A* causes *B* if *B* counterfactually depends on *A* under some contingency.

This definition can be fleshed out a bit. The following is in line with [Halpern & Pearl 2005](http://bjps.oxfordjournals.org/content/56/4/843.short):

  1. Both the cause and what is caused must be the case.
  2. The endogenous variables can be partitioned into a causal path *Z* and the rest *W*, such that the counterfactual can be shown for some assigment of *W*. Furthermore, all the changing value of the caused in the counterfactual cannot be acchieved by *W* in the actual case.
  3. The cause is minimal.

# A simple example of two causes

Let's take some examples from [Halpern & Hitchcock 2011](http://arxiv.org/abs/1106.2652): lightning may cause a forest to burn; and so does dropping a burning match stick. We represent that either cause suffices with the following structural equation:

> *F #= max(L,M)*

If both causes are needed, *max* would be replaced by *min* (i.e., conjunction as opposed to disjunction).

![](https://rawgit.com/wouterbeek/Actual-Causation/master/img/forest_fire.svg "Trumping example")

# The case of preemption

Suppose a patient has the bad habit of smoking and is developing emphsema in response.
This would eventually

![](https://rawgit.com/wouterbeek/Actual-Causation/master/img/billy_suzy_bottle.svg "Billy & Suzy & bottle")

![](https://cdn.rawgit.com/wouterbeek/Actual-Causation/master/img/trumping.svg "Trumping example")
