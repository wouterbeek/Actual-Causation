TODO
====

Implementation
--------------

  [ ] Swipl:
    [x] BUG: GUI tracer names multiple variables `A`.
  [ ] Libs:
    [x] BUG: Causal graph does not render due to float// bug in plGraphViz.
  [ ] Architecture:
    [x] BUG: Check why simulations no longer work.
    [o] BUG: Multiple assertions of the same cause (in Billy&Suzy model).
        This may be due to non-determinism in calculating condition 2B
        in ac_models.pl.
    [x] Add RDFS schema for AC models.
    [ ] Allow models to be entered via an HTML form.
    [ ] Store structural equations using IRIs and internal structure.
    [ ] Store causal formulas using IRIs and internal structure.
    [x] Rewrite of AC simulator using RDF models.
    [x] RDF term writing in signature HTML table.
    [x] Remove old models.
    [x] Display causal formula in Web interface.
    [x] Make causes in table clickable and color cause in graphic.
    [x] Return causal path next to clause. Color causal path as well as cause.
  [ ] Modeling environment:
    [ ] Finalize CLI + Web UI.
    [ ] Document CLI + Web UI.
    [ ] Top-level (CLI) language translation into causal formalism.
    [ ] Document top-level language.
    [x] Quantify over contexts (potential/actual causer).
    [x] Unit testing framework.
    [o] Unit testing content: Example models from the literature.
  [ ] Modeling:
    [ ] Golden standard: a collection of examples.
    [ ] Expressivity: Can we express common intuitions in terms of causation?
    [ ] Patterns: What are the often occurring composed expressions that constitute causal principles in medical-ethical discourse?
      [ ] Quality Adjusted Life Years (QALY)
    [ ] Create some models.
    [ ] Combine some models (model composition).

Dissemination
-------------

  [ ] Counterfactual reasoning in ClioPatria paper?
    [ ] Discuss with JW whether this is sufficiently novel.
  [ ] Implemented KR paper?
  [ ] Medical-ethics paper:
    [ ] Introduction
    [ ] Related work:
      [ ] Actual Causation (Pearl, Halpern)
      [ ] Causality in ethics (Aristotle, wierd paper, determinism in philosophy of mind).
      [ ] Responsibility (as counterfactual, Frankfurt)
      [ ] Lifestyle differentiation in health care
      [ ] Alt. causation def.
    [ ] Methodology/Approach:
      [ ] Conceptual  analysis: use of an existing axiomatization of causality for modeling responsibility as causation.
    [ ] Implementation: modeling interface for responsibility as causation.
    [ ] Evaluation:
      [ ] Taking an existing example from the ethics domain it is possible to construe a consistent model that supports purported intuitions.
    [ ] Conclusion & future work

Loose ends
----------

  [ ] Responsibility as causation under rationality assumptions.
