---
title: ANN training visualisation with d3
date: 2016-04-21
tags: computers
requires: vendor/d3.min.js, vendor/jquery-1.12.3.min.js, ann-viz.js
---

One weekend last year I decided an artificial neural network (ANN) training visualisation would be a fun thing to do with d3. It took a little longer than I thought, and `the code <https://github.com/andrewhn/andrewhn.github.io/tree/src/js/ann-viz.es6>`_ could do with a clean-up, but it does what I was hoping it would.
  
.. raw:: html

    <!--more-->

The general sequence, as depicted, is:

- inputs (left) with an associated dependent value (right) are fed into the first layer of the ANN
- the ANN's layers fire (the black dots moving along the synapses) in sequence, with the size of the dot representing the absolute value of the signal (a combination of the synapse weight and the input value)
- the sigmoid functions determine the size of the signal from the previous layers (the red dots denote the output from the sigmoid functions)
- the difference between the answer and the model's guess (error) is calculated
- the error-weighted derivatives are propagated backwards to adjust the synapse weightings (the black fades)
- training inputs/outputs are cycled

The ``Skip 500`` button below runs 500 training iterations instantly. A few thousand iterations (i.e. a couple of clicks) should be enough to have the ANN estimating the output with resonable accuracy.

.. raw:: html

    <button onclick="skip500()">Skip 500</button>
    <div id="ann-viz"></div>
    <style>
      .path {
          stroke: steelblue;
          stroke-width: 2;
          fill: none;
      }
      .axis path,
      .axis line {
          fill: none;
          stroke: grey;
          stroke-width: 1;
          opacity: 0.3;
          shape-rendering: crispEdges;
      }
      .line {
          stroke: black;
          stroke-width: 1;
          fill: none;
      }
    </style>

