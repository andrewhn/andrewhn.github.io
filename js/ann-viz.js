"use strict";

var _slicedToArray = function () { function sliceIterator(arr, i) { var _arr = []; var _n = true; var _d = false; var _e = undefined; try { for (var _i = arr[Symbol.iterator](), _s; !(_n = (_s = _i.next()).done); _n = true) { _arr.push(_s.value); if (i && _arr.length === i) break; } } catch (err) { _d = true; _e = err; } finally { try { if (!_n && _i["return"]) _i["return"](); } finally { if (_d) throw _e; } } return _arr; } return function (arr, i) { if (Array.isArray(arr)) { return arr; } else if (Symbol.iterator in Object(arr)) { return sliceIterator(arr, i); } else { throw new TypeError("Invalid attempt to destructure non-iterable instance"); } }; }();

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

function _toConsumableArray(arr) { if (Array.isArray(arr)) { for (var i = 0, arr2 = Array(arr.length); i < arr.length; i++) { arr2[i] = arr[i]; } return arr2; } else { return Array.from(arr); } }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

// layout params
var _neuronMultiplier = 5,
    // rel sizes
_synapseMultiplier = 5,
    _speed = 100; // animation speed

// number format
var fmt = function fmt(x) {
  return (Math.round(x * 100) / 100).toString().replace("0.", ".");
};

var reverse = function reverse(l) {
  var reversed = [];
  for (var i = l.length - 1; i >= 0; i--) {
    reversed.push(l[i]);
  };
  return reversed;
};

// fire function - sigmoid
var f = function f(x) {
  return 1 / (1 + Math.exp(-x));
};

// derivative
var df = function df(x) {
  return f(x) * (1 - f(x));
};

// ANN related calculations

var ANN = function () {
  function ANN(hidden, X, y) {
    var _this = this;

    _classCallCheck(this, ANN);

    // first nodes determined by the input
    var structure = [X[0].length].concat(_toConsumableArray(hidden), [1]);
    this._lastLayer = structure.length - 1;

    // make neurons
    this._flatNodes = [];
    structure.forEach(function (nodesInLayer, layerIndex) {
      d3.range(nodesInLayer).forEach(function (nodeIndex) {
        _this._flatNodes.push({
          id: layerIndex + "-" + nodeIndex,
          // structural
          layerIndex: layerIndex,
          nodeIndex: nodeIndex,
          leftSynapses: [],
          rightSynapses: []
        });
      });
    });
    this._nodes = d3.nest().key(function (d) {
      return d.layerIndex;
    }).entries(this._flatNodes);
    this._layerNodes = structure.map(function (_, i) {
      return _this._nodes[i].values;
    });

    // make synapses
    this._synapses = [];
    structure.forEach(function (_, layerIndex) {
      if (layerIndex === _this._lastLayer) return;
      _this._nodes[layerIndex].values.forEach(function (leftNode, l) {
        _this._nodes[layerIndex + 1].values.forEach(function (rightNode, r) {
          var weight = 2 * Math.random() - 1;
          var synapse = {
            id: layerIndex + "-" + l + "-" + r,
            leftNode: leftNode,
            rightNode: rightNode,
            weight: weight,
            layer: layerIndex
          };
          _this._synapses.push(synapse);
          leftNode.rightSynapses.push(synapse);
          rightNode.leftSynapses.push(synapse);
        });
      });
    });

    // format inputs and outputs to play nicely with d3
    this._X = X.map(function (line, i) {
      return line.map(function (xValue, j) {
        return {
          id: i + "-" + j,
          offset: j,
          value: xValue,
          placeInLine: i + 2
        };
      });
    }).reduce(function (x, y) {
      return x.concat(y);
    }, []);

    this._y = y.map(function (yValue, i) {
      return {
        id: "" + i,
        value: yValue,
        placeInLine: i + 2
      };
    });
  }

  _createClass(ANN, [{
    key: "calculate",
    value: function calculate() {
      var _this2 = this;

      // shuffle input/output data in queue
      [this._X, this._y].forEach(function (l) {
        l.forEach(function (v) {
          return v.placeInLine = --v.placeInLine || _this2._y.length;
        });
      });

      var input = this._X.filter(function (x) {
        return x.placeInLine == 1;
      }).map(function (x) {
        return x.value;
      });

      var _y$filter = this._y.filter(function (x) {
        return x.placeInLine == 1;
      });

      var _y$filter2 = _slicedToArray(_y$filter, 1);

      var answer = _y$filter2[0];


      this._flatNodes.forEach(function (n) {
        n.sigX = 0;
        n.sigY = 0;
        n.error = 0;
        n.pulseSize = 0; // only nodes that fire should show pulse effect
      });

      // feed forward through each layer
      this._layerNodes.forEach(function (layer) {
        layer.forEach(function (node, i) {
          if (node.layerIndex === 0) {
            node.sigY = input[i];
          } else {
            node.sigY = f(node.sigX);
          }
          node.rightSynapses.forEach(function (s) {
            s.fire = node.sigY * s.weight;
            s.rightNode.sigX += s.fire;
            node.pulseSize += s.fire;
          });
        });
      });

      var _nodes$_lastLayer$val = _slicedToArray(this._nodes[this._lastLayer].values, 1);

      var lastNode = _nodes$_lastLayer$val[0];

      lastNode.error = answer.value - lastNode.sigY;

      // back propogate
      reverse(this._layerNodes).forEach(function (layer) {
        layer.forEach(function (node, i) {
          var x = node.sigX,
              y = f(x),
              M = df(x),
              c = y - M * x;
          // line to draw showing derivative
          node.sigDerivDisplay = {
            x1: x - 1,
            x2: x + 1,
            y1: M * (x - 1) + c,
            y2: M * (x + 1) + c
          };
          // error weighted derivative
          node.delta = node.error * M;
          node.leftSynapses.forEach(function (synapse) {
            synapse.leftNode.error += node.delta * synapse.weight;
            synapse.weight += synapse.leftNode.sigY * node.delta;
          });
        });
      });
    }
  }, {
    key: "getError",
    value: function getError() {
      var _nodes$_lastLayer$val2 = _slicedToArray(this._nodes[this._lastLayer].values, 1);

      var lastNode = _nodes$_lastLayer$val2[0];

      return lastNode.error;
    }
  }, {
    key: "getOutput",
    value: function getOutput() {
      var _nodes$_lastLayer$val3 = _slicedToArray(this._nodes[this._lastLayer].values, 1);

      var lastNode = _nodes$_lastLayer$val3[0];

      return lastNode.sigY;
    }
  }, {
    key: "setLayoutSize",
    value: function setLayoutSize(xNodeSpacing, yNodeSpacing) {
      this._flatNodes.forEach(function (n) {
        n.x = xNodeSpacing * (n.layerIndex + 1);
        n.y = yNodeSpacing[n.layerIndex] * (n.nodeIndex + 1);
      });
    }
  }]);

  return ANN;
}();

var initialDraw = function initialDraw(elm, width, height, ann) {

  d3.select(elm).selectAll("*").remove();

  var layout = {
    width: width,
    height: height
  };
  layout.figureHeight = Math.min(height, width / 1.5);
  layout.circleRadius = layout.figureHeight / 12;
  // pythagoras gives size of square in circle
  layout.sigPlotSize = Math.sqrt(Math.pow(layout.circleRadius * 2, 2) / 2);
  layout.speed = _speed;
  layout.fontHeight = layout.figureHeight / 10;
  layout.plotMargins = {
    t: layout.sigPlotSize / 20,
    b: layout.sigPlotSize / 20,
    l: layout.sigPlotSize / 20,
    r: layout.sigPlotSize / 20
  };

  var svg = d3.select(elm).append("svg");
  svg.attr("width", layout.width).attr("height", layout.height);

  var mainGroup = svg.append("g");

  mainGroup.append("rect").attr("width", layout.width).attr("height", layout.figureHeight).attr("fill", "gray").attr("opacity", 0.05);

  // get x, y intervals for node placement
  layout.xNodeSpacing = width / (ann._layerNodes.length + 1);
  layout.yNodeSpacing = ann._layerNodes.map(function (n) {
    return layout.figureHeight / (n.length + 1);
  });
  ann.setLayoutSize(layout.xNodeSpacing, layout.yNodeSpacing);

  // right IO group should be under the nodes
  layout.rightIOG = mainGroup.append("g");
  // synapses should be under the nodes
  layout.synapseG = mainGroup.append("g").attr("id", "synapses");
  layout.nodeG = mainGroup.append("g").attr("id", "nodes");
  // left IO group should be above nodes
  layout.leftIOG = mainGroup.append("g");

  var nodes = layout.nodeG.selectAll(".node").data(ann._flatNodes, function (d) {
    return d.id;
  }).enter().append("g");

  var synapses = layout.synapseG.selectAll(".synapse").data(ann._synapses, function (d) {
    return d.id;
  }).enter().append("g");

  nodes.append("circle").attr("cx", function (d) {
    return d.x;
  }).attr("cy", function (d) {
    return d.y;
  }).classed("node-border", true).attr("r", layout.circleRadius).attr("fill", "white").attr("stroke-width", 1).attr("stroke", "black");

  var sigP = layout.sigPlotSize / 2 - layout.plotMargins.t;
  var nodePlots = nodes.filter(function (n) {
    return n.layerIndex !== 0;
  }).append("g").attr("transform", function (d) {
    return "translate(" + (d.x - sigP) + "," + (d.y - sigP) + ")";
  });

  layout.pltwidth = layout.sigPlotSize - layout.plotMargins.l - layout.plotMargins.r;
  layout.pltheight = layout.sigPlotSize - layout.plotMargins.t - layout.plotMargins.b;
  layout.x = d3.scale.linear().range([0, layout.pltwidth]).clamp(true).domain([-6, 6]);
  layout.y = d3.scale.linear().range([layout.pltheight, 0]).domain([0, 1]);
  var xAxis = d3.svg.axis().scale(layout.x).tickSize(0, 0).tickFormat("").orient("bottom");
  var yAxis = d3.svg.axis().scale(layout.y).tickSize(0, 0).tickFormat("").orient("left");
  var sigLine = d3.svg.line().x(function (d) {
    return layout.x(d.x);
  }).y(function (d) {
    return layout.y(d.y);
  });
  var sigLineData = d3.range(-6, 6.25, 0.25).map(function (x) {
    return {
      x: x,
      y: f(x)
    };
  });

  nodePlots.append("g").attr("class", "axis").attr("transform", "translate(0," + layout.pltheight + ")").call(xAxis);

  nodePlots.append("g").attr("transform", "translate(" + layout.pltwidth / 2 + ",0)").attr("class", "axis").call(yAxis);

  nodePlots.append("path").attr("class", "line").attr("d", sigLine(sigLineData));

  nodePlots.append("circle").attr("class", "sigdot").attr("r", 0).attr("fill", "red").attr("cx", function (d) {
    return layout.x(d.sigX);
  }).attr("cy", function (d) {
    return layout.y(d.sigY);
  });

  nodePlots.append("line").attr("class", "deriv").attr("opacity", 0).attr("stroke", "red").attr("stroke-width", 2);

  synapses.append("line").attr("x1", function (d) {
    return d.leftNode.x;
  }).attr("x2", function (d) {
    return d.rightNode.x;
  }).attr("y1", function (d) {
    return d.leftNode.y;
  }).attr("y2", function (d) {
    return d.rightNode.y;
  }).attr("stroke", "gray").attr("stroke-width", function (d) {
    return _synapseMultiplier * Math.abs(d.weight);
  }).attr("opacity", 0.5);

  layout.rightIOG.attr("transform", "translate(" + ann._layerNodes.length * layout.xNodeSpacing / 1.05 + "," + (layout.figureHeight / 2 + layout.fontHeight / 3) + ")");

  layout.rightIOG.selectAll("text.target-values").data(ann._y, function (d) {
    return d.id;
  }).enter().append("text").classed("target-values", true).attr("font-size", layout.fontHeight + "px").attr("text-anchor", "left").attr("fill", "gray").attr("transform", function (d) {
    return "translate(" + (layout.xNodeSpacing / 2 + (d.placeInLine - 1) * layout.xNodeSpacing / 2) + ", " + -layout.fontHeight + ")";
  }).attr("opacity", 0.5).text(function (d) {
    return d.value;
  });

  // draw on the initial inputs waiting
  layout.leftIOG.attr("transform", "translate(0," + (layout.yNodeSpacing[0] + layout.fontHeight / 3) + ")");

  layout.leftIOG.selectAll("text").data(ann._X, function (d) {
    return d.id;
  }).enter().append("g").attr("transform", function (d) {
    return "translate(0," + layout.yNodeSpacing[0] * d.offset + ")";
  }).append("text").classed("input-values", true).attr("font-size", layout.fontHeight + "px").attr("text-anchor", "middle").attr("fill", "gray").attr("opacity", 0.5).text(function (d) {
    return d.value;
  });

  return layout;
};

// number of nodes in each hidden layer
var structure = [4];
var trainingXs = [[0, 0, 1], [0, 1, 1], [1, 0, 1], [1, 1, 1]];
var trainingYs = [0, 1, 1, 0];

var ann = new ANN(structure, trainingXs, trainingYs);
ann.calculate();

var scrollInputs = function scrollInputs(ann, layer, layout) {
  d3.selectAll("text.input-values").data(ann._X, function (d) {
    return d.id;
  }).transition().duration(5 * layout.speed).attr("transform", function (d) {
    return "translate(" + (4 - d.placeInLine) * layout.xNodeSpacing / 3 + ",0)";
  }).call(endall, function () {
    return pulse(ann, layer, layout);
  });

  d3.selectAll("text.target-values").data(ann._y, function (d) {
    return d.id;
  }).transition().duration(5 * layout.speed).attr("transform", function (d) {
    return "translate(" + (layout.xNodeSpacing / 2 + (d.placeInLine - 1) * layout.xNodeSpacing / 2) + ", " + -layout.fontHeight + ")";
  });
};

var scrollOutputs = function scrollOutputs(ann, layer, layout) {

  var removeAndMove = function removeAndMove() {
    sel.transition().duration(5 * layout.speed).attr("opacity", 0).remove();

    layout.rightIOG.selectAll("text.error").transition().duration(5 * layout.speed).attr("transform", "translate(0,0)").attr("text-anchor", "left").attr("opacity", 0).call(endall, function () {
      return drawDerivative(ann, layer, layout);
    });
  };

  var addErrorTerm = function addErrorTerm() {

    var err = layout.rightIOG.append("text").attr("font-size", layout.fontHeight + "px").attr("text-anchor", "left").attr("fill", "gray").attr("opacity", 0).classed("error", true).text(fmt(ann.getError())).attr("transform", "translate(" + layout.xNodeSpacing / 2 + ",0)").transition().duration(5 * layout.speed).attr("transform", "translate(" + layout.xNodeSpacing / 2 + "," + layout.fontHeight + ")").attr("opacity", 0.5).call(endall, removeAndMove);
  };

  var sel = layout.rightIOG.selectAll("text.attempt").data([ann.getOutput()]);

  sel.enter().append("text").classed("attempt", true).attr("font-size", layout.fontHeight + "px").attr("text-anchor", "left").attr("fill", "gray").attr("opacity", 0).text(function (d) {
    return fmt(d);
  });

  sel.transition().duration(3 * layout.speed).attr("opacity", 0.5).attr("transform", "translate(" + layout.xNodeSpacing / 2 + ",0)").call(endall, addErrorTerm);
};

var fire = function fire(ann, layer, layout) {
  var fireData = ann._synapses.filter(function (s) {
    return s.layer == layer;
  });
  layout.synapseG.selectAll(".fire").data(fireData).enter().append("circle").attr("cx", function (d) {
    return d.leftNode.x;
  }).attr("cy", function (d) {
    return d.leftNode.y;
  }).attr("r", function (d) {
    return Math.abs(d.fire) * _neuronMultiplier;
  }).attr("fill", "black").transition().duration(15 * layout.speed).attr("cx", function (d) {
    return d.rightNode.x;
  }).attr("cy", function (d) {
    return d.rightNode.y;
  }).transition().remove();

  // use timeout here rather than endall because we don't want
  // to wait for the pulse to hit the center of the node (the
  // delay looks odd)
  setTimeout(function () {
    return drawSig(ann, layer + 1, layout);
  }, 10 * layout.speed);
};

var drawSig = function drawSig(ann, layer, layout) {
  var sel = layout.nodeG.selectAll(".sigdot").data(ann._nodes[layer].values, function (d) {
    return d.id;
  });

  sel.attr("cx", function (d) {
    return layout.x(d.sigX);
  }).attr("cy", function (d) {
    return layout.y(d.sigY);
  }).transition().duration(1 * layout.speed).attr("r", 5).call(endall, function () {
    return pulse(ann, layer, layout);
  }).transition().duration(15 * layout.speed).attr("r", 0);
};

var drawDerivative = function drawDerivative(ann, layer, layout) {
  layout.nodeG.selectAll("line.deriv").data(ann._nodes[layer].values, function (d) {
    return d.id;
  }).attr("x1", function (d) {
    return layout.x(d.sigDerivDisplay.x1);
  }).attr("x2", function (d) {
    return layout.x(d.sigDerivDisplay.x1);
  }).attr("y1", function (d) {
    return layout.y(d.sigDerivDisplay.y1);
  }).attr("y2", function (d) {
    return layout.y(d.sigDerivDisplay.y1);
  }).attr("opacity", 1).transition().duration(5 * layout.speed).attr("x2", function (d) {
    return layout.x(d.sigDerivDisplay.x2);
  }).attr("y2", function (d) {
    return layout.y(d.sigDerivDisplay.y2);
  }).call(endall, function () {
    return sizeSynapses(ann, layer - 1, layout);
  }).transition().duration(15 * layout.speed).attr("x1", function (d) {
    return layout.x(d.sigDerivDisplay.x2);
  }).attr("y1", function (d) {
    return layout.y(d.sigDerivDisplay.y2);
  });
};

var sizeSynapses = function sizeSynapses(ann, layer, layout) {

  var data = ann._nodes[layer].values.map(function (node) {
    return node.rightSynapses;
  }).reduce(function (x, y) {
    return x.concat(y);
  }, []);

  var done = function done() {
    if (layer === 0) {
      ann.calculate();
      scrollInputs(ann, layer, layout);
    } else {
      drawDerivative(ann, layer, layout);
    }
  };

  layout.synapseG.selectAll("line").data(data, function (d) {
    return d.id;
  }).transition().duration(5 * layout.speed).attr("opacity", 1).attr("stroke", "black").attr("stroke-width", function (d) {
    return 5 * Math.abs(d.weight);
  }).transition().duration(5 * layout.speed).attr("opacity", 0.5).attr("stroke", "gray").call(endall, done);
};

var pulse = function pulse(ann, layer, layout) {
  var selection = layout.nodeG.selectAll("circle.node-border").filter(function (n) {
    return n.pulseSize && n.layerIndex == layer;
  });

  selection.transition().duration(layout.speed * 2).attr("stroke-width", 5).transition().duration(layout.speed * 2).attr("stroke-width", 1);

  if (layer == ann._lastLayer) {
    scrollOutputs(ann, layer, layout);
  } else {
    fire(ann, layer, layout);
  }
};

// wait for all transitions to finish before calling callback
// from mbostok's answer here
// https://groups.google.com/forum/#!topic/d3-js/WC_7Xi6VV50
function endall(transition, callback) {
  var n = 0;
  transition.each(function () {
    ++n;
  }).each("end", function () {
    if (! --n) callback.apply(this, arguments);
  });
}

var _skip = function _skip(n) {
  var i = 0;
  while (i++ < n) {
    ann.calculate();
  }
};

var skip500 = _skip.bind(null, 500);

document.addEventListener('DOMContentLoaded', function () {

  var elt = $("#ann-viz"),
      w = elt.width(),
      h = w / 1.5,
      layout = initialDraw("#ann-viz", w, h, ann);

  $(window).resize(function () {
    var w = $(elt).width();
    var h = $(elt).height();
    setTimeout(function () {
      // only redraw once we've finished resizing
      var newW = $(elt).width();
      var newH = $(elt).height();
      if (newW === w && newH === h) {
        layout = initialDraw("#ann-viz", newW, newW / 1.5, ann);
        scrollInputs(ann, 0, layout);
      }
    }, 300);
  });

  scrollInputs(ann, 0, layout);
}, false);

