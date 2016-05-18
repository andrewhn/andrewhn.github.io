// layout params
const _neuronMultiplier = 5,  // rel sizes
      _synapseMultiplier = 5,
      _speed = 100;  // animation speed

// number format
let fmt = x => (Math.round(x * 100) / 100).toString().replace("0.", ".");

let reverse = (l) => {
  let reversed = [];
  for (var i = l.length - 1; i >= 0; i--) {
    reversed.push(l[i]);
  };
  return reversed;
}

// fire function - sigmoid
let f = (x) => 1 / (1 + Math.exp(-x));

// derivative
let df = (x) => f(x) * (1 - f(x));

// ANN related calculations
class ANN {

  constructor(hidden, X, y) {
    // first nodes determined by the input
    let structure = [X[0].length, ...hidden, 1];
    this._lastLayer = structure.length - 1;

    // make neurons
    this._flatNodes = [];
    structure.forEach((nodesInLayer, layerIndex) => {
      d3.range(nodesInLayer).forEach(nodeIndex => {
        this._flatNodes.push({
          id: `${layerIndex}-${nodeIndex}`,
          // structural
          layerIndex: layerIndex,
          nodeIndex: nodeIndex,
          leftSynapses: [],
          rightSynapses: [],
        });
      });
    });
    this._nodes = d3.nest()
      .key(d => d.layerIndex)
      .entries(this._flatNodes);
    this._layerNodes = structure.map((_, i) => this._nodes[i].values);

    // make synapses
    this._synapses = [];
    structure.forEach((_, layerIndex) => {
      if (layerIndex === this._lastLayer) return;
      this._nodes[layerIndex].values.forEach((leftNode, l) => {
        this._nodes[layerIndex + 1].values.forEach((rightNode, r) => {
          let weight = 2 * Math.random() - 1;
          let synapse = {
            id: `${layerIndex}-${l}-${r}`,
            leftNode: leftNode,
            rightNode: rightNode,
            weight: weight,
            layer: layerIndex,
          };
          this._synapses.push(synapse);
          leftNode.rightSynapses.push(synapse);
          rightNode.leftSynapses.push(synapse);
        });
      });
    });

    // format inputs and outputs to play nicely with d3
    this._X = X.map((line, i) => line.map((xValue, j) => ({
      id: `${i}-${j}`,
      offset: j,
      value: xValue,
      placeInLine: i + 2,
    }))).reduce((x, y) => x.concat(y), []);

    this._y = y.map((yValue, i) => ({
      id: `${i}`,
      value: yValue,
      placeInLine: i + 2,
    }));
  }

  calculate() {

    // shuffle input/output data in queue
    [this._X, this._y].forEach(l => {
      l.forEach(v => v.placeInLine = --v.placeInLine || this._y.length);
    });

    let input = this._X
      .filter(x => x.placeInLine == 1)
      .map(x => x.value);
    let [answer] = this._y
      .filter(x => x.placeInLine == 1);

    this._flatNodes.forEach(n => {
      n.sigX = 0;
      n.sigY = 0;
      n.error = 0;
      n.pulseSize = 0;  // only nodes that fire should show pulse effect
    });

    // feed forward through each layer
    this._layerNodes.forEach(layer => {
      layer.forEach((node, i) => {
        if (node.layerIndex === 0) {
          node.sigY = input[i];
        } else {
          node.sigY = f(node.sigX);
        }
        node.rightSynapses.forEach(s => {
          s.fire = node.sigY * s.weight;
          s.rightNode.sigX += s.fire;
          node.pulseSize += s.fire;
        });
      });
    });

    let [lastNode] = this._nodes[this._lastLayer].values;
    lastNode.error = answer.value - lastNode.sigY;

    // back propogate
    reverse(this._layerNodes).forEach(layer => {
      layer.forEach((node, i) => {
        let x = node.sigX,
            y = f(x),
            M = df(x),
            c = y - M * x;
        // line to draw showing derivative
        node.sigDerivDisplay = {
          x1: x - 1,
          x2: x + 1,
          y1: M * (x - 1) + c,
          y2: M * (x + 1) + c,
        };
        // error weighted derivative
        node.delta = node.error * M;
        node.leftSynapses.forEach(synapse => {
          synapse.leftNode.error += node.delta * synapse.weight;
          synapse.weight += synapse.leftNode.sigY * node.delta;
        });
      });
    });

  }

  getError() {
    let [lastNode] = this._nodes[this._lastLayer].values;
    return lastNode.error;
  }

  getOutput() {
    let [lastNode] = this._nodes[this._lastLayer].values;
    return lastNode.sigY;
  }

  setLayoutSize(xNodeSpacing, yNodeSpacing) {
    this._flatNodes.forEach(n => {
      n.x = xNodeSpacing * (n.layerIndex + 1);
      n.y = yNodeSpacing[n.layerIndex] * (n.nodeIndex + 1);
    });
  }

}

let initialDraw = (elm, width, height, ann) => {

  d3.select(elm).selectAll("*").remove();

  let layout = {
    width: width,
    height: height
  };
  layout.figureHeight = Math.min(height, width / 1.5);
  layout.circleRadius = layout.figureHeight / 12;
  // pythagoras gives size of square in circle
  layout.sigPlotSize = Math.sqrt((Math.pow(layout.circleRadius * 2, 2) / 2));
  layout.speed = _speed;
  layout.fontHeight = layout.figureHeight / 10;
  layout.plotMargins = {
    t: layout.sigPlotSize / 20,
    b: layout.sigPlotSize / 20,
    l: layout.sigPlotSize / 20,
    r: layout.sigPlotSize / 20
  }

  let svg = d3.select(elm).append("svg");
  svg.attr("width", layout.width)
     .attr("height", layout.height);

  let mainGroup = svg.append("g");

  mainGroup.append("rect")
    .attr("width", layout.width)
    .attr("height", layout.figureHeight)
    .attr("fill", "gray")
    .attr("opacity", 0.05);

  // get x, y intervals for node placement
  layout.xNodeSpacing = width / (ann._layerNodes.length + 1);
  layout.yNodeSpacing = ann._layerNodes.map(n => layout.figureHeight / (n.length + 1));
  ann.setLayoutSize(layout.xNodeSpacing, layout.yNodeSpacing);

  // right IO group should be under the nodes
  layout.rightIOG = mainGroup.append("g");
  // synapses should be under the nodes
  layout.synapseG = mainGroup.append("g")
    .attr("id", "synapses");
  layout.nodeG = mainGroup.append("g")
    .attr("id", "nodes");
  // left IO group should be above nodes
  layout.leftIOG = mainGroup.append("g");

  let nodes = layout.nodeG.selectAll(".node")
    .data(ann._flatNodes, d => d.id)
      .enter().append("g");

  let synapses = layout.synapseG.selectAll(".synapse")
    .data(ann._synapses, d => d.id)
    .enter().append("g");

  nodes.append("circle")
    .attr("cx", d => d.x)
    .attr("cy", d => d.y)
    .classed("node-border", true)
    .attr("r", layout.circleRadius)
    .attr("fill", "white")
    .attr("stroke-width", 1)
    .attr("stroke", "black");

  let sigP = layout.sigPlotSize / 2 - layout.plotMargins.t;
  let nodePlots = nodes.filter(n => n.layerIndex !== 0)
    .append("g")
    .attr("transform", d => `translate(${d.x-sigP},${d.y-sigP})`);

  layout.pltwidth = layout.sigPlotSize - layout.plotMargins.l - layout.plotMargins.r;
  layout.pltheight = layout.sigPlotSize - layout.plotMargins.t - layout.plotMargins.b;
  layout.x = d3.scale
    .linear()
    .range([0, layout.pltwidth])
    .clamp(true)
    .domain([-6, 6]);
  layout.y = d3.scale
    .linear()
    .range([layout.pltheight, 0])
    .domain([0, 1]);
  let xAxis = d3.svg.axis()
    .scale(layout.x)
    .tickSize(0, 0)
    .tickFormat("")
    .orient("bottom");
  let yAxis = d3.svg.axis()
    .scale(layout.y)
    .tickSize(0, 0)
    .tickFormat("")
    .orient("left");
  let sigLine = d3.svg.line()
    .x(d => layout.x(d.x))
    .y(d => layout.y(d.y));
  let sigLineData = d3.range(-6, 6.25, 0.25)
    .map(x => ({
      x: x,
      y: f(x),
    }));

  nodePlots.append("g")
    .attr("class", "axis")
    .attr("transform", `translate(0,${layout.pltheight})`)
    .call(xAxis);

  nodePlots.append("g")
    .attr("transform", `translate(${layout.pltwidth/2},0)`)
    .attr("class", "axis")
    .call(yAxis);

  nodePlots.append("path")
    .attr("class", "line")
    .attr("d", sigLine(sigLineData));

  nodePlots.append("circle")
    .attr("class", "sigdot")
    .attr("r", 0)
    .attr("fill", "red")
    .attr("cx", d => layout.x(d.sigX))
    .attr("cy", d => layout.y(d.sigY));

  nodePlots.append("line")
    .attr("class", "deriv")
    .attr("opacity", 0)
    .attr("stroke", "red")
    .attr("stroke-width", 2);

  synapses.append("line")
    .attr("x1", d => d.leftNode.x)
    .attr("x2", d => d.rightNode.x)
    .attr("y1", d => d.leftNode.y)
    .attr("y2", d => d.rightNode.y)
    .attr("stroke", "gray")
    .attr("stroke-width", d => _synapseMultiplier * Math.abs(d.weight))
    .attr("opacity", 0.5);

  layout.rightIOG.attr("transform",
                `translate(${(ann._layerNodes.length)*layout.xNodeSpacing/1.05},${layout.figureHeight/2+layout.fontHeight/3})`)

  layout.rightIOG.selectAll("text.target-values")
    .data(ann._y, d => d.id)
      .enter().append("text")
    .classed("target-values", true)
    .attr("font-size", `${layout.fontHeight}px`)
    .attr("text-anchor", "left")
    .attr("fill", "gray")
    .attr("transform", d => `translate(${layout.xNodeSpacing/2+((d.placeInLine-1)*layout.xNodeSpacing/2)}, ${-layout.fontHeight})`)
    .attr("opacity", 0.5)
    .text(d => d.value);

  // draw on the initial inputs waiting
  layout.leftIOG.attr("transform",
               `translate(0,${layout.yNodeSpacing[0]+layout.fontHeight/3})`);

  layout.leftIOG.selectAll("text")
    .data(ann._X, d => d.id)
      .enter().append("g")
    .attr("transform", d => `translate(0,${layout.yNodeSpacing[0]*d.offset})`)
    .append("text")
    .classed("input-values", true)
    .attr("font-size", `${layout.fontHeight}px`)
    .attr("text-anchor", "middle")
    .attr("fill", "gray")
    .attr("opacity", 0.5)
    .text(d => d.value);

  return layout;

}

// number of nodes in each hidden layer
let structure = [4];
let trainingXs = [
  [0, 0, 1],
  [0, 1, 1],
  [1, 0, 1],
  [1, 1, 1],
];
let trainingYs = [0, 1, 1, 0];

let ann = new ANN(structure, trainingXs, trainingYs);
ann.calculate();

var scrollInputs = (ann, layer, layout) => {
  d3.selectAll("text.input-values")
    .data(ann._X, d => d.id)
    .transition()
    .duration(5 * layout.speed)
    .attr("transform", d => `translate(${(4-d.placeInLine)*layout.xNodeSpacing/3},0)`)
    .call(endall, () => pulse(ann, layer, layout));

  d3.selectAll("text.target-values")
    .data(ann._y, d => d.id)
    .transition()
    .duration(5 * layout.speed)
    .attr("transform", d => `translate(${layout.xNodeSpacing/2+((d.placeInLine - 1)*layout.xNodeSpacing/2)}, ${-layout.fontHeight})`)
};

var scrollOutputs = (ann, layer, layout) => {

  let removeAndMove = () => {
    sel.transition()
      .duration(5 * layout.speed)
      .attr("opacity", 0)
      .remove();

    layout.rightIOG.selectAll("text.error")
      .transition()
      .duration(5 * layout.speed)
      .attr("transform", "translate(0,0)")
      .attr("text-anchor", "left")
      .attr("opacity", 0)
      .call(endall, () => drawDerivative(ann, layer, layout));
  }

  let addErrorTerm = () => {

    let err = layout.rightIOG.append("text")
      .attr("font-size", `${layout.fontHeight}px`)
      .attr("text-anchor", "left")
      .attr("fill", "gray")
      .attr("opacity", 0)
      .classed("error", true)
      .text(fmt(ann.getError()))
      .attr("transform", `translate(${layout.xNodeSpacing/2},0)`)
      .transition()
      .duration(5 * layout.speed)
      .attr("transform", `translate(${layout.xNodeSpacing/2},${layout.fontHeight})`)
      .attr("opacity", 0.5)
      .call(endall, removeAndMove);
  }

  let sel = layout.rightIOG.selectAll("text.attempt")
    .data([ann.getOutput()]);

  sel.enter()
    .append("text")
    .classed("attempt", true)
    .attr("font-size", `${layout.fontHeight}px`)
    .attr("text-anchor", "left")
    .attr("fill", "gray")
    .attr("opacity", 0)
    .text(d => fmt(d));

  sel.transition()
    .duration(3 * layout.speed)
    .attr("opacity", 0.5)
    .attr("transform", `translate(${layout.xNodeSpacing/2},0)`)
    .call(endall, addErrorTerm);

};

var fire = (ann, layer, layout) => {
  let fireData = ann._synapses.filter(s => s.layer == layer);
  layout.synapseG.selectAll(".fire")
    .data(fireData)
      .enter()
    .append("circle")
    .attr("cx", d => d.leftNode.x)
    .attr("cy", d => d.leftNode.y)
    .attr("r", d => Math.abs(d.fire) * _neuronMultiplier)
    .attr("fill", "black")
    .transition()
    .duration(15 * layout.speed)
    .attr("cx", d => d.rightNode.x)
    .attr("cy", d => d.rightNode.y)
    .transition()
    .remove();

  // use timeout here rather than endall because we don't want
  // to wait for the pulse to hit the center of the node (the
  // delay looks odd)
  setTimeout(() => drawSig(ann, layer + 1, layout),
             10 * layout.speed);
};

var drawSig = (ann, layer, layout) => {
  let sel = layout.nodeG.selectAll(".sigdot")
    .data(ann._nodes[layer].values, d => d.id);

  sel.attr("cx", d => layout.x(d.sigX))
    .attr("cy", d => layout.y(d.sigY))
    .transition()
    .duration(1 * layout.speed)
    .attr("r", 5)
    .call(endall, () => pulse(ann, layer, layout))
    .transition()
    .duration(15 * layout.speed)
    .attr("r", 0);
};

var drawDerivative = (ann, layer, layout) => {
  layout.nodeG.selectAll("line.deriv")
    .data(ann._nodes[layer].values, d => d.id)
    .attr("x1", d => layout.x(d.sigDerivDisplay.x1))
    .attr("x2", d => layout.x(d.sigDerivDisplay.x1))
    .attr("y1", d => layout.y(d.sigDerivDisplay.y1))
    .attr("y2", d => layout.y(d.sigDerivDisplay.y1))
    .attr("opacity", 1)
    .transition()
    .duration(5 * layout.speed)
    .attr("x2", d => layout.x(d.sigDerivDisplay.x2))
    .attr("y2", d => layout.y(d.sigDerivDisplay.y2))
    .call(endall, () => sizeSynapses(ann, layer - 1, layout))
    .transition()
    .duration(15 * layout.speed)
    .attr("x1", d => layout.x(d.sigDerivDisplay.x2))
    .attr("y1", d => layout.y(d.sigDerivDisplay.y2));
};


var sizeSynapses = (ann, layer, layout) => {

  let data = ann._nodes[layer].values
    .map(node => node.rightSynapses)
    .reduce((x, y) => x.concat(y), []);

  let done = () => {
    if (layer === 0) {
      ann.calculate();
      scrollInputs(ann, layer, layout);
    } else {
      drawDerivative(ann, layer, layout);
    }
  }

  layout.synapseG.selectAll("line")
    .data(data, d => d.id)
    .transition()
    .duration(5 * layout.speed)
    .attr("opacity", 1)
    .attr("stroke", "black")
    .attr("stroke-width", d => 5 * (Math.abs(d.weight)))
    .transition()
    .duration(5 * layout.speed)
    .attr("opacity", 0.5)
    .attr("stroke", "gray")
    .call(endall, done);
};


var pulse = (ann, layer, layout) => {
  let selection = layout.nodeG.selectAll("circle.node-border")
    .filter(n => n.pulseSize && n.layerIndex == layer);

  selection.transition()
    .duration(layout.speed * 2)
    .attr("stroke-width", 5)
    .transition()
    .duration(layout.speed * 2)
    .attr("stroke-width", 1);

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
  transition 
    .each(function() { ++n; }) 
    .each("end", function() { if (!--n) callback.apply(this, arguments); }); 
}

let _skip = (n) => {
  let i = 0;
  while (i++ < n) {
    ann.calculate()
  }
}

let skip500 = _skip.bind(null, 500);

document.addEventListener('DOMContentLoaded', () => {

  let elt = $("#ann-viz"),
      w = elt.width(),
      h = w / 1.5,
      layout = initialDraw("#ann-viz", w, h, ann);

  $(window).resize(() => {
    let w = $(elt).width();
    let h = $(elt).height();
    setTimeout(() => {
      // only redraw once we've finished resizing
      let newW = $(elt).width();
      let newH = $(elt).height();
      if (newW === w && newH === h) {
        layout = initialDraw("#ann-viz", newW, newW / 1.5, ann);
        scrollInputs(ann, 0, layout);
      }
    }, 300);
  });

  scrollInputs(ann, 0, layout);

}, false);
