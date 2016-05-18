"use strict";

var _slicedToArray = function () { function sliceIterator(arr, i) { var _arr = []; var _n = true; var _d = false; var _e = undefined; try { for (var _i = arr[Symbol.iterator](), _s; !(_n = (_s = _i.next()).done); _n = true) { _arr.push(_s.value); if (i && _arr.length === i) break; } } catch (err) { _d = true; _e = err; } finally { try { if (!_n && _i["return"]) _i["return"](); } finally { if (_d) throw _e; } } return _arr; } return function (arr, i) { if (Array.isArray(arr)) { return arr; } else if (Symbol.iterator in Object(arr)) { return sliceIterator(arr, i); } else { throw new TypeError("Invalid attempt to destructure non-iterable instance"); } }; }();

var genid = function genid() {
  // http://stackoverflow.com/questions/105034/create-guid-uuid-in-javascript/21963136
  // n.b. result is not a guid (true to spec), just for element ids here
  var _p8 = function _p8(s) {
    var p = (Math.random().toString(16) + "000000000").substr(2, 8);
    return s ? "-" + p.substr(0, 4) + "-" + p.substr(4, 4) : p;
  };
  return _p8() + _p8(true) + _p8(true) + _p8();
};

/*
Facet box plots - not generalised, makes assumptions about the data
*/
var drawBoxPlots = function drawBoxPlots(elt, dataStub) {

  var draw = function draw(box, outliers) {
    var nest = d3.nest().key(function (d) {
      return d.month;
    });
    var boxFacets = nest.entries(box);
    // outlier months not sorted
    var monthOrder = boxFacets.map(function (d) {
      return d.key;
    });
    var sortMth = function sortMth(a, b) {
      return monthOrder.indexOf(a) - monthOrder.indexOf(b);
    };
    var outlierFacets = nest.sortKeys(sortMth).entries(outliers);
    var ylimit = d3.max(outliers, function (d) {
      return parseFloat(d.V1);
    });

    // combine the two sets of data - box and whiskers, and outliers
    d3.zip(boxFacets, outlierFacets).map(function (_ref) {
      var _ref2 = _slicedToArray(_ref, 2);

      var b = _ref2[0];
      var o = _ref2[1];
      return chart(elt, b, o, ylimit);
    });
  };

  var chart = function chart(elt, box, outliers, ylimit) {
    var node = d3.select(elt).append("div").style("width", "200px").style("display", "inline-block").style("display", "float-left").attr("id", genid());

    // highcharts chart
    new Highcharts.Chart({
      chart: {
        type: 'boxplot',
        renderTo: node.attr('id'),
        plotBackgroundColor: theme.chartBackdrop
      },
      colors: theme.series,
      title: { text: box.key },
      legend: { enabled: false },
      xAxis: {
        categories: d3.range(0, 24),
        title: { text: 'Hour of day' }
      },
      yAxis: {
        title: { text: 'Knots' },
        min: 0,
        max: ylimit,
        gridLineColor: theme.gridLine
      },
      credits: { enabled: false },
      series: [{
        name: 'Wind speed (knots)',
        data: box.values.map(function (d) {
          return [d.V1, d.V2, d.V3, d.V4, d.V5].map(parseFloat);
        })
      }, {
        name: 'Outlier (knots)',
        color: theme.series[0],
        type: 'scatter',
        data: outliers.values.map(function (d) {
          return [parseInt(d.hour), parseFloat(d.V1)];
        }),
        marker: {
          fillColor: 'white',
          lineWidth: 1,
          lineColor: theme.series[0],
          radius: 1 // small
        },
        tooltip: { pointFormat: '{point.y} knots' }
      }]
    });
  };

  d3.csv("/data/" + dataStub + "box.csv", function (box) {
    d3.csv("/data/" + dataStub + "outliers.csv", function (outliers) {
      draw(box, outliers);
    });
  });
};

document.addEventListener('DOMContentLoaded', function () {
  //drawBoxPlots("#kts-box-plots", "stk-kts-hr-mth-");
}, false);

