// ClipDiagram FFI — renders SVG timeline visualization of LoopyPro clip settings

var SVG_NS = "http://www.w3.org/2000/svg";

function el(tag, attrs, children) {
  var e = document.createElementNS(SVG_NS, tag);
  if (attrs) for (var k in attrs) e.setAttribute(k, attrs[k]);
  if (children) for (var i = 0; i < children.length; i++) {
    if (typeof children[i] === "string") e.appendChild(document.createTextNode(children[i]));
    else e.appendChild(children[i]);
  }
  return e;
}

function parseColor(hex) {
  var r = parseInt(hex.slice(1, 3), 16);
  var g = parseInt(hex.slice(3, 5), 16);
  var b = parseInt(hex.slice(5, 7), 16);
  return { r: r, g: g, b: b };
}

function rgba(hex, a) {
  var c = parseColor(hex);
  return "rgba(" + c.r + "," + c.g + "," + c.b + "," + a + ")";
}

export const renderClipDiagramImpl = function (settings) {
  return function (loopColor) {
    return function (container) {
      return function () {
        var s = settings;
        var W = 400, H = 60;
        var PAD = 4;
        var TRACK_Y = 16, TRACK_H = 28;

        // Zone widths
        var threshW = s.threshold ? 30 : 0;
        var countInW = s.countIn !== "none" ? 30 : 0;
        var countOutW = s.countOut !== "none" ? 30 : 0;
        var tailW = s.tail ? 25 : 0;
        var recW = W - PAD * 2 - threshW - countInW - countOutW - tailW;
        if (recW < 40) recW = 40;

        // X positions
        var x = PAD;
        var threshX = x; x += threshW;
        var countInX = x; x += countInW;
        var recX = x; x += recW;
        var countOutX = x; x += countOutW;
        var tailX = x;

        var svg = el("svg", {
          viewBox: "0 0 " + W + " " + H,
          preserveAspectRatio: "xMidYMid meet",
          xmlns: SVG_NS,
          width: "100%"
        });

        // --- Defs ---
        var defs = el("defs");

        if (countInW > 0) {
          var hatchPat = el("pattern", { id: "clip-hatch", width: 6, height: 6, patternUnits: "userSpaceOnUse", patternTransform: "rotate(45)" },
            [el("line", { x1: 0, y1: 0, x2: 0, y2: 6, stroke: rgba(loopColor, 0.3), "stroke-width": 1.5 })]);
          defs.appendChild(hatchPat);
        }

        if (tailW > 0) {
          var tailGrad = el("linearGradient", { id: "clip-tail-grad", x1: 0, y1: 0, x2: 1, y2: 0 },
            [el("stop", { offset: "0%", "stop-color": loopColor, "stop-opacity": 0.5 }),
             el("stop", { offset: "100%", "stop-color": loopColor, "stop-opacity": 0 })]);
          defs.appendChild(tailGrad);
        }

        if (threshW > 0) {
          var wavePat = el("pattern", { id: "clip-wave", width: 12, height: 6, patternUnits: "userSpaceOnUse" },
            [el("path", { d: "M0 3 Q3 0 6 3 Q9 6 12 3", fill: "none", stroke: "#999", "stroke-width": 0.8 })]);
          defs.appendChild(wavePat);
        }

        svg.appendChild(defs);

        // --- Threshold zone ---
        if (threshW > 0) {
          svg.appendChild(el("rect", {
            x: threshX, y: TRACK_Y, width: threshW, height: TRACK_H,
            fill: "url(#clip-wave)", stroke: "#999", "stroke-width": 0.8, "stroke-dasharray": "3,2", rx: 2
          }));
          svg.appendChild(el("text", {
            x: threshX + threshW / 2, y: TRACK_Y + TRACK_H / 2 + 1,
            "text-anchor": "middle", "dominant-baseline": "middle",
            fill: "#999", "font-size": 7, "font-weight": 500, "font-family": "inherit"
          }, ["thresh"]));
        }

        // --- Count-in zone ---
        if (countInW > 0) {
          svg.appendChild(el("rect", {
            x: countInX, y: TRACK_Y, width: countInW, height: TRACK_H,
            fill: "url(#clip-hatch)", stroke: rgba(loopColor, 0.4), "stroke-width": 0.8
          }));
          if (s.intro) {
            svg.appendChild(el("rect", {
              x: countInX + 1, y: TRACK_Y + 1, width: countInW - 2, height: TRACK_H - 2,
              fill: rgba(loopColor, 0.25), rx: 1
            }));
            svg.appendChild(el("text", {
              x: countInX + countInW / 2, y: TRACK_Y + TRACK_H / 2 + 1,
              "text-anchor": "middle", "dominant-baseline": "middle",
              fill: loopColor, "font-size": 7, "font-weight": 600, "font-family": "inherit"
            }, ["intro"]));
          } else {
            svg.appendChild(el("line", {
              x1: countInX, y1: TRACK_Y + 2, x2: countInX, y2: TRACK_Y + TRACK_H - 2,
              stroke: rgba(loopColor, 0.6), "stroke-width": 1.5
            }));
          }
        }

        // --- Recording block ---
        svg.appendChild(el("rect", {
          x: recX, y: TRACK_Y, width: recW, height: TRACK_H,
          fill: rgba(loopColor, 0.7), stroke: loopColor, "stroke-width": 1, rx: 2
        }));
        svg.appendChild(el("text", {
          x: recX + recW / 2, y: TRACK_Y + TRACK_H / 2 + 1,
          "text-anchor": "middle", "dominant-baseline": "middle",
          fill: "#fff", "font-size": 9, "font-weight": 700, "letter-spacing": "0.5", "font-family": "inherit"
        }, ["RECORDING"]));

        // --- Phase-lock grid lines ---
        if (s.phaseLocked) {
          var spacing = recW / 8;
          for (var i = 1; i < 8; i++) {
            svg.appendChild(el("line", {
              x1: recX + i * spacing, y1: TRACK_Y + 1,
              x2: recX + i * spacing, y2: TRACK_Y + TRACK_H - 1,
              stroke: "rgba(255,255,255,0.25)", "stroke-width": 0.8, "stroke-dasharray": "3,3"
            }));
          }
        }

        // --- Beat quantization ticks ---
        if (s.beatQuant !== "off") {
          var divisions = s.beatQuant.indexOf("16") === 0 ? 16 : 32;
          var tickSpacing = recW / divisions;
          for (var j = 1; j < divisions; j++) {
            svg.appendChild(el("line", {
              x1: recX + j * tickSpacing, y1: TRACK_Y + TRACK_H - 3,
              x2: recX + j * tickSpacing, y2: TRACK_Y + TRACK_H,
              stroke: "#999", "stroke-width": 0.5
            }));
          }
        }

        // --- Record end action icon ---
        var iconX = recX + recW - 2;
        var iconY = TRACK_Y - 2;
        if (s.recordEndAction === "play") {
          svg.appendChild(el("polygon", {
            points: (iconX - 3) + "," + (iconY - 5) + " " + (iconX - 3) + "," + (iconY + 1) + " " + (iconX + 2) + "," + (iconY - 2),
            fill: "#fff"
          }));
        } else if (s.recordEndAction === "stop") {
          svg.appendChild(el("rect", { x: iconX - 4, y: iconY - 5, width: 6, height: 6, fill: "#fff", rx: 0.5 }));
        } else if (s.recordEndAction === "overdub") {
          svg.appendChild(el("line", { x1: iconX - 3, y1: iconY - 2, x2: iconX + 3, y2: iconY - 2, stroke: "#fff", "stroke-width": 1.5 }));
          svg.appendChild(el("line", { x1: iconX, y1: iconY - 5, x2: iconX, y2: iconY + 1, stroke: "#fff", "stroke-width": 1.5 }));
        }

        // --- Count-out zone ---
        if (countOutW > 0) {
          svg.appendChild(el("rect", {
            x: countOutX, y: TRACK_Y, width: countOutW, height: TRACK_H,
            fill: rgba(loopColor, 0.25), stroke: rgba(loopColor, 0.4), "stroke-width": 0.8
          }));
          if (s.autoCountOut) {
            svg.appendChild(el("text", {
              x: countOutX + countOutW / 2, y: TRACK_Y + TRACK_H / 2 + 1,
              "text-anchor": "middle", "dominant-baseline": "middle",
              fill: loopColor, "font-size": 8, "font-weight": 700, "font-family": "inherit"
            }, ["A"]));
          }
          svg.appendChild(el("line", {
            x1: countOutX + countOutW, y1: TRACK_Y + 2,
            x2: countOutX + countOutW, y2: TRACK_Y + TRACK_H - 2,
            stroke: rgba(loopColor, 0.6), "stroke-width": 1.5
          }));
        }

        // --- Tail zone ---
        if (tailW > 0) {
          svg.appendChild(el("rect", {
            x: tailX, y: TRACK_Y, width: tailW, height: TRACK_H,
            fill: "url(#clip-tail-grad)", rx: 2
          }));
          svg.appendChild(el("text", {
            x: tailX + tailW / 2, y: TRACK_Y + TRACK_H / 2 + 1,
            "text-anchor": "middle", "dominant-baseline": "middle",
            fill: "#999", "font-size": 7, "font-family": "inherit"
          }, ["tail"]));
        }

        // --- Overdub ramp ---
        if (s.overdubFeedback !== 1.0) {
          var rampY = TRACK_Y - 1;
          var rampH = 10;
          var endRight = recX + recW + countOutW;
          var endY;
          if (s.overdubFeedback < 1.0) {
            endY = rampY - rampH * s.overdubFeedback;
          } else {
            var gain = Math.min(s.overdubFeedback, 2.0);
            endY = rampY - rampH * (gain - 0.5);
          }
          svg.appendChild(el("path", {
            d: "M" + recX + "," + (rampY - rampH) + " L" + endRight + "," + endY + " L" + endRight + "," + rampY + " L" + recX + "," + rampY + " Z",
            fill: rgba(loopColor, 0.15), stroke: rgba(loopColor, 0.4), "stroke-width": 0.8
          }));
        }

        // --- Loop arrow or one-shot end cap ---
        var endX = tailX + tailW;
        if (s.loop) {
          var midY = TRACK_Y + TRACK_H / 2;
          svg.appendChild(el("path", {
            d: "M" + endX + "," + (midY - 4) + " C" + (endX + 12) + "," + (midY - 8) + " " + (endX + 12) + "," + (midY + 8) + " " + endX + "," + (midY + 4),
            fill: "none", stroke: loopColor, "stroke-width": 1.2
          }));
          svg.appendChild(el("polygon", {
            points: endX + "," + (midY + 4) + " " + (endX + 4) + "," + (midY + 1) + " " + (endX + 3) + "," + (midY + 6),
            fill: loopColor
          }));
        } else {
          svg.appendChild(el("line", {
            x1: endX, y1: TRACK_Y + 2, x2: endX, y2: TRACK_Y + TRACK_H - 2,
            stroke: loopColor, "stroke-width": 2
          }));
          svg.appendChild(el("text", {
            x: endX + 6, y: TRACK_Y + TRACK_H / 2 + 1,
            "dominant-baseline": "middle",
            fill: "#999", "font-size": 7, "font-weight": 600, "font-family": "inherit"
          }, ["\u00d71"]));
        }

        // --- Retrospective indicator ---
        if (s.retrospective) {
          svg.appendChild(el("text", {
            x: recX + 6, y: TRACK_Y + TRACK_H - 4,
            fill: "rgba(255,255,255,0.6)", "font-size": 7, "font-weight": 500, "font-family": "inherit"
          }, ["retro"]));
        }

        // Replace container contents
        container.textContent = "";
        container.appendChild(svg);
      };
    };
  };
};
