// Port of the Observable notebooks @wifo-viz/tarifloehne and d/662b7dbdbc98b5f9 ("Szenario fix").
// Library versions are pinned to what the notebooks used, so calculations and charts match the live site.
import * as aq from "npm:arquero@5.1.0";
import * as vega from "npm:vega@5.23.0";
import * as vegalite from "npm:vega-lite@5.6.0";
import * as vegaLiteApi from "npm:vega-lite-api@5.6.0";
import {Handler} from "npm:vega-tooltip@0.30.0";
import * as d3 from "npm:d3";
import * as Inputs from "npm:@observablehq/inputs";
import {html} from "npm:htl";
import writeXlsxFile from "npm:write-excel-file@4/browser";

const op = aq.op;

export const vl = vegaLiteApi.register(vega, vegalite, {
  config: {
    config: {
      view: {continuousWidth: 400, continuousHeight: 300},
      mark: {tooltip: null}
    }
  },
  init: (view) => {
    view.tooltip(new Handler().call);
  },
  view: {renderer: "canvas"}
});

export const indices = ["Tariflohnindex", "Produktivitätsindex", "Verbraucherpreisindex", "Benyaindex", "Reallohnindex"];

export const colors = [
  "#559e8f", // TLI
  "#72bb6f", // PROD
  "#559bd5", // VPI
  "#f3d039", // Benyaindex
  "#c3423f", // Reallohnindex
  "#12a7e7",
  "#96b428",
  "#559e8f",
  "#f3d039",
  "#559e8f",
  "#2d5b5e"
];

const locale = d3.timeFormatLocale({
  dateTime: "%A, der %e. %B %Y, %X",
  date: "%d.%m.%Y",
  time: "%H:%M:%S",
  periods: ["AM", "PM"],
  days: ["Sonntag", "Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag"],
  shortDays: ["So", "Mo", "Di", "Mi", "Do", "Fr", "Sa"],
  months: ["Januar", "Februar", "März", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Dezember"],
  shortMonths: ["Jan", "Feb", "Mär", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez"]
});

export const formatDate = locale.format("%b %Y");
export const parseDate = d3.timeParse("%Y-%m-%d");

const germanTime = {
  dateTime: "%A %e %B %Y, %X",
  date: "%d/%m/%Y",
  time: "%H:%M:%S",
  periods: ["AM", "PM"],
  days: ["Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag", "Samstag", "Sonntag"],
  shortDays: ["Mo", "Di", "Mi", "Do", "Fr", "Sa", "So"],
  months: ["Jänner", "Februar", "März", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Dezember"],
  shortMonths: ["Jan", "Feb", "Mar", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez"]
};

// The main notebook used font "FFDaxPro" and a German time locale; the scenario notebook used
// "Century Gothic" and Vega's default time locale.
export function makeConfig({font = "FFDaxPro", time = germanTime} = {}) {
  return {
    view: {stroke: "transparent"},
    facet: {spacing: 5},
    concat: {spacing: 25},
    font,
    title: {fontSize: 14, subtitleFontSize: 13, subtitlePadding: 6},
    axis: {labelFontSize: 12, titleFontSize: 12, titleFontWeight: "normal"},
    legend: {labelFontSize: 12, titleFontSize: 12, titleFontWeight: "normal"},
    mark: {fontSize: 12},
    locale: {
      number: {decimal: ",", thousands: ".", grouping: [3]},
      ...(time ? {time} : {})
    },
    background: "transparent"
  };
}

// --- Inputs ------------------------------------------------------------------

// Inputs.select whose options are grouped into <optgroup>s (replaces @zzzev/grouped-dropdown-input).
// `groups` are consecutive slices of `options`, so the option order Inputs.select relies on is kept.
function groupedSelect(options, groups, {label}) {
  const input = Inputs.select(options, {label});
  const select = input.querySelector("select");
  const optionElements = [...select.options];
  let start = 0;
  for (const {label, size} of groups) {
    const optgroup = html`<optgroup label=${label}>`;
    optgroup.append(...optionElements.slice(start, start + size));
    select.append(optgroup);
    start += size;
  }
  return input;
}

// Two-thumb range slider (port of @mootari/range-slider, theme "Flat").
const themeFlat = `
:scope { color: #3b99fc; width: 240px; }
:scope {
  position: relative; display: inline-block;
  --thumb-size: 15px; --thumb-radius: calc(var(--thumb-size) / 2);
  padding: var(--thumb-radius) 0; margin: 2px; vertical-align: middle;
}
:scope .range-track {
  box-sizing: border-box; position: relative; height: 7px; background-color: hsl(0, 0%, 80%);
  overflow: visible; padding: 0 var(--thumb-radius);
}
:scope .range-track-zone { box-sizing: border-box; position: relative; }
:scope .range-select {
  box-sizing: border-box; position: relative; left: var(--range-min);
  width: calc(var(--range-max) - var(--range-min)); cursor: ew-resize;
  background: currentColor; height: 7px; border: inherit;
}
:scope .range-select:before {
  content: ""; position: absolute; width: 100%; height: var(--thumb-size); left: 0;
  top: calc(2px - var(--thumb-radius));
}
:scope .range-select:focus, :scope .thumb:focus { outline: none; }
:scope .thumb {
  box-sizing: border-box; position: absolute; width: var(--thumb-size); height: var(--thumb-size);
  background: #fcfcfc; top: -4px; border: 1px solid hsl(0,0%,55%);
  cursor: default; margin: 0;
}
:scope .thumb:active { box-shadow: inset 0 var(--thumb-size) #0002; }
:scope .thumb-min { left: calc(-1px - var(--thumb-radius)); }
:scope .thumb-max { right: calc(-1px - var(--thumb-radius)); }
`;

export function rangeSlider({min = 0, max = 1, step = "any", value: defaultValue = [min, max], color, label, format = (v) => v, separator = " … "}) {
  const scope = "scope-" + (performance.now() + Math.random()).toString(32).replace(".", "-");
  const clamp = (a, b, v) => (v < a ? a : v > b ? b : v);
  const sanitizer = html`<input type=range min=${min} max=${max} step=${step}>`;
  const controls = {};

  const slider = html`<div class=${`${scope} range-slider`} style=${{color, flex: "1 1 auto"}}>
  ${(controls.track = html`<div class="range-track">
    ${(controls.zone = html`<div class="range-track-zone">
      ${(controls.range = html`<div class="range-select" tabindex=0>
        ${(controls.min = html`<div class="thumb thumb-min" tabindex=0>`)}
        ${(controls.max = html`<div class="thumb thumb-max" tabindex=0>`)}
      `)}
    `)}
  `)}
  ${html`<style>${themeFlat.replace(/:scope\b/g, "." + scope)}`}
</div>`;
  // Fixed width, so the slider keeps its length whatever month names are shown.
  const output = html`<output style="flex: none; width: 150px; margin-left: 0.5em; white-space: nowrap;">`;
  // Same layout as Observable Inputs (label column on the left), using its CSS classes.
  const ns = Inputs.text().classList[0];
  const form = html`<form class=${ns}>${label == null ? null : html`<label>${label}</label>`}<div class=${`${ns}-input`}>${slider}${output}</div></form>`;
  form.onsubmit = (e) => e.preventDefault();

  let value = [];
  let changed = false;
  const sanitize = (a, b) => {
    a = isNaN(a) ? min : ((sanitizer.value = a), sanitizer.valueAsNumber);
    b = isNaN(b) ? max : ((sanitizer.value = b), sanitizer.valueAsNumber);
    return [Math.min(a, b), Math.max(a, b)];
  };
  const update = () => {
    const ratio = (v) => (v - min) / (max - min);
    slider.style.setProperty("--range-min", `${ratio(value[0]) * 100}%`);
    slider.style.setProperty("--range-max", `${ratio(value[1]) * 100}%`);
    output.value = value.map(format).join(separator);
  };
  const setValue = (vmin, vmax) => {
    const [pmin, pmax] = value;
    value = sanitize(vmin, vmax);
    update();
    if (pmin === value[0] && pmax === value[1]) return;
    form.dispatchEvent(new Event("input", {bubbles: true}));
    changed = true;
  };
  Object.defineProperty(form, "value", {get: () => [...value], set: ([a, b]) => ((value = sanitize(a, b)), update())});
  setValue(...defaultValue);

  const handlers = new Map([
    [controls.min, (dt, ov) => setValue(clamp(min, ov[1], ov[0] + dt * (max - min)), ov[1])],
    [controls.max, (dt, ov) => setValue(ov[0], clamp(ov[0], max, ov[1] + dt * (max - min)))],
    [controls.range, (dt, ov) => {
      const d = ov[1] - ov[0];
      const v = clamp(min, max - d, ov[0] + dt * (max - min));
      setValue(v, v + d);
    }]
  ]);
  const pointer = (e) => (e.touches ? e.touches[0] : e);
  const on = (e, fn) => e.split(" ").forEach((e) => document.addEventListener(e, fn, {passive: false}));
  const off = (e, fn) => e.split(" ").forEach((e) => document.removeEventListener(e, fn, {passive: false}));

  let initialX, initialV, target, dragging = false;
  function handleDrag(e) {
    if (!e.buttons && !e.touches) return handleDragStop();
    dragging = true;
    const w = controls.zone.getBoundingClientRect().width;
    e.preventDefault();
    handlers.get(target)((pointer(e).clientX - initialX) / w, initialV);
  }
  function handleDragStop() {
    off("mousemove touchmove", handleDrag);
    off("mouseup touchend", handleDragStop);
    if (changed) form.dispatchEvent(new Event("change", {bubbles: true}));
  }
  slider.ontouchstart = slider.onmousedown = (e) => {
    dragging = false;
    changed = false;
    if (!handlers.has(e.target)) return;
    on("mousemove touchmove", handleDrag);
    on("mouseup touchend", handleDragStop);
    e.preventDefault();
    e.stopPropagation();
    target = e.target;
    initialX = pointer(e).clientX;
    initialV = value.slice();
  };
  controls.track.onclick = (e) => {
    if (dragging) return;
    changed = false;
    const r = controls.zone.getBoundingClientRect();
    const v = min + clamp(0, 1, (pointer(e).clientX - r.left) / r.width) * (max - min);
    const [vmin, vmax] = value;
    const d = vmax - vmin;
    if (v < vmin) setValue(v, v + d);
    else if (v > vmax) setValue(v - d, v);
  };
  return form;
}

// --- Data --------------------------------------------------------------------

export function mergeData({VPI, TLI, PROD, Produktgruppe, Sparte, Sektor, Einheit}) {
  const tli = TLI.filter((e) => e.Sparte === Sparte);
  const prod = PROD.filter((d) => d.Sektor === Sektor && d.Einheit === Einheit);
  return VPI.filter((e) => e.Produktgruppe === Produktgruppe).map((d) => {
    const addTLI = tli.find((e) => e.Monat === d.Monat);
    const addPROD = prod.find((e) => e.Monat === d.Monat);
    return {...d, ...addTLI, ...addPROD};
  });
}

// Smoothing, rebasing and derived series; `Zeit` holds the first and last row index of the shown period.
// "Ausgangsjahr" averages rows with `row < baseLimit` (12 in the main notebook, i.e. 11 months; 13 in the scenario notebook).
export function vizData({merged, smooth, Zeit, Methode, baseLimit = 12}) {
  let temp = aq.from(merged);

  temp = smooth
    ? temp.derive({
        Produktivitätsindex: aq.rolling((d) => op.average(d.Produktivitätsindex), [-11, 0]),
        Verbraucherpreisindex: aq.rolling((d) => op.average(d.Verbraucherpreisindex), [-11, 0]),
        Tariflohnindex: aq.rolling((d) => op.average(d.Tariflohnindex), [-11, 0]),
        Lohnerhöhung: (d) => (d.Tariflohnindex - op.lag(d.Tariflohnindex, 12)) / op.lag(d.Tariflohnindex, 12),
        rollierendeInflation: (d) => (d.Verbraucherpreisindex - op.lag(d.Verbraucherpreisindex, 12)) / op.lag(d.Verbraucherpreisindex, 12)
      })
    : temp.derive({
        Produktivitätsindex: aq.rolling((d) => op.average(d.Produktivitätsindex), [-11, 0]),
        Verbraucherpreisindex: (d) => +d.Verbraucherpreisindex,
        Tariflohnindex: (d) => +d.Tariflohnindex,
        Lohnerhöhung: (d) => (d.Tariflohnindex - op.lag(d.Tariflohnindex, 12)) / op.lag(d.Tariflohnindex, 12),
        rollierendeInflation: (d) => (d.Verbraucherpreisindex - op.lag(d.Verbraucherpreisindex, 12)) / op.lag(d.Verbraucherpreisindex, 12)
      });

  temp = temp
    .derive({
      Benyaindex: (d) => d.Verbraucherpreisindex + d.Produktivitätsindex - 100,
      Reallohnindex: (d) => d.Tariflohnindex - d.Verbraucherpreisindex + 100
    })
    .derive({
      VeränderungProduktivitätsindex: (d) => (d.Produktivitätsindex - op.lag(d.Produktivitätsindex, 12)) / op.lag(d.Produktivitätsindex, 12),
      VeränderungVerbraucherpreisindex: (d) => (d.Verbraucherpreisindex - op.lag(d.Verbraucherpreisindex, 12)) / op.lag(d.Verbraucherpreisindex, 12),
      VeränderungTariflohnindex: (d) => (d.Tariflohnindex - op.lag(d.Tariflohnindex, 12)) / op.lag(d.Tariflohnindex, 12),
      VeränderungBenyaindex: (d) => (d.Benyaindex - op.lag(d.Benyaindex, 12)) / op.lag(d.Benyaindex, 12),
      VeränderungReallohnindex: (d) => (d.Reallohnindex - op.lag(d.Reallohnindex, 12)) / op.lag(d.Reallohnindex, 12),
      rollierendeInflation: aq.rolling((d) => op.average(d.rollierendeInflation), [-11, 0])
    })
    .derive({row: () => op.row_number()})
    .derive({
      rollierendeInflation: (d) => op.lag(d.rollierendeInflation, 3),
      langfristigesProduktivitätswachstum: aq.rolling((d) => op.average(d.VeränderungProduktivitätsindex), [-47, 0])
    })
    .filter(aq.escape((d) => d.row >= Zeit[0] + 1 && d.row <= Zeit[1] + 1))
    .derive({row: () => op.row_number()});

  temp = (Methode === "Basisjahr" ? temp.groupby({key: `d => d.row < ${baseLimit}`}) : temp.groupby({key: (d) => d.row == 1}))
    .derive({
      meanProduktivitätsindex: (d) => op.mean(d.Produktivitätsindex),
      meanVerbraucherpreisindex: (d) => op.mean(d.Verbraucherpreisindex),
      meanTariflohnindex: (d) => op.mean(d.Tariflohnindex)
    })
    .ungroup()
    .derive({
      Produktivitätsindex: (d) => (d.Produktivitätsindex / op.first_value(d.meanProduktivitätsindex)) * 100,
      Verbraucherpreisindex: (d) => (d.Verbraucherpreisindex / op.first_value(d.meanVerbraucherpreisindex)) * 100,
      Tariflohnindex: (d) => (d.Tariflohnindex / op.first_value(d.meanTariflohnindex)) * 100
    })
    .derive({
      Benyaindex: (d) => d.Verbraucherpreisindex + d.Produktivitätsindex - 100,
      Reallohnindex: (d) => d.Tariflohnindex - d.Verbraucherpreisindex + 100
    });

  return temp;
}

const changeColumns = {
  Produktivitätsindex: (d) => d.VeränderungProduktivitätsindex,
  Verbraucherpreisindex: (d) => d.VeränderungVerbraucherpreisindex,
  Tariflohnindex: (d) => d.VeränderungTariflohnindex,
  Benyaindex: (d) => d.VeränderungBenyaindex,
  Reallohnindex: (d) => d.VeränderungReallohnindex
};

const formelColumns = ["langfristigesProduktivitätswachstum", "rollierendeInflation", "Lohnerhöhung"];

// `maxPROD` hides productivity values after the last month with data (main notebook only).
function dropLateProd(table, maxPROD) {
  return maxPROD === undefined
    ? table
    : table.filter(aq.escape((d) => !(d.key == "Produktivitätsindex" && d.Monat > maxPROD)));
}

export function indexData(viz, selected, maxPROD) {
  const t = viz
    .select(selected.concat("Monat"))
    .fold(selected)
    .filter(aq.escape((d) => !isNaN(d.value)));
  return dropLateProd(t, maxPROD);
}

export function changeData(viz, selected, maxPROD) {
  const t = viz
    .derive(changeColumns)
    .select(selected.concat("Monat").concat("row"))
    .fold(selected)
    .filter(aq.escape((d) => !isNaN(d.value)));
  return dropLateProd(t, maxPROD);
}

export const flatIndexData = (viz) => viz.select(indices.concat("Monat")).fold(indices);
export const flatChangeData = (viz) => viz.derive(changeColumns).select(formelColumns.concat("Monat")).fold(formelColumns);

// --- Charts ------------------------------------------------------------------

const ruleAt = (y) => vl.markRule({strokeWidth: 1}).transform(vl.calculate(`${y}`).as("zero")).encode(vl.y().fieldQ("zero"));

const xAxis = {
  title: null,
  format: "%b %y",
  minExtent: 40,
  grid: false,
  labelPadding: 10,
  labelOverlap: true,
  labelBound: true,
  labelSeparation: 5
};

const yAxis = (input) => ({
  title: null,
  domain: false,
  labelPadding: 10,
  minExtent: 40,
  ticks: false,
  labelOverlap: true,
  labelBound: true,
  labelSeparation: 5,
  format: input.axisFormat,
  gridDash: input.gridDash,
  grid: false
});

function makeLines(input, selected) {
  const lines = vl.markLine({point: {fill: "transparent", size: 200}}).encode(
    vl.x().fieldT("Monat").axis({...xAxis, labelAngle: -0}),
    vl.y().fieldQ("value").axis(yAxis(input)).scale({zero: false, nice: false}),
    vl.color()
      .fieldN("key")
      .legend({orient: "top-left", symbolType: "stroke", title: null, symbolSize: 200, values: selected})
      .scale({domain: indices, range: colors}),
    vl.tooltip([
      {field: "key", title: "Index"},
      {field: "value", title: "Indexwert", format: input.tipFormat},
      {field: "Monat", title: "Monat", type: "temporal", format: "%b %y"}
    ])
  );

  const text = vl
    .markText({xOffset: 30, fontSize: 15})
    .transform(
      vl.window(vl.row_number().as("row")),
      vl.joinaggregate({op: "max", field: "row", as: "max"}).groupby("key"),
      vl.filter("datum.row == datum.max"),
      vl.calculate("(datum.value - 100) / 100").as("value2")
    )
    .encode(
      vl.x().fieldT("Monat"),
      vl.y().fieldQ("value"),
      vl.text().fieldQ(input.textValue).format("+,.1%"),
      vl.color().fieldN("key").scale({domain: indices, range: colors})
    );

  return vl.layer(lines, text);
}

function makeFormel(input, wide) {
  const lineData = input.inputData.filter(aq.escape((d) => input.variablesLine.includes(d.key)));
  const areaData = input.inputData
    .filter(aq.escape((d) => input.variablesArea.includes(d.key)))
    .derive({MonatTT: (d) => op.lag(d.Monat, 3)})
    .derive({MonatTT: (d) => op.lag(d.MonatTT, 3)});

  const lines = vl
    .markLine({point: {fill: "transparent", size: 200}, opacity: 1})
    .transform(vl.calculate("datum.key == 'rollierendeInflation' ? 'Rollierende Inflation**' : datum.key").as("key"))
    .encode(
      vl.x().fieldT("Monat").axis(xAxis),
      vl.y().fieldQ("value").axis(yAxis(input)).scale({zero: false, nice: false}),
      vl.color()
        .fieldN("key")
        .legend({
          orient: wide ? "bottom" : "none",
          legendX: 225,
          legendY: 30,
          direction: "vertical",
          symbolOpacity: 1,
          title: "Linien",
          symbolSize: 200,
          symbolType: "stroke",
          values: ["Lohnerhöhung"]
        })
        .scale({domain: [...indices, "rollierendeInflation", "langfristigesProduktivitätswachstum", "Lohnerhöhung"], range: colors}),
      vl.tooltip([
        {field: "key", title: "Veränderung"},
        {field: "value", title: "Wert", format: "+,.1%"},
        {field: "Monat", title: "Monat", type: "temporal", format: "%b %y"}
      ])
    )
    .data(lineData);

  const area = vl
    .markArea({opacity: 0.4})
    .transform(
      vl.calculate("datum.key == 'rollierendeInflation' ? 'Rollierende Inflation**' : datum.key").as("key"),
      vl.calculate("datum.key == 'langfristigesProduktivitätswachstum' ? 'Produktivitätsgewinne***' : datum.key").as("key")
    )
    .encode(
      vl.x().fieldT("Monat"),
      vl.y().fieldQ("value"),
      vl.fill()
        .fieldN("key")
        .legend({
          orient: wide ? "bottom" : "none",
          direction: "vertical",
          symbolType: "square",
          legendX: 225,
          legendY: 70,
          title: "Flächen",
          symbolSize: 200,
          values: ["Rollierende Inflation**", "Produktivitätsgewinne***"]
        })
        .scale({domain: [...indices, "Rollierende Inflation**", "Produktivitätsgewinne***", "Lohnerhöhung"], range: colors}),
      vl.tooltip([
        {field: "key", title: "Veränderung"},
        {field: "value", title: "Wert", format: "+,.1%"},
        {field: "MonatTT", title: "Monat", type: "temporal", format: "%b %y"}
      ])
    )
    .data(areaData);

  return vl.layer(area, lines, ruleAt(0).data(lineData));
}

function makeReallohn(input, wide) {
  const lineData = input.inputData.filter(aq.escape((d) => input.variablesLine.includes(d.key)));
  const areaData = input.inputData
    .filter(aq.escape((d) => input.variablesArea.includes(d.key)))
    .derive({value: (d) => (d.key === "Verbraucherpreisindex" ? 100 - (d.value - 100) : d.value)});

  const lines = vl
    .markLine({point: {fill: "transparent", size: 200}, opacity: 1})
    .transform(vl.calculate("(datum.value - 100) / 100").as("value"))
    .encode(
      vl.x().fieldT("Monat").axis(xAxis),
      vl.y().fieldQ("value").axis(yAxis(input)).scale({zero: false, nice: false}),
      vl.color()
        .fieldN("key")
        .legend({
          orient: wide ? "bottom" : "none",
          legendX: 225,
          legendY: 30,
          direction: "vertical",
          symbolOpacity: 1,
          title: "Linien",
          symbolSize: 200,
          symbolType: "stroke",
          labelLimit: 1000,
          values: ["Reallohnindex"]
        })
        .scale({domain: indices, range: colors}),
      vl.tooltip([
        {field: "key", title: "Index"},
        {field: "value", title: "Wert", format: ",.1%"},
        {field: "Monat", title: "Monat", type: "temporal", format: "%b %y"}
      ])
    )
    .data(lineData);

  const area = vl
    .markArea({opacity: 0.4})
    .transform(vl.calculate("(datum.value - 100) / 100").as("value"))
    .encode(
      vl.x().fieldT("Monat"),
      vl.y().fieldQ("value"),
      vl.fill()
        .fieldN("key")
        .legend({
          orient: wide ? "bottom" : "none",
          direction: "vertical",
          symbolType: "square",
          legendX: 225,
          legendY: 70,
          title: "Flächen",
          symbolSize: 200,
          labelLimit: 1000,
          values: ["Tariflohnindex", "Verbraucherpreisindex"]
        })
        .scale({domain: indices, range: colors}),
      vl.tooltip([
        {field: "key", title: "Index"},
        {field: "value", title: "Wert", format: ",.1%"},
        {field: "Monat", title: "Monat", type: "temporal", format: "%b %y"}
      ])
    )
    .data(areaData);

  return vl.layer(area, lines, ruleAt(0).data(lineData));
}

// Source and footnotes below the main chart (also written to the xlsx download).
const footnotes = [
  "Q: Statistik Austria, Eigene Berechnungen",
  "* Produktivitätsindex wird aufgrund starker Schwankungen immer als gleitender Mittelwert (12 Monate) dargestellt.",
  "** Gleitender Mittelwert (12 Monate) um 3 Monate zeitverzögert um Stand bei Verhandlungen widerzuspiegeln.",
  "*** Gleitender Mittelwert über 48 Monate um langfristiges Produktivitätswachstum widerzuspiegeln.",
  "Tariflohnindex: Basis 2006 und 2016 verkettet",
  "Produktivitätsindex: Basis 2010, 2015 und 2020 verkettet",
  "Verbraucherpreisindex: Basis 2010, 2015 und 2020 verkettet"
];

function makeLogo(logoUrl) {
  const corners = [{y: 0, x: 0}, {y2: 1, x2: 1}];
  return vl.layer(
    vl
      .markImage({align: "left", baseline: "middle", clip: false, height: 24, width: 100, xOffset: -40, yOffset: -50})
      .data(corners)
      .encode(vl.x().fieldQ("x").axis(null), vl.y().fieldQ("y").axis(null), vl.url().value(logoUrl), vl.href().value("https://www.wifo.ac.at/"))
      .width(10)
      .height(20),
    vl
      .markText({xOffset: -40, yOffset: -20, align: "left"})
      .data(corners)
      .encode(
        vl.x().fieldQ("x").axis(null),
        vl.y().fieldQ("y").axis(null),
        vl.text().value(footnotes)
      )
      .width(20)
      .height(20)
  );
}

// Main panel ("Index" or "Veränderung") above the Benya-Formel and Reallohnentwicklung panels.
export function renderTracker({viz, selected, Index, Methode, startIndexDate, endIndexDate, maxPROD, logoUrl, config, wide = window.innerWidth > 900}) {
  const lines = vl
    .layer(makeLines({axisFormat: ",.0f", tipFormat: ",.1f", gridDash: null, textValue: "value2"}, selected), ruleAt(100))
    .data(indexData(viz, selected, maxPROD))
    .width(wide ? 680 : 280)
    .height(wide ? 320 : 220)
    .title({
      text: "Index",
      subtitle: Methode === "Basisjahr" ? "Durchschnitt " + startIndexDate + " bis " + endIndexDate + " = 100" : startIndexDate + " = 100",
      align: "left",
      anchor: "start"
    });

  const changes = vl
    .layer(ruleAt(0), makeLines({axisFormat: "+,.0%", tipFormat: "+,.1%", gridDash: null, textValue: "value"}, selected))
    .data(changeData(viz, selected, maxPROD))
    .width(wide ? 680 : 280)
    .height(wide ? 320 : 220)
    .title({text: "Veränderung des Index", subtitle: "im Vergleich zum Vorjahresmonat", align: "left", anchor: "start"});

  const real = makeReallohn(
    {axisFormat: "+,.0%", variablesLine: ["Reallohnindex"], variablesArea: ["Tariflohnindex", "Verbraucherpreisindex"], inputData: flatIndexData(viz)},
    wide
  )
    .width(wide ? 330 : 200)
    .height(wide ? 200 : 120)
    .title({text: "Reallohnentwicklung", subtitle: "Tariflohn - Inflation", align: "left", anchor: "start"});

  const formel = makeFormel(
    {
      axisFormat: "+,.0%",
      variablesLine: ["Lohnerhöhung"],
      variablesArea: ["rollierendeInflation", "langfristigesProduktivitätswachstum"],
      inputData: flatChangeData(viz)
    },
    wide
  )
    .width(wide ? 330 : 200)
    .height(wide ? 200 : 120)
    .title({text: "Benya-Formel", subtitle: "Grundlage der Lohnforderungen", align: "left", anchor: "start"});

  const panels = (wide ? vl.hconcat(formel, real) : vl.vconcat(formel, real)).resolve({legend: {fill: "independent", color: "independent"}});

  return vl
    .vconcat(vl.hconcat(vl.vconcat(Index === "Index" ? lines : changes)), panels, ...(logoUrl ? [makeLogo(logoUrl)] : []))
    .config(config)
    .render({renderer: "svg"});
}

// --- Page helpers --------------------------------------------------------------

// Choice between a few options, shown as a row of buttons with the selected one highlighted
// (styled via .button-radio in style.css). `options` is a list of [label, value] pairs.
function buttonRadio(options, {label, value}) {
  const input = Inputs.radio(new Map(options), {label, value});
  input.classList.add("button-radio");
  return input;
}

// Colors each checkbox like its series in the charts (drawn via CSS variables in style.css);
// light colors get a dark check mark.
function colorCheckboxes(input, palette) {
  input.querySelectorAll("input[type=checkbox]").forEach((box, i) => {
    box.style.setProperty("--box-color", palette[i]);
    box.style.setProperty("--mark-color", d3.lab(palette[i]).l > 75 ? "#333" : "#fff");
  });
  return input;
}

// Controls of the main tracker, with the labels and defaults of the notebook.
export function trackerInputs({TLI, VPI, PROD}) {
  const sparten = [...new Set(TLI.map((d) => d.Sparte))];
  const months = [...new Set(VPI.map((d) => d.Monat))].sort();
  const start = parseDate(months[0]);
  const end = parseDate(months[months.length - 1]);
  return {
    Sparte: groupedSelect(
      sparten,
      [
        {label: "Insgesamt", size: 1},
        {label: "WKO-Systematik", size: 71},
        {label: "Kollektivverträge", size: 52},
        {label: "Gruppen", size: sparten.length - 124}
      ],
      {label: "Tariflohn"}
    ),
    checkboxes: colorCheckboxes(
      Inputs.checkbox(indices, {label: "Index", value: ["Tariflohnindex", "Produktivitätsindex", "Verbraucherpreisindex"]}),
      colors
    ),
    Glättung: colorCheckboxes(Inputs.checkbox(["gleitende Mittelwerte"], {label: "Glättung*", value: ["gleitende Mittelwerte"]}), ["#2d5b5e"]),
    Methode: buttonRadio(
      [
        ["Ausgangsjahr", "Basisjahr"],
        ["Ausgangsmonat", "Rollierend"]
      ],
      {label: "Indexbasis", value: "Basisjahr"}
    ),
    Index: buttonRadio(
      [
        ["Indexwert", "Index"],
        ["Veränderung", "Veränderung"]
      ],
      {label: "Werte", value: "Index"}
    ),
    Zeit: rangeSlider({
      min: 24,
      max: d3.timeMonth.count(start, end),
      step: 1,
      format: (d) => formatDate(d3.timeMonth.offset(start, d)),
      separator: " → ",
      color: "#2d5b5e",
      label: "Zeitraum"
    }),
    Einheit: buttonRadio(
      [
        ["Arbeitsstunde", "Produktivitätsindex je geleisteter Arbeitsstunde"],
        ["ArbeitnehmerIn", "Produktivitätsindex je unselbstständig Beschäftigten"]
      ],
      {label: "Produktivität pro", value: "Produktivitätsindex je geleisteter Arbeitsstunde"}
    ),
    Sektor: Inputs.select(new Set(PROD.map((d) => d.Sektor)), {label: "Produktivitätsindex", value: "Gesamt (VGR-Daten; Alle Sektoren)"}),
    Produktgruppe: Inputs.select(new Set(VPI.map((d) => d.Produktgruppe)), {label: "Verbraucherpreisindex", value: "Gesamt"})
  };
}

// Data and settings of the main tracker for the current selection; shared by the chart and the download.
export function trackerState({TLI, VPI, PROD, Sparte, checkboxes, Glättung, Methode, Index, Zeit, Einheit, Sektor, Produktgruppe}) {
  const merged = mergeData({VPI, TLI, PROD, Produktgruppe, Sparte, Sektor, Einheit});
  const months = merged.map((d) => d.Monat);
  const smooth = Glättung.includes("gleitende Mittelwerte");
  return {
    viz: vizData({merged, smooth, Zeit, Methode}),
    selected: checkboxes,
    Index,
    Methode,
    smooth,
    startIndexDate: formatDate(parseDate(months[Zeit[0]])),
    endIndexDate: formatDate(parseDate(months[Zeit[0] + 11])),
    period: [months[Zeit[0]], months[Zeit[1]]].map((m) => formatDate(parseDate(m))),
    maxPROD: d3.max(PROD, (d) => d.Monat),
    selection: {Sparte, Sektor, Einheit, Produktgruppe}
  };
}

// Main tracker chart (notebook @wifo-viz/tarifloehne, cell "display").
export function trackerChart(state, logoUrl) {
  return renderTracker({...state, logoUrl, config: makeConfig()});
}

// "Metaller-Verhandlungen" scenario (notebook d/662b7dbdbc98b5f9). The notebook selects rows 129–200,
// which was Oct 2021 onwards when the CSVs started in 2011; the window is anchored to that date here.
export function scenarioChart({TLI, VPI, PROD, from = "2021-10-01"}) {
  const merged = mergeData({
    VPI,
    TLI,
    PROD,
    Produktgruppe: "Gesamt",
    Sparte: "Fachverband Bergwerke und Stahl",
    Sektor: "Produzierender Bereich (ÖNACE Abschnitte B-F)",
    Einheit: "Produktivitätsindex je geleisteter Arbeitsstunde"
  });
  const first = merged.findIndex((d) => d.Monat === from);
  const Zeit = [first, first + 71];
  const months = merged.map((d) => d.Monat);
  return renderTracker({
    viz: vizData({merged, smooth: false, Zeit, Methode: "Rollierend", baseLimit: 13}),
    selected: ["Tariflohnindex", "Verbraucherpreisindex"],
    Index: "Index",
    Methode: "Rollierend",
    startIndexDate: formatDate(parseDate(months[Zeit[0]])),
    endIndexDate: formatDate(parseDate(months[Zeit[0] + 12])),
    config: makeConfig({font: "Century Gothic", time: null})
  });
}

// --- Download ------------------------------------------------------------------

const xlsxHeader = (value) => ({value, fontWeight: "bold"});
const xlsxMonth = (month) => ({value: new Date(month), type: Date, format: "mmm yyyy"});
const xlsxNumber = (value, format) => (value == null || Number.isNaN(value) ? null : {value, type: Number, format});

// One row per month and one column per series, from folded {Monat, key, value} rows.
function wideSheet(rows, columns, format, transform = (v) => v) {
  const byMonth = d3.group(rows, (d) => d.Monat);
  return [
    [xlsxHeader("Monat"), ...columns.map(([, label]) => xlsxHeader(label))],
    ...[...byMonth].map(([month, values]) => [
      xlsxMonth(month),
      ...columns.map(([key]) => xlsxNumber(transform(values.find((d) => d.key === key)?.value), format))
    ])
  ];
}

// The values shown in the three panels of the main tracker, plus the settings that produced them.
function trackerSheets({viz, selected, Index, Methode, smooth, startIndexDate, endIndexDate, period, maxPROD, selection}) {
  const series = indices.filter((key) => selected.includes(key)).map((key) => [key, key]);
  const main =
    Index === "Index"
      ? {sheet: "Index", data: wideSheet(indexData(viz, selected, maxPROD).objects(), series, "0.0")}
      : {sheet: "Veränderung ggü. Vorjahr", data: wideSheet(changeData(viz, selected, maxPROD).objects(), series, "0.0%")};
  const base = Methode === "Basisjahr" ? `Durchschnitt ${startIndexDate} bis ${endIndexDate} = 100` : `${startIndexDate} = 100`;
  const info = [
    ["Tariflohn-Tracker", "WIFO, https://data-science.wifo.ac.at/tariflohntracker/"],
    ["Tariflohnindex", selection.Sparte],
    ["Verbraucherpreisindex", selection.Produktgruppe],
    ["Produktivitätsindex", `${selection.Sektor}, ${selection.Einheit}`],
    ["Zeitraum", `${period[0]} bis ${period[1]}`],
    ["Indexbasis", base],
    ["Glättung", smooth ? "gleitender Mittelwert (12 Monate)" : "keine"],
    ["Blatt Index / Veränderung", Index === "Index" ? "Indexwerte" : "Veränderung gegenüber dem Vorjahresmonat"],
    ["Blatt Benya-Formel", "Veränderung gegenüber dem Vorjahresmonat"],
    ["Blatt Reallohnentwicklung", "Veränderung gegenüber der Indexbasis"],
    ["Heruntergeladen", new Date().toLocaleDateString("de-AT")],
    [],
    ...footnotes.map((note) => [note])
  ];
  return [
    {sheet: "Info", data: info.map((row) => (row.length === 2 ? [xlsxHeader(row[0]), row[1]] : row)), columns: [{width: 28}, {width: 60}]},
    {...main, columns: [{width: 12}, ...series.map(() => ({width: 22}))], stickyRowsCount: 1},
    {
      sheet: "Benya-Formel",
      data: wideSheet(
        flatChangeData(viz).objects(),
        [
          ["Lohnerhöhung", "Lohnerhöhung"],
          ["rollierendeInflation", "Rollierende Inflation**"],
          ["langfristigesProduktivitätswachstum", "Produktivitätsgewinne***"]
        ],
        "0.0%"
      ),
      columns: [{width: 12}, {width: 22}, {width: 22}, {width: 22}],
      stickyRowsCount: 1
    },
    {
      sheet: "Reallohnentwicklung",
      data: wideSheet(
        flatIndexData(viz).objects(),
        [
          ["Tariflohnindex", "Tariflohnindex"],
          ["Verbraucherpreisindex", "Verbraucherpreisindex"],
          ["Reallohnindex", "Reallohnindex"]
        ],
        "0.0%",
        (v) => (v == null ? v : (v - 100) / 100)
      ),
      columns: [{width: 12}, {width: 22}, {width: 22}, {width: 22}],
      stickyRowsCount: 1
    }
  ];
}

// Button that downloads the currently displayed values of the main tracker as .xlsx.
export function downloadButton(state, {label = "Ausgewählte Daten herunterladen"} = {}) {
  const icon = html`<svg width="12" height="12" viewBox="0 0 12 12" fill="none" stroke="currentColor" stroke-width="1.5" aria-hidden="true">
    <path d="M6 1v7M3 5l3 3 3-3M1 10.5h10"/>
  </svg>`;
  const button = html`<button type="button" class="download-button" title="Excel-Datei (.xlsx)">${icon}${label}</button>`;
  button.onclick = async () => {
    button.disabled = true;
    try {
      const name = state.selection.Sparte.replace(/[^\p{L}\p{N}]+/gu, "-").replace(/^-|-$/g, "");
      await writeXlsxFile(trackerSheets(state)).toFile(`Tariflohn-Tracker_${name}_${new Date().toISOString().slice(0, 10)}.xlsx`);
    } finally {
      button.disabled = false;
    }
  };
  return button;
}
