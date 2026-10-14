---
title: Collective Wage Tracker
---

```js
import {trackerInputs, trackerState, trackerChart, downloadButton, scenarioChart} from "../components/tracker.js";

const TLI = FileAttachment("../data/TLI.csv").csv();
const VPI = FileAttachment("../data/VPI.csv").csv();
const PROD = FileAttachment("../data/PROD.csv").csv();
const logoUrl = FileAttachment("../assets/wifo-logo-chart.png").url();
```

```js
const inputs = trackerInputs({TLI, VPI, PROD});
const Sparte = Generators.input(inputs.Sparte);
const checkboxes = Generators.input(inputs.checkboxes);
const Glättung = Generators.input(inputs.Glättung);
const Methode = Generators.input(inputs.Methode);
const Index = Generators.input(inputs.Index);
const Zeit = Generators.input(inputs.Zeit);
const Einheit = Generators.input(inputs.Einheit);
const Sektor = Generators.input(inputs.Sektor);
const Produktgruppe = Generators.input(inputs.Produktgruppe);
```

```js
const tracker = trackerState({TLI, VPI, PROD, Sparte, checkboxes, Glättung, Methode, Index, Zeit, Einheit, Sektor, Produktgruppe});
```

<div class="page-header">
  <a class="lang-toggle en" href="../" aria-label="Zur deutschen Version wechseln"><span>DE</span><span class="active">EN</span></a>
  <a class="logo" href="https://www.wifo.ac.at/"><img src="../assets/logo-100.svg" alt="WIFO"></a>
</div>

<h1>Collective Wage Tracker</h1>

<h2>The Interactive Tool To Understand Austrias Collective Bargaining</h2>

In the run-up to the autumn wage round, WIFO's interactive wage tracker enables a comparison of productivity, collective wages and consumer prices. The explorative tool offers a variety of setting options. On the one hand, in addition to the development of collective wages as a whole, sectors and selected collective agreements can also be viewed individually.

Secondly, under "Index" it is possible to select which indicators are to be displayed in the main panel. In addition to the indices mentioned, the "Benya index" or the real wage index can also be selected there. The former is the addition of the consumer price index and the productivity index, i.e., a hypothetical wage development in which both price increases and productivity gains would (always) completely determine the wage development. The real wage index merely reflects what remains of the collective wage increases after deducting inflation.

As the indices are subject to seasonal fluctuations, they are shown by default as a moving average over the last 12 months, but this can be switched off under the option "Smoothing". In this case it is easy to see, for example, for individual sectors and collective agreements, at which point in time the collective wage increases took place.

Furthermore, under "Index base" it can be determined whether the average values of the first 12 months of the selected period (starting year) or the first month shown (starting month) of the indices should be set to 100. Together with the selection of the observation period, it can thus be excluded that the choice of the index base influences the observed results. Under "Values" you can also set whether the development of the indices or their change compared to the same month of the previous year should be shown. For those who are particularly keen to experiment, there is also the option below the graph to vary the productivity and consumer price index.

<div class="inner-div">${inputs.Sparte}</div>
<div class="inner-div">${inputs.checkboxes}</div>
<div class="inner-div">${inputs.Glättung}</div>
<div class="inner-div">${inputs.Methode}</div>
<div class="inner-div">${inputs.Index}</div>
<div class="inner-div">${inputs.Zeit}</div>
<div class="inner-div">${trackerChart(tracker, logoUrl)}</div>
<div class="inner-div">${inputs.Einheit}</div>
<div class="inner-div">${inputs.Sektor}</div>
<div class="inner-div">${inputs.Produktgruppe}</div>
<div class="inner-div download">${downloadButton(tracker, {label: "Download selected data"})}</div>

The "Metalworkers' Negotiations" scenario shows, by way of example, the development of the Collective Wage Index (TLI), the Consumer Price Index (CPI), the current real wages and the components of the Benya formula for the last two collective agreements in November of each of the years 2021 and 2022.

The index values for the TLI and the CPI show that the consumer price index has increased by 16.1 percent and the collectively agreed wage index by 9.6 percent since October 2021. Consumer prices are rising steadily over time. In contrast, collective agreement wages are only increased once a year, during the collective bargaining negotiations in November.

It follows from this in the graph on the bottom right that real wages (red line) will fall from March 2022, as wage increases were lower than price increases. The wage development is shown in the graph as a green area, the CPI price increases as a blue area. In October 2022, the real wage loss is –8.0 percent. Due to the wage negotiations in November, real wages rise again, but cannot catch up with the price increases. Since prices have continued to rise since November 2022 while wages have remained unchanged, the real wage has also continued to develop negatively.

<div class="inner-div">${scenarioChart({TLI, VPI, PROD})}</div>

The trade unions base their wage negotiations on the Benya formula, the development of which is shown in the graph on the left below. This is not based on the current inflation rate, but on the average of the last 12 monthly rates ("rolling inflation"). Added to this is the medium-term overall economic labour productivity – in our Collective Wage Monitor the average of the last 48 months. For the wage negotiations in November 2021, it is then shown that the wage increases are the sum of rolling inflation and half of productivity growth. In the November 2022 wage negotiations, on the other hand, only rolling inflation was compensated.

Please note: The wage development reflects the collectively agreed minimum wages and not the actual wages. Actual wages are wages that result when more is paid than agreed in the collective agreement. However, the graphs provide information on changes and not the level, i.e., the amount of wages. This is important because, by and large, actual wages grow at a similar rate as wages under collective agreements.

<div class="footer"></div>
