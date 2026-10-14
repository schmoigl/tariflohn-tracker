---
title: Tariflohn-Tracker
---

```js
import {trackerInputs, trackerChart, scenarioChart} from "./components/tracker.js";

const TLI = FileAttachment("data/TLI.csv").csv();
const VPI = FileAttachment("data/VPI.csv").csv();
const PROD = FileAttachment("data/PROD.csv").csv();
const logoUrl = FileAttachment("assets/wifo-logo-chart.png").url();
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

<div class="page-header">
  <a class="lang-toggle" href="./en/" aria-label="Switch to English"><span class="active">DE</span><span>EN</span></a>
  <a class="logo" href="https://www.wifo.ac.at/"><img src="./assets/logo-100.svg" alt="WIFO"></a>
</div>

# Tariflohn-Tracker

Der interaktive Tariflohntracker des WIFO ermöglicht den Vergleich von Produktivität, Tariflöhnen und Verbraucherpreisen. Dabei bietet das explorative Tool eine Vielzahl von Einstellungsmöglichkeiten. Zum einen können neben der Entwicklung der Tariflöhne insgesamt auch Branchen und ausgewählte Kollektivverträge einzeln betrachtet werden.

Zum anderen kann unter "Index" ausgewählt werden, welche Indikatoren im Hauptpanel angezeigt werden sollen. Neben den genannten Indizes kann dort auch der "Benyaindex" oder der Reallohnindex ausgewählt werden. Bei ersterem handelt es sich um die Addition von Verbraucherpreisindex und Produktivitätsindex, also einer hypothetischen Tariflohnentwicklung, bei der sowohl Preissteigerungen als auch Produktivitätsgewinne die Tariflohnentwicklung (immer) vollständig bestimmen würden. Der Reallohnindex gibt lediglich wieder, was von den Tariflohnerhöhungen nach Abzug der Inflation übrig bleibt.

Da die Indizes saisonalen Schwankungen unterliegen, werden sie standardmäßig als gleitender Mittelwert über die letzten 12 Monate dargestellt, welcher aber unter der Option "Glättung" ausgeschaltet werden kann. In diesem Fall kann man z. B. für einzelne Branchen und Kollektivverträge gut erkennen, zu welchem Zeitpunkt die Tariflohnerhöhungen stattgefunden haben.

Darüber hinaus lässt sich unter "Indexbasis" bestimmen, ob die Durchschnittswerte der ersten 12 Monate des gewählten Zeitraums (Ausgangsjahr) oder der erste dargestellte Monat (Ausgangsmonat) der Indizes auf 100 gesetzt werden sollen. Gemeinsam mit der Auswahl des Betrachtungszeitraums kann so selbst ausgeschlossen werden, dass die Wahl der Indexbasis die beobachteten Ergebnisse beeinflusst. Unter "Werte" lässt sich außerdem einstellen, ob die Entwicklung der Indizes oder deren Veränderung gegenüber dem Vorjahresmonat gezeigt werden soll. Für besonders Experimentierfreudige gibt es unterhalb der Grafik noch die Möglichkeit, auch Produktivitäts- und Verbraucherpreisindex zu variieren.

<div class="inner-div">${inputs.Sparte}</div>
<div class="inner-div">${inputs.checkboxes}</div>
<div class="inner-div">${inputs.Glättung}</div>
<div class="inner-div">${inputs.Methode}</div>
<div class="inner-div">${inputs.Index}</div>
<div class="inner-div">${inputs.Zeit}</div>
<div class="inner-div">${trackerChart({TLI, VPI, PROD, Sparte, checkboxes, Glättung, Methode, Index, Zeit, Einheit, Sektor, Produktgruppe, logoUrl})}</div>
<div class="inner-div">${inputs.Einheit}</div>
<div class="inner-div">${inputs.Sektor}</div>
<div class="inner-div">${inputs.Produktgruppe}</div>

Das unten abgebildete Szenario "Metaller-Verhandlungen" zeigt beispielhaft die Entwicklung des Tariflohnindex (TLI), des Verbraucherpreisindex (VPI), der laufenden Reallöhne und die Komponenten der Benya-Formel für die beiden letzten Kollektivvertragsabschlüsse jeweils im November der Jahre 2021 und 2022.

Die Indexwerte für den TLI und den VPI zeigen, dass der Verbraucherpreisindex seit Oktober 2021 um 16,1% und der Tariflohnindex um 9,6% gestiegen ist. Die Verbraucherpreise steigen im Zeitverlauf kontinuierlich an. Im Gegensatz dazu werden die Kollektivvertragslöhne nur einmal jährlich, bei den Tarifverhandlungen im November, angehoben.

Daraus folgt in der Grafik rechts unten, dass die Reallöhne (rote Linie) ab März 2022 sinken, da die Lohnsteigerungen geringer waren als die Preissteigerungen. Die Lohnentwicklung ist in der Grafik als grüne Fläche dargestellt, die Preissteigerungen des VPI als blaue Fläche. Im Oktober 2022 beträgt der Reallohnverlust –8,0%. Aufgrund der Lohnverhandlungen im November steigen die Reallöhne wieder an, können aber die Preissteigerungen nicht aufholen. Da seit November 2022 die Preise bei gleichbleibenden Löhnen weiter gestiegen sind, hat sich auch der Reallohn weiter negativ entwickelt.

<div class="inner-div">${scenarioChart({TLI, VPI, PROD})}</div>

Die Gewerkschaften orientieren sich bei den Lohnverhandlungen an der Benya-Formel, deren Entwicklung in der Grafik links unten dargestellt ist. Dabei wird nicht die aktuelle Inflationsrate zugrunde gelegt, sondern der Mittelwert der letzten 12 Monatsraten zugrunde gelegt ("rollierende Inflation"). Hinzu kommt die mittelfristige gesamtwirtschaftliche Arbeitsproduktivität  – in unserem Tariflohnmonitor wird „mittelfristig“ als der Durchschnitt der letzten 48 Monate dargestellt. Für die Lohnverhandlungen im November 2021 zeigt sich, dass die Lohnsteigerungen der Benya-Formel entsprechend und sich aus der Summe der rollierenden Inflation und dem Produktivitätswachstum ergeben. Bei den Lohnverhandlungen im November 2022 hingegen wurde nur die rollierende Inflation abgegolten.

Zu beachten ist: Die Lohnentwicklung gibt die kollektivvertraglichen Mindestlöhne und nicht die Ist-Löhne wieder. Ist-Löhne sind Löhne, die sich ergeben, wenn mehr als kollektivvertraglich vereinbart bezahlt wird. Allerdings geben die Grafiken Auskunft über Veränderungen und nicht das Niveau, also die Höhe der Löhne. Dies ist deshalb wichtig, da im Großen und Ganzen die Ist-Löhne in einem ähnlichen Ausmaß wie die Kollektivvertragslöhne wachsen.

*Anmerkung: Die Reihe Produktivitätsgewinne im Panel Benya-Formel wurde in der ursprünglichen Version vom 25.09.2023 falsch berechnet. Nach einer Änderung am 26.09.2023 (16:23 MEZ), wird sie nun korrekt dargestellt.*

<div class="footer"></div>
