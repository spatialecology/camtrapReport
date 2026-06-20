---
output:
  pdf_document: default
  html_document: default
---
# Resources

<style>
.resource-list {
  display: grid;
  grid-template-columns: 1fr;
  gap: 1rem;
  margin-top: 1.5rem;
}

.resource-card {
  display: grid;
  grid-template-columns: 180px 1fr;
  gap: 1.2rem;
  align-items: start;
  border: 1px solid #DDE7E1;
  border-radius: 14px;
  padding: 1rem;
  background: #FFFFFF;
  box-shadow: 0 4px 14px rgba(0, 0, 0, 0.06);
}

.resource-card img {
  width: 100%;
  height: 120px;
  object-fit: contain;
  background: #FFFFFF;
  border-radius: 10px;
  padding: 0.35rem;
}

.workshop-card img {
  height: 140px;
  object-fit: cover;
  object-position: center;
}

.resource-title {
  margin-top: 0.2rem;
  margin-bottom: 0.45rem;
  color: #0F5E4D;
  font-size: 1.05rem;
  line-height: 1.25;
  font-weight: 700;
}

.resource-card p {
  font-size: 0.93rem;
  line-height: 1.45;
  margin-bottom: 0.55rem;
}

.resource-type {
  display: inline-block;
  margin-bottom: 0.45rem;
  padding: 0.18rem 0.55rem;
  border-radius: 999px;
  background: #E7F2ED;
  color: #1F6F54;
  font-size: 0.75rem;
  font-weight: 700;
}

.resource-card a {
  font-weight: 600;
}

.section-label {
  display: block;
  margin-top: 1.2rem;
  margin-bottom: 0.6rem;
  color: #0F5E4D;
  font-weight: 700;
  font-size: 1.05rem;
}

@media (max-width: 800px) {
  .resource-card {
    grid-template-columns: 1fr;
  }

  .resource-card img,
  .workshop-card img {
    height: 170px;
  }
}
</style>

This page lists publications, workshops, and training activities related to `camtrapReport`. It is intended as a central place for users to find scientific outputs, tutorials, and workshop information.

## Publications

The following publications describe `camtrapReport` or related workflows.

<div class="resource-list">

<div class="resource-card">
<img src="man/figures/Outreach/ECM9.png" alt="IX European Congress of Mammalogy logo">

<div>
<span class="resource-type">Conference abstract</span>

<div class="resource-title">Automating camera-trap data reporting for wildlife monitoring</div>

<p>Ebrahimi, E., Stubbe, A., Dijkhuis, L., de Knegt, H., Liefting, Y., & Jansen, P. A. (2025). Automating camera-trap data reporting for wildlife monitoring. <em>IX European Congress of Mammalogy (ECM9)</em>, Patras, Greece, 31 March – 4 April 2025.</p>

<p><a href="https://doi.org/10.5281/zenodo.15721045">View publication</a></p>
</div>
</div>

<div class="resource-card">
<img src="man/figures/Outreach/TIBS2026.png" alt="The International Biogeography Society conference logo">

<div>
<span class="resource-type">Conference abstract</span>

<div class="resource-title">CamtrapReport: An R package for automating camera-trap data reporting for wildlife monitoring</div>

<p>Ebrahimi, E., & Jansen, P. A. (2026). CamtrapReport: An R package for automating camera-trap data reporting for wildlife monitoring. <em>The International Biogeography Society – 12th Biennial Conference</em>, Aarhus, Denmark, 5–10 January 2026.</p>

<p><a href="https://doi.org/10.5281/zenodo.18405441">View publication</a></p>
</div>
</div>

<div class="resource-card">
<img src="man/figures/Outreach/NAEM2026.png" alt="Netherlands Annual Ecology Meeting logo">

<div>
<span class="resource-type">Conference abstract</span>

<div class="resource-title">camtrapReport: An R package for automating camera-trap data reporting for wildlife monitoring</div>

<p>Ebrahimi, E., Stubbe, A., Dijkhuis, L. R., Liefting, Y., de Knegt, H. J., & Jansen, P. A. (2026). camtrapReport: An R package for automating camera-trap data reporting for wildlife monitoring. <em>Netherlands Annual Ecology Meeting (NAEM 2026)</em>, Lunteren, the Netherlands, 10–11 February 2026.</p>

<p><a href="https://doi.org/10.5281/zenodo.20774222">View publication</a></p>
</div>
</div>

</div>

## Workshops and training

This section lists workshops, tutorials, and training activities related to `camtrapReport`.

<div class="resource-list">

<div class="resource-card workshop-card">
<img src="man/figures/Outreach/ws2.png" alt="camtrapReport TrapLab workshop">

<div>
<span class="resource-type">Training workshop</span>

<div class="resource-title">An overview of the functionality of <code>camtrapReport</code>: An R package for automating camera-trap data reporting in wildlife monitoring</div>

<p><strong>Host:</strong> TrapLab community, Utrecht University, Utrecht, the Netherlands<br>
<strong>Date:</strong> May 4, 2026</p>
</div>
</div>

<div class="resource-card workshop-card">
<img src="man/figures/Outreach/ws1.png" alt="camtrapReport INBO workshop">

<div>
<span class="resource-type">Technical workshop</span>

<div class="resource-title"><code>camtrapReport</code>: A modular R package for automating camera-trap data reporting in wildlife monitoring</div>

<p><strong>Host:</strong> Research Institute for Nature and Forest (INBO), Brussels, Belgium<br>
<strong>Date:</strong> May 29, 2026</p>
</div>
</div>

</div>

<span class="section-label">Upcoming workshops</span>


## Community and contact

`camtrapReport` is designed for the camera-trap community. Organisations, research teams, and camera-trap networks interested in arranging workshops, exploring collaboration opportunities, or contributing new `camtrapReport` modules are welcome to get in touch by email at [e.ebrahimi@uu.nl](mailto:e.ebrahimi@uu.nl) or via [LinkedIn](https://www.linkedin.com/in/elham-ebrahimi-90b519b8/).
