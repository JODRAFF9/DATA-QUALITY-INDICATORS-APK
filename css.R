
# CSS personnalisé
custom_css <- "
/* Styles généraux */
body {
  font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
  background-color: #f8f9fa;
}

/* En-tête */
.navbar {
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  border: none;
  border-radius: 0;
}

.navbar-brand {
  font-weight: bold;
  color: white !important;
  font-size: 1.5em;
  text-align: center;
}

/* Panneau latéral */
.sidebar {
  background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
  border-radius: 12px;
  box-shadow: 0 4px 12px rgba(0, 0, 0, 0.08);
  padding: 20px;
  margin: 10px;
  border: 1px solid #e9ecef;
  transition: all 0.3s cubic-bezier(0.4, 0, 0.2, 1);
}

.sidebar:hover {
  box-shadow: 0 8px 24px rgba(0, 0, 0, 0.12);
  transform: translateY(-2px);
}

/* Cartes des sections */
.well {
  background-color: white;
  border: 1px solid #f0f0f0;
  border-radius: 10px;
  box-shadow: 0 2px 8px rgba(0, 0, 0, 0.04);
  padding: 18px;
  margin-bottom: 16px;
  transition: all 0.3s ease;
  border-left: 4px solid transparent;
}

.well:hover {
  border-left-color: #3498db;
  box-shadow: 0 4px 12px rgba(0, 0, 0, 0.08);
}

/* En-têtes des sections */
.well h4 {
  color: #2c3e50;
  font-weight: 600;
  margin-bottom: 16px;
  font-size: 28px;
  display: flex;
  align-items: center;
  gap: 8px;
}

.well h4 i {
  color: #3498db;
  font-size: 16px;
}

/* Boutons améliorés */
.btn-primary {
  background: linear-gradient(135deg, #3498db, #2980b9);
  border: none;
  border-radius: 8px;
  font-weight: 600;
  transition: all 0.3s ease;
  padding: 10px 16px;
  font-size: 14px;
  box-shadow: 0 2px 6px rgba(52, 152, 219, 0.2);
}

.btn-primary:hover {
  transform: translateY(-2px);
  box-shadow: 0 6px 16px rgba(52, 152, 219, 0.4);
  background: linear-gradient(135deg, #2980b9, #2471a3);
}

.btn-success {
  background: linear-gradient(135deg, #27ae60, #229954);
  border: none;
  border-radius: 8px;
  font-weight: 600;
  transition: all 0.3s ease;
  padding: 10px 16px;
  font-size: 14px;
  box-shadow: 0 2px 6px rgba(39, 174, 96, 0.2);
}

.btn-success:hover {
  transform: translateY(-2px);
  box-shadow: 0 6px 16px rgba(39, 174, 96, 0.4);
  background: linear-gradient(135deg, #229954, #1e8449);
}

/* Zone d'upload améliorée */
.help-block {
  background: #f8f9fa;
  border-radius: 6px;
  padding: 10px;
  margin-top: 10px;
  font-size: 12px;
  color: #6c757d;
  border-left: 3px solid #3498db;
}

.help-block i {
  color: #3498db;
  margin-right: 5px;
}

/* Checkboxes et radios améliorés */
.checkbox, .radio {
  margin-bottom: 8px;
}

.checkbox label, .radio label {
  font-weight: 500;
  color: #495057;
  cursor: pointer;
  transition: color 0.2s ease;
}

.checkbox label:hover, .radio label:hover {
  color: #3498db;
}

/* Slider amélioré */
.irs--shiny .irs-bar {
  background: #3498db;
}

.irs--shiny .irs-handle {
  border: 3px solid #3498db;
}

/* Input numérique */
.form-control {
  border-radius: 6px;
  border: 1px solid #ddd;
  transition: all 0.3s ease;
}

.form-control:focus {
  border-color: #3498db;
  box-shadow: 0 0 0 0.2rem rgba(52, 152, 219, 0.25);
}

/* Responsive */
@media (max-width: 768px) {
  .sidebar {
    margin: 5px;
    padding: 15px;
  }
  
  .well {
    padding: 12px;
  }
}

/* Onglets */
.nav-tabs > li > a {
  color: #555;
  font-weight: 600;
  border-radius: 8px 8px 0 0;
  margin-right: 5px;
}

.nav-tabs > li.active > a {
  background-color: #3498db;
  color: white;
  border: none;
}

/* Cartes de contenu */
.tab-content {
  background-color: white;
  border-radius: 0 8px 8px 8px;
  padding: 20px;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.05);
  min-height: 500px;
}

/* Indicateurs */
.help-block {
  color: #666;
  font-size: 0.9em;
}

/* Tableaux */
.dataTables_wrapper {
  border-radius: 8px;
  overflow: hidden;
}

/* Graphiques */
.plotly.html-widget {
  border-radius: 8px;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
}

/* Notifications */
.shiny-notification {
  border-radius: 8px;
  font-weight: 600;
}

/* En-têtes */
h3, h4 {
  color: #2c3e50;
  font-weight: 700;
}

h4 {
  border-bottom: 2px solid #3498db;
  padding-bottom: 5px;
  margin-top: 20px;
}

/* Icônes dans les titres */
.fa, .fas, .far {
  margin-right: 8px;
}

/* Responsive */
@media (max-width: 768px) {
  .sidebar {
    margin: 5px;
    padding: 15px;
  }
  
  .tab-content {
    padding: 15px;
    margin: 5px;
  }
}

/* Animation de chargement */
.shiny-progress-container {
  position: fixed;
  top: 0;
  width: 100%;
  z-index: 9999;
}

.shiny-progress .progress {
  height: 8px;
  margin-bottom: 0;
}

.shiny-progress .bar {
  background: linear-gradient(90deg, #3498db, #2980b9);
}

/* Amélioration des contrôles */
.form-control {
  border-radius: 6px;
  border: 1px solid #ddd;
  box-shadow: inset 0 1px 2px rgba(0,0,0,0.1);
}

.form-control:focus {
  border-color: #3498db;
  box-shadow: 0 0 0 0.2rem rgba(52, 152, 219, 0.25);
}

/* Checkboxes et radios */
.checkbox, .radio {
  margin-top: 10px;
  margin-bottom: 10px;
}

.checkbox-inline, .radio-inline {
  margin-right: 15px;
}

/* Séparateurs */
hr {
  border-top: 1px solid #e0e0e0;
  margin: 20px 0;
}

/* Badges pour les indicateurs */
.badge {
  background-color: #3498db;
  border-radius: 12px;
  padding: 4px 8px;
  font-size: 0.8em;
}

/* Styles spécifiques pour les cartes de résultats */
.result-card {
  font-size: 20px; /* Taille générale augmentée */
  padding: 20px;
  background: white;
  border-radius: 10px;
  box-shadow: 0 2px 8px rgba(0,0,0,0.1);
  margin: 15px 0;
}

/* Sorties de texte (verbatimTextOutput) */
.result-card pre {
  font-size: 20px !important;
  line-height: 1.6;
  background: #f8f9fa;
  padding: 15px;
  border-radius: 8px;
  border-left: 4px solid #3498db;
  font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
}

/* DataTables dans les result-card */
.result-card .dataTables_wrapper {
  font-size: 20px !important;
}

.result-card .dataTables_wrapper th {
  font-size: 20px !important;
  font-weight: 600;
}

.result-card .dataTables_wrapper td {
  font-size: 20px !important;
}

/* Titres dans les cartes */
.result-card h3, 
.result-card h4 {
  font-size: 18px !important;
  font-weight: 600;
  color: #2c3e50;
  margin-bottom: 15px;
}

/* Textes standards dans les cartes */
.result-card p, 
.result-card div {
  font-size: 18px;
  line-height: 1.5;
}
/* Solution simple - Augmente tout */
.main-panel-custom * {
  font-size: 18px;
}

.main-panel-custom .nav-tabs > li > a {
  font-size: 16px !important;
  font-weight: 600;
}

.main-panel-custom pre {
  font-size: 15px !important;
  line-height: 1.5;
}
"


