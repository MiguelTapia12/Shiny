// =============================================================================
// i18n.js — Motor de Traducción del lado del Cliente
// CR Breeding System — Módulo 8: Internacionalización
// =============================================================================
// Funciona tanto dentro de Shiny (www/) como en las apps HTML de tablet.
//
// USO:
//   1. Agregar <script src="i18n.js"></script> al HTML
//   2. Etiquetar elementos con data-i18n="clave":
//        <span data-i18n="btn_save">Guardar</span>
//        <input data-i18n-placeholder="lbl_search" placeholder="Buscar">
//        <option data-i18n="soil_good">Bueno</option>
//   3. Llamar i18n.setLang('en') o i18n.setLang('es') para cambiar idioma
//   4. El idioma se persiste en localStorage('cr_lang')
// =============================================================================

(function() {
  'use strict';

  var I18N = {
    dict: {},        // { key: { es: '...', en: '...' } }
    lang: 'es',      // idioma activo
    loaded: false,
    callbacks: [],

    // ── Cargar el CSV de traducciones ──────────────────────────────────────
    load: function(csvUrl) {
      var self = this;
      // Restaurar idioma guardado
      var saved = localStorage.getItem('cr_lang');
      if (saved && (saved === 'es' || saved === 'en')) {
        self.lang = saved;
      }

      return fetch(csvUrl)
        .then(function(r) { return r.text(); })
        .then(function(text) {
          self._parseCSV(text);
          self.loaded = true;
          self.apply();
          // Ejecutar callbacks pendientes
          self.callbacks.forEach(function(cb) { cb(); });
          self.callbacks = [];
          console.log('[i18n] Diccionario cargado: ' + Object.keys(self.dict).length + ' claves.');
        })
        .catch(function(err) {
          console.warn('[i18n] Error cargando traducciones:', err);
        });
    },

    // ── Parser CSV simple (sin dependencias) ──────────────────────────────
    _parseCSV: function(text) {
      var lines = text.split('\n');
      for (var i = 1; i < lines.length; i++) {  // skip header
        var line = lines[i].trim();
        if (!line) continue;
        // Split por comas, respetando posibles comas dentro de comillas
        var parts = this._splitCSVLine(line);
        if (parts.length >= 3) {
          var key = parts[0].trim();
          var es  = parts[1].trim();
          var en  = parts[2].trim();
          if (key) {
            this.dict[key] = { es: es, en: en };
          }
        }
      }
    },

    _splitCSVLine: function(line) {
      var result = [];
      var current = '';
      var inQuotes = false;
      for (var i = 0; i < line.length; i++) {
        var ch = line[i];
        if (ch === '"') {
          inQuotes = !inQuotes;
        } else if (ch === ',' && !inQuotes) {
          result.push(current);
          current = '';
        } else {
          current += ch;
        }
      }
      result.push(current);
      return result;
    },

    // ── Obtener traducción por clave ──────────────────────────────────────
    t: function(key, lang) {
      lang = lang || this.lang;
      var entry = this.dict[key];
      if (!entry) return '[' + key + ']';
      return entry[lang] || entry['es'] || '[' + key + ']';
    },

    // ── Cambiar idioma activo ─────────────────────────────────────────────
    setLang: function(lang) {
      if (lang !== 'es' && lang !== 'en') lang = 'es';
      this.lang = lang;
      localStorage.setItem('cr_lang', lang);
      this.apply();
      this._updateToggleUI();
      // Notificar a Shiny (si existe) para que regenere gráficos/tablas
      if (typeof Shiny !== 'undefined' && Shiny.setInputValue) {
        Shiny.setInputValue('global_lang', lang, { priority: 'event' });
      }
    },

    // ── Aplicar traducciones a todos los elementos con data-i18n ─────────
    apply: function() {
      var self = this;
      // Texto interior
      document.querySelectorAll('[data-i18n]').forEach(function(el) {
        var key = el.getAttribute('data-i18n');
        var val = self.t(key);
        if (val && val[0] !== '[') {
          el.textContent = val;
        }
      });
      // Placeholders
      document.querySelectorAll('[data-i18n-placeholder]').forEach(function(el) {
        var key = el.getAttribute('data-i18n-placeholder');
        var val = self.t(key);
        if (val && val[0] !== '[') {
          el.placeholder = val;
        }
      });
      // Titles (tooltips)
      document.querySelectorAll('[data-i18n-title]').forEach(function(el) {
        var key = el.getAttribute('data-i18n-title');
        var val = self.t(key);
        if (val && val[0] !== '[') {
          el.title = val;
        }
      });
    },

    // ── Toggle visual del botón de idioma ────────────────────────────────
    _updateToggleUI: function() {
      var btn = document.getElementById('lang_toggle_label');
      if (btn) {
        if (this.lang === 'en') {
          btn.innerHTML = '&#127468;&#127463; EN';
        } else {
          btn.innerHTML = '&#127466;&#127480; ES';
        }
      }
      // Actualizar clases activas en botones segmentados (si existen)
      var btnEs = document.getElementById('lang_btn_es');
      var btnEn = document.getElementById('lang_btn_en');
      if (btnEs && btnEn) {
        if (this.lang === 'en') {
          btnEs.classList.remove('active');
          btnEn.classList.add('active');
        } else {
          btnEs.classList.add('active');
          btnEn.classList.remove('active');
        }
      }
    },

    // ── Ejecutar callback cuando el diccionario esté listo ───────────────
    onReady: function(callback) {
      if (this.loaded) {
        callback();
      } else {
        this.callbacks.push(callback);
      }
    }
  };

  // Exponer globalmente
  window.i18n = I18N;

  // ── Auto-inicialización ─────────────────────────────────────────────────
  // Detectar si estamos dentro de Shiny (www/) o en una app HTML independiente
  var csvPath = 'traducciones.csv';
  // Si el script se carga desde un subdirectorio, intentar ruta relativa
  if (document.querySelector('script[src*="i18n.js"]')) {
    var scriptSrc = document.querySelector('script[src*="i18n.js"]').src;
    var baseDir = scriptSrc.substring(0, scriptSrc.lastIndexOf('/') + 1);
    csvPath = baseDir + 'traducciones.csv';
  }

  // Cargar cuando el DOM esté listo
  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', function() {
      I18N.load(csvPath);
    });
  } else {
    I18N.load(csvPath);
  }

  // Re-aplicar después de que Shiny actualice el DOM
  if (typeof Shiny !== 'undefined') {
    $(document).on('shiny:value', function() {
      setTimeout(function() { I18N.apply(); }, 100);
    });
  }
})();
