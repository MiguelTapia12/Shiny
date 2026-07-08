# CR Breeding — Sistema de Mejoramiento Genético

![GitHub last commit](https://img.shields.io/github/last-commit/MiguelTapia12/Shiny)
![GitHub issues](https://img.shields.io/github/issues/MiguelTapia12/Shiny)
![License](https://img.shields.io/badge/license-MIT-green)

**CR Breeding** es una plataforma integral de inteligencia agronómica y selección genética desarrollada para **Central Romana Corp.** Esta herramienta web permite gestionar el programa de mejoramiento, analizar cruces óptimos y explorar el pedigrí de variedades de caña de azúcar de manera interactiva.

![Dashboard Preview](https://img.shields.io/badge/UI-bslib_&_Shiny-1a5276?style=for-the-badge&logo=r)

## 🌟 Características Principales

La aplicación está dividida en módulos diseñados para cubrir las distintas fases de la investigación agrícola:

- 📊 **Dashboard Analítico**: Resumen en tiempo real del catálogo de variedades, evaluación de desempeño y simulación de cruces (Biparentales y Policruces) optimizando el EBV (Estimated Breeding Value) y controlando la consanguinidad.
- 🌾 **Asistente de Campo**: Herramienta táctica para la planificación in-situ. Permite encontrar las mejores parejas (macho/hembra) para un tipo de suelo específico basándose en el nivel de expresión sexual y compatibilidad fenotípica.
- 🧬 **Visor de Genealogía**: Explorador visual interactivo de árboles genealógicos conectado a una base de datos con más de 12,000 registros históricos unificados. Soporta exportación directa a formato de imagen de alta calidad (PNG).
- ⚙️ **Administración**: Gestión y sincronización segura de las bases de datos locales y fuentes de origen.

## 🛠️ Tecnologías y Arquitectura

La aplicación está construida en **R** utilizando el framework **Shiny** con una arquitectura fuertemente modular.

- **Frontend**: Utiliza `bslib` para una interfaz moderna, limpia y responsiva (Bootstrap 5). Los gráficos y redes interactivos se implementan vía `visNetwork` y `ggplot2`.
- **Backend & Base de Datos**: Reemplazó los antiguos archivos CSV/Excel dispersos por una base de datos **SQLite** ultrarrápida (`breeding_system.db`) que consolida el *Maestro de Pedigrí* y los catálogos de variedades.

## 📦 Requisitos e Instalación

Para correr este proyecto en entorno local necesitas:
- R (4.2 o superior)
- RStudio (recomendado)

Asegúrate de instalar los siguientes paquetes de CRAN antes de iniciar:

```r
install.packages(c(
  "shiny", "bslib", "dplyr", "DBI", "RSQLite", 
  "visNetwork", "DT", "ggplot2", "shinycssloaders", 
  "shinyWidgets", "readxl"
))
```

## 🚀 Cómo ejecutar la aplicación

1. Clona el repositorio o abre RStudio en la carpeta principal del proyecto.
2. Abre el archivo `app.R`.
3. Haz clic en el botón **Run App** en RStudio, o ejecuta el siguiente comando en la consola de R:

```r
shiny::runApp()
```

## 🗃️ Estructura de Datos (Backend)

El sistema ahora lee datos normalizados desde la base de datos `data/breeding_system.db`. Las tablas principales incluyen:
- `cat_variedades`: Datos de rendimiento, fenotipado e índices de adaptación por suelo.
- `pedigree_maestro`: Catálogo unificado y saneado con más de 18,000 relaciones de cruces históricos (Variedad, Madre, Padre).

> **Nota de Migración**: Los archivos Excel heredados (como el antiguo *Maestro de Pedigree.xlsx*) ahora solo sirven como insumo para el ETL. La app lee el 100% de la genealogía desde SQLite para garantizar un rendimiento instantáneo.

## 🤝 Contribución

- Trabaja siempre en ramas descriptivas (`feature/nueva-funcionalidad`, `fix/correccion-error`).
- Asegúrate de que los cambios en UI mantengan el estilo corporativo y el esquema de colores de `bslib`.
- Abre un Pull Request contra la rama `main` para revisión.
