# Guía de Mantenimiento Técnico
## Breeding System CR v3.0

---

### Estructura del proyecto
BreedingSystemCR/
├── app.R                    # Punto de entrada de la app Shiny (Interfaz central)
├── global.R                 # Orquestador: carga datos y módulos
├── api.R / plumber.R        # API REST para sincronización con tablets (Plumber)
├── .Renviron                # Variables de entorno (Seguridad: BREEDING_API_TOKEN)
├── DESCRIPTION              # Metadatos del proyecto y dependencias
│
├── R/
│   ├── dependencies.R       # Carga de librerías
│   ├── utils_pedigree.R     # Motor BFS de genealogía  ← función crítica
│   ├── utils_selection.R    # Lógica de cruces          ← función crítica
│   ├── utils_real_data.R    # Carga de Excel (AllAct, Familias)
│   ├── utils_db.R           # Conexión, esquemas y migraciones de SQLite
│   ├── mod_genealogia.R     # Módulo: Visor de Pedigrí
│   ├── mod_cruzamientos.R   # Módulo: Sugerencia de Cruces
│   ├── mod_seleccion.R      # Módulo: Captura de Campo y Analítica (Ensayos ST)
│   ├── mod_fitopatologia.R  # Módulo: Sanidad y Enfermedades
│   ├── mod_floracion.R      # Módulo: Gestión de Floración
│   ├── mod_estado_variedad.R
│   ├── mod_trazabilidad.R
│   ├── mod_dashboard.R
│   ├── mod_gerencial.R      # Panel Gerencial y KPIs
│   └── mod_archivo.R
│
├── www/                     # Aplicaciones web offline para Tablets (HTML/JS)
│   ├── capture_v3.html      # Captura de Campo (Clones ST1-ST5) con GPS
│   ├── floracion_v1.html    # Registro de Tallo y Floración
│   └── evaluacion_enfermedades.html # Evaluación de Fitopatología
│
├── data/
│   └── breeding_system.db   # Base de datos SQLite (generada, no versionar)
│
└── tests/
    └── testthat/            # Pruebas unitarias automatizadas

---

### Seguridad y Configuración de API (Tablets)

La API (usada por las tablets de campo para enviar datos) está protegida mediante autenticación Bearer Token.

**Para configurar el servidor:**
1. Crear o editar el archivo `.Renviron` en la raíz del proyecto.
2. Añadir la variable: `BREEDING_API_TOKEN=MiTokenSecreto123`
3. Reiniciar la aplicación/Plumber.

**Para configurar una tablet nueva:**
1. Abrir la app de captura (ej. `capture_v3.html`).
2. Ir a la pestaña "Configuración".
3. Ingresar la URL del servidor y el Token configurado en el `.Renviron`.
4. El modo oscuro también se puede configurar aquí y se guardará en `localStorage`.

---

### Bases de Datos y Migraciones (SQLite)

El sistema genera automáticamente `data/breeding_system.db`. Si la BD se borra, se regenera el esquema automáticamente al iniciar `app.R`.
Las migraciones de columnas nuevas (ej. `evaluador`, `latitud`, `longitud`, `fecha_evaluacion`) se ejecutan en `R/utils_db.R` con sentencias `PRAGMA table_info`. 

**Para reiniciar la base de datos limpia:**
1. Detener la aplicación.
2. Eliminar o renombrar `data/breeding_system.db`.
3. Iniciar la aplicación y hacer clic en **"Sincronizar Sistema"** para recargar todos los Excel iniciales.

---

### Cómo actualizar los datos de una nueva temporada

**Paso 1 — Reemplazar los archivos fuente**
Copia los nuevos archivos a la raíz del proyecto con exactamente estos nombres:
- `AllAct2025.xls` → renombrar al año correspondiente y actualizar la referencia en `global.R` (línea con `load_allact_data(...)`)
- `Evaluacion de Familias.xlsx` → reemplazar directamente

**Paso 2 — Sincronizar desde la aplicación**
Abre la app y presiona **"Sincronizar Sistema"** en el menú lateral.
La barra de progreso mostrará cada etapa. Si aparece un error en rojo, el mensaje indica exactamente qué archivo o tabla falló.

**Paso 3 — Verificar categorías**
En la pestaña **Estado de Variedad**, confirma que las categorías C1–C6 se actualizaron para las variedades del nuevo ciclo.

---

### Cómo añadir una nueva variedad manualmente

Las variedades nuevas se registran en dos lugares:

```r
# 1. En CAT_VARIEDADES.txt — añadir una línea:
# ID_VARIEDAD  DESCRIPCION_VARIEDAD  VARIEDAD_CODE  STATUS  IND_AGRUPAR_ESPECIAL
# 9999         CR240001                              EVALUACION  N

# 2. En PARENTESCO_VARIEDADES.txt — si se conocen los padres:
# ID_PARENTESCO  ID_COMPANIA  ID_VARIEDAD  ID_VARIEDAD_ANCESTRO  TIPO_ANCESTRO
# 9999           1            9999         1234                  PADRE
# 10000          1            9999         5678                  MADRE
```

Después de editar los archivos, presionar **"Sincronizar Sistema"**.

---

### Cómo correr los tests localmente

```r
# Desde RStudio o la consola R, en la carpeta del proyecto:
testthat::test_local()

# Para ver cobertura:
covr::file_coverage(
  source_files = list.files("R", pattern = "\\.R$", full.names = TRUE),
  test_files   = list.files("tests/testthat", pattern = "^test-",
                            full.names = TRUE)
)
```

Los tests no requieren conexión a la base de datos ni los archivos Excel. Se ejecutan en modo ligero con datos mínimos definidos en `setup.R`.

---

### Funciones críticas — no modificar sin correr tests

| Función | Archivo | Qué hace |
|---|---|---|
| `get_full_ancestry_robust` | `R/utils_pedigree.R` | BFS de genealogía O(n) |
| `sugerir_cruces` | `R/utils_selection.R` | Calcula F y filtra cruces |
| `assign_genetic_categories` | `R/utils_real_data.R` | Clasifica C1–C6 |
| `db_sync_repository_to_db` | `R/utils_db.R` | Sincroniza Excel → SQLite |
| `/api/selection/sync`      | `api.R`              | Validación estricta y guardado de datos de campo GPS |

Cualquier cambio en estas funciones debe ir acompañado de tests que pasen antes del merge.
