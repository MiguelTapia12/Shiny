# Memoria Agronómica: Motor de Cruzamientos

Este archivo contiene las reglas y limitantes intocables del módulo de cruzamientos (`mod_cruzamientos.R`). **Ningún agente ni desarrollador futuro debe intentar optimizar o remover estas limitantes matemáticas, ya que fueron diseñadas para proteger la viabilidad operativa en campo.**

## 1. Proporción de Sexos en Cruzamientos (2:1)
La logística de cruzamientos requiere una estricta relación de **2 tallos Machos por cada 1 tallo Hembra**.
- Cualquier sugerencia de alterar este ratio para "hacerlo 1:1 y maximizar flores" está estrictamente prohibida, ya que biológicamente los machos deben producir suficiente polen para asegurar la fertilización.

## 2. Freno de Monopolio de Recomendación (max_por_variedad = 5)
El algoritmo tiene una limitante estricta: `max_por_variedad <- 5`.
- **Razón:** Si se libera este límite (o se intenta hacerlo dinámico), las variedades que son matemáticamente élites (Ej. CR110011) acapararán todo el "Top 50" de recomendaciones.
- Dado el inventario físico (las variedades rara vez tienen más de 5 flores funcionales disponibles a la vez), sugerir una misma variedad 15 veces produciría una "cascada" irrealizable para los técnicos.
- Esta regla asegura la diversidad genética forzando al algoritmo a usar otras variedades una vez que las élites topan su cuota de 5 cruzamientos sugeridos.

## 3. Límite del Bono EBV (Datos de Campo Reales)
Actualmente, los datos reales de ensayos avanzados (TCA y Rendimiento empírico) se incorporan a la fórmula de la progenie como un multiplicador. Este multiplicador se encuentra matemáticamente topado a un **máximo de +/- 0.5**.
- **Prohibición:** Nunca remover este tope de 0.5. Si se remueve, el peso de los datos de campo abrumará a la categoría genética teórica principal, causando de nuevo el efecto "monopolio" mencionado en el punto 2. En lugar de cambiar la matemática, el Bono EBV solo debe mostrarse visualmente (UI) para la toma de decisión del experto.

## 4. Umbrales de Promoción entre Etapas
El programa sigue un flujo direccional estricto: **ST1 → ST2 → ST3 → ST4 → ST5 → Comercial**.
- **Regla:** Para que un clon avance a la siguiente etapa, debe superar consistentemente los umbrales de su grupo o cuartil en métricas críticas (Brix, Vigor/TCA).
- No se permiten promociones "saltando" etapas, a menos que sean re-ingresos autorizados desde el archivo maestro. Las métricas relativas (isoproductividad) son las que validan la promoción.

## 5. Criterios de Eliminación (Drop)
Se aplicará el criterio de "Rechazo" (Eliminación) a cualquier clon o variedad que presente:
- **Susceptibilidad a Enfermedades:** Infección severa confirmada de patógenos letales como Roya (Puccinia) o Carbón (Sporisorium).
- **Rendimiento deficiente:** Caída sistemática por debajo del cuartil inferior en ensayos replicados (ST4-ST5) frente al testigo base.

## 6. Reglas de Testigos
Para el cálculo de los índices de Rendimiento y Calidad relativa, el sistema utiliza variedades de control (testigos) estandarizadas.
- **Testigos actuales y legacy:** `BR0402`, `BR0010`, `CR87339`, `CR74250`, `CR951007`, `CR93003`, `CR61001` y `CR800291`.
- **Razón:** Estos testigos históricos permiten empalmar datos de experimentos antiguos con análisis modernos, asegurando una línea base comparable a lo largo de los años.

## 7. Definición de 'Adaptado' por Suelo
El algoritmo de cruzamientos posee un filtro de "Suelo Objetivo" que asegura que los parentales elegidos cuenten con adaptabilidad demostrada.
- **Categorías:** `BUENO`, `ROCOSO`, `MAL_DRENADO`.
- **Regla:** Al activar el filtro, se impone un descarte fuerte sobre progenitores que históricamente (según fenotipo y `pedigree`) no rinden en el tipo de suelo seleccionado. No remover este filtro del optimizador genético.
