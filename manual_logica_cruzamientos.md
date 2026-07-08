# Manual Técnico: Lógica de Sugerencia de Cruzamientos
**Sistema de Mejoramiento Genético - Central Romana**

Este documento detalla la arquitectura matemática y lógica detrás del motor de sugerencias de cruces (`mod_cruzamientos.R` y `utils_selection.R`), diseñado para maximizar la ganancia genética, minimizar la endogamia y automatizar la toma de decisiones.

---

## 1. Filtros Previos (Elegibilidad)
Antes de que una pareja sea evaluada matemáticamente, debe pasar filtros estrictos:

1. **Adaptación a Suelo:** El sistema filtra las Madres (SX=3) y los Padres (SX=1, 2) garantizando que estén adaptados al "Suelo Objetivo" seleccionado en la interfaz (Ej. *BUENO*, *MAL_DRENADO*, *ROCOSO*).
2. **Disponibilidad Floral:** Solo se evalúan variedades presentes en el archivo de "Floración Semanal" subido por el usuario, exigiendo un número de flores mayor a cero.

---

## 2. Motor de Consanguinidad (Matriz de Parentesco $A$)
El sistema procesa **más de 12,000 registros históricos** del catálogo (Padre, Madre, Abuelos...) y construye una Matriz Numérica Aditiva ($A$) que calcula la probabilidad de que dos genes sean idénticos por descendencia.

* **Filtro Estricto:** Cualquier cruce proyectado cuya progenie supere el **Límite de Consanguinidad Máxima** definido por el usuario (Por defecto $F > 0.0625$, equivalente a un cruce entre primos hermanos), es **descartado inmediatamente**.
* **Puntaje Genético:** Se premia a las parejas más distantes genéticamente.
  * *Fórmula:* `Score_Genetico = (1 - F_progenie) * Peso_Genetico`

---

## 3. Asignación de Categorías (Tiers Genéticos)
A cada variedad se le asigna dinámicamente un "Tier" de calidad basado en sus datos históricos y fenotípicos:

| Categoría | Descripción | Condición Lógica |
| :--- | :--- | :--- |
| **C1: Progeny Tested** | Padres Probados. | Historial $\ge$ 3 cruces **Y** Tasa de Éxito $\ge$ 50%. |
| **C2: V.H.Q** | Muy Alta Calidad. | Score Agronómico de $Y \le 4$ **Y** $Q \le 4$. |
| **C3: Alto Y\|Q** | Alto Tonelaje o Calidad. | Score Agronómico de $Y \le 4$ **O** $Q \le 4$. |
| **C4: Comercial** | Variedad en explotación. | Pertenece a la lista comercial oficial aprobada. |
| **C5: Exploratorio** | Clones Nuevos. | No cumple ninguna de las anteriores. |

---

## 4. Índice de Selección Base (Smith-Hazel v2)
La calidad intrínseca de cada padre se calcula combinando Tonelaje ($Y$), Calidad ($Q$) y Sanidad ($S$), ponderados por su Heredabilidad ($h^2$) y Peso Económico ($w$):

1. **Inversión de Escala:** Dado que los scores fenotípicos de campo van de 1 (Excelente) a 9 (Pobre), se invierten matemáticamente (`10 - Score`) para que mayor número signifique mejor calidad.
2. **Cálculo del Valor:** 
   * $Indice_{Individual} = (Y \times h^2_y \times w_y) + (Q \times h^2_q \times w_q) + (S \times h^2_s \times w_s)$
3. **Score Combinado:** El valor base de la pareja es el promedio del índice del Padre y la Madre.

---

## 5. Modificadores y Bonos de Cruce (La "Cascada")
El motor aplica modificadores matemáticos al *Score Combinado* dependiendo de *quién se cruza con quién*.

### Bonos por Tier (Estrategia de Apareamiento)
El score se multiplica por un factor si la pareja pertenece a combinaciones estratégicas:
* **Élite (C1 x C1):** Bono del **15%** (Asegura descendencia ganadora).
* **V.H.Q (C1/C2 x C1/C2/C3):** Bono del **12%**.
* **Amplio (C1/C2/C3 x C1/C2/C3):** Bono del **10%** (Maximiza variabilidad en clones top).
* **Comercial (C3/C4 x C4/C1/C2):** Bono del **8%**.
* **Exploratorio (C1 x C5):** Bono del **5%** (Usa un padre probado para evaluar a un novato).

### Bono Estadístico por EBV (Valores Genéticos Estimados)
* Si las variedades cuentan con registros reales de campo (TCA, REND), el sistema usa la **Desviación contra el Testigo del Ambiente**.
* Padres con desviaciones positivamente probadas otorgan un bono directo al puntaje total (limitado a un máximo de $+1.0$ punto combinado) para servir como **desempate definitivo**.

---

## 6. Penalizaciones (Castigos de Calidad y Operativos)
Para proteger la integridad del programa, el sistema resta puntos por factores de riesgo:

* **Penalización C5 x C5 (-20 ptos):** Se destruye el puntaje si se intenta cruzar a dos clones sin historial (Cruzar incógnita con incógnita es perder recursos).
* **Sanidad Extrema (-10 ptos):** Si algún padre tiene un Score de Enfermedad $> 7$ (Muy susceptible).
* **Fracaso Histórico EVF (-3.0 ptos):** Si un padre ha sido utilizado $\ge 5$ veces y su tasa de éxito procreando clones comerciales es menor al 20%.
* **Desincronización Floral (-0.15 ptos):** Castigo leve si las flores reales reportadas en el Excel tienen una diferencia de maduración (EMF) mayor a 1 semana.

---

## 7. Módulo de Policruces (Agrupación de Machos)
Para la pestaña de Policruces, la lógica agrupa 1-3 Hembras con 3-6 Machos, garantizando las siguientes reglas biológicas:
1. Pertenencia estricta al mismo Tier (Ej. *Poli V.H.Q*, *Poli Amplio*).
2. Sumatoria de flores masculinas suficientes para cubrir a las femeninas, garantizando un **Ratio Mínimo de 1 Hembra : 2 Machos**.

---
*Este manual refleja la arquitectura algorítmica activa actual. El resultado final de cada cruce se muestra en la columna "Score Total", la cual ya incluye el balance perfecto entre Variabilidad Genética y Rendimiento Agroeconómico.*
