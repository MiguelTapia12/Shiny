# Skills de Ingeniería Bioinformática y Fitomejoramiento (R/Shiny)

Este documento define el perfil de habilidades necesario para asistir en el desarrollo del **Pipeline de Selección Genética de Central Romana**.

## 1. Skill: Integridad de Pedigrí y Manejo de Nodos (kinship2)
* **Capacidad**: Resolución de inconsistencias en árboles genealógicos.
* **Conocimiento Técnico**: Garantizar que cada `dadid` (padre) y `momid` (madre) declarados existan obligatoriamente como un registro en la columna de individuos (`id`).
* **Manejo de Fundadores**: Implementación de lógica de "ID 0" para ancestros desconocidos o variedades sin registro previo, evitando el error crítico: `Value of dadid not found in the id list`.

## 2. Skill: Análisis Matricial Genético (AGHmatrix)
* **Capacidad**: Construcción y manipulación de la Matriz de Parentesco Aditivo (Matriz A).
* **Conocimiento Técnico**: Transformación de pedigrís de formato largo a formato ancho (ID | Padre | Madre) y cálculo de relaciones genéticas mediante `Amatrix()`. Manejo de valores perdidos (`missingValue`) para asegurar la convergencia de la matriz.

## 3. Skill: Lógica de Consanguinidad Cuantitativa
* **Capacidad**: Predicción de parámetros genéticos para la toma de decisiones.
* **Lógica**: Cálculo del Coeficiente de Consanguinidad ($F$) de la futura progenie basándose en la relación de los padres ($F_{hijo} = A_{padre,madre} / 2$).
* **Aplicación**: Filtrado de cruces potenciales basados en umbrales máximos de consanguinidad para evitar la depresión por consanguinidad en la caña de azúcar.

## 4. Skill: Arquitectura de Aplicaciones Reactivas (Shiny)
* **Capacidad**: Diseño de interfaces eficientes bajo `shinydashboard`.
* **Conocimiento Técnico**: Uso experto de `reactive()` para el cálculo pesado de matrices, `observeEvent()` para disparar simulaciones y `validate(need())` para proporcionar feedback al usuario en lugar de errores crudos del sistema.

## 5. Skill: Normalización de Datos Agrícolas
* **Capacidad**: Gestión de "Big Data" de campo.
* **Conocimiento Técnico**: Tratamiento estricto de identificadores (IDs) como tipo `character` para prevenir errores de emparejamiento entre catálogos de variedades y registros de parentesco. Limpieza de datos mediante el uso de `janitor` y `data.table`.

---
**Objetivo Final:** Optimizar el flujo de trabajo de selección de Central Romana, permitiendo que el usuario busque una variedad, visualice su árbol genealógico completo sin errores de nodos, y simule cruces con el inventario activo de la estación.