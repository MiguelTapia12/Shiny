# Proyecto: Pipeline de Selección Genética - Central Romana

## 1. Objetivo del Proyecto
Desarrollar una aplicación interactiva en Shiny (R) para la gestión y simulación de cruces en el programa de mejoramiento de caña de azúcar de Central Romana. El sistema debe permitir visualizar genealogías y predecir la consanguinidad (F) de futuros cruces.

## 2. Estructura de Datos
El proyecto se basa en dos archivos de texto principales:
- **CAT_VARIEDADES.txt**: Catálogo maestro. 
    - Columnas clave: `ID_VARIEDAD` (Char), `DESCRIPCION_VARIEDAD` (Nombre comercial).
- **PARENTESCO_VARIEDADES.txt**: Registro histórico de parentesco.
    - Columnas clave: `ID_VARIEDAD`, `ID_VARIEDAD_ANCESTRO`, `TIPO_ANCESTRO` (PADRE/MADRE).

## 3. Lógica de Negocio Aplicada
- **Genealogía Recursiva**: Se utiliza un algoritmo de búsqueda hacia atrás (queue) para reconstruir el árbol completo de una variedad.
- **Matriz de Parentesco (A)**: Se utiliza la librería `AGHmatrix` para calcular la relación aditiva entre todos los individuos.
- **Consanguinidad (F)**: Se calcula como $F_{progenie} = \frac{A_{padre, madre}}{2}$.

## 4. Estado Actual y Desafíos
- Se ha logrado levantar la interfaz en `shinydashboard`.
- **Desafío Técnico**: La librería `kinship2` requiere una integridad referencial absoluta. Si un individuo es padre, debe existir en la lista de IDs. Se ha implementado un "ID 0" para representar fundadores/ancestros desconocidos.
- **Próximo Paso**: Optimizar la pestaña de "Sugerencia de Cruces" filtrando por variedades activas.