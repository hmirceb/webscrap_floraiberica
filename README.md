# Webscrapping Flora Ibérica

Proyecto de **webscrapping** en R para extraer sinónimos de la web de [Flora Iberica](http://www.floraiberica.es/) for synonyms. This allows an easier or more automated way of looking up several taxa instead of using the search option in Flora Iberica.

La carpeta R contiene varios scripts:
  - 1_Leer_FI.R: Lee el código HTML de cada género en la web. 
  - 2_Limpiar_FI.R: Da formato a los nombres para obtener una tabla útil. 
  - utils: Algunas funciones auxiliares.

La carpeta "temp" contiene un archivo .RData con los nombres en bruto.

La carpeta "thesaurus" contiene un archivo .zip comprimido con un archivo .csv: "Flora_iberica_thesaurus.csv".
  - familia: Familia del taxón correspondiente.
  - genero: Género del taxón correspondiente.
  - rango_taxonomico: Rango taxonómico del taxón. Uno de: género, especie, subespecie, forma, híbrido, subforma o [?]. [?]
  - nombre_completo: Nombre del taxón incluyendo autoría.
  - nombre: Nombre sin autoría.
  - autoridad: Autoridad que describió el taxón.
  - nombre_canonico: Nombre sin autoría ni abreviaturas del rango taxonómico.
  - nombre_aceptado: Nombre aceptado del taxón.
  - tipo_sinonimo: Tipo de sinónimo según FI. Uno de: aceptado, heterotípico, homotípico.


Todos los datos se encuentran bajo la licencia usada por Flora Iberica: [CC BY-NC-SA 2.5 ES](https://creativecommons.org/licenses/by-nc-sa/2.5/es/)

La cita sugerida para Flora Iberica al completo es:

  - Castroviejo, S. (coord. gen.). 1986-2012. Flora iberica 1-8, 10-15, 17-18, 21. Real Jardín Botánico, CSIC, Madrid.
