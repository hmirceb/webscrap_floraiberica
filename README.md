# Webscrapping *Flora iberica*

Proyecto de *webscrapping* en R para extraer sinónimos de la web de [*Flora iberica*](http://www.floraiberica.es/). De esta forma se puede consultar nombres sin tener que emplear el formulario web de *Flora iberica*.

La carpeta **R** contiene varios scripts:
    - 1_Leer_FI.R: Lee el código HTML de cada género en la web. 
    - 2_Limpiar_FI.R: Da formato a los nombres para obtener una tabla útil. 
    - utils: Algunas funciones auxiliares.

La carpeta **temp** contiene un archivo .RData con los nombres en bruto.

La carpeta **thesaurus** contiene un archivo .zip comprimido con un archivo .csv: "Flora_iberica_thesaurus.csv". 
  - familia: Familia del taxón correspondiente. 
  - genero: Género del taxón correspondiente. 
  - rango_taxonomico: Rango taxonómico del taxón. Uno de: género, especie, subespecie, forma, híbrido, subforma o [?]. [?] se refiere a taxa que en la web de *Flora iberica* contienen [?] en el nombre y por tanto no se puede conocer el rango taxonómico completo. Normalmente se refiere a [\alpha], [\beta] o similares, pero en las fichas de cada especie suele aparecer como [?].
  - nombre_completo: Nombre del taxón incluyendo autoría. 
  - nombre: Nombre sin autoría. 
  - autoridad: Autoridad que describió el taxón. 
  - nombre_canonico: Nombre sin autoría ni abreviaturas del rango taxonómico. 
  - nombre_aceptado: Nombre aceptado del taxón. 
  - tipo_sinonimo: Tipo de sinónimo según FI. Uno de: aceptado, heterotípico, homotípico.

# Licencia
Todos los datos pertenecen a *Flora iberica* y se encuentran bajo la licencia: [CC BY-NC-SA 2.5 ES](https://creativecommons.org/licenses/by-nc-sa/2.5/es/)

# Sugerencias para citar *Flora iberica*:
La cita sugerida para *Flora iberica* al completo es:
  - Castroviejo, S. (coord. gen.). 1986-2012. Flora iberica 1-8, 10-15, 17-18, 21. Real Jardín Botánico, CSIC, Madrid.
