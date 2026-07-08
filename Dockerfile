# Dockerfile para la aplicación Breeding System (Shiny + Plumber)
FROM rocker/shiny-verse:4.3.2

# Instalar dependencias del sistema para PostgreSQL y otras utilidades
RUN apt-get update && apt-get install -y \
    libpq-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Configurar renv para aislar el entorno
ENV RENV_PATHS_LIBRARY="renv/library"
ENV RENV_PATHS_CACHE="/renv/cache"

# Instalar renv globalmente
RUN Rscript -e "install.packages('renv', repos = 'https://cloud.r-project.org')"

# Crear el directorio de trabajo
WORKDIR /srv/shiny-server/breeding_app

# Copiar archivos de configuración de renv
# COPY renv.lock ./
# COPY .Rprofile ./
# COPY renv/activate.R renv/

# Restaurar paquetes de R (Descomentar cuando renv esté inicializado localmente)
# RUN Rscript -e "renv::restore()"

# (Por ahora, si no usamos renv activamente en Docker, instalamos manual:)
RUN Rscript -e "install.packages(c('shiny', 'bslib', 'DT', 'dplyr', 'ggplot2', 'plotly', 'readxl', 'plumber', 'RSQLite', 'RPostgres', 'janitor', 'DBI'), repos = 'https://cloud.r-project.org')"

# Copiar el resto del código
COPY . /srv/shiny-server/breeding_app/

# Exponer el puerto de Shiny (3838) y de Plumber (8000 si se ejecuta separado)
EXPOSE 3838

# Dar permisos
RUN chown -R shiny:shiny /srv/shiny-server/breeding_app

# Arrancar Shiny Server
CMD ["/usr/bin/shiny-server"]
