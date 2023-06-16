targets:
	Rscript -e "targets::tar_make()"

build:
	docker build -t cognate-beginnings .
run:
	RENV_PATHS_CACHE_HOST=%LOCALAPPDATA%/R/cache/R/renv
	RENV_PATHS_CACHE_CONTAINER=/renv/cache
	docker run --rm \
		-e PASSWORD=yourpassword \
		-p 8787:8787 \
		cognate-beginnings:latest
	