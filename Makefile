all: targets docker

targets: _targets.R
	@echo "Running targets"
	@Rscript -e "targets::tar_make($(target))"
	@echo "Cleaning repository"
	@Rscript -e "source('src/helpers.R'); clean_repo()"

docker: targets Dockerfile .dockerignore
	@echo "Pulling Docker image from gongcastro/cognate-beginnings:latest"
	@docker pull gongcastro/cognate-beginnings:latest
	@echo "Running Docker container at http://localhost:8787"
	docker run --rm \
		-e DISABLE_AUTH=true -e ROOT=true \
		-p 8787:8787 \
		--name rstudio gongcastro/cognate-beginnings:latest
	