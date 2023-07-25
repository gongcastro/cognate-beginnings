all: targets docker

targets: _targets.R
	@echo "Running targets..."
	@Rscript -e "targets::tar_make($(target))"
	@echo "Cleaning repository..."
	@Rscript -e "source('src/helpers.R'); clean_repo()"

docker-push: Dockerfile .dockerignore
	@echo "Pushing Dockerfile to GitHub Actions..."
	@git add Dockerfile
	@git diff --quiet && git diff --staged --quiet || git commit -am "Update Dockerfile"
	@git push
	@gh run watch -i 5 --repo gongcastro/cognate-beginnings

docker-run:
	@echo "Pulling Docker image..."
	@docker pull gongcastro/cognate-beginnings:latest
	@echo "Running Docker container at http://localhost:8787"
	@docker run --rm \
		-e DISABLE_AUTH=true -e ROOT=true \
		-p 8787:8787 \
		--name rstudio gongcastro/cognate-beginnings:latest
	