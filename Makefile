targets:
	Rscript -e "targets::tar_make()"
	Rscript -e "source('src/helpers.R'); clean_repo()"

docker:
	docker pull gongcastro/cognate-beginnings:latest
	docker run --rm -e PASSWORD=yourpassword -p 8787:8787 gongcastro/cognate-beginnings:latest
	