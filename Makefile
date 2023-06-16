targets:
	Rscript -e "targets::tar_make()"

build:
	docker build -t cognate-beginnings .
run:
	docker run --rm -e PASSWORD=yourpassword -p 8787:8787 cognate-beginnings:latest
	