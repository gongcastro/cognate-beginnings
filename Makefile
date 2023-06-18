targets:
	Rscript -e "targets::tar_make()"

pull:
	docker pull gongcastro/cognate-beginnings:latest
build:
	docker build -t cognate-beginnings .
run:
	docker run --rm -e PASSWORD=yourpassword -p 8787:8787 gongcastro/cognate-beginnings:latest
	