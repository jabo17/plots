init: docker-build install

docker-build:
	docker compose build

install:
	docker compose run --rm r ./install.R

example-pdf:
	docker compose run --rm r ./example_pdf.R
	docker compose run --rm r ./example_breakdown_plot.R

example-tex:
	docker compose run --rm r ./example_tex.R

test:
	docker compose run --rm r ./test.R
