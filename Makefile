init: docker-build install

docker-build:
	docker compose build

install:
	docker compose run --rm r ./install.R
