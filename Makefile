####
# DOCKER
####
docker_compose:
	# docker build -t data-science-template .
	docker compose -f docker-compose.yml up --build

docker_run: zsh

zsh:
	docker exec -it r-bash-1 /bin/zsh

examples:
	Rscript -e "rmarkdown::render('examples/exmaples.Rmd')"
