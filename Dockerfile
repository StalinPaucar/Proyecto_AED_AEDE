FROM virtualstaticvoid/heroku-docker-r:shiny

CMD ["/usr/bin/R", "--no-save", "-f", "/app/global.R"]