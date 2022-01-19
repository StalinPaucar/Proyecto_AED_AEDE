FROM virtualstaticvoid/heroku-docker-r:shiny

CMD ["/usr/bin/R", "--no-save", "--gui-none", "-f", "/app/global.R"]