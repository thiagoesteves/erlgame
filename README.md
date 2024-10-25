![github workflow](https://github.com/thiagoesteves/erlgame/workflows/Erlgame%20CI/badge.svg)
[![Erlant/OTP Release](https://img.shields.io/badge/Erlang-OTP--26.0-green.svg)](https://github.com/erlang/otp/releases/tag/OTP-26.0)

# Game webserver written in Erlang
![Erlgame](/doc/erlgame_snake.png)


The app is an example of how Erlang can be used to be part of the Game backend as a server for a single player or multiple players. In this code you will find examples of how to use cowboy webserver, gen_statem (and how to recover from a crash), supervision tree using simple_one_for_one, aws terraform to deploy, common tests, gun to test webserver, etc.

## Getting started ##
You need to clone the repository and download [rebar3](https://rebar3.org/docs/getting-started/) (if it's not already available in your path).
```
git clone https://github.com/thiagoesteves/erlgame.git
cd erlgame
```
To compile and run
```
make
```
Open your web browser, got to http://127.0.0.1:4000/, type your Name and play

### Unit Test and coverage

The following command will invoke common test and coverage.

```
make test
```

### Deploy at AWS Amazon
