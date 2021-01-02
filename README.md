[![Build Status](https://secure.travis-ci.org/thiagoesteves/erlgame.svg?branch=main)](http://travis-ci.org/thiagoesteves/erlgame)
[![Coverage Status](https://coveralls.io/repos/github/thiagoesteves/erlgame/badge.svg?branch=main)](https://coveralls.io/github/thiagoesteves/erlgame?branch=main)
[![Erlant/OTP Release](https://img.shields.io/badge/Erlang-OTP--23.0-green.svg)](https://github.com/erlang/otp/releases/tag/OTP-23.0)

# Game webserver written in Erlang
![Erlgame](/doc/erlgame_snake.png)


The app is an example of how Erlang can be used to be part of the Game backend as a server for a single player or multiple players. In this code you will find examples of how to use cowboy webserver, gen_statem (and how to recover from a crash), supervision tree using simple_one_for_one, aws cloud formation to deploy using aws create stack, common tests, gun to test webserver, etc.

The Games available are:
 * Snake Game
 * To Be defined

## Getting started ##
You need to clone the repository and download rebar/rebar3 (if it's not already available in your path).
```
git clone https://github.com/thiagoesteves/erlgame.git
cd erlgame
```
To compile and run
```
make
```
Open your web browser, got to http://127.0.0.1:8080/ type your Name and play

### Unit Test and coverage

The following command will invoke common test and coverage.

```
make test
```

### Deploy at AWS Amazon

The folder aws contains a cloudFormation template file you can use to create a EC2 with ubuntu 18.04 and automatically install and run the erlgame (The full installation takes up to 10 minutes).
