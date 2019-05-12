# Jack The Bartender

Jack The Bartender is a [telegram](https://telegram.org) bot.

# Installation

## Make sure you have python 3.x.x

Check python version by typing:
`python --version`
or`python3 --version`

Further we will use `python` in all of the commands.

## Setting up environment
In the project directory run the following commands to set up virtual environment.

```
python -m venv env
source env/bin/activate
```

Install required packages.

```
pip install requests pyswip
```

Install `swi-prolog` from [this link](http://www.swi-prolog.org/Download.html).

## Run the bot

Running the bot is very simple, however you will need to get an API key first. Contact me, to get one. 
Put this key in a file called `key` in the same directory where with the file `bot.py`. 
Once done, simply activate python environment that was setup before, navigate to project directory and run:

```
python bot.py
```

To stop the bot use **Crtl-C**.
