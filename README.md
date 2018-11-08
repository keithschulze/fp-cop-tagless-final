# FP COP - Tagless Final DSLs in Scala

This repo contains Jupyter notebook/presentation and supporting Ammonite
scripts for the Functional Programming COP talk on Tagless Final DSLs in Scala.


## Prerequisites
### Jupyter notebook

This assumes you have Python and `pip` installed. Using a `virtualenv` or
`Anaconda/miniconda` environment is also recommended.

1. Install `jupyter`: `pip install jupyter`

2. Install the `almond` Scala kernel for Jupyter.

```
$ SCALA_VERSION=2.12.7 ALMOND_VERSION=0.1.11
$ coursier bootstrap \
    -i user -I user:sh.almond:scala-kernel-api_$SCALA_VERSION:$ALMOND_VERSION \
    sh.almond:scala-kernel_$SCALA_VERSION:$ALMOND_VERSION \
    -o almond
$ ./almond --install
```

More comprehensive instructions for installing `almond` can be found on the
[`almond`](http://almond-sh.github.io/almond/stable/docs/quick-start-install)
website.

### Ammonite scripts

In order to run the Ammonite scripts, you will need to install Ammonite.
Installations instructions can be found on the [Ammonite website](http://ammonite.io/).

Homebrew can be used to install Ammonite on Mac OS:
```
$ brew install ammonite-repl
```

## Running the Jupyter notebook

1. Clone or download this repository:

```
$ cd /path/to/projects
$ git clone https://github.com/keithschulze/fp-cop-tagless-final
```

2. Navigate to the cloned repository and run Jupyter notebook server.

```
$ cd /path/to/projects/fp-cop-tagless-final
$ jupyter notebook
```
Note: Remember to activate you `virtualenv` if you installed Jupyter in one.
Note: Jupyter on work with JDK1.8. If your kernel fails to start, see these
[instructions](https://github.com/almond-sh/almond/issues/188#issuecomment-383221894).

## Run Ammonite scripts

```
$ cd /path/to/projects/fp-cop-tagless-final
$ amm banking.sc
$ amm imgprocessor.sc
```

## Build Jupyter slides

```
$ cd /path/to/projects/fp-cop-tagless-final
$ jupyter nbconvert fp-cop-tagless-final.ipynb --to slides --post serve
```

