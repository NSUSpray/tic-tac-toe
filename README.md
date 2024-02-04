# Tic-tac-toe
<img src='demo.gif' alt='Demonstration' width='139' align='right'>
Simple web browser Tic-tac-toe game based on Scotty library.

## Requirements
1. [Haskell2010](https://www.haskell.org/downloads/) – compiler and toolchain
1. [Matrix](https://hackage.haskell.org/package/matrix) – storing and manipulating board data
1. [Scotty](https://hackage.haskell.org/package/scotty) – back-end web engine
1. [Blaze.Html](https://hackage.haskell.org/package/blaze-html) – writing HTML templates
1. [Wai.Middleware.Static](https://hackage.haskell.org/package/wai-middleware-static) – applying CSS and attaching favicon


## Usage
Setup your Haskell environment:

* cabal 3.10.2.1+
* GHC 9.4.8+ (base ^>=4.17.2.1)

Next clone this repository, run the application

```shell
$ git clone https://github.com/NSUSpray/tic-tac-toe
$ cd tic-tac-toe
$ cabal run
```

and wait for the message

```
Setting phasers to stun... (port 3000) (ctrl-c to quit)
```

Then open your browser and enter in the address bar http://localhost:3000/.
