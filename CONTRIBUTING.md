Thank you for your willingness to make `cellout` better!

If you're new to Haskell, don't worry, so am I, and I'm writing these
instructions for newcomers like us.

We're using [stack](https://docs.haskellstack.org/en/stable/README/) to keep
track of and install all of the dependencies. Fair warning that when you get
`stack`, it will, by default download its own version of ghc (the Glasgow
Haskell Compiler), which is a pretty big download. Luckily you'll only need to
do that once, and you can even uncomment the `system-ghc: true` remove the
leading `#` around line 53 of `stack.yaml` in a pinch to get going.

# Building

Building the first time will take a while as you get all of the dependencies
and compile them on your machine. Don't worry, builds after that will be
faster, since stack will cache these and not rebuild them unless the versions
change.

```shell
stack build --fast
```

# Running the executable

```shell
stack exec cellout-exe
```

If everything worked, you'll see the usage printout, since we did not specify
an input file.


# Testing

While you can use the running instructions above for running the resulting
binary, we also have a test suite that you can run.

```shell
stack test
```

Later on, to iterate quickly, you can re-run the test suite every time you write
changes to the files like this:

```shell
stack test --file-watch
```

This is particularly useful when isolating to a particular test.

```shell
stack test --no-rerun-tests --file-watch --ta=--match="three"
```


# Working iteratively

As a relatively inexperienced Haskell programmer, I enjoy working `ghci`, the
interactive shell of the Glasgow Haskell Compiler. `cellout` has a few
dependencies (like Aeson, the JSON parsing library), which means you will need
a Haskell environment where those dependencies are available. Luckily, we can
do that with

```shell
stack exec ghci
```

This will startup GHCi with a message like this:

```haskell
GHCi, version 8.4.3: http://www.haskell.org/ghc/  :? for help
*Main>
```

that `*Main>` prompt is where we can now load (and later reload) the files
we're working with, to see the compiler errors and try out our new functions
interactively after we succeed in compiling.

```haskell
:load src\Cellout.hs
```

If we haven't broken anything, we'll get a message like this:

```haskell
[1 of 1] Compiling Cellout              ( src\Cellout.hs, interpreted )
Ok, one module loaded.
*Cellout>
```

Otherwise, we'll get compiler error message. Either way, as we edit that file,
we can reload it with:

```haskell
:r
```

You can also `:browse` to see what's available, use `:t` to find the type
information. and try to use some of these functions interactively.


Something that might come in handy is a sample in-memory representation of a
notebook. We have a few of those in the test/sample.hs folder.

You can also read one while you're in ghci:

```
x <- readNb "test\\data\\hi.ipynb"
```

How did this work? Let's take a look at the type of `readNb`

```
:t readNb
readNb :: FilePath -> IO (Either String Notebook))
```

So `readNB` is a function that takes a FilePath, and returns an IO wrapped
result of either a successfull reading of a notebook, or a String error message
when trying to read it.


