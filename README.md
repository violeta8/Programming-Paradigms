# Welcome to the Programming Paradigms course! #

Here you will find tutorials, examples, and other accompanying material.

# Installing Erlang #

Erlang can be installed on multiple platforms by either compiling it from source, or by downloading and installing one of the pre-compiled packages hosted on [erlang-solutions.com](https://www.erlang-solutions.com/resources/download.html).

## Configuring the Erlang Shell ##

In this course, we will not use sophisticated code-building mechanisms like `make` or `rebar`.
Instead, we shall stick to the simplest possible compilation mechanism: namely compilation through the Erlang shell.
This allows us to get used to the basic compilation commands, while at the same time, eliminating unnecessary complexities related to source code dependency management and others.

Compilation can be done by invoking the `c()` function on the Erlang shell like so:
```
> c(hello).
```

This compiles the module `hello.erl` (note we did not specify the `.erl` extension in the above example), resulting in the creation of an object code file `hello.beam`.
When a module is successfully compiled, it is automatically loaded by Erlang into the Erlang shell.
We can verify this by calling the `m()` function that displays the list of all loaded modules, together with the fully-qualified path pointing to the corresponding `.beam` file on the local file system.
Compiling modules in this way can become cumbersome when the number of modules is beyond a couple.

Erlang makes it possible for the user to configure and expose a number of utility functions that are automatically loaded and made accessible from the Erlang shell.
We will use this mechanism to facilitate the compilation and loading of Erlang modules:

1. Create a file named `user_default.erl` in your user home directory (mine is `/Users/duncan`), and paste in the following Erlang code:

```
-module(user_default).

-export([lm/0, cm/0]).

lm() ->
  [purge_load(F) || F <- (fun() -> List = filelib:wildcard("*.beam"), lists:map(fun(I) -> list_to_atom(lists:takewhile(fun(C) -> C =/= $. end, I)) end, List) end)()].

cm() ->
  [compile:file(F) || F <- (fun() -> List = filelib:wildcard("*.erl"), lists:map(fun(I) -> list_to_atom(lists:takewhile(fun(C) -> C =/= $. end, I)) end, List) end)()].

purge_load(M) ->
  code:purge(M), code:load_file(M).
```

2. Create a `.erlang` file in your user home directory, and paste in the following code:
```
code:load_abs("/Users/duncan/user_default").
io:format("Loading my Erlang shell..~n").
```

3. Fire up the Erlang shell (make sure you are in your home directory) by typing `erl` on the command prompt, and compile the `user_default.erl` file. This should result in the creation of a `user_default.beam` file:

```
> c(user_default).
```

4. Close the Erlang shell (by typing `q().`) and reopen it again, to allow the shell to take into account these new changes. One loaded, you should see something like the following in your shell:

```
Erlang/OTP 18 [erts-7.3] [source-d2a6d81] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Loading my Erlang shell..
Eshell V7.3  (abort with ^G)

```

The first file `user_default.erl` exports two functions (you can add your own if you like):

1. `lm/1` loads all the compiled modules (the `.beam` files) found in the current directory.

2. `cm/1` compiles all the Erlang source code files, but does not load them.

To compile and load the Erlang files in one step, you can type the following on the Erlang shell:

```
> cm(), lm().
```

A more neat way of doing this would be to add a new function, say `clm/0` into the `user_default.erl` file like so:

```
clm() -> cm, lm().
```

Don't forget to export the function correctly (_i.e._ `-export([clm/0])`), save the file, and recompile it again from the Erlang shell using `c()`. Restart the Erlang shell for the changes to take effect. Test your new function by invoking `clm/0` on the shell, for instance:

```
clm().
[{module,log},
 {module,mobile},
 {module,server_gen},
 {module,switch},
 {module,switch_naive},
 {module,util}]
```

The Erlang compiler can also be invoked from the terminal (_i.e._ outside the Erlang shell) using `erlc`:

```
$> erlc hello.erl
```

Note that by contrast to compiling from the Erlang shell, the `.erl` extension must be specified this time round.

This completes the Erlang shell configuration guide.

## Setting up the Source Code Editor ##

In this step, we shall configure the Atom text editor which will suffice for our coding purposes.
Atom offers basic code syntax highlighting for a number of well known languages including Erlang.
It also offers support for a ton of user-created packages that can be downloaded and used for free, making it possible to extend the basic functionality provided by Atom.
Atom can be downloaded from [atom.io](https://atom.io); for the list of available Atom packages, look at [https://atom.io/packages](https://atom.io/packages).

We can provide Erlang language support by downloading the `language-erlang` Atom plugin.
Open a new terminal window and type the following:

```
apm install language-erlang
```

If you get any complaints when invoking this command, make sure you have installed shell command support for Atom.
To do this, open Atom, select `Atom` -> `Install Shell Commands` from the menu.

### Bonus ###

If you dislike the current look-and-feel that comes with the default Atom installation, feel free to download and install the following packages:

* `atom-material-ui`: a dynamic UI theme for that follows Google's Material Design guidelines.
* `atom-material-syntax`: a dark syntax theme  that uses Google's Material Design color palette.
* `file-icons`: file extension icons and colors.

# Installing Go #

Similar to Erlang, Go can be easily installed on multiple platforms using the pre-compiled packages found on [golang.org](https://golang.org/dl/).

## Using the Go compiler ##

As done for the Erlang case, we shall not use sophisticated code-building mechanisms, as the plain vanilla Go compiler will suffice for our purposes.
Go comes with an easy-to-use compiler that can be invoked from the terminal through the `go` command.
Invoking `go` displays a number of switches that can be used to manage the lifecycle of Go programs:

```
$> go
Go is a tool for managing Go source code.

Usage:

	go command [arguments]

The commands are:

	build       compile packages and dependencies
	clean       remove object files
	doc         show documentation for package or symbol
  ...
```

Go files can be built as follows:

```
$> go build hello.go
```

This generates the file `hello` containing the Go executable that can be run by simply:

```
$> ./hello
Hello
```

To build and run your program in one step, use the following command:

```
$> go run hello.go
Hello
```

Unlike `build`, the `run` command does _not_ leave the executable file `hello` behind it once the file has been successfully executed.
Note that the file extension `.go` is specified when building the Go file.

## Setting up the Source Code Editor ##

We shall use the same Atom editor that comes already equipped with syntax highlighting support for Go.
