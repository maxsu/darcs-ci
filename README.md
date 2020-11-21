About
-----

[Darcs](http://darcs.net) is a distributed version control system
written in Haskell.

Building
--------

To build and install the latest release, use

```
cabal update && cabal install darcs
```

with a recent cabal (version 3.2 or later is recommended). Any version of
ghc from 8.2 up to 8.10 should work.

From inside a clone or a source dist, use

```
> cabal build
```

or

```
> cabal install
```

If you prefer stack:

```
> stack install
```

Note that using stack will select older versions for some dependencies,
which may mean that performance is slightly less than optimal.

Running the test suite
----------------------

This is optional, of course, but useful if you want to help find bugs or
before you contribute patches.

```
> cabal build --enable-tests
> cabal test --test-show-details=direct
```

Using
-----

To clone a repository via HTTP and send patches by mail:

```
> darcs clone --lazy http://darcs.net
> # edit files...
> darcs add my_new_file
> darcs record -m "my changes"
> darcs send
```

To clone via SSH and push patches:

```
> darcs clone user@hub.darcs.net:user/repo
> # edit files...
> darcs add my_new_file
> darcs record -m "my changes"
> darcs push
```

To create a project and start working:

```
> darcs init my_project
> cd my_project
> # create and edit files...
> darcs add my_new_file
> darcs record -m "initial version"
```

Pull new patches from upstream:

```
> darcs pull
```

Documentation
-------------

Concise and up-to-date documentation is available from darcs itself:

```
> darcs help # list all commands
> darcs help command # help for specific command
> darcs command --help # dito
```

The complete documentation is available as a man page which can be generated
using

```
darcs help manpage > darcs.1
```

Reporting bugs
--------------

Please send bug reports to <bugs@darcs.net>. This will automatically add
your report to the bug tracker. If you are unsure or just have a question or
a comment, you can subscribe to darcs-users@darcs.net and post your question
or comments there. See http://darcs.net/MailingLists for details.

There is also an IRC channel named `#darcs` on freenode.net, where you can
report problems or ask questions.


Hacking
-------

We are happy to receive patches and will try our best to review them in a
timely fashion. Just record your patches in a clone of
<http://darcs.net/screened> and `darcs send` them. You are encouraged, but not
required, to look at <http://darcs.net/Development/GettingStarted> for
additional information.

BTW, the wiki is a darcs repo, you can clone it with:

```
> darcs clone --lazy http://darcs.net/darcs-wiki
```

to edit the contents and send us patches.
