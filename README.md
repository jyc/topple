# Topple

Topple is a small script for running commands associated with vertices of a graph with versions in order.
Most likely, the vertices of the graph are subprojects in a big project and an edge from A to B means A depends on B.

**Most of the time, the right tool for that sort of thing is Make.**
I wrote this partly for fun and partly because I actually thought it'd help me save some time with a project (it did!).

Suppose you have the graph (stored in `_topple`):

    A --> B --> C
          \
           \--> D --> E

... some commands associated with A, B, C, D, and E, their current versions, and their versions at the last time you ran the command.

Suppose the version of B, stored in `B/.topple-version`, is different than the version of B from when you last ran `topple`.
Topple keeps track of this in `.topple-status`.
Then `topple` will run the commands for B, C, and D, then E.

The `_topple` file for the example above might look like this:

    # A comment.

    A:
        # Some commands for A.

    B: A
        # Some commands for B.

    C: B
        # Some commands for C.

    D: B
        # Some commands for D.

    E: A B D
        # Not necessary to list A and B (because D depends on them).

Looks suspiciously like a Makefile!

If these commands create files whose modification times you can easily compare to determine whether or not you want to rerun the commands for a vertex (say, if B's output is newer than C's, we want to rerun the commands for C), then **you should use Make**.

Topple will be a little more annoying because you have to specify that a vertex has a new version yourself.
A new version might come in when you `git pull`, along with sources modified by your teammate. 
Of course, you yourself can increase the version by running `topup` or `topup <path>`.

When you run `topple` in the directory with a `_topple` file it'll rebuild everything downstream of the vertex with the new version.

# Installing

Topple is written in OCaml. If you have OPAM,

    $ git clone https://github.com/jonathanyc/topple.git
    $ opam pin add topple topple

... should both install dependencies and install `topple` and `topup` to your `$PATH`.

# Automatic Versioning

Starting with version 0.3.0, Topple has support for "automatic versioning" of directories based on glob patterns.
Topple derives a tree of filenames that match a positive pattern and do not match any negative patterns then hashes the tree with the last modification time of the files in the tree.
This approach takes care of deletion of files in addition to addition of files, unlike the naive approach that only considers modification times.

The syntax is as follows:

    <pos> ::= <glob>
    <neg> ::= !<glob>
    
    <pos | neg>
    <subproject>: <dep> ...
        <command>
        ...

... for example, we can add a subproject `core` whose version depends on all of the `*.ml` files except for `tileAuto.ml`, which is generated:

    *.ml !tileAuto.ml
    core:
        echo "Building core subproject!"

The hash is then stored instead of the version number in the `.topple-status` file.
The presence of patterns overrides the existence of a `.topple-version` file.

# Really, what's the use?

As far as I know, Make is great for when:

1. You want to generate an artifact by running some commands,
2. you can easily query the modification time of the artifact,
3. you can easily query the modification times of the inputs,
4. so you can compare the last modification time of an input with that of the artifact in order to determine whether or not you need to run the commands again.

This works for almost every project I've ever worked on. Unfortunately, I was working on an OCaml project with subprojects that installed as findlib packages (why I had to do this was another story!)
It seemed pretty terrible to have Make check the modification times of the files that findlib installed, somewhere deep in `~/.opam/...`, to the modification times of files in my project.
I could have had the commands create a "phony" file that served only to mark the last time the commands installing the subproject was run, but I decided I might as well have fun writing this instead.

# To Do

- Better parsing of the _topple file. It'd be nice to allow escapes in more places.
- Add support for Git-style `**`-patterns.
