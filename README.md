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

    git clone https://github.com/jonathanyc/topple.git
    opam pin add topple topple

... should both install dependencies and install `topple` and `topup` to your `$PATH`.
