# CS141 Coursework 1 - Extension

This extension is a basic haskell webserver using [Spock](https://www.spock.li/) (was an absolute pain to setup)

## What it does

The code initiates a web server running on port 8080.
The server is purely and API only, and the front-end is in the respective folder of the main coursework zip. (You can also view the frontend live [here](https://lac-solver.noahdhollowell.co.uk/))

The server accepts a get request to the root showing a default message, and accepts a POST request where the body is a JSON encoded grid. It returns either and empty list for not solvable or a list of grids that are either the solutions for the given grid or the rotations required to get to the solution.

### JSON

The JSON encoding and decoding has been taken from Level.hs within the main coursework as the implementation for it was easy to use and follow.
All credit goes to Michael Gale for them.

**However** I have modified the toJSON for a cell to include the truth option as the front end uses this to display which cells are enabled

### Solving implementation

This works by first calling **solve** on the grid, if it returns a list of grids then we return them to the request, if it doesn't we then try to get a solution using **steps**. If this is fails we return an empty list, otherwise we return the list of grids that are the rotations

## Running

To run make sure to build using:

```bash
stack build --fast --pedantic
```

And then run using

```bash
stack exec cw1-ext-exe
```

In the project folder
You can also run it via:

```bash
stack repl
```

then running

```
main
```

Then post a request to _localhost:8080/grid_ where the post body is the JSON
E.g.

```json
{
  "rows": [
    {
      "cells": [
        { "action": "add", "operand": 2 },
        { "action": "add", "operand": 3 }
      ],
      "target": 3
    },
    {
      "cells": [
        { "action": "add", "operand": 5 },
        { "action": "add", "operand": 2 }
      ],
      "target": 4
    }
  ],
  "columns": [5, 2]
}
```

The expected output will be:

```json
[
  {
    "rows": [
      {
        "cells": [
          { "act": { "action": "add", "operand": 5 }, "truth": false },
          { "act": { "action": "add", "operand": 3 }, "truth": false }
        ],
        "target": 3
      },
      {
        "cells": [
          { "act": { "action": "add", "operand": 2 }, "truth": false },
          { "act": { "action": "add", "operand": 2 }, "truth": false }
        ],
        "target": 4
      }
    ],
    "columns": [5, 2]
  },
  {
    "rows": [
      {
        "cells": [
          { "act": { "action": "add", "operand": 3 }, "truth": true },
          { "act": { "action": "add", "operand": 5 }, "truth": false }
        ],
        "target": 3
      },
      {
        "cells": [
          { "act": { "action": "add", "operand": 2 }, "truth": true },
          { "act": { "action": "add", "operand": 2 }, "truth": true }
        ],
        "target": 4
      }
    ],
    "columns": [5, 2]
  }
]
```

If you want a nicer view then I recommend using the web panel that visualises
the returned grids [here](https://lac-solver.noahdhollowell.co.uk/)
