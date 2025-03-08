# THE PROGRAMMING LANGUAGE

Scala has multiple assets that make it a good language for this small project:
immutability and surety, functional paradigm, lambdas, parallel programming features,
object oriented programming, portability, etc.


# COMPILING AND RUNNING

## Prerequisites

### *The Scala build tool: sbt*

```bash
brew install sbt
```

Or download *sbt* from [Scala website](https://www.scala-sbt.org/download.html)

### *Scala*

```bash
brew install coursier && coursier setup
```

More help to get start from [Scala install page](https://docs.scala-lang.org/getting-started/install-scala.html)

## Compiling, packaging, running

1. To check if the project compile from the command line, go to the root of project
with the command line and enter:

```
$sbt compile
```

2. To run the project directly from sbt in the root folder, enter:

```
$sbt run
```

3. To produce a jar from sbt in the root folder and run the tests, enter:

```
$sbt assembly
```

Then, navigate to the project folder and execute the jar with Scala:

```
$ cd target/scala-X-XX
$ scala search-assembly-X-X.jar path/of/your/folder/of/txt/files
```

If everything has gone well you should see the following prompt:

```
Working dir: /path/to/the/project/target/scala-2.13
Please enter a command -->>
      | load: ## Load all resources from the resources folder, no parameters.
      | index: name path ## New document from file.
      | search: List, of, strings ## Exact match. None alphanumeric chars are separator.
      | fuzzy: List, of, strings ## Fuzzy search. Based on Levenshtein distance.
      | quit: ## Quit the app.
      |
```

If you choose this method to run the program, then you will have to set your own folder.
The default folder of the app is for test purpose and work only when executed from sbt
at the root folder of the project (or from an IDE).

To set and load the default resources folder then execute from the target/scala.X.XX folder

```
$ cd target/scala.X.XX
$ scala search-assembly-X-X.jar ../../resources
$
$ Log...
``` 


# USAGE

The CLI only understand and execute 5 commands:

* load  : Will load the files from the folder passed as first parameter to the application.
          This is normally done by default at startup time, but it is possible to re-index
* index : index a new document placed at a specific path.
* search: Search a list of words with an exact match.
* fuzzy : Proceed a fuzzy search. This may take a long time if one of the word in the given
          list is complex. For small words, it should be relatively usable.
* quit  : Quit the program.

A help prompt is given when a command fails.


# GLOBAL DESIGN

The global design is quite simple. The entry point is in the class `com.search.Search`, then CLI
is executed and can be found at `com.search.api.CLI`

The CLI interact with the engine described at com.search.engine.Engine which interact with model.

The model can be found at `com.search.model` and contains three classes:
* SearchTree: a generic binary search tree
* Token: generic wrapper that hold the data manipulated by the tree
* StringItem: a specialization of the `Token` class dedicated to String. It has been designed to
use Java String.

A very simple sequence diagram of a search would look like this:

`CLI --> Engine --> SearchTree --> Token`

Normally, only the engine is able to interact with the SearchTree and Token objects. The CLI would
be the view in a MVC, the engine would be the controller, and the classes in the model package, the
model.xs


# PERSONAL NOTES:

Implementing a good search engine is not a trivial thing, and I will not pretend that this
program is well-designed. It will surely not be suitable for production purposes and is
largely perfectible.

Some clues are (for an in memory storage):
* Reimplement the search tree to a red / black tree,
* Change the approach of proceeding a search (scalability, security, etc.),
* Add reference object to a document for each indexed token,
* Refactor the engine to proceed search on a global index,
* Implement a command pattern for the API package,
* Implement a sharding algorithm for distributed search,
* And much more...


















 
