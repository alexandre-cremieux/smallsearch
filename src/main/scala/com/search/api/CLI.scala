package com.search.api

import java.io.{InputStream, PrintStream}
import java.util.Scanner

import com.search.engine.{Engine, SearchResult}
import com.search.io.DocumentReader

/**
 * CONTENT: The CLI object definition for parsing, a set of Instructions to interact
 * with the search engine, a set of messages to be prompt in the default output.
 */

/**
 * Command line interface representation.
 *
 * This CLI offers a set of instructions for the command line.
 *
 * The available commands:
 *      - default: read all files from default folder
 *      - create: read and index a document from a specific path
 *      - search: proceed an exact match search
 *      - fuzzy: proceed a fuzzy search based on the Levenshtein distance.
 *      - quit: exit the process.
 *
 * By default, the directory path is set to the resources folder of the project.
 *
 * @param defaultDirectory The default directory.
 */
class CLI(defaultDirectory:  Option[String]) {

  /**
   * This constructor let the calling code define the input and output streams used by
   * the CLI to prompt the user and take instructions.
   * @param in The input stream to be used.
   * @param out The output stream to be used.
   * @param defaultDirectory The default directory to use to load and index the load the
   *                         default set of documents.
   */
  def this(in: InputStream, out: PrintStream, defaultDirectory:  Option[String]) {
    this(defaultDirectory)
    System.setIn(in)
    System.setOut(out)
  }

  /**
   * Parse the command line instructions.
   *
   * NOTE: The instructions could be designed around a command pattern in order to
   * ease the further development of features offered to the command line.
   */
  def parse(): Unit = {
    val context = new Variables(defaultDirectory)
    if (context.resources.isDefined) {
      Instructions.readDefaultResources(Array("LOAD"), context)
    }
    val line = new Scanner(System.in)
    this.printWorkingDir()
    this.printHelp()
    var continue = true
    while(continue) {
      this.prompt()
      val entry = line.nextLine()
      val instructions = entry.split(":")
      if (instructions.isEmpty) {
        this.printHelp()
      } else {
        continue = instructions(0).toUpperCase() match {
          case "LOAD" =>
            Instructions.readDefaultResources(instructions, context)
            true
          case "INDEX" =>
            Instructions.createDocument(instructions)
            true
          case "SEARCH" =>
            Instructions.searchDocument(instructions)
            true
          case "FUZZY" =>
            Instructions.fuzzySearch(instructions)
            true
          case "QUIT" => false;
          case _ =>
            printUnknownArgument(instructions)
            printHelp()
            true
        }
      }
    }
  }

  /**
   * Print the working directory to the user.
   */
  def printWorkingDir(): Unit = {
    println("Working dir: %s".format(new java.io.File(".").getCanonicalPath))
  }

  /**
   * Print an invite prompt to the user.
   */
  def prompt(): Unit = {
    print("Search ->>")
  }

  /**
   * Print the helps.
   */
  def printHelp(): Unit = {
    println(Msg.help)
  }

  /**
   * Print the unknown argument error.
   * @param instructions The instructions entered by the user.
   */
  def printUnknownArgument(instructions: Array[String]): Unit = {
    println(Msg.unknownArguments(instructions))
  }
}

/**
 * Set of functions describing the features offered to the user from the command line.
 *
 * NOTE: The instructions could be redesigned by defining a generic response suitable for
 * different API. Moreover a command patterns could be designed in order to offer the
 * same signature for each instruction. This could ease further development and allow
 * the creation of a static code register accessible from a service opened to any
 * end point.
 */
object Instructions {

  /**
   * List the files contained in a folder.
   *
   * Note: This method is here to offer an Exception handling mechanism
   *
   * @param path The path of the folder to scan.
   * @return A list of .txt files if any.
   */
  private def listFiles(path: String): List[(String, String)] = {
    try {
      DocumentReader.listFiles(path)
    } catch {
      case e: Exception =>
        println (e)
        List.empty
    }
  }

  /**
   * Read all available items from a .txt file at a specific path.
   * @param path The path of the file.
   * @return The list of items or an empty list.
   */
  private def read(path: String): Iterator[String] = {
    try {
      DocumentReader.read(path)
    } catch {
      case e: Exception =>
        println (e)
        List ().iterator
    }
  }

  /**
   * Make the engine index a .txt file from a specific path.
   * @param name The name of the file.
   * @param path The system path of the file.
   * @return A success | error message
   */
  private def create(name: String, path: String): Option[String] = {
    val items = Instructions.read(path)
    if (items.isEmpty) Option(Msg.searchAbortedError())
    else {
      Engine.index(name, items)
      Option(Msg.createSuccess(name, path))
    }
  }

  /**
   * Index the default folder.
   * @param instructions The instruction to parse
   * @param context The context vars.
   */
  def readDefaultResources(instructions: Array[String], context: Variables): Unit =  {
    if (instructions.length > 1) {
      println(Msg.wrongNumberOfArguments("DEFAULT", "", 1, instructions.length))
    }
    val folder = {
      if (context.resources.isDefined) context.resources.get
      else context.defaultResourceFolder
    }
    val documents = Instructions.listFiles(folder: String)
    if (documents.isEmpty) {
      println(Msg.noFileFoundInFolder(folder: String))
    } else {
      documents.map({
        case (name, path) =>  Instructions.create(name, path)
      }).filter(_.isDefined).foreach(msg => {
        println(msg.get)
      })
    }
  }

  /**
   * Index a document from API commands.
   * @param instructions The instructions to parse.
   */
  def createDocument(instructions: Array[String]): Unit = {
    val params = instructions(1).trim().split(" ")
    if (params.length != 2) {
      println(Msg.indexingError)
    } else {
      val name = params(0)
      val items = Instructions.read(params(1))
      if (items.isEmpty) println(Msg.searchAbortedError())
      else Engine.index(name, items)
    }
  }

  /**
   * Make the engine search for a list of string.
   *
   * This is the generic method. It exposes the boiling plate code needed to execute
   * the a search. The type of search is defined by the engineSearchType argument.
   * @param instructions Instructions to parse.
   * @param engineSearchType Callback, defines the type of search to be executed by the
   *                         engine.
   */
  private def search
  (instructions: Array[String], engineSearchType: List[String] => SearchResult):
  Unit = {
    if (instructions.length < 2) {
      println(Msg.wrongNumberOfArguments("SEARCH", "At least", 2, instructions.length))
    } else {
      val params = instructions(1)
        .replaceAll("[^A-Za-z0-9]", " ")
        .split("\\s+")
        .map(s => s.trim.toUpperCase())
        .filter(s => !s.isBlank || !s.isEmpty)
      val items = params.toList
      try {
        val start = System.currentTimeMillis()
        val result = engineSearchType(items)
        val end = System.currentTimeMillis()
        println("Found in %s ms:".format(end - start))
        val docs = result.documents().filter(doc => doc.getScore > 0)
        if (docs.isEmpty) println("\tNothing")
        else {
          var count = 0
          while (count < 10 && count < docs.size) {
            val doc = docs(count)
            println("\tdDocument name: %s, score: %s".format(doc.name, doc.getScore))
            count += 1
          }
        }
      } catch {
        case e: Exception => println(e)
      }
      println()
    }
  }

  /**
   * Make the engine execute an exact match search
   * @param instructions Instructions to parse.
   */
  def searchDocument(instructions: Array[String]): Unit = {
    search(instructions, items => Engine.exactMatch(items, s => s))
  }

  /**
   * Make the engine execute a fuzzy search
   * @param instructions Instructions to parse.
   */
  def fuzzySearch(instructions: Array[String]): Unit = {
    search(instructions, items => Engine.fuzzy(items, s => s))
  }
}

/**
 * The context variables.
 * @param userFolder Default folder of the user.
 */
class Variables(userFolder: Option[String]) {
  final val defaultResourceFolder: String = "resources"

  /**
   * Define the resource folder to use (user defined or default project resource folder)
   */
  val resources: Option[String] = this.userFolder
}

/**
 * Set of message to be prompted to the user.
 */
object Msg {
  def help: String =
    """Please enter a command -->>
      | load: ## Load all resources from the resources folder, no parameters.
      | index: name path ## New document from file.
      | search: List, of, strings ## Exact match. None alphanumeric chars are separator.
      | fuzzy: List, of, strings ## Fuzzy search. Based on Levenshtein distance.
      | quit: ## Quit the app.
      |"""

  def createSuccess(name: String, path: String): String = {
    """Loaded document: %s at path %s""".format(name, path)
  }

  def indexingError: String = {
    """To index:
      | index: name path ## Only two parameters separated by space
      |"""
  }

  def readError: String = {
    """To search:
      | search: List, of, items ## Separated by comma, no colon, no sanitizer
      |"""
  }

  def searchAbortedError(): String = "An error occurred. Please retry"

  def quit(): String = "Bye..."

  def commandError(s: String): String = "Unknown command: %s".format(s)

  def foundMsg(doc: SearchResult): String =
    "Found %s document"
      .format(doc.documents().filter(d => d.getScore > 0))

  def wrongNumberOfArguments
  (instruction: String, prefix: String, allowed: Int, given: Int):
  String = {
    "Wrong number of argument for %s. %s%s%d required. %d given".format(
      instruction,
      prefix,
      if (prefix == "") "" else " ",
      allowed,
      given
    )
  }

  def unknownArguments(instructions: Array[String]): String = {
    if (instructions.isEmpty) {
      "No arguments"
    } else {
      "Unknown arguments %s".format(instructions.toString)
    }
  }

  def noFileFoundInFolder(s: String): String = {
    "No file found in folder at %s".format(s)
  }
}
