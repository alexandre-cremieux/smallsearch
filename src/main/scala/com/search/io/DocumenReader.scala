package com.search.io

import java.io.File
import java.nio.file.Path
import scala.io.{BufferedSource, Source}

/**
 * Exposes utilities method to read files from disk.
 */
object DocumentReader {

  /**
   * Read a .txt file and return it's content as an iterator over string.
   *
   * Close the file only when the iterator is exhausted.
   *
   * @param path The path of the .txt file to read.
   * @return An iterator over the content of the file.
   */
  def read(path: String): Iterator[String] = read(Path.of(path))

  def read(path: Path): Iterator[String] = {
    val source: BufferedSource = Source.fromFile(path.toFile)
    val lines: Iterator[String] = source.getLines()
    new Iterator[String] {
      override def hasNext: Boolean =
        if (lines.hasNext) {
          true
        } else {
          source.close()
          false
        }
      override def next(): String = lines.next()
    }.flatMap(line =>
      line.replaceAll("[^A-Za-z0-9]", " ")
        .split(" ")
        .map(s => s.trim.toUpperCase())
    )
  }

  /**
   * List the files contained in a folder if any.
   * @param dirPath The folder path.
   * @return A tuple of (filename, filepath)
   */
  def listFiles(dirPath: String): List[(String, String)] = {
    val extensions = List("txt")
    val directory = new File(dirPath)
    if (!directory.exists() || !directory.isDirectory) {
      List.empty
    } else {
      directory
        .listFiles()
        .filter(_.isFile)
        .filter(file => {
          extensions.exists(ext => file.getName.endsWith(ext))
        })
        .map(file => {
          (file.getName, file.getAbsolutePath)
        })
        .toList
    }
  }
}
