package com.search.io

import java.io.File

import scala.io.Source

/**
 * Exposes utilities method to read files from disk.
 */
object DocumentReader {

  /**
   * Read a .txt file an return it's content as an iterator over string.
   *
   * Note: The method could be optimize by implementing a proper iterator that
   * would iterate over the fs and close it when the iteration ends. This would save
   * space into memory.
   *
   * @param path The path of the .txt file to read.
   * @return An iterator over the content of the file.
   */
  def read(path: String): Iterator[String] = {
    val source = Source.fromFile(path)
    val list = source.getLines()
      .flatMap(line =>
        line.replaceAll("[^A-Za-z0-9]", " ")
          .split(" ")
          .map(s => s.trim.toUpperCase())
      ).toList
    source.close()
    list.iterator
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
      directory.listFiles()
        .filter(_.isFile)
        .filter(file => {
          extensions.exists(ext => file.getName.endsWith(ext))
        }).map(file => {
        (file.getName, file.getAbsolutePath)
      }).toList
    }
  }
}
