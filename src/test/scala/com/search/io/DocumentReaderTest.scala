package com.search.io

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.nio.file.{Files, Path}

abstract class UnitSpec extends AnyFlatSpec
    with should.Matchers with Inside with Inspectors

class DocumentReaderTest extends UnitSpec {

  // Create a temporary file using java.nio
  def createTemporaryFile(tokens: List[String]): Path = {
    val path = Files.createTempFile("temp", ".txt")
    Files.write(path, tokens.mkString("\n").getBytes)
    path.toFile.deleteOnExit()
    path
  }

  "Streaming String from file content" should "be possible" in {
    // Given
    val tokens = List("Hello", "World", "Scala", "is", "awesome")
    val testPath = createTemporaryFile(tokens)

    // When
    val content: Iterator[String] = DocumentReader.read(testPath.toAbsolutePath.toString)

    // Then
    content.toList should contain theSameElementsAs tokens.map(it => it.toUpperCase)
  }

}
