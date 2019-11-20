package com.search.model

/**
 * Represent a document indexed by the search engine.
 *
 * A document is basically a tree that implements search and add traits.
 *
 * @param name Document name.
 */
class Document(name: String) {

  /*Name getter*/
  def name(): String = this.name

  /*The search tree.*/
  var tree: SearchTree[StringItem] = Leaf()
}
