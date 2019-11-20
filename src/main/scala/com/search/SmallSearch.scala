package com.search

import com.search.api.CLI

object SmallSearch extends App {
  new CLI(
    System.in,
    System.out,
    if (args.length == 1) Option(args(0)) else Option.empty
  ).parse()
}
