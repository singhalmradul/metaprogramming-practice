package course.wartimiser

case class Person(
    name: String,
    age: Int,
    favouriteLanguage: String,
    colourBlind: Boolean
)

@main
def main(args: String*): Unit =
  wartimise( /* StringPlusAny, */ CollectionOptimiser, CopyChain):
    // println("Scala <" + 3)
    val value = Map(1 -> "Scala").get(2).getOrElse("scala")
    val firstEven = List(1, 2, 3, 4, 5).filter(_ % 2 == 0).headOption
    val oneTwoThree = List(1, 2, 3)
      .map(_ * 2)
      .map(_.toString)
      .map(_.toInt)
      .map(_ / 2)
      .map(_.toString)
      .mkString

    val a = 1
    val b = 2
    val scala = "Scala"

    val person1 = Person("madz", 1, scala, false)
    val person2 = person1
      .copy(name = "someone")
      .copy(age = a + b)
      .copy(favouriteLanguage = scala + 3)
      .copy(colourBlind = true)

    /*
      val $4$ =
        val $3$ =
          val $2$ =
            val $1$ =
              $0$.copy(name = "someone", $0$.copy$default$2, $0$.copy$default$3, $0$.copy$default$4)

            val age$1 = (a + b)
            val name$1 = $1$.copy$default$1
            val favouriteLanguage$1 = $1$.copy$default$3
            val colourBlind$1 = $1$.copy$default$4

            $1$.copy(name$1, age = age$1, favouriteLanguage$1, colourBlind$1)

        val favouriteLanguage$2: String = (scala + 3)
        val name$2 = $2$.copy$default$1
        val age$2 = $2$.copy$default$2
        val colourBlind$2 = $2$.copy$default$4

        $2$.copy(name$2, age$2, favouriteLanguage = favouriteLanguage$2, colourBlind$2)

        $3$.copy($3$.copy$default$1, $3$.copy$default$2, $3$.copy$default$3, colourBlind = true)
     */
    /*
    Block(
      List(
        ValDef(
          "$3$",
          _,
          Some(
            Block(
              List(
                ValDef(
                  "$2$",
                  _,
                  Some(
                    Block(
                      List(
                        ValDef(
                          "$1$",
                          _,
                          Some(
                            Apply(
                              Select(Ident("$0$"), "copy"),
                              List(
                                NamedArg(
                                  "name",
                                  Literal(StringConstant("someone"))
                                ),
                                Select(Ident("$0$"), "copy$default$2"),
                                Select(Ident("$0$"), "copy$default$3"),
                                Select(Ident("$0$"), "copy$default$4")
                              )
                            )
                          )
                        ),
                        ValDef(
                          "age$1",
                          _,
                          Some(Apply(Select(Ident("a"), "+"), List(Ident("b"))))
                        ),
                        ValDef(
                          "name$1",
                          _,
                          Some(Select(Ident("$1$"), "copy$default$1"))
                        ),
                        ValDef(
                          "favouriteLanguage$1",
                          _,
                          Some(Select(Ident("$1$"), "copy$default$3"))
                        ),
                        ValDef(
                          "colourBlind$1",
                          _,
                          Some(Select(Ident("$1$"), "copy$default$4"))
                        )
                      ),
                      Apply(
                        Select(Ident("$1$"), "copy"),
                        List(
                          Ident("name$1"),
                          NamedArg("age", Ident("age$1")),
                          Ident("favouriteLanguage$1"),
                          Ident("colourBlind$1")
                        )
                      )
                    )
                  )
                ),
                ValDef(
                  "favouriteLanguage$2",
                  _,
                  Some(
                    Apply(
                      Select(Ident("scala"), "+"),
                      List(Literal(IntConstant(3)))
                    )
                  )
                ),
                ValDef(
                  "name$2",
                  _,
                  Some(Select(Ident("$2$"), "copy$default$1"))
                ),
                ValDef(
                  "age$2",
                  _,
                  Some(Select(Ident("$2$"), "copy$default$2"))
                ),
                ValDef(
                  "colourBlind$2",
                  _,
                  Some(Select(Ident("$2$"), "copy$default$4"))
                )
              ),
              Apply(
                Select(Ident("$2$"), "copy"),
                List(
                  Ident("name$2"),
                  Ident("age$2"),
                  NamedArg("favouriteLanguage", Ident("favouriteLanguage$2")),
                  Ident("colourBlind$2")
                )
              )
            )
          )
        )
      ),
      Apply(
        Select(Ident("$3$"), "copy"),
        List(
          Select(Ident("$3$"), "copy$default$1"),
          Select(Ident("$3$"), "copy$default$2"),
          Select(Ident("$3$"), "copy$default$3"),
          NamedArg("colourBlind", Literal(BooleanConstant(true)))
        )
      )
    )
     */

  /*
  Block(
    List(
      ValDef(
        "$2$",
        _,
        Some(
          Block(
            List(
              ValDef(
                "$1$",
                _,
                Some(
                  Apply(
                    Select(Ident("$0$"), "copy"),
                    List(
                      NamedArg("name", Literal(StringConstant("someone"))),
                      Select(Ident("$0$"), "copy$default$2"),
                      Select(Ident("$0$"), "copy$default$3"),
                      Select(Ident("$0$"), "copy$default$4")
                    )
                  )
                )
              ),
              ValDef(
                "age$1",
                _,
                Some(Apply(Select(Ident("a"), "+"), List(Ident("b"))))
              ),
              ValDef(
                "name$1",
                _,
                Some(Select(Ident("$1$"), "copy$default$1"))
              ),
              ValDef(
                "favouriteLanguage$1",
                _,
                Some(Select(Ident("$1$"), "copy$default$3"))
              ),
              ValDef(
                "colourBlind$1",
                _,
                Some(Select(Ident("$1$"), "copy$default$4"))
              )
            ),
            Apply(
              Select(Ident("$1$"), "copy"),
              List(
                Ident("name$1"),
                NamedArg("age", Ident("age$1")),
                Ident("favouriteLanguage$1"),
                Ident("colourBlind$1")
              )
            )
          )
        )
      ),
      ValDef(
        "favouriteLanguage$2",
        _,
        Some(Apply(Select(Ident("scala"), "+"), List(Literal(IntConstant(3)))))
      ),
      ValDef(
        "name$2",
        _,
        Some(Select(Ident("$2$"), "copy$default$1"))
      ),
      ValDef("age$2", _, Some(Select(Ident("$2$"), "copy$default$2"))),
      ValDef(
        "colourBlind$2",
        _,
        Some(Select(Ident("$2$"), "copy$default$4"))
      )
    ),
    Apply(
      Select(Ident("$2$"), "copy"),
      List(
        Ident("name$2"),
        Ident("age$2"),
        NamedArg("favouriteLanguage", Ident("favouriteLanguage$2")),
        Ident("colourBlind$2")
      )
    )
  )
   */

  /*
  Block(
    List(
      ValDef(
        "$1$",
        _,
        Some(
          Apply(
            Select(Ident("$0$"), "copy"),
            List(
              NamedArg("name", Literal(StringConstant("someone"))),
              Select(Ident("$0$"), "copy$default$2"),
              Select(Ident("$0$"), "copy$default$3"),
              Select(Ident("$0$"), "copy$default$4")
            )
          )
        )
      ),
      ValDef(
        "age$1",
        _,
        Some(Apply(Select(Ident("a"), "+"), List(Ident("b"))))
      ),
      ValDef(
        "name$1",
        _,
        Some(Select(Ident("$1$"), "copy$default$1"))
      ),
      ValDef(
        "favouriteLanguage$1",
        _,
        Some(Select(Ident("$1$"), "copy$default$3"))
      ),
      ValDef(
        "colourBlind$1",
        _,
        Some(Select(Ident("$1$"), "copy$default$4"))
      )
    ),
    Apply(
      Select(Ident("$1$"), "copy"),
      List(
        Ident("name$1"),
        NamedArg("age", Ident("age$1")),
        Ident("favouriteLanguage$1"),
        Ident("colourBlind$1")
      )
    )
  )
   */
