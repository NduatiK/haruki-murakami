module Models.Book exposing (..)


type alias Book =
    { imageUrl : String
    , title : String
    , stars : Int
    , reviews : Int
    }


allBooks : List Book
allBooks =
    [ { imageUrl = "book1-large.png"
      , title = "The Wind-Up Bird Chronicle"
      , stars = 4
      , reviews = 1604
      }
    , { imageUrl = "book2-large.png"
      , title = "1Q84 - Book 1,2,3"
      , stars = 4
      , reviews = 1718
      }
    , { imageUrl = "book3-large.png"
      , title = "Kafka on the Shore"
      , stars = 5
      , reviews = 616
      }
    , { imageUrl = "book4-large.jpeg"
      , title = "The Elephant Vanishes"
      , stars = 5
      , reviews = 616
      }
    , { imageUrl = "book5-large.jpeg"
      , title = "South of the Border, West of the Sun"
      , stars = 5
      , reviews = 616
      }
    ]
