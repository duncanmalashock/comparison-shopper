OK, what I was thinking is that we start with one big list 

type alias Model =
    { values : List (List Value)
    , stack : List Decision
    }

type alias Decision =
    { choices : (Value, Value)
    , selection : Maybe Value  
    }

model = [ [1, 2, 3, 4, 5] ]

we chop it up into lists of one element each 

model = [ [1], [2], [3], [4], [5] ]

by means of some user interactions we merge it back up into progressively larger and larger sublists

type Msg
    = UserClicked Value

compare : Model -> List (Value, Value)
// find the first two lists with the smallest length
//      map each list to its length as a number
//      sort the list
//      take the first two items (which are lists)
//      call them (a, b)
// iterate through a & b, and return all pairs
// CAN BE OPTIMIZED???

compare model -> [(1, 2)]

UserClicked 2

model = [ [1, 2], [3], [4], [5] ]

compare model -> [(3, 4)]

UserClicked 4

compare model -> [(5, 1), (5, 2)]
model = 
    { values = [ [1, 2], [3, 4], [5] ]
    , stack = 
        [ Decision 5 1 Nothing
        , Decision 5 2 Nothing
        ]
    }

UserClicked 5

model = 
    { values = [ [1, 2], [3, 4], [5] ]
    , stack = 
        [ Decision 5 1 (Just 5)
        , Decision 5 2 Nothing
        ]
    }

UserClicked 5

model = 
    { values = [ [1, 2, 5], [3, 4] ]
    , stack = []
    }

compare model -> [ [1, 3], [1, 4], [2, 3], [2, 4], [5, 3], [5, 4] ]

by means of that series of interactions we end up with a list of lists of length one and we know we have sorted everything

I'm not sure if that's possible or not because now that while I was talking, I was watching this animation take place.

We see it getting broken up into list of length one and then out into this workin, so we kind of have two lists of lists in a sense it's not true that's that wasn't the issue.

It was when we got to these these comparisons we have these will have these temporary cases where we've got lists of different lengths sublists of different lengths that are being built up gradually, that's maybe not a problem 

but what I was thinking is that could be beneficial by really just having fewer things to model 

like we're not modeling the process of merge sort we're modeling sublists getting merged up and to me that was easier for me to understand and wrap my head around than your idea

modeling kind of the recipe for for merge sorting and all of the questions that were required and running through it as the series of interactions

that's cool because we just have a flat list of things to iterate through and then we're but it does feel more intimidating to me

----

WHICH ONE?

[ a ] [ b ]