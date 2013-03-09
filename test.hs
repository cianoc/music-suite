
{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction #-}

module Main where

import Data.Default
import Data.Semigroup
import Control.Apply.Reverse
import System.Posix.Process

import Music.Pitch.Literal
import Music.MusicXml
import Music.MusicXml.Dynamics
import Music.MusicXml.Simple


score = testGliss

testNotes = fromPart
    "Test notes"
    "None" 
    "Treble"
    [      
        bar [   
            defaultDivisions,
            note c (1/4), 
            note d (1/2), 
            note e (1/4) ],
        
        bar [
            beam $ mconcat [ 
                note c  (1/8), 
                note d  (1/8),
                note e  (1/8),
                note f  (1/8) ],

            beam $ mconcat [ 
                note g  (1/8),
                note a  (1/8),
                note b  (1/8),
                note c' (1/8) ]
        ]
    ]

-- FIXME #1
testTuplets = fromPart
    "Test tuplets"
    "None"
    "Treble"
    [
        bar [
            defaultDivisions,
           beam $ mconcat [ 
               note c  (1/8), 
               note d  (1/8),
               note e  (1/8),
               note f  (1/8) 
           ],
           tuplet 3 2 $ mconcat [ 
               note g  (1/4),
               note a  (1/4),
               note b  (1/4) 
           ]
        ]
    ]

testArticulations = fromPart
    "Test articulations"
    "None"
    "Viola"
    [
        bar [
            defaultDivisions,
            
            beam $ mconcat [
                note c (1/16) & staccato,
                note d (1/16) & tenuto,
                note c (1/16) & staccato & tenuto,
                note d (1/16) & spiccato
            ],

            beam $ mconcat [
                note c (1/16) & staccatissimo,
                note d (1/16) & accent,
                note c (1/16) & strongAccent,
                note d (1/16) & accent & staccato & tenuto
            ],

            beam $ mconcat [
                note c (1/16) & beginSlur & accent,
                note d (1/16) & endSlur & staccato,
                note g (1/16) & beginSlur & staccato,
                note g (1/16) & endSlur & staccato
            ],

            beam $ mconcat [
                note c (1/16) & doit,
                note d (1/16) & falloff,
                note c (1/16) & stress,
                note d (1/16) & unstress
            ]
        ]
    ]

testDynamics = fromPart
    "Test dynamics"
    "None"
    "Viola"
    [    
        bar [
            defaultDivisions,
            metronome (1/4) False 72,

            crescFromTo PPP MP $ beam $ mconcat [
                note c (1/8) & tenuto & beginSlur,
                note c (1/8) & tenuto,
                note c (1/8) & tenuto,
                note c (1/8) & tenuto 
            ],
            beam $ mconcat [
                note c (1/8) & tenuto & endSlur,
                dimFromTo FF PP $ mconcat [
                    note c (1/8) & tenuto & beginSlur,
                    note c (1/8) & tenuto,
                    note c (1/8) & tenuto & endSlur 
                ]
            ]
        ],
        bar [
            defaultDivisions,            
            beam $ mconcat [
                note c (1/8),
                note c (1/8),
                note c (1/8),
                note c (1/8)
            ],

            beam $ mconcat [
                note c (1/8),
                note c (1/8),
                note c (1/8),
                note c (1/8)
            ]
        ]
    ]

-- Note not supported by Sibelius...
testGliss = fromParts
    "Test gliss"
    "None"
    (partList ["Violin", "Viola"])
    [
        [
            bar [
                defaultDivisions,
                metronome (1/4) False 120,
                note g (3/4) & beginSlide,
                note c' (1/4) & endSlide & beginTie
            ],
            bar [
                note c' (1/4)  & endTie & beginSlide,
                note b (3/4) & endSlide
            ]
        ],
        [
            bar [
                defaultDivisions,
                metronome (1/4) False 120,
                note g (3/4) & beginSlide,
                note c' (1/4) & endSlide & beginTie
            ],
            bar [
                note c' (1/4)  & endTie & beginSlide,
                note b (3/4) & endSlide
            ]
        ]
    ] 

testTies = fromParts
    "Test ties"
    "None"
    (partList ["Violin", "Viola"])
    [
        [
            bar [
                defaultDivisions,
                metronome (1/2) False 90,
                
                note g (1/2),
                note c' (1/2) & beginTie
            ],
            bar [
                note c' (1/2) & endTie,
                note b (1/2) & beginTie
            ],
            bar [
                note b (1/2) & endTie,
                note a (1/2) & beginTie
            ],
            bar [
                note a (1/2) & endTie,
                note g (1/2) & beginTie
            ],
            bar [
                note g (1/2) & endTie,
                note a (1/2) & beginTie
            ],
            bar [
                note a (1/2) & endTie,
                note g (1/2)
            ]
        ],
        [
            bar [
                defaultDivisions,
                note c 1
            ],
            bar [
                note e 1
            ],
            bar [
                note d 1
            ],
            bar [
                note f 1
            ],
            bar [
                note e 1
            ],
            bar [
                note d 1
            ]
        ]
    ]




misc = fromParts 
    "Miscellaneous tests"
    "None" 
    (partList ["Voice"])
    [ 
        [
            bar [
                defaultDivisions,
                trebleClef,
                key eb Major,
                commonTime,                       
                
                rehearsal "A",
                text "hello world!",

                dynamic MP,
                
                tuplet 5 4 $ beam $ slur $ mconcat [
                    note c  (1/8) & tenuto,
                    note d  (1/8) & tenuto,
                    note c  (1/8) & tenuto,
                    note d  (1/8) & tenuto,
                    note c  (1/8) & tenuto
                ],
                
                tuplet 3 2 $ mconcat [
                    note eb (1/4),
                    chord [d,a,fs'] (1/4),
                    note g_  (1/4)      & beginTie & beginSlur
                ] 
            ]     
            ,
            bar [
                note g_  (1/4) & endTie,
                note ab_ (1/4) & endSlur,
            
                beam $ mconcat [
                    note c (1/16) & accent,
                    note c (2/16),
                    note c (1/16)
                ],
                beam $ mconcat [
                    note c  (3/8) & beginGliss,
                    note b_ (1/8) & endGliss
                ]
            ]  
            ,  
            bar [
                crescFromTo PP FF $ mconcat [
                    note c (3/8),
                    note d (1/8),
                    note e (3/8)
                ],
                note f (1/8)
            ]
        ]    
    ]





main = openScore

openScore = openSib score
-- openScore = openLy score

showScore = putStrLn $ showXml $ score

openSib :: Score -> IO ()
openSib score =
    do  writeFile "test.xml" (showXml score)
        execute "open" ["-a", "/Applications/Sibelius 6.app/Contents/MacOS/Sibelius 6", "test.xml"]

openLy :: Score -> IO ()
openLy score =
    do  writeFile "test.xml" (showXml score)
        execute "musicxml2ly" ["test.xml"]
        execute "lilypond" ["test.ly"]
        execute "open" ["test.pdf"]

execute :: FilePath -> [String] -> IO ()
execute program args = do
    forkProcess $ executeFile program True args Nothing
    return ()

