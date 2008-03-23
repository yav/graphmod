module Main where

import Data.Dot

data Animation = Start

src label = node $ [ ("shape","none"),("label",label) ]
box label = node $ [ ("shape","box"),("style","rounded"),("label",label) ]

diamond label = node $ [("shape","diamond"),("label",label),("fontsize","10")]

same :: [DM a] -> DM [a]
same m = scope $ do
	  attribute "rank" "same"
	  sequence m

equiv n1 n2 = do
	cmp1 <- diamond "A"
	evidence1 <- src "Eq"
	return ()

main = putStrLn $ showDot $ do
	attribute "size" "40,15"
	attribute "rankdir" "LR"
        [refSpec,tarSpec] <- same 
		[ src "S"
		, src "T"
	        ]

	[c1,c2,c3] <- same 	  
		[ box "S"
	        , box "C"
	        , box "F"
		]
	refSpec .->. c1	
	tarSpec .->. c2	
	tarSpec .->. c3

	[m1,m2,ntm] <- same
		[ box "x"
		, box "y"
		, box "z"
		]
	
	c1 .->. m1
	c2 .->. m2

	xilinxSynthesis <- box "x"
	c3 .->. xilinxSynthesis

	gns <- box "G"
	xilinxSynthesis .->. gns

	gns .->. ntm

	ecs <- same    
		[ diamond "E"
		, diamond "E"
		, diamond "Eq"
		]

	m1 .->. (ecs !! 0)
	m1 .->. (ecs !! 1)
	m2 .->. (ecs !! 0)
	m2 .->. (ecs !! 2)
	ntm .->. (ecs !! 1)
	ntm .->. (ecs !! 2)

	sequence [ do evidence <- src "EE"
		      n .->. evidence
		 | n <- ecs 
		 ]


	edge refSpec tarSpec [("label","Engineering\nEffort"),("style","dotted")]

	return ()

