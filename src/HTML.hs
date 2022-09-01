module HTML where

import Types

document :: Element  
document = 
        element DoctypeHtml () ++ show
        element Html 
            [ Content (Value_ "I love you Najat!")
            , Value (Value_ "You are awesome !!")
            , Style (Value_ "display : Block;")
            ] 
            [ element Head [] []
            , element Body [] [] 
            ]    

element :: Tag -> [Child] -> Element
element = Element . getValid  