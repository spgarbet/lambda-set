module Main where

import Text.ParserCombinators.Parsec

-- There are three objects in the Lambda set
-- Ignoring the whitespace issue at present
-- This will construct a parser for terms
-- in the Lambda set
lambdaset :: Parser String
lambdaset = variable <|> parenthesis
-- Unfortunately, an intermediate "parenthesis" holder is needed
-- for parse disambiguation
parenthesis :: Parser String
parenthesis = do { char '(';
                   r <- lambda <|> application;
                   char ')';
                   return r
                 }
-- So now, the lambda set is
--   a variable
--   a lambda expression or
--   an application. 

-- For simplicity in this demo, a variable is a single letter 
variable :: Parser String
variable = do { c <- letter; return [c] }

-- Now for the lambda expression itself
lambda :: Parser String
lambda = do { char '\\';
              variable;
              char '.'; -- This is needed, to distinguish variable contexts
              l <- lambdaset;
              return "lambda"
            }

application :: Parser String
application = do { lambdaset;
                   lambdaset;
                   return "Application"
                 }
-- At this point the definition of a parser is complete,
-- But it would be nice to have a nice main loop to 
-- demonstrate this gedankenexperiment 

run :: Show a => Parser a -> String -> IO ()
run p input
        = case (parse p "" input) of
            Left err -> do{ putStr "parse error at "
                          ; print err
                          }
            Right x  -> print "VALID"

-- Demonstrations
-- run lambdaset "x"
-- run lambdaset "(\\x.x)"
-- run lambdaset "(\\x.(xx))(\\x.(xx))"

-- Now for a SKI combinator expression, Sxyz = xz(yz)
-- run lambdaset "(\\x.(\\y.(\\z.((xz)(yz)))))"

-- Obviously at this point, a multiple variable lambda syntax is required
-- and some manner to define a named operator

