-- Practice 2.I
import Data.Char (toLower, isAlpha)

main :: IO ()
main = palindrome

palindrome :: IO ()
palindrome = do { putStrLn "Enter a string:"
          ; s <- getLine
          ; putStrLn (if isPalindrome s then "Yes!" else "No!")
          }

isPalindrome :: String -> Bool
isPalindrome xs
    | null xs = True
    | otherwise = s == r
    where
        s = map toLower $ filter isAlpha xs
        r = reverse s

sampleText1 :: String
sampleText1 = "Madam, I'm Adam"

sampleText2 :: String
sampleText2 = "A Man, a plan, a canal - Suez!"

sampleText3 :: String
sampleText3 = "Doc, note I dissent. A fast never prevents a fatness. I diet on cod."
