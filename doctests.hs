import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "src/Language/Wordly.hs"]
