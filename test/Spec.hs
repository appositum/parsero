import Data.Char (isHexDigit, isOctDigit)
import Data.Either (isRight)
import Text.Parsero
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Hexadecimal literals" $ do
    it "Can parse a hex literal" $ do
      testLiteral hex propHex "0x"

  describe "Octal literals" $ do
    it "Can parse a oct literal" $ do
      testLiteral oct propOct "0o"

  describe "Binary literals" $ do
    it "Can parse a bin literal" $ do
      testLiteral bin propBin "0b"

testLiteral :: Parsero String Integer -> Gen Char -> String -> IO ()
testLiteral parser prop prefix = do
  samples <- sample' prop
  putStr "  Samples: "
  print samples
  let (res, rest) = parse parser (prefix ++ samples)
  putStrLn $ "  Inconsumed input for " ++ show res ++ ": " ++ rest
  res `shouldSatisfy` isRight

propHex :: Gen Char
propHex = arbitrary `suchThat` isHexDigit

propOct :: Gen Char
propOct = arbitrary `suchThat` isOctDigit

propBin :: Gen Char
propBin = arbitrary `suchThat` isBinDigit
  where isBinDigit c = c `elem` "01"
