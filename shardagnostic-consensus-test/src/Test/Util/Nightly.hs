-- | A @tasty@ command-line option for enabling nightly tests
module Test.Util.Nightly (
    BcccoinNightlyEnabled (..)
  , askBcccoinNightlyEnabled
  , defaultMainWithBcccoinNightly
  , bcccoinNightlyIngredient
  ) where

import           Data.Proxy (Proxy (..))
import           Test.Tasty
import           Test.Tasty.Ingredients
import           Test.Tasty.Options

-- | 'defaultMain' extended with 'bcccoinNightlyIngredient'
defaultMainWithBcccoinNightly :: TestTree -> IO ()
defaultMainWithBcccoinNightly =
    defaultMainWithIngredients (bcccoinNightlyIngredient : defaultIngredients)

-- | This ingredient merely adds the 'BcccoinNightlyEnabled' 'Option' to the
-- @tasty@ command-line parser.
bcccoinNightlyIngredient :: Ingredient
bcccoinNightlyIngredient =
    TestManager [Option (Proxy :: Proxy BcccoinNightlyEnabled)] $
    \_optionSet _testTree -> Nothing

-- | Query if the 'BcccoinNightlyEnabled' 'Option' is enabled
askBcccoinNightlyEnabled :: (Bool -> TestTree) -> TestTree
askBcccoinNightlyEnabled f = askOption $ \(BcccoinNightlyEnabled b) -> f b

-- | An 'Option' that indicates the test suite should run more tests, run
-- longer tests, etc
newtype BcccoinNightlyEnabled = BcccoinNightlyEnabled Bool

instance IsOption BcccoinNightlyEnabled where
  defaultValue = BcccoinNightlyEnabled False
  parseValue = fmap BcccoinNightlyEnabled . safeReadBool
  optionName = pure "tbco-enable-nightly-tests"
  optionHelp = pure "Enable more expensive tests (specific to TBCO)"

  -- Use typical Un*x syntax for Boolean flags
  optionCLParser = flagCLParser Nothing (BcccoinNightlyEnabled True)
