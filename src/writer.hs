import Control.Monad.Writer
import Control.Monad.State

type MyWriter = Writer [Int] String

example :: MyWriter
example = do
  tell [1..3]
--   tell [3..5]
  return "foo"

output :: (String, [Int])
output = runWriter example
-- ("foo", [1, 2, 3, 3, 4, 5])


test :: State Int Int
test = do
  put 3
  modify (+1)
  get

main :: IO ()
main = print $ execState test 0