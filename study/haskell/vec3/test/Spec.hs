import qualified Test.Framework as Test
import qualified Test.Framework.Providers.HUnit as Test
import Test.HUnit ((~:), (~?=))

import Lib

main :: IO()
main = do
    putStrLn "hello"

tests =
    [ "+"      ~: (v + u)   ~?= Vec3  5  7  9
    , "-"      ~: (u - v)   ~?= Vec3  3  3  3
    , "*"      ~: (v * u)   ~?= Vec3  4 10 18
    , "dot"    ~: (v .|. u) ~?= 32
    , "cross"  ~: (v *|* u) ~?= Vec3 (-3) 6 (-3)
    , "invert" ~: invert v  ~?= Vec3 (-1) (-2) (-3)
    , "scalar" ~: scalar (Vec3 1 2 2) ~?= 3
    ]
    where
        v = Vec3 1 2 3
        u = Vec3 4 5 6
