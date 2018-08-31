module StdTypes(Int8,
    Int16,
    Int32,
    Uint8,
    Uint16,
    Nat,
    Uint32,
    Bool(..),
    Ieee_real,
    Int64,
    Uint64) where
import Prelude (Maybe(..),return)
import qualified Prelude
import qualified SexpPkl
import qualified StdPkl
import qualified StdPrims

type Int8 = (StdPrims.Int)
type Int16 = (StdPrims.Int)
type Int32 = (StdPrims.Int)
type Uint8 = (StdPrims.Int)
type Uint16 = (StdPrims.Int)
type Nat = (StdPrims.Int)
type Uint32 = (StdPrims.Int)
data Bool =
    TRUE
  | FALSE
  deriving (Prelude.Eq, Prelude.Ord, Prelude.Show)
type Ieee_real = (StdPrims.String)
type Int64 = (StdPrims.Int)
type Uint64 = (StdPrims.Int)


