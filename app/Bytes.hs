module Bytes where

import Numeric.NumType.DK.Integers
    ( TypeInt (..)
    )
import Numeric.Units.Dimensional.Prelude
import Numeric.Units.Dimensional.UnitNames (atom)
import Unsafe.Coerce (unsafeCoerce)

type DBytes =
    'Dim
        'Zero -- Length: m^(-2)
        'Zero -- Mass: kg^(-1)
        'Zero -- Time: s^3
        'Zero -- Electric current
        'Zero -- Thermodynamic temperature
        'Pos1 -- Amount of substance
        'Zero -- Luminous intensity

type Bytes = Quantity DBytes

byte :: (Num a) => Unit 'Metric DBytes a
byte = mkUnitZ (unsafeCoerce $ atom "B" "B" "Bytes") 1 mole

type DTransferSpeed =
    'Dim
        'Zero -- Length: m^(-2)
        'Zero -- Mass: kg^(-1)
        'Neg1 -- Time: s^3
        'Zero -- Electric current
        'Zero -- Thermodynamic temperature
        'Pos1 -- Amount of substance
        'Zero -- Luminous intensity

type TransferSpeed = Quantity DTransferSpeed
