-- | Have you ever wondered why, when using a microwave, pressing \"60\" is the same as pressing \"100\"? Well wonder no longer! I, through epic trials and hardships, have tamed the vexacious wiles of that most enigmatic appliance.
--
--   @
--     λ> let n = 200 :: Microwave
--     λ> n
--        2:00
--     λ> n > 150
--        True
--     λ> n > 170
--        False
--     λ> n \`quotRem\` 7
--        (0:17,0:01)
--   @
module Data.Microwave (Microwave) where

newtype Microwave = Microwave {fromMicrowave :: Integer}

instance Show Microwave where
  show (Microwave n) = let (mins, secs) = n `quotRem` 100
                           secs' = case (show secs) of
                                     [c] -> '0':[c]
                                     s -> s
                       in show mins ++ ":" ++ secs'

instance Read Microwave where
  readsPrec n s = let s' = filter (/= ':') s in map (\(n, r) -> (fixSecs n, r)) $ readsPrec n s'

fixSecs n = let (mins, secs) = n `quotRem` 100
                (carry, secs') = secs `quotRem` 60
            in Microwave (100 * (mins + carry) + secs')

-- turn display time into raw seconds
normalize (Microwave n) = let (mins, secs) = n `quotRem` 100 in 60 * mins + secs

-- turn raw seconds into display time
denormalize n = let (mins, secs) = n `quotRem` 60 in Microwave (100 * mins + secs)

instance Eq Microwave where
  n == m = normalize n == normalize m

instance Ord Microwave where
  n `compare` m = normalize n `compare` normalize m

instance Enum Microwave where
  fromEnum = fromInteger . normalize
  toEnum = denormalize . toInteger

instance Num Microwave where
  fromInteger = fixSecs
  n + m = Microwave (normalize n + normalize m)
  n * m = Microwave (normalize n * normalize m)
  abs = Microwave . abs . fromMicrowave
  signum = Microwave . signum . fromMicrowave

instance Real Microwave where
  toRational = toRational . normalize

instance Integral Microwave where
  toInteger = normalize
  quotRem n m = let (q, r) = quotRem (normalize n) (normalize m) in (denormalize q, denormalize r)

