{-# LANGUAGE RecordWildCards #-}

module Day21 where

{- Day 21: RPG Simulator 20XX -}

data Player = Player { hp :: Int, dmg :: Int, armor :: Int, price :: Int} deriving (Eq)
data Weapon = Weapon { costw :: Int, dmgw :: Int } deriving (Eq)
data Armor = Armor { costa :: Int, armora :: Int } deriving (Eq)
data Ring = Ring { costr :: Int, dmgr :: Int, armorr :: Int } deriving (Eq)
data Shop = Shop { weapons :: [Weapon], armors :: [Armor], rings :: [Ring] } deriving (Eq)

damage :: Player -> Int -> Player
damage Player{..} attack = Player (hp - max 1 (attack - armor)) dmg armor price

shop :: Shop
shop = Shop weapons armors rings
  where weapons = [ Weapon 8 4
                  , Weapon 10 5
                  , Weapon 25 6
                  , Weapon 40 7
                  , Weapon 74 8
                  ]
        armors  = [ Armor 0 0
                  , Armor 13 1
                  , Armor 31 2
                  , Armor 53 3
                  , Armor 75 4
                  , Armor 102 5
                  ]
        rings   = [ Ring 0 0 0
                  , Ring 0 0 0
                  , Ring 25 1 0
                  , Ring 50 2 0
                  , Ring 100 3 0
                  , Ring 20 0 1
                  , Ring 40 0 2
                  , Ring 80 0 3
                  ]

boss :: Player
boss = Player 100 8 2 0

combinations :: [Player]
combinations = [ Player { hp = 100
                        , dmg = dmgw w + dmgr r1 + dmgr r2
                        , armor = armora a + armorr r1 + armorr r2
                        , price = costw w + costa a + costr r1 + costr r2
                        }
               | w  <- weapons shop
               , a  <- armors shop
               , r1 <- rings shop
               , r2 <- rings shop
               , costr r1 == 0 && costr r1 == 0 || r1 /= r2
               ]

simulate :: Player -> Player -> Bool -> Bool
simulate p _ _ | hp p <= 0 = False
simulate _ b _ | hp b <= 0 = True
simulate p b True          = simulate p (damage b (dmg p)) False
simulate p b False         = simulate (damage p (dmg b)) b True

day21 :: IO ()
day21 = print $ foldl f (maxBound :: Int) combinations
  where f mini c = if price c < mini && simulate c boss True
                   then price c
                   else mini

{- Part Two -}

day21' :: IO ()
day21' = print $ foldl f (minBound :: Int) combinations
  where f maxi c = if price c > maxi && not (simulate c boss True)
                   then price c
                   else maxi
