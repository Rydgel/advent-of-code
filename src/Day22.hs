{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE RecordWildCards #-}

module Day22 where

import Data.List
import Data.Ord (comparing)
import Data.Maybe

data Player = Player  { hp    :: Int
                      , dmg   :: Int
                      , armor :: Int
                      , mana  :: Int
                      } deriving (Eq)

data Spell = Spell { nameS        :: String
                   , cost         :: Int
                   , castCallback :: (Spell,Player,Player) -> (Player,Player)
                   , effect       :: Maybe Effect
                   }

instance Eq Spell where
  a == b = nameS a == nameS b

data Effect = Effect { nameE          :: String
                     , turns          :: Int
                     , effectCallback :: (Effect,Player,Player) -> (Player,Player)
                     }
instance Eq Effect where
  a == b = nameE a == nameE b

data Game = Game { player   :: Player
                 , boss     :: Player
                 , usedMana :: Int
                 , effects  :: [Effect]
                 } deriving (Eq)

initPlayer :: Player
initPlayer = Player 50 0 0 500

initBoss :: Player
initBoss = Player 55 8 0 0

damage :: Player -> Int -> Player
damage Player{..} attack = Player (hp - max 1 (attack - armor)) dmg armor mana

heal :: Player -> Int -> Player
heal Player{..} healz = Player (hp + healz) dmg armor mana

changeMana :: Player -> Int -> Player
changeMana Player{..} change = Player hp dmg armor (mana + change)

changeArmor :: Player -> Int -> Player
changeArmor Player{..} increase = Player hp dmg (armor + increase) mana

spells :: [Spell]
spells = [ Spell "Magic Missile" 53 (\(s,c,o) -> (changeMana c (negate (cost s)), damage o 4)) Nothing
         , Spell "Drain" 73 (\(s,c,o) -> (changeMana (heal c 2) (negate (cost s)), damage o 2)) Nothing
         , Spell "Shield" 113 (\(s,c,o) -> (changeMana (changeArmor c 7) (negate (cost s)), o)) $ Just $
             Effect "Shield Effect" 6 (\(e,c,o) -> (if turns e == 1 then changeArmor c 7 else c, o))
         , Spell "Poison" 173 (\(s,c,o) -> (changeMana c (negate (cost s)), o)) $ Just $
             Effect "Poison Effect" 6 (\(_,c,o) -> (c, damage o 3))
         , Spell "Recharge" 229 (\(s,c,o) -> (changeMana c (negate (cost s)), o)) $ Just $
             Effect "Recharge Effect" 5 (\(_,c,o) -> (changeMana c 101, o))
         ]

possible :: Spell -> Game -> Bool
possible Spell{..} Game{..} = cost <= mana player && maybe True checkExist effect
  where checkExist e = (>0) $ length $ filter (\r -> turns r > 1 && nameE r == nameE e) effects

cast :: Spell -> Player -> Player -> (Player,Player)
cast s@Spell{..} caster oppposition = castCallback (s,caster,oppposition)

doEffect :: Effect -> Player -> Player -> (Player,Player)
doEffect e@Effect{..} caster oppposition = effectCallback (e,caster,oppposition)

deplete :: Effect -> Effect
deplete Effect{..} = Effect nameE (turns-1) effectCallback

playerWon :: Game -> Bool
playerWon Game{..} = hp boss <= 0

bossWon :: Game -> Bool
bossWon Game{..} = hp player <= 0

runEffects :: Game -> (Player,Player)
runEffects Game{..} = foldl (\(pl,bo) e -> doEffect e pl bo) (player,boss) effects

depleteEffects :: Game -> [Effect]
depleteEffects Game{..} = filter ((> 0) . turns) $ fmap deplete effects

playerTurn :: Game -> Spell -> Bool -> Game
playerTurn g@Game{..} s h =
  let changedPlayer = if h then damage player 1 else player
      (effectedPlayer, effectedBoss) = runEffects g
      (castPlayer, castBoss) = cast s effectedPlayer effectedBoss
      game = if h && hp changedPlayer <= 0
             then Game changedPlayer boss usedMana effects
             else Game castPlayer castBoss (usedMana+cost s) $ foldl (\z x -> z ++ [x]) (depleteEffects g) (effect s)
  in  game

bossTurn :: Game -> Game
bossTurn g@Game{..} =
  let (effectedPlayer, effectedBoss) = runEffects g
  in Game (damage effectedPlayer (dmg boss)) effectedBoss usedMana (depleteEffects g)

simulate :: [Game] -> Int -> Bool -> Bool -> Int
simulate [] bestGame _ _              = bestGame
simulate games bestGame True hardMode =
  let gamePossibilities = nub $ games >>= (\g -> map (\x -> playerTurn g x hardMode) $ filter (`possible` g) spells)
      score = getBestScore bestGame gamePossibilities
  in  simulate (getGamesToCheck bestGame gamePossibilities) score False hardMode
simulate games bestGame False hardMode =
  let gamePossibilities = nub $ map bossTurn games
  in  simulate (getGamesToCheck bestGame gamePossibilities) (getBestScore bestGame gamePossibilities) True hardMode

getGamesToCheck :: Int -> [Game] -> [Game]
getGamesToCheck bestGame = filter (\g -> not (playerWon g) && not (bossWon g) && usedMana g < bestGame)

getBestScore :: Int -> [Game] -> Int
getBestScore bestGame = min bestGame
                      . maybe bestGame usedMana
                      . listToMaybe
                      . sortBy (comparing usedMana)
                      . filter playerWon

day22 :: IO ()
day22 = print $ simulate [Game initPlayer initBoss 0 []] (maxBound :: Int) True False
