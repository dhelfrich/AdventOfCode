{-# LANGUAGE OverloadedStrings #-}
module Day21 where
import Text.Megaparsec
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec.Char (alphaNumChar, space, char, string, eol, numberChar)
import Control.Monad (void)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Maybe (maybeToList)
import Debug.Trace (traceShow)
import Data.List (sortBy, sortOn)

data Fighter = Fighter {
  hp :: Int,
  damage :: Int,
  armor :: Int,
  cost :: Int
} deriving (Show)

data Item = Item {
  name :: String,
  itemType :: ItemType,
  itemCost :: Int,
  attack :: Int,
  defense :: Int
} deriving (Show)

data ItemType = Weapon | Armor | Ring deriving (Show, Eq)
data Equipment = Equipment {
  eWeapon :: Item,
  eArmor :: Maybe Item,
  eRings :: [Item]
 } deriving (Show)
type Player = Fighter
type Boss = Fighter
type Parser = Parsec Void Text

day21 :: IO ()
day21 = do
  input <- readFile "./inputs/day21.txt"
  case parse pInput "" (pack input) of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right result -> do
      let (items, boss) = result
          p1 = cheapestWin items boss
          p2 = expensiveLoss items boss
      print $ boss
      print $ "Part 1: " ++ show p1
      print $ "Part 2: " ++ show p2

isWin :: Boss -> Player -> Bool
isWin b p 
  | hp b <= 0 = True
  | hp p <= 0 = False
  | otherwise = isWin b' p'
    where
    bossTaken = max 1 (damage p - armor b)
    playerTaken = max 1 (damage b - armor p)
    hpb' = hp b - bossTaken
    hpp' = hp p - playerTaken
    b' = Fighter {hp = hpb', damage = damage b, armor = armor b, cost = cost b}
    p' = Fighter {hp = hpp', damage = damage p, armor = armor p, cost = cost p}

expensiveLoss :: [Item] -> Boss -> Int
expensiveLoss items boss = maximum $ cost <$> losingPlayers
  where
  losingPlayers = filter (not . isWin boss) players
  players = sortOn cost (equip <$> equipments)
  weapons = filter ((== Weapon) . itemType) items
  armors = filter ((== Armor) . itemType) items
  rings = filter ((== Ring) . itemType) items
  nullItem = Item {itemCost = 0, attack = 0, defense = 0}
  equipments = do
    weap <- weapons
    arm <- nullItem : armors
    ring1 <- nullItem : rings
    ring2 <- nullItem : rings
    return Equipment {eWeapon = weap, eArmor = Just arm, eRings = [ring1, ring2]}

cheapestWin :: [Item] -> Boss -> Int
cheapestWin items boss = minimum $ cost <$> winningPlayers
  where
  winningPlayers = filter (isWin boss) players
  players = sortOn cost (equip <$> equipments)
  weapons = filter ((== Weapon) . itemType) items
  armors = filter ((== Armor) . itemType) items
  rings = filter ((== Ring) . itemType) items
  nullItem = Item {itemCost = 0, attack = 0, defense = 0}
  equipments = do
    weap <- weapons
    arm <- nullItem : armors
    ring1 <- nullItem : rings
    ring2 <- nullItem : rings
    return Equipment {eWeapon = weap, eArmor = Just arm, eRings = [ring1, ring2]}


equip :: Equipment -> Player
equip e = Fighter {hp = 100, damage = dmg, armor = amr, cost = cst}
  where
  dmg = sum $ attack <$> items
  amr = sum $ defense <$> items 
  cst = sum $ itemCost <$> items
  items = [eWeapon e] ++ maybeToList (eArmor e) ++ eRings e

pInput :: Parser ([Item], Boss)
pInput = do
  void (chunk "Weapons:") *> some (alphaNumChar <|> char ' ') *> space
  weaps <- some (pItem Weapon)
  void (chunk "\nArmor:") *> some (alphaNumChar <|> char ' ') *> space
  armors <- some (pItem Armor)
  void (chunk "\nRings:") *> some (alphaNumChar <|> char ' ') *> space
  rings <- some (pItem Ring) <* space
  hp' <- (chunk "Hit Points: ") *> L.decimal <* eol
  dmg' <- (chunk "Damage: ") *> L.decimal <* eol
  armor' <- (chunk "Armor: ") *> L.decimal <* eof
  return (concat [weaps, armors, rings],
    Fighter {hp = hp', damage = dmg', armor = armor', cost = 0})

pItem :: ItemType -> Parser Item
pItem t = do
  n <- some alphaNumChar
  void $ try (string " +" <* numberChar <* space) <|> string "  " <* space
  c <- L.decimal <* space
  a <- L.decimal <* space
  d <- L.decimal <* eol
  return Item {name = n, itemType = t, itemCost = c, attack = a, defense = d}





-- pBoss :: Parser Boss

