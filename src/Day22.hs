module Day22 where
import Data.List (intersect, sortBy, sortOn, nub, (\\))
import Control.Monad (guard)



data GameState = GameState {
  bossHp :: Int,
  bossDmg :: Int,
  playerHp :: Int,
  playerMana :: Int,
  currentEffects :: [Effect],
  manaSpent :: Int,
  gameOver :: Bool,
  playerWon :: Bool
} deriving (Show, Eq)

data EffectType = ShieldEffect | PoisonEffect | RechargeEffect
  deriving (Show, Eq)
data Effect = Effect {effectType :: EffectType, effectDur :: Int}
  deriving (Show, Eq)
data SpellType = Missile | Drain | Shield | Poison | Recharge
  deriving (Show, Eq)
data Spell = Spell {spellType :: SpellType, manaCost :: Int}
  deriving (Show, Eq)

day22 :: IO ()
day22 = do
  print $ allStates 
  -- print $ "Part 1: " ++ show p1
  -- print $ "Part 2: " ++ show p2

allStates :: [GameState]
allStates = games'
  where
  games = (iterate (\x -> x >>= execTurn) [startState]) !! 20
  games' = ((filter (playerWon)) . (sortOn manaSpent)) games

spellList :: [Spell]
spellList = [Spell Missile 53, Spell Drain 73, Spell Shield 113,
             Spell Poison 173, Spell Recharge 229]

startState :: GameState
startState = GameState {bossHp = 51, bossDmg = 9, playerHp = 50, 
  playerMana = 500, currentEffects = [], manaSpent = 0, 
  gameOver = False, playerWon = False}

possSpells :: GameState -> [Spell]
possSpells g = hasMana `intersect` notActive
  where
  hasMana = filter (\x -> playerMana g >= manaCost x) spellList
  notActive = spellList \\ active
  active = do
    eff <- currentEffects g
    guard (effectDur eff > 1)
    pure $ case effectType eff of
      ShieldEffect -> Spell Shield 113
      PoisonEffect -> Spell Poison 173
      RechargeEffect -> Spell Recharge 229

applyPoison :: GameState -> GameState
applyPoison g = if PoisonEffect `elem` (effectType <$> currentEffects g)
  then g {bossHp = bossHp g - 3}
  else g

applyRecharge :: GameState -> GameState
applyRecharge g = if RechargeEffect `elem` (effectType <$> currentEffects g)
  then g {playerMana = playerMana g + 101}
  else g

effectTic :: GameState -> GameState
effectTic g = g {currentEffects = filter ((>0) . effectDur) effects'}
  where
  effects = currentEffects g
  effects' = map (\(Effect t d) -> Effect t (d-1)) effects

castSpell :: GameState -> Spell -> GameState
castSpell g sp =
  case sp of
    Spell Missile _ -> g' {bossHp = bossHp g' - 4}
    Spell Drain _ -> g' {bossHp = bossHp g' - 2, playerHp = playerHp g' + 2}
    Spell Shield _ -> g' {currentEffects =
      Effect ShieldEffect 6 : currentEffects g'}
    Spell Poison _ -> g' {currentEffects =
      Effect PoisonEffect 6 : currentEffects g'}
    Spell Recharge _ -> g' {currentEffects =
      Effect RechargeEffect 5 : currentEffects g'}
    where
      g' = g {playerMana = playerMana g - manaCost sp, 
              manaSpent = manaSpent g + manaCost sp}

playerTurn :: GameState -> Spell -> GameState
playerTurn g s = checkWinner g'
  where 
  g' = (effectTic . applyPoison . applyRecharge . \x -> castSpell x s) g

bossTurn :: GameState -> GameState
bossTurn g = (checkWinner . bossTurn' . effectTic . applyPoison . applyRecharge) g
  where
  bossTurn' g' = if ShieldEffect `elem` (effectType <$> currentEffects g')
              then g {playerHp = playerHp g' - (bossDmg g' - 7)}
              else g {playerHp = playerHp g' - bossDmg g'}

checkWinner :: GameState -> GameState
checkWinner g
  | gameOver g = g
  | playerHp g <= 0 = g {gameOver = True, playerWon = False}
  | bossHp g <= 0 = g {gameOver = True, playerWon = True}
  | otherwise = g

execTurn :: GameState -> [GameState]
execTurn g 
  | gameOver g = [g]
  | otherwise = do
      s <- possSpells g
      nub $ pure $ (bossTurn . \x -> playerTurn x s) g