-- Oppgave 2

-- 2.1
data BT a = Empty | Branch a (BT a) (BT a)
  deriving (Show, Eq)

-- 2.2
mir :: (BT a) -> (BT a)
mir Empty = Empty
mir (Branch a left right) = Branch a (mir right) (mir left)

-- 2.3
sym :: (BT a) -> Bool
sym tree = mir tree == tree