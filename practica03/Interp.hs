module Interp where

import Grammars

freeVars :: ASA -> [String]
freeVars (Id x) = [x]
freeVars (Num _) = []
freeVars (Boolean _) = []
freeVars (And xs) = foldr union [] (map freeVars xs)
freeVars (Or xs) = foldr union [] (map freeVars xs)
freeVars (Add xs) = foldr union [] (map freeVars xs)
freeVars (Sub xs) = foldr union [] (map freeVars xs)
freeVars (Mul xs) = foldr union [] (map freeVars xs)
freeVars (Div xs) = foldr union [] (map freeVars xs)
freeVars (Lt xs) = foldr union [] (map freeVars xs)
freeVars (Gt xs) = foldr union [] (map freeVars xs)
freeVars (Le xs) = foldr union [] (map freeVars xs)
freeVars (Ge xs) = foldr union [] (map freeVars xs)
freeVars (Expt e1 e2) = freeVars e1 `union` freeVars e2
freeVars (EqP e1 e2) = freeVars e1 `union` freeVars e2
freeVars (Not e) = freeVars e
freeVars (Add1 e) = freeVars e
freeVars (Sub1 e) = freeVars e
freeVars (ZeroP e) = freeVars e
freeVars (Let bindings body) =
  let bindVars = map fst bindings
      exprsFV = foldr union [] (map (freeVars . snd) bindings)
      bodyFV = freeVars body \\ bindVars
  in exprsFV `union` bodyFV
freeVars (LetStar bindings body) = freeVarsLetStar bindings body
  where
    freeVarsLetStar [] b = freeVars b
    freeVarsLetStar ((x, e):bs) b =
      freeVars e `union` (freeVarsLetStar bs b \\ [x])

names :: ASA -> [String]
names (Id x) = [x]
names (Num _) = []
names (Boolean _) = []
names (And xs) = foldr union [] (map names xs)
names (Or xs) = foldr union [] (map names xs)
names (Add xs) = foldr union [] (map names xs)
names (Sub xs) = foldr union [] (map names xs)
names (Mul xs) = foldr union [] (map names xs)
names (Div xs) = foldr union [] (map names xs)
names (Lt xs) = foldr union [] (map names xs)
names (Gt xs) = foldr union [] (map names xs)
names (Le xs) = foldr union [] (map names xs)
names (Ge xs) = foldr union [] (map names xs)
names (Expt e1 e2) = names e1 `union` names e2
names (EqP e1 e2) = names e1 `union` names e2
names (Not e) = names e
names (Add1 e) = names e
names (Sub1 e) = names e
names (ZeroP e) = names e
names (Let bs body) = map fst bs `union` foldr union [] (map (names . snd) bs) `union` names body
names (LetStar bs body) = map fst bs `union` foldr union [] (map (names . snd) bs) `union` names body

freshName :: [String] -> String
freshName used = head [ name | i <- [1..], let name = "x_" ++ show i, name `notElem` used ]

-- Sustitución simple evitando captura: sust e x s  ->  e[x := s]
sust :: ASA -> String -> ASA -> ASA
sust expr x s
  | x `notElem` freeVars expr = expr
  | otherwise = case expr of
      Id y -> if x == y then s else Id y
      Num n -> Num n
      Boolean b -> Boolean b
      Add xs -> Add (map (\e -> sust e x s) xs)
      Sub xs -> Sub (map (\e -> sust e x s) xs)
      Mul xs -> Mul (map (\e -> sust e x s) xs)
      Div xs -> Div (map (\e -> sust e x s) xs)
      And xs -> And (map (\e -> sust e x s) xs)
      Or xs -> Or (map (\e -> sust e x s) xs)
      Lt xs -> Lt (map (\e -> sust e x s) xs)
      Gt xs -> Gt (map (\e -> sust e x s) xs)
      Le xs -> Le (map (\e -> sust e x s) xs)
      Ge xs -> Ge (map (\e -> sust e x s) xs)
      Expt e1 e2 -> Expt (sust e1 x s) (sust e2 x s)
      EqP e1 e2 -> EqP (sust e1 x s) (sust e2 x s)
      Not e -> Not (sust e x s)
      Add1 e -> Add1 (sust e x s)
      Sub1 e -> Sub1 (sust e x s)
      ZeroP e -> ZeroP (sust e x s)
      Let bs body ->
        let bindVars = map fst bs
            bs' = map (\(v, e) -> (v, sust e x s)) bs
        in if x `elem` bindVars
           then Let bs' body
           else if any (`elem` freeVars s) bindVars
                then
                  let used = freeVars body `union` freeVars s `union` bindVars `union` [x]
                      renamings = map (\v -> (v, freshName used)) bindVars
                      body' = foldl (\acc (v, v') -> sust acc v (Id v')) body renamings
                      bs'' = zip (map snd renamings) (map snd bs')
                  in Let bs'' (sust body' x s)
                else Let bs' (sust body x s)
      LetStar [] body -> LetStar [] (sust body x s)
      LetStar ((y, e):bs) body ->
        let e' = sust e x s
        in if x == y
           then LetStar ((y, e'):bs) body
           else if y `elem` freeVars s
                then
                  let used = freeVars (LetStar bs body) `union` freeVars s `union` [x, y]
                      y' = freshName used
                      rest' = sust (LetStar bs body) y (Id y')
                  in case sust rest' x s of
                       LetStar bs' body' -> LetStar ((y', e'):bs') body'
                       _ -> error "Error en renombrado de let*"
                else
                  case sust (LetStar bs body) x s of
                    LetStar bs' body' -> LetStar ((y, e'):bs') body'
                    _ -> error "Error en sustitución de let*"

-- Sustitución simultánea (para let)
sustMany :: ASA -> [Binding] -> ASA
sustMany expr env = case expr of
  Id x -> case lookup x env of
            Just val -> val
            Nothing  -> Id x
  Num n -> Num n
  Boolean b -> Boolean b
  Add xs -> Add (map (`sustMany` env) xs)
  Sub xs -> Sub (map (`sustMany` env) xs)
  Mul xs -> Mul (map (`sustMany` env) xs)
  Div xs -> Div (map (`sustMany` env) xs)
  And xs -> And (map (`sustMany` env) xs)
  Or xs -> Or (map (`sustMany` env) xs)
  Lt xs -> Lt (map (`sustMany` env) xs)
  Gt xs -> Gt (map (`sustMany` env) xs)
  Le xs -> Le (map (`sustMany` env) xs)
  Ge xs -> Ge (map (`sustMany` env) xs)
  Expt e1 e2 -> Expt (sustMany e1 env) (sustMany e2 env)
  EqP e1 e2 -> EqP (sustMany e1 env) (sustMany e2 env)
  Not e -> Not (sustMany e env)
  Add1 e -> Add1 (sustMany e env)
  Sub1 e -> Sub1 (sustMany e env)
  ZeroP e -> ZeroP (sustMany e env)
  Let bs body ->
    let bindVars = map fst bs
        bs' = map (\(v, e) -> (v, sustMany e env)) bs
        env' = filter (\(v, _) -> v `notElem` bindVars) env
    in Let bs' (sustMany body env')
  LetStar bs body ->
    let (bs', env') = foldl processBinding ([], env) bs
        processBinding (accBs, currentEnv) (v, e) =
          let e' = sustMany e currentEnv
              nextEnv = filter (\(x, _) -> x /= v) currentEnv
          in (accBs ++ [(v, e')], nextEnv)
    in LetStar bs' (sustMany body env')
    
    
    
-- RETO 4: semantica operacional de paso grande
-- let es simultaneo; let* se evalua directamente, asociacion por asociacion.
bigStep :: ASA -> Maybe ASA
bigStep (Id _) = Nothing
bigStep (Num n) = Just (Num n)
bigStep (Boolean b) = Just (Boolean b)

bigStep (And xs) = case mapM evalBool xs of
  Nothing -> Nothing
  Just v -> Just (Boolean (and v))

bigStep (Or xs) = case mapM evalBool xs of
  Nothing -> Nothing
  Just v -> Just (Boolean (or v))

bigStep (Add xs) = case mapM evalNum xs of
  Nothing -> Nothing
  Just v -> Just (Num (sum v))

bigStep (Sub []) = Nothing
bigStep (Sub (x:xs)) = case evalNum x of
  Nothing -> Nothing
  Just v -> case mapM evalNum xs of
    Nothing -> Nothing
    Just w -> Just (Num (foldl monus v w))

bigStep (Mul xs) = case mapM evalNum xs of
  Nothing -> Nothing
  Just v -> Just (Num (product v))

bigStep (Div []) = Nothing
bigStep (Div (x:xs)) = case evalNum x of
  Nothing -> Nothing
  Just v -> case mapM evalNum xs of
    Nothing -> Nothing
    Just w -> if 0 `elem` w 
                then Nothing
                else Just (Num (foldl div v w))

bigStep (Lt xs) = case mapM evalNum xs of
  Nothing -> Nothing
  Just values -> Just (Boolean (compara (<) values))

bigStep (Gt xs) = case mapM evalNum xs of
  Nothing -> Nothing
  Just values -> Just (Boolean (compara (>) values))

bigStep (Le xs) = case mapM evalNum xs of
  Nothing -> Nothing
  Just values -> Just (Boolean (compara (<=) values))

bigStep (Ge xs) = case mapM evalNum xs of
  Nothing -> Nothing
  Just values -> Just (Boolean (compara (>=) values))

bigStep (Expt e1 e2) =
  case evalNum e1 of
    Nothing -> Nothing
    Just x -> case evalNum e2 of 
      Nothing -> Nothing
      Just y -> if y < 0
                  then Nothing
                  else Just (Num (x ^ y))

bigStep (EqP e1 e2) = 
  case (bigStep e1, bigStep e2) of 
    (Just (Num x), Just (Num y)) -> Just (Boolean (x == y))
    (Just (Boolean a), Just (Boolean b)) -> Just (Boolean (a == b))

    _ -> Nothing

bigStep (Not e) =
  case bigStep e of 
    Just (Boolean b) -> Just (Boolean (not b))
    Just (Num _) -> Just (Boolean (False))
    Nothing -> Nothing

bigStep (Add1 e) = 
  case evalNum e of
    Nothing -> Nothing
    Just n -> Just (Num (n + 1))

bigStep (Sub1 e) = 
  case evalNum e of
    Nothing -> Nothing
    Just n -> Just (Num (monus n 1))

bigStep (ZeroP e) = 
  case evalNum e of 
    Nothing -> Nothing
    Just n -> Just (Boolean (n == 0))

bigStep (Let bindings body) =
  case mapM evalBinding bindings of
    Nothing -> Nothing
    Just values ->
      let variables = map fst bindings
          environment = zip variables values
          body' = sustMany body environment
      in bigStep body'

bigStep (LetStar [] body) =
  bigStep body

bigStep (LetStar ((x, expression) : bindings) body) =
  case bigStep expression of
    Nothing -> Nothing
    Just value ->
      case sust (LetStar bindings body) x value of
        LetStar bindings' body' ->
          bigStep (LetStar bindings' body')
        _ -> Nothing


evalNum :: ASA -> Maybe Int
evalNum e = case bigStep e of
  Just (Num n) -> Just n
  _ -> Nothing

evalBool :: ASA -> Maybe Bool
evalBool e = case bigStep e of
  Just (Boolean n) -> Just n
  _ -> Nothing

evalBinding :: Binding -> Maybe ASA
evalBinding (_, expression) =
  bigStep expression

monus :: Int -> Int -> Int
monus n m = if n < m 
            then 0
            else n - m

compara :: (Int -> Int -> Bool) -> [Int] -> Bool
compara _ [] = True
compara _ [_] = True
compara op (x:y:resto) = if op x y 
                            then compara op (y:resto)
                            else False

                  
