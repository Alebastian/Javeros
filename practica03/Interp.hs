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
