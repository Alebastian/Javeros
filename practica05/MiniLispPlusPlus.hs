module MiniLispPlusPlus where

import Control.Monad.IO.Class (liftIO)
import Grammars
import Interp
import Lexer
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, runInputT)
import Text.Printf (vFmt)

-- RETO 5: integrar el combinador Y ----------------------------------------

-- Representa en el ASA del nucleo el combinador clasico:
--
-- Y = lambda f.
--       (lambda x. f (x x))
--       (lambda x. f (x x))
combinadorY :: ASA
combinadorY =  Fun "f"  (App (Fun "x" (App (Id "f") (App (Id "x") (Id "x")))) (Fun "x" (App (Id "f") (App (Id "x") (Id "x")))))
--             Fun( f, App(Fun (x, App(f, App(x, x))) , Fun (x, App(f, App(x, x))))
--             \f. (\x. f (x x)) (\x. f (x x))

-- Evalua combinadorY en el ambiente vacio y asocia su valor con el nombre Y.
prelude :: Env
prelude = [("Y", valY)]
  where 
    desempaqueta :: Maybe a -> a
    desempaqueta (Just x) = x
    desempaqueta Nothing = error "¿Qué pasó, Master? Error: combinador Y no se puede evaluar. Algo explotó o está muy mal! \n (Nota: CombinadorY está en prelude y no debería fallar nunca)"

    valY = desempaqueta (bigStep [] combinadorY)


-- Integra el analisis, el desazucarado y la evaluacion desde prelude.
-- El resultado final debe pasar por strict antes de devolverse.
evalua :: String -> Maybe Value
evalua s = analiza asaMaybe
  where
    sasa = parse $ lexer s
    asaMaybe = desugar sasa

    embona :: Maybe Value -> Maybe Value
    embona (Just v) = strict v
    embona Nothing = Nothing

    analiza :: Maybe ASA -> Maybe Value
    analiza Nothing = Nothing
    analiza (Just asa) = embona $ bigStep prelude asa




-- Infraestructura provista. No forma parte de los retos.
repl :: IO ()
repl = runInputT defaultSettings loop

loop :: InputT IO ()
loop =
  getInputLine "MiniLisp++> " >>= maybe (pure ()) procesaEntrada

procesaEntrada :: String -> InputT IO ()
procesaEntrada ":q" = pure ()
procesaEntrada entrada =
  liftIO (maybe muestraBloqueo print (evalua entrada)) >> loop

muestraBloqueo :: IO ()
muestraBloqueo =
  putStrLn "Error: evaluacion bloqueada"

main :: IO ()
main = repl
