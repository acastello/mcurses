{-# LANGUAGE TemplateHaskell #-}

module UI.MCurses.Internal.TH where

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

formatCall :: Exp -> Q Exp
formatCall e = duped
    where
    args e' = case e' of 
        AppE (VarE funNam) rhs -> arrow (LitE $ StringL $ nameBase funNam)
                                                 rhs
        AppE lhs rhs -> arrow (args lhs) rhs
        VarE funNam -> LitE $ StringL $ nameBase funNam
        _ -> e' 
    duped = do
        varnam <- newName "ret"
        let lhfun = LamE [VarP varnam]
                        (TupE [equ (VarE varnam), VarE varnam])
        return $ app2 fmape lhfun e
    equ rc = app2 cat (app2 cat (args e) equals) (showe rc)
    arrow lhe rhe = InfixE (Just lhe) 
                           cat 
                           (Just $ InfixE (Just space) cat (Just $ showe rhe)) 
    cat = VarE (mkName "++")
    space = LitE (StringL " ")
    equals = LitE (StringL " = ")
    fmape = VarE (mkName "fmap")
    showe = AppE (VarE (mkName "show") )
    app2 op rhs lhs = AppE (AppE op rhs) lhs

padFormatCall :: [String] -> String
padFormatCall [] = ""
padFormatCall (x:xs) = lhs ++ rhs xs
    where
    lhs | length x <= 14 = x ++ replicate (14 - length x + 2) ' '
        | otherwise = pad 4 1 x
    rhs ["=",r] = " = " ++ r
    rhs [y,"=",r] = pad 4 1 y ++ rhs ["=",r]
    rhs (y:ys) = pad 4 2 y ++ rhs ys
    rhs [] = []
    pad n m str = let l = ((length str + m - 1) `quot` n + 1) * n 
              in (str ++ replicate (l - length str) ' ')


qprint :: Exp -> Q Exp
qprint e = do
    let str = show e 
    return $ AppE (VarE $ mkName "putStrLn") (LitE $ StringL str)

rprint :: Exp -> Q Exp
rprint e = lift . fmap show =<< reif e

reif :: Exp -> Q [Info]
reif (VarE n) = pure <$> reify n
reif (ConE n) = pure <$> reify n
reif (LitE _) = return []
reif (AppE l r) = liftM2 (++) (reif l) (reif r)
reif (AppTypeE l _) = reif l
reif _ = return []
