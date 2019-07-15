{-# LANGUAGE PatternSynonyms #-}
module Options
    ( pattern OptArg
    , pattern ReqArg
    , pattern NoArg
    , pattern Option
    , OptDescr
    , usageInfo
    , getOptions
    ) where

import qualified System.Console.GetOpt as Opt
import System.Console.GetOpt (ArgDescr(NoArg), usageInfo)
import System.Environment (getArgs)

pattern Option s l dsc arg = Opt.Option s l arg dsc
pattern OptArg s f = Opt.OptArg f s
pattern ReqArg s f = Opt.ReqArg f s

type OptDescr e a = Opt.OptDescr (a -> Either e a)

getOptions :: [OptDescr e a] -> a -> IO (Either e a, [String], [String])
getOptions opts d = do
    args <- getArgs
    let (res, others, errs) = Opt.getOpt Opt.Permute opts args
    let chainl = foldl (\a b x -> a x >>= b) return
    return (chainl res d, others, errs)
