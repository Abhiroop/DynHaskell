module CoinChangeAlt where

import Data.Array

type Coin = Integer
type Cents = Integer

defaultCoins :: [Coin]
defaultCoins = [100, 50, 25, 10, 5, 2, 1]

takeCoinDP :: [Coin] -> Cents -> Maybe (Integer, [Coin])
takeCoinDP coins cents = get ltCoin cents
    where arr = array ((0,0), (ltCoin, cents)) [((i,c), takeC i c) | i <- [0..ltCoin], c <- [0..cents]]
          get i c
            | c < 0 || i < 0  = Nothing
            | c == 0          = Just (0, [])
            | otherwise       = arr!(i,c)
          ltCoin = length coins - 1
          takeC cNr cts
            | coin > cts          = get (cNr-1) cts
            | otherwise           = case (get cNr (cts-coin), get (cNr-1) cts) of
                                       (Just (n, t), Just (n',t')) -> Just $ if n+1 <= n' then (n+1, coin:t) else (n', t')
                                       (Nothing,     Just (n',t')) -> Just (n', t')
                                       (Just (n,t),  Nothing)      -> Just (n+1, coin:t)
                                       (Nothing,     Nothing)      -> Nothing
            where coin = coins !! cNr
