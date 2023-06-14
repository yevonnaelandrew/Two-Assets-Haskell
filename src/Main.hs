{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Network.HTTP.Simple (httpBS, getResponseBody, parseRequest)
import qualified Data.Aeson as A
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import GHC.Generics
import Data.ByteString.Lazy as BL
import qualified Statistics.Sample as Stats
import Statistics.Distribution
import Data.Vector as V
import Data.List (maximumBy, elemIndex)
import Data.Ord (comparing)
import Data.Maybe (fromJust)
import Text.Printf (printf)

data CoinData = CoinData {
    prices :: [[Double]]
} deriving Show

instance A.FromJSON CoinData where
    parseJSON = A.withObject "CoinData" $ \v -> CoinData
        <$> v A..: "prices"

main = do
    putStrLn ""
    putStrLn "Portfolio Optimization with Two Assets"
    putStrLn "Yevonnael Andrew"
    putStrLn ""
    putStrLn "Enter the number of days (30, 60, or 90):"
    days <- getLine
    putStrLn ""
    putStrLn "Coin name follows Coingecko convention!"
    putStrLn ""
    putStrLn "Enter the first coin to fetch data for (bitcoin, ethereum, cardano, etc):"
    coin1 <- getLine
    putStrLn "Enter the second coin to fetch data for (bitcoin, ethereum, cardano, etc):"
    coin2 <- getLine
    let requestUrl1 = "https://api.coingecko.com/api/v3/coins/" Prelude.++ coin1 Prelude.++ "/market_chart?vs_currency=usd&days=" Prelude.++ days
    let requestUrl2 = "https://api.coingecko.com/api/v3/coins/" Prelude.++ coin2 Prelude.++ "/market_chart?vs_currency=usd&days=" Prelude.++ days
    request1 <- parseRequest requestUrl1
    request2 <- parseRequest requestUrl2
    response1 <- httpBS request1
    response2 <- httpBS request2
    let btcData1 = A.decode (BL.fromStrict $ getResponseBody response1) :: Maybe CoinData
    let btcData2 = A.decode (BL.fromStrict $ getResponseBody response2) :: Maybe CoinData
    case (btcData1, btcData2) of
        (Nothing, _) -> putStrLn $ "Failed to fetch data from CoinGecko API for " Prelude.++ coin1
        (_, Nothing) -> putStrLn $ "Failed to fetch data from CoinGecko API for " Prelude.++ coin2
        (Just coinData1, Just coinData2) -> do
            let priceData1 = prices coinData1
            let priceData2 = prices coinData2
            let pricesVector1 = V.fromList [price | [_, price] <- priceData1]
            let pricesVector2 = V.fromList [price | [_, price] <- priceData2]

            -- Convert 5-minute returns to daily returns
            let dailyReturns1 = V.map (\i -> (pricesVector1 V.! (i * 288) - pricesVector1 V.! ((i - 1) * 288)) / pricesVector1 V.! ((i - 1) * 288)) (V.fromList [1..(V.length pricesVector1 `div` 288)])
            let dailyReturns2 = V.map (\i -> (pricesVector2 V.! (i * 288) - pricesVector2 V.! ((i - 1) * 288)) / pricesVector2 V.! ((i - 1) * 288)) (V.fromList [1..(V.length pricesVector2 `div` 288)])


            -- Calculate mean returns and standard deviations
            let meanReturn1 = Stats.mean dailyReturns1
            let meanReturn2 = Stats.mean dailyReturns2
            let stdDev1 = Stats.stdDev dailyReturns1
            let stdDev2 = Stats.stdDev dailyReturns2

            putStrLn ""
            putStrLn $ "Mean Return for Asset 1: " Prelude.++ printf "%.4f" meanReturn1
            putStrLn $ "Standard Deviation of Return for Asset 1: " Prelude.++ printf "%.4f" stdDev1

            putStrLn $ "Mean Return for Asset 2: " Prelude.++ printf "%.4f" meanReturn2
            putStrLn $ "Standard Deviation of Return for Asset 2: " Prelude.++ printf "%.4f" stdDev2

            -- Calculate portfolio return and risk for each allocation
            let allocations = [(w1, 1 - w1) | w1 <- [0, 0.01 .. 1]]
            let portfolioReturns = [w1 * meanReturn1 + w2 * meanReturn2 | (w1, w2) <- allocations]
            let portfolioRisks = [sqrt (w1^2 * stdDev1^2 + w2^2 * stdDev2^2) | (w1, w2) <- allocations]  -- This line assumes no correlation between the returns

            -- Calculate Sharpe Ratios and find allocation with highest Sharpe Ratio
            let sharpeRatios = Prelude.zipWith (/) portfolioReturns portfolioRisks
            let (optimalAllocation, maxSharpeRatio) = Data.List.maximumBy (comparing snd) (Prelude.zip allocations sharpeRatios)

            -- Find the mean return and standard deviation of the optimized portfolio
            let idx = fromJust $ Data.List.elemIndex optimalAllocation allocations
            let optimizedPortfolioReturn = portfolioReturns !! idx
            let optimizedPortfolioRisk = portfolioRisks !! idx

            putStrLn ""
            putStrLn $ "Optimal Allocation: " Prelude.++ show optimalAllocation
            putStrLn $ "Mean Return of Optimized Portfolio: " Prelude.++ printf "%.4f" optimizedPortfolioReturn
            putStrLn $ "Standard Deviation of Optimized Portfolio: " Prelude.++ printf "%.4f" optimizedPortfolioRisk
            putStrLn $ "Maximum Sharpe Ratio: " Prelude.++ printf "%.4f" maxSharpeRatio
            putStrLn ""
            let (optimalAllocation1, optimalAllocation2) = optimalAllocation
            putStrLn $ "In summary, Asset 1 has an average daily return of " Prelude.++ printf "%.4f" meanReturn1 Prelude.++ " with a volatility of " Prelude.++ printf "%.4f" stdDev1 Prelude.++ ", while Asset 2 has an average daily return of " Prelude.++ printf "%.4f" meanReturn2 Prelude.++ " with a volatility of " Prelude.++ printf "%.4f" stdDev2 Prelude.++ ". The optimal portfolio allocation for highest risk-adjusted return is " Prelude.++ printf "%.2f" (optimalAllocation1 * 100) Prelude.++ "% in Asset 1 and " Prelude.++ printf "%.2f" (optimalAllocation2 * 100) Prelude.++ "% in Asset 2, yielding a return of " Prelude.++ printf "%.4f" (Prelude.maximum portfolioReturns) Prelude.++ " with a risk level (standard deviation) of " Prelude.++ printf "%.4f" (Prelude.minimum portfolioRisks) Prelude.++ ". This portfolio achieves a maximum Sharpe Ratio of " Prelude.++ printf "%.4f" maxSharpeRatio Prelude.++ "."
