-- Based on http://www.evanmiller.org/how-not-to-sort-by-average-rating.html
module Ranking (ci_lower_bound) where


-- Wrong Solution 1
-- lower ratios can have higher scores
-- eg: (600,400) < (5,500, 4,500) despite the first being 60% pos v 55%
bad_1 :: (Int, Int) -> Int
bad_1 (positive, negative) = positive - negative

-- Wrong Solution 2
-- eg: 2 pos 0 neg vs 100 pos 1 neg, first comes out as better
bad_2 :: Int -> Int -> Float
bad_2 positive total = (fromIntegral positive) / (fromIntegral total)


-- Lower bound of Wilson score confidence interval
-- "Given the ratings I have, there is a 95% chance that the real
-- fraction of positive ratings is at least what?

-- alt: instead of this func, use 1.96 for a confidence level of 0.95
norm :: Float -> Float
norm x = exp(-x**2/2)/sqrt(2*pi)

ci_lower_bound :: Int -> Int -> Float -> Float
ci_lower_bound _ 0 0 = 0.0
ci_lower_bound positive total confidence = result
  where n = fromIntegral total
        z = norm(1.0-(1.0-confidence)/2.0)
        phat = 1.0*(fromIntegral positive) / n
        dividor = 1 + (z*z)/n
        result = (phat + (z*z / (2*n)) - z * sqrt((phat*(1-phat)+(z*z/(4*n)))/n)) / dividor

cilb = ci_lower_bound


-- Exponential decay
exp_decay :: Int -> Int -> Int -> Float -> Float
exp_decay positive total ticks scale = result
  where score = ci_lower_bound positive total 0.95
        mod = 1 + min 0.0 (- log (fromIntegral ticks * scale))
        result = score * mod


