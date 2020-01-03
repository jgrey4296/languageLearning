
--average :: Fractional a => [a] -> a
average arr = total / len
  where total = realToFrac (foldl (\m v -> m+v) 0 arr)
        len = realToFrac $ length arr
      
variance arr = sqrt $ average indVariances
  where arrAverage = average arr
        indVariances = map (\x -> (x-arrAverage)^2) arr

