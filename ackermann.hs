ack :: Int -> Int -> Int 
ack 0 y = succ y
ack x 0 = ack (x - 1) 1
ack x y = ack (x-1) (ack x (y-1))

ackermann :: Int -> Int -> Int
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann 1 n = n + 2
ackermann 2 n = 2 * n + 3
ackermann 3 n = 2 ^ (n + 3) - 3
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))