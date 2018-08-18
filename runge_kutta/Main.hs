import Data.List (unfoldr)

-- dy / dx = f
f t x v = v

-- dv / dt = g
g t x v = - omega ** 2 * x

-- parameter
omega = 2
-- 区間
t0 = 0
t1 = 2 * pi
x0 = 1
v0 = 0
-- 分割数
n = 1000

dt :: Double
dt = (t1 - t0) / n

rk4 :: (Double, Double, Double) -> Maybe ((Double, Double, Double), (Double, Double, Double))
rk4 st@(t, x, v)    | t < t1 = Just(st, st')
                    | otherwise = Nothing
                    where   st' = (t', x', v')
                            t' = t + dt
                            x' = x + dt / 6 * (k1 + 2 * k2 + 2 * k3 + k4) 
                            v' = v + dt / 6 * (l1 + 2 * l2 + 2 * l3 + l4)
                            k1 = f t x v
                            k2 = f (t + dt / 2) (x + dt / 2 * k1) (v + dt / 2 * l1)
                            k3 = f (t + dt / 2) (x + dt / 2 * k2) (v + dt / 2 * l2)
                            k4 = f (t + dt) (x + dt * k3) (v + dt / 2 * l3)
                            l1 = g t x v
                            l2 = g (t + dt / 2) (x + dt / 2 * k1) (v + dt / 2 * l1)
                            l3 = g (t + dt / 2) (x + dt / 2 * k2) (v + dt / 2 * l2)
                            l4 = g (t + dt) (x + dt * k3) (v + dt / 2 * l3)
                           


main :: IO()
main = do
    writeFile "./data/rk4.dat" (toData (unfoldr rk4 (t0, x0, v0)))

toData :: [(Double, Double, Double)] -> String
toData xvs = unlines $ map (\(t, x, v) ->(show t) ++ " " ++ (show x) ++ " " ++ (show v)) xvs