import Data.List (unfoldr)

-- parameter
l1 = 1.0
l2 = 1.0

x1_0 = - l1
y1_0 = 0.0
x2_0 = -(l1 + l2)
y2_0 = 0.0

t0 = 0.0
t1 = 10.0

n = 1000

dt :: Double
dt = (t1 - t0) / n

-- 角速度
omega1 = 0.0
omega2 = 0.0
-- 質量
m1 = 1.0
m2 = 1.0


main :: IO()
main = do
    writeFile "./data/rk4.dat" (toData (unfoldr rk4 (t0, x0, v0)))

toData :: [(Double, Double, Double, Double, Double)] -> String
toData xvs = unlines $ map (\(x1, y1, x2, y2) ->(show x1) ++ " " ++ (show x) ++ " " ++ (show v)) xvs

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