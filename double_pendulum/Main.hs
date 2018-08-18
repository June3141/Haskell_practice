import Data.List (unfoldr)

-- parameter
length1 = 1.0
length2 = 1.0
-- t = 0での座標
theta1_0 = - pi / 2
theta2_0 = - pi / 2
x1_0 =   length1 * sin theta1_0
y1_0 = - length1 * cos theta1_0
x2_0 =   length1 * sin theta1_0 + length2 * sin theta2_0
y2_0 = - length1 * cos theta1_0 - length2 * cos theta2_0
-- シミュレーションの時間
t0 = 0.0
t1 = 10.0
-- シミュレーション回数
n = 100 
-- 角速度
omega1_0 = 0.0
omega2_0 = 0.0
-- 質量
m1 = 1.0
m2 = 1.0

-- 重力加速度
g = 9.80


dt :: Double
dt = (t1 - t0) / n


-- 角度の1階微分方程式
f1 t theta1 theta2 omega1 omega2 = omega1
f2 t theta1 theta2 omega1 omega2 = omega2

-- 角度の2階微分方程式
g1 :: Double -> Double -> Double -> Double -> Double -> Double
g1 t theta1 theta2 omega1 omega2 = (a - b) / c
    where
        a = g / length1 * (sin theta2 * cos del_theta - mu * sin theta1)
        b = (omega2 ** 2 / l + omega1 ** 2 * cos del_theta) * sin del_theta
        c = mu  - (cos del_theta) ** 2
        l = length1 / length2
        mu = 1 + m1 / m2
        del_theta = theta1 - theta2


g2 :: Double -> Double -> Double -> Double -> Double -> Double
g2 t theta1 theta2 omega1 omega2 = (a - b) / c
    where
        a = g * mu / length2 * (sin theta1 * cos del_theta - sin theta2)
        b = (mu * l * omega1 ** 2 + omega2 ** 2 * cos del_theta) * sin del_theta
        c = mu  - (cos del_theta) ** 2
        l = length1 / length2
        mu = 1 + m1 / m2
        del_theta = theta1 - theta2


simulate :: (Double, Double, Double, Double, Double) -> Double -> IO()
simulate st@(theta1, omega1, theta2, omega2, t) time = do
    if time < n then do
        let (theta'1, omega'1, theta'2, omega'2, t') = rk4_theta (theta1, omega1, theta2, omega2, t)
        writeFile ("./data/" ++ (show (truncate time)) ++ ".dat") (toData (toxyFromTheta (theta1, omega1, theta2, omega2, t)))
        simulate (theta'1, omega'1, theta'2, omega'2, t') (time + 1)
    else do
        return()
       

main = do
    simulate (theta1_0, omega1_0, theta2_0, omega2_0, t0) 0


rk4_theta :: (Double, Double, Double, Double, Double) -> (Double, Double, Double, Double, Double)
rk4_theta st@(theta1, omega1, theta2, omega2, t) = st'
    where
        st' = (theta'1, omega'1, theta'2, omega'2, t')
        -- 質点1に関するルンゲクッタ
        theta'1 = theta1 + dt / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
        omega'1 = omega1 + dt / 6 * (l1 + 2 * l2 + 2 * l3 + l4)
        -- 質点2に関するルンゲクッタ
        theta'2 = theta2 + dt / 6 * (m1 + 2 * m2 + 2 * m3 + m4)
        omega'2 = omega2 + dt / 6 * (n1 + 2 * n2 + 2 * n3 + n4)
        k1 = f1 t theta1 theta2 omega1 omega2
        k2 = f1 (t + dt / 2) (theta1 + dt / 2 * k1) (omega1 + dt / 2 * l1) (theta2 + dt / 2 * m1) (omega2 + dt / 2 * n1)
        k3 = f1 (t + dt / 2) (theta1 + dt / 2 * k2) (omega2 + dt / 2 * l2) (theta2 + dt / 2 * m2) (omega2 + dt / 2 * n2)
        k4 = f1 (t + dt) (theta1 + dt * k3) (omega1 + dt * l3) (theta2 + dt * m3) (omega2   + dt * n3)
        l1 = g1 t theta1 theta2 omega1 omega2
        l2 = g1 (t + dt / 2) (theta1 + dt / 2 * k1) (omega1 + dt / 2 * l1) (theta2 + dt / 2 * m1) (omega2 + dt / 2 * n1)
        l3 = g1 (t + dt / 2) (theta1 + dt / 2 * k1) (omega1 + dt / 2 * l1) (theta2 + dt / 2 * m1) (omega2 + dt / 2 * n1)
        l4 = g1 (t + dt) (theta1 + dt * k3) (omega1 + dt * l3) (theta2 + dt * m3) (omega2 + dt * n3)
        m1 = f2 t theta1 theta2 omega1 omega2
        m2 = f2 (t + dt / 2) (theta1 + dt / 2 * k1) (omega1 + dt / 2 * l1) (theta2 + dt / 2 * m1) (omega2 + dt / 2 * n1)
        m3 = f2 (t + dt / 2) (theta1 + dt / 2 * k1) (omega1 + dt / 2 * l1) (theta2 + dt / 2 * m1) (omega2 + dt / 2 * n1)
        m4 = f2 (t + dt) (theta1 + dt * k3) (omega1 + dt * l3) (theta2 + dt * m3) (omega2 + dt * n3)
        n1 = g2 t theta1 theta2 omega1 omega2
        n2 = g2 (t + dt / 2) (theta1 + dt / 2 * k1) (omega1 + dt / 2 * l1) (theta2 + dt / 2 * m1) (omega2 + dt / 2 * n1)
        n3 = g2 (t + dt / 2) (theta1 + dt / 2 * k1) (omega1 + dt / 2 * l1) (theta2 + dt / 2 * m1) (omega2 + dt / 2 * n1)
        n4 = g2 (t + dt) (theta1 + dt * k3) (omega1 + dt * l3) (theta2 + dt * m3) (omega2 + dt * n3)
        -- tの更新
        t' = t + dt


toData :: (Double, Double, Double, Double, Double) -> String
toData st@(x1, y1, x2, y2, t) = (show x1) ++ " " ++ (show y1) ++ " " ++ (show x2) ++ " " ++ (show y2) ++ " " ++ (show t)


toxyFromTheta :: (Double, Double, Double, Double, Double) -> (Double, Double, Double, Double, Double)
toxyFromTheta st@(theta1, omega1, theta2, omega2, t) = st'
    where 
        st' = (x1, y1, x2, y2, t)
        x1  =   length1 * sin theta1
        y1  = - length1 * cos theta1
        x2  =   length1 * sin theta1 + length2 * sin theta2
        y2  = - length1 * cos theta1 - length2 * cos theta2