import numpy as np
import matplotlib.pyplot as plt
import os
from tqdm import tqdm

x_min = -3.0
x_max =  3.0
y_min = -3.0
y_max =  3.0

origin = [0.0, 0.0]


path = "./data/"
dat_list = os.listdir(path)

for i in tqdm(range(len(dat_list))):
    dat_name = "{0}.dat".format(i)
    info= np.loadtxt(path + dat_name)

    pendulum_x = [origin[0], info[0], info[2]] 
    pendulum_y = [origin[1], info[1], info[3]]
    time = info[4]

    fig = plt.figure(figsize = (6, 6))
    plt.grid()
    plt.title("time:{0:2f}".format(time))

    plt.xlim(x_min, x_max)
    plt.ylim(y_min, y_max)
    plt.plot(pendulum_x, pendulum_y, color = "b", marker = "o", markersize = 12)
    plt.savefig("./picture/{0:05d}.png".format(i))
    plt.close()