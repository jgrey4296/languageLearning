import matplotlib.pyplot as plt
import numpy as np

a = np.linspace(0,1,100)

def soft_knee(i,t,r,k):
    under = np.array([x for x in i if x < (t - k/2)])
    inKnee = np.array([x for x in i if (t - k/2) <= x and x <= (t + k/2)])
    over = np.array([x for x in i if (t + k/2) < x])

    k_f = (1/r - 1)
    k_red_1 = inKnee - t + k/2
    k_red_1_pow = pow(k_red_1,2)
    k_div = 2 * k
    k_reduced = (k_f * k_red_1_pow) / k_div
    k_red = inKnee + k_reduced

    over_red = t + ((over - t)) / r

    return np.concatenate((under,k_red,over_red))


b = soft_knee(a,0.6,10,0.5)

plt.plot(a)
plt.plot(b)
plt.show()
