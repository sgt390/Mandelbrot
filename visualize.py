import matplotlib.pyplot as plt
import numpy as np
import json
from tqdm import tqdm
from math import sqrt

with open('results.txt', 'r') as f:
    mand = f.readline()
    mand = mand.replace('{', '[')
    mand = mand.replace('}', ']')

mandelbrot = json.loads(mand)


print(f'working with {len(mandelbrot)} points...')
#heatmap = np.array([])
#for heat in tqdm(mandelbrot):
#    heatmap = np.append(heatmap, 255-heat)

size = int(sqrt(len(mandelbrot)))
shape = (size, size)
heatmap = np.reshape(mandelbrot, shape).T

print('printing...')
img = plt.imshow(heatmap, cmap='plasma')

plt.show()