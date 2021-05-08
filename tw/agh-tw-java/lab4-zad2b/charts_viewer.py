import numpy as np
from matplotlib import pyplot as plt
import collections
import json

with open('results.json') as json_file:
    data = json.load(json_file)

    producers = collections.OrderedDict(data['producerResults'].items())
    consumers = collections.OrderedDict(data['consumerResults'].items())

    size = len(producers.keys())

    plot_x = np.linspace(1, size, size)
    p_plot_y = []
    c_plot_y = []

    for k, v in producers.items():
        p_plot_y.append(v['allTime'] / v['attempts'])

    for k, v in consumers.items():
        c_plot_y.append(v['allTime'] / v['attempts'])

    plt.plot(plot_x, p_plot_y, 'b')
    plt.plot(plot_x, c_plot_y, 'g')
    plt.show()