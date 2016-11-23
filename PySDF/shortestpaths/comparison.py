from shortestpaths import *
import math
import random

__algorithms__ = [bfct, gor, policy]

def compare_rand5(families = [1,2,3,4,5], sizes = [2 ** 10], seed = 1, runs = 10):
    random.seed(seed)
    for family in families:
        for size in sizes:
            print("\nGraph family: RAND5 {:02}, size: {}".format(family, size))
            for alg in __algorithms__:
                sum_scans = 0
                sum_sq_scans = 0
                print(alg.name, end = ':', flush=True)
                for _ in range(runs):
                    g = graphs.rand5(size, family)
                    dist, scans = alg.find_shortest_paths(g, 0)
                    sum_scans += scans
                    sum_sq_scans += (scans * scans)
                    print(" {:6.1f}".format(scans), end = '', flush=True)

                avg_scans = sum_scans / runs
                var_scans = sum_sq_scans / runs - avg_scans * avg_scans
                print("\rAlgorithm: {}: scans per vertex: {:.3f}, std.dev: {:.3f}                        ".format(alg.name, avg_scans, math.sqrt(var_scans)))

def compare_sqnc(families = [1], sizes = [512], seed = 1, runs = 10):
    random.seed(seed)
    for family in families:
        for size in sizes:
            print("\nGraph family: SQNC {:02}, size: {}".format(family, size * size + 1))
            for alg in __algorithms__:
                sum_scans = 0
                sum_sq_scans = 0
                print(alg.name, end = ':', flush=True)
                for _ in range(runs):
                    g = graphs.sqnc(size, family)
                    dist, scans = alg.find_shortest_paths(g, 0)
                    sum_scans += scans
                    sum_sq_scans += (scans * scans)
                    print(" {:6.1f}".format(scans), end = '', flush=True)

                avg_scans = sum_scans / runs
                var_scans = sum_sq_scans / runs - avg_scans * avg_scans
                print("\rAlgorithm: {}: scans per vertex: {:.3f}, std.dev: {:.3f}                       ".format(alg.name, avg_scans, math.sqrt(var_scans)))




