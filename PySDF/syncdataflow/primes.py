import math

primes_list = [2, 3, 5]

def xgcd(a, b):
    """ Returns triplet g, x, y such that ax + by = g = gcd(a, b)
    Taken from: http://en.wikibooks.org/wiki/Algorithm_Implementation/Mathematics/Extended_Euclidean_algorithm
    """
    x,y, u,v = 0,1, 1,0
    while a != 0:
        q, r = b//a, b%a
        m, n = x-u*q, y-v*q
        b,a, x,y, u,v = a,r, u,v, m,n
    _gcd = b
    return _gcd, x, y

def from_vector_repr(n, radix):
    ps = primes()
    prod = 1
    while n > 0:
        i = n % radix
        n = n // radix
        prod *= (next(ps) ** i)
    return prod

def prime_factors_nomul(n):
    factors = set()
    for p in primes():
        exponent = 0
        while n > 1 and n % p == 0:
            exponent = 1
            n = n // p

        if exponent > 0:
            factors.add(p)

        if n == 1:
            return factors

def prime_factors(n):
    factors = {}
    for p in primes():
        exponent = 0
        while n > 1 and n % p == 0:
            n = n // p
            exponent += 1
        if exponent > 0:
            factors[p] = exponent
        if n == 1:
            return factors

def is_prime(n: int):
    for i in primes():
        if n % i == 0:
            return False
        elif i * i > n:
            return True

def primes():
    for p in primes_list:
        yield p
    k = p
    while True:
        k = k + 2 if k % 6 == 5 else k + 4
        isqrt = math.floor(math.sqrt(k))
        for p in primes_list:
            if p > isqrt:
                primes_list.append(k)
                yield k
                break
            elif k % p == 0:
                break

