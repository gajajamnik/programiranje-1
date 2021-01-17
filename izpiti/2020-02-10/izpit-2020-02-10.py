from functools import lru_cache

def f(k, n):

    @lru_cache(maxsize=None)
    def zaporedja(trenutni, abs, dol):
        st_zaporedij = 0
        if dol == 0:
            return 1
        else:
            for x in range(max(0, trenutni - abs), trenutni + abs + 1):
                st_zaporedij += zaporedja(x, abs, dol - 1)
        return st_zaporedij

    return zaporedja(0, k, n)