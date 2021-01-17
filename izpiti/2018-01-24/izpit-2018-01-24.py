primer = [[2, 4, 1, 1], 
          [3, 2, 0, 5],
          [8, 0, 7, 2]]


from functools import lru_cache

def lisjacek(matrika, n):
    vrstice = len(matrika)
    stolpci = len(matrika[0])

    @lru_cache(maxsize=None)
    def lisjacek_notranji(i, j, n):
        jabolka = matrika[i][j]
        if n == 0:
            return 0
        #nemoremo vec dol
        elif i == vrstice - 1:
            #nemoremo niti vec desno - prisli smo do konca
            if j == stolpci - 1:
                return jabolka
            #v nasprotnem primeru gremo lahko samo desno
            else:
                return jabolka + lisjacek_notranji(i, j + 1, n - 1)
        #gremo lahko dol
        else:
            #ne moremo desno -> lahko gremo samo dol
            if j == stolpci - 1:
                return jabolka + lisjacek_notranji(i + 1, 0, n - 1)
            #lahko gremo bilokam
            else:
                return jabolka + max(lisjacek_notranji(i + 1, 0, n - 1), lisjacek_notranji(i, j + 1, n - 1))
       
    return lisjacek_notranji(0, 0, n)

lisjacek(primer, 6)