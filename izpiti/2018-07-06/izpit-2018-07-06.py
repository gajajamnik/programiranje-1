from functools import lru_cache

def simetricen(niz):
    return niz == niz[:: -1]

#def stevilo_delov(niz):
#
#    @lru_cache(maxsize=None)
#    def deli(podniz):
#        stevilo = 0
#
#        #ROBNI
#        #ce niza sploh ni
#        if len(podniz) == 0:
#            return 0
#        #ce je podniz ze simetricen ga razdelis na en del
#        elif simetricen(podniz) == True:
#            return 1
#        
#        #OSTALO
#        moznosti = []
#        for i in range(1, len(podniz)):
#            prvi_del = podniz[:i]
#            drugi_del = podniz[i:]
#            if deli(prvi_del) != 0:
#                stevilo += deli(prvi_del)
#            elif deli(drugi_del) != 0:
#                stevilo += deli(drugi_del)
#            
#            moznosti.append(stevilo)
#        
#        return min(moznosti)
#    
#    return deli(niz)
#        
#stevilo_delov("123")

def stevilo_delov(niz):

    @lru_cache(maxsize=None)
    def prestej_pomozna(niz ,i):
        dobri_deli = 0
        if len(niz) == 0:
            return dobri_deli
        elif simetricen(niz[i:]) == False:
            return dobri_deli + prestej_pomozna(niz, i + 1)
        elif simetricen(niz[i:]) == True:
            dobri_deli += 1
            return dobri_deli + prestej_pomozna(niz[:i], 0)
        return dobri_deli

    return prestej_pomozna(niz, 0)