from functools import lru_cache

def mama_franca(n, m, l):

    @lru_cache(maxsize=None)
    def nageljni(sirina_balkona, st_korit, sirina_korit):

        #ROBNI PRIMERI
        prosta_mesta = sirina_balkona - st_korit * sirina_korit
        #nimamo nic korit
        if st_korit == 0:
            return 0
        # imamo prevec korit za dolzino nasega balkona
        elif prosta_mesta < st_korit - 1:
            return 0
        #imamo tocno korit za postavit balkon
        elif prosta_mesta == st_korit - 1:
            return 1
        
        #OSTALI PRIMERI
        else:
            #smo na i-tem mestu
            #na i+1-to mesto lahko postavimo korito ali ga pa ne postavimo
            #ce ne postavimo st_korit ostane ista sirina "balkona" pa se zmanjsa
            ne_postavimo = nageljni(sirina_balkona - 1, st_korit, sirina_korit)
            
            #ce postavimo korito se zmanjsa st korit in balkon se zmanjsa za dolzino enega korita
            postavimo = nageljni(sirina_balkona - sirina_korit, st_korit - 1, sirina_korit)

            return postavimo + ne_postavimo
        
    return nageljni(n, m, l)
