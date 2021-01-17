from functools import lru_cache

def pobeg(seznam):
    l = len(seznam)

    @lru_cache(maxsize=None)
    def mozni_pobegi(i, denar):
        #ROBNI PRIMERI
        #uspe pobegnit s pozitivnim denarjem
        if i >= l and denar >= 0:
            return [i]
        #pobegne ampak z negativnim denarjem
        elif i >= l and denar < 0:
            return None

        #OSTALE MOZNOSTI
        else:
            moznosti = seznam[i]

            najboljsa_pot = []
            for (smer, strosek) in moznosti:
                nov_denar = strosek + denar
                nov_i = smer
                if mozni_pobegi(nov_i, nov_denar) is not None:
                    najboljsa_pot.append(mozni_pobegi(nov_i, nov_denar))
                
            if len(najboljsa_pot) == 0:
                return None
            else:
                return [i] + sorted(najboljsa_pot)[0]
    
    return mozni_pobegi(0, 0)


primer = [[(1, 10), (3, -10)], [(2, 10), (5, -20)], [(3, -10)], [(4, 15)], [(5, 0)]]