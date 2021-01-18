from functools import lru_cache

def zabica(mocvara):

    @lru_cache(maxsize=None)
    def izhod(polozaj, energija):
        #ce smo ze zunaj ne rabimo vec nic korakov
        if polozaj >= len(mocvara):
            return 0
        elif polozaj == len(mocvara) - 1:
            return 1

        #pridemo na polje se s staro energijo
        #prejmemo enrgijo
        nova_energija = energija + mocvara[polozaj]
        #iz nasega polozaja lahko skocimo na katerokoli polje do konca
        #dolzina skoka je lahko <= energiji
        #najblizje mesto = polozaj + 1,
        #najbolj oddaljeno = ce je polzaj + energija < dolzine potem polozaj + energija, cene dolzina
        moznosti = []
        for x in range(polozaj + 1, min(polozaj + nova_energija, len(mocvara) - 1) + 1):
            dolzina_skoka = x - polozaj
            en_pristanek = nova_energija - dolzina_skoka
            moznosti.append(1 + izhod(x, en_pristanek))
        
        return min(moznosti)
    
    return izhod(0, 0)

zabica([2, 4, 1, 2, 1, 3, 1, 1, 5])