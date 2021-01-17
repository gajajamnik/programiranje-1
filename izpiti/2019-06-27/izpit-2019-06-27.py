from functools import lru_cache

zabojniki = [1, 3, 4, 7, 10]

def ladja(zabojniki, nosilnost):

    @lru_cache(maxsize=None)
    # k bo stevec ki se sprehaja po szenamu zabojnikov
    def natovori(k, nosilnost):
        # 1. napises robne pogoje
        if nosilnost == 0:
            return 1
        if k >= len(zabojniki):
            return 0
        
        #koliko je nacinov da zapolnis ce dodas v tem koraku k-to zabojnik
        teza_k = zabojniki[k]
        n = 0
        #ce lahko dodamo k-ti zabojnik gremo pogledat vse moznosti ko je ta zabojnik ze dodan
        if teza_k <= nosilnost:
            n += natovori(k, nosilnost - teza_k)
        
        #v vsakem primeru nas zanimajo se moznosti ce namesto k dodamo naslednjika
        n += natovori(k + 1, nosilnost)

        return n
    return natovori(0, nosilnost)
