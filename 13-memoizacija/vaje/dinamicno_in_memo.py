from functools import lru_cache
# =============================================================================
# Najdaljše naraščajoče podzaporedje
# =============================================================================

# -----------------------------------------------------------------------------
# Napišite funkcijo `najdaljse_narascajoce_podazporedje`, ki sprejme seznam in
# poišče najdaljše (ne strogo) naraščajoce podzaporedje števil v seznamu.
#
# Primer: v seznamu `[2, 3, 6, 8, 4, 4, 6, 7, 12, 8, 9]` kot rezultat vrne
# podzaporedje `[2, 3, 4, 4, 6, 7, 8, 9]`.
# -----------------------------------------------------------------------------

#def najdaljse_narascajoce_podazporedje(seznam):
#    
#    l = len(seznam)
#    for i in range(0, l - 2):
#        x = seznam[i]
#        naslednji = seznam[i + 1]
#        if naslednji < x:
#            seznam.remove(naslednji)
#            l = len(seznam)
#    return seznam
#
#

def najdaljse_narascajoce_podazporedje_2(seznam):
    #Rabimo dve stvari
    #Memoiziramo to funkcijo
    @lru_cache(maxsize=None)
    def podzaporedje(i, zadnji): #i je clen na katerem smo
        if i >= len(seznam):
            return []
        if seznam[i] >= zadnji:
            #ce ga vzamemo
            vzamemo = [seznam[i]] + podzaporedje(i+1, seznam[i])
            #ce ga ne vzamemo
            ne_vzamemo = podzaporedje(i+1, zadnji)
            if len(vzamemo) >= len(ne_vzamemo):
                return vzamemo
            else:
                return ne_vzamemo
        else: #seznam[i] < zadnji
            #Gremo naprej
            return podzaporedje(i + 1, zadnji)
    return podzaporedje(0, float("-inf"))
            # tu das minus neskoncno da bo prvi element seznama vedno vecji

najdaljse_narascajoce_podazporedje_2([2, 3, 6, 8, 4, 4, 6, 7, 12, 8, 9])

# -----------------------------------------------------------------------------
# Rešitev sedaj popravite tako, da funkcija `vsa_najdaljsa` vrne seznam vseh
# najdaljših naraščajočih podzaporedij.
# -----------------------------------------------------------------------------



# =============================================================================
# Žabica
# =============================================================================
# Žabica se je izgubila v močvari in želi kar se da hitro odskakljati ven. Na
# srečo močvara vsebuje veliko muh, s katerimi si lahko povrne energijo, kajti
# utrujena žabica ne skoči daleč.
# 
# S funkcijo `zabica(mocvara)` želimo ugotoviti, kako hitro lahko žabica
# odskaklja iz močvare. Močvaro predstavimo s tabelo, kjer žabica prične na
# ničtem polju. Če je močvara dolžine `k`, je cilj žabice priskakljati vsaj na
# `k`-to polje ali dlje (torej prvo polje, ki ni več vsebovano v tabeli).
# 
# Energičnost žabice predstavimo z dolžino najdaljšega možnega skoka. Torej
# lahko žabica z količino energije `e` skoči naprej za katerokoli razdaljo med
# `1` in `e`, in če skoči naprej za `k` mest ima sedaj zgolj `e - k` energije.
# Na vsakem polju močvare prav tako označimo, koliko energije si žabica povrne,
# ko pristane na polju. Tako se včasih žabici splača skočiti manj daleč, da
# pristane na polju z več muhami. Predpostavimo, da ima vsako polje vrednost
# vsaj `1`, da lahko žabica v vsakem primeru skoči naprej.
# 
# V primeru `[2, 4, 1, 2, 1, 3, 1, 1, 5]` lahko žabica odskaklja iz močvare v
# treh skokih, v močvari `[4, 1, 8, 2, 11, 1, 1, 1, 1, 1]` pa potrebuje zgolj
# dva.
# =============================================================================

def zabica(mocvara):
    #vedeti moramo dve stvari:
    #   - kje smo
    #   - koliko energije ima zabica, torej na katere lokvanje lahko dejansko skoci
    
    # stevilo moznosti ki jih moramo sprobat je odvisno od stanja zabice
    @lru_cache(maxsize=None)
    def zabica_notranja(i, e_ostanek):
        #kdaj nehamo
        if i >= len(mocvara):
            return 0
        # koliko energije imamo na prvem lokvanju
        energija = e_ostanek + mocvara[i]
        #skoci navzgor
        #kam lahko skocimo navzdol (na keri lokvanj)
        navzdol = [zabica_notranja(i + dolzina_skoka, energija -dolzina_skoka)
                    for dolzina_skoka in range(1, energija + 1)]
        #najbolja bo moznost, ki nam vzame najmanj energije
        return 1 + min(navzdol)

    return zabica_notranja(0, 0)

zabica([2, 4, 1, 2, 1, 3, 1, 1, 5])

# =============================================================================
# Nageljni
# =============================================================================
# Mama Franca želijo na balkon širine `n` postaviti `m` korit z nageljni širine
# `l` (korit, ne nageljnov). Zaradi lažjega zalivanja mora biti med dvema
# koritoma vsaj za 1 enoto prostora. Mama Franca želijo postaviti vsa korita,
# jih pa zaradi slabega vida med seboj ne razlikujejo. 
# 
# Vnuk je že spisal program, ki poišče število možnih postavitev, ne zna pa
# vrniti rešitev. Napišite funkcijo `nageljni(n, m, l)`, ki vrne seznam vseh
# možnih postavitev, da se bodo mama Franca lažje odločili.
# 
# Primer vseh štirih možnih postavitev pri balkonu širine 9 s tremi koriti
# širine 2 (kjer z 1 označimo nagelj in z 0 prazen prostor):
# 
#     [1, 1, 0, 1, 1, 0, 1, 1, 0]
#     [1, 1, 0, 1, 1, 0, 0, 1, 1]
#     [1, 1, 0, 0, 1, 1, 0, 1, 1]
#     [0, 1, 1, 0, 1, 1, 0, 1, 1]
# =============================================================================

@lru_cache(maxsize=None) 
def nageljni_stevilo(n, m, l): 
    if m <= 0: 
        return 1
    elif n < l: 
        return 0 
    else: 
        return nageljni_stevilo(n-1, m, l) + nageljni_stevilo(n-l-1, m-1, l)



# =============================================================================
# Pobeg iz Finske
# =============================================================================
# Vaš sošolec Mortimer se je med potovanjem po Finski spravil v krepko godljo.
# Po divjem poskušanju lokalne vodke se je namreč stepel s kravo, zaradi česar
# ga sedaj lovi finska govedorejska mafija. Na srečo so za njegovo hrabro bitko
# slišale vse rokovske in metalske skupine, ki so mu pripravljene ponuditi
# prevoz.
# 
# Ker je Mortimer pridno poslušal predavanja iz finančne matematike, med potjo
# uspe prislužiti nekaj denarja, s katerim bo lahko plačal prevoz. Finci,
# navdušeni nad Mortimerjevim pogumom, mu dovolijo, da se med potjo zadolži,
# dokler na koncu pobega vse stroške povrne.
# 
# Mesta na poti predstavimo kot seznam, katerega elementi so seznami vseh
# možnih nadaljnjih poti. Pot je par `(indeks_cilja, denar)`. Kot primer
# 
#     [[(1, 10), (3, -10)],    # 0 
#     [(2, 10), (5, -20)],     # 1
#     [(3, -10)],              # 2 
#     [(4, 15)],               # 3 
#     [(5, 0)]]                # 4 
# 
# pomeni, da lahko v mestu 1 Mortimer izbere med prevozom v mesto 2, kjer
# dodatno zasluži 10 evrov, ali pa prevoz v mesto 5, ki ga stane 20 evrov. Ker
# beži pred mafijo, lahko predpostavite, da bodo možne zgolj poti na mesta z
# višji indeksom (torej ni ciklov).
# 
# Pobeg je uspešen, čim lahko odpotuje v mesto, ki ni več na seznamu (torej
# skok na indeks, ki preseže seznam) in ima po koncu zadnjega skoka 0 ali več
# evrov. Napišite program, ki nam vrne pot z najmanjšim številom skokov,
# predstavljeno kot seznam indeksov mest na poti. Ker pobeg morda ni možen, naj
# v tem primeru funkcija vrne `None`.
# 
# Na primeru je optimalna pot `[0, 3, 4, 5]`, kjer se Mortimer sicer zadolži,
# vendar v skoku iz 3 v 4 zasluži dovolj, da konča z 5 evri. Hitrejša pot bi
# bila `[0, 1, 5]`, vendar v tem primeru Mortimer na koncu dolguje še 10 evrov.
# 
# Mortimer pot vedno začne v mestu z indeksom 0 in ima 0 evrov (saj je vse
# zapil). Funkcija `pobeg` sprejme seznam, ki predstavlja finska mesta in vrne
# seznam indeksov mest, v katerih se Mortimer ustavi.
# =============================================================================

def pobeg(seznam_mest):
    @lru_cache(maxsize=None)
    def pobegi(i, denar):
        #ROBNI PRIMERI
            #ce pridemo do konca z negativnim denarjem
        if i >= len(seznam_mest) and denar < 0:
            return None
            #ce pridemo do koca z pozitivnim denarjem
        elif i >= len(seznam_mest) and denar >= 0:
            return [i]
        else:
            usmeritve = seznam_mest[i]

            #SHRANJEVANJE MOZNOSTI
            mozne_poti = []  # tu bomo zbrali vse moznosti od mesta i do konca
            for (i_mesta, stroski) in usmeritve:
                #kam pridemo do konca ce izberemo to usmeritev - UPORABIS REKURZIJO
                beg = pobegi(i_mesta, denar + stroski)
                
                #ce je beg sploh mozen dodamo to pot v moznosti
                if beg is not None:
                    mozne_poti.append(beg)
            
            #IZ SHRANJENIH MOZNOSTI IZLUSCIS PRAVE
            if len(mozne_poti) == 0:
                return None
            else:
                return [i] + sorted(mozne_poti, key=len)[0]
        
        #poklices na zacetnem primeru
        return pobegi(0, 0)

# =============================================================================
# Pričetek robotske vstaje
# =============================================================================
# Nepreviden študent je pustil robotka z umetno inteligenco nenadzorovanega.
# Robotek želi pobegniti iz laboratorija, ki ga ima v pomnilniku
# predstavljenega kot matriko števil:
# 
#   - ničla predstavlja prosto pot
#   - enica predstavlja izhod iz laboratorija
#   - katerikoli drugi znak označuje oviro, na katero robotek ne more zaplejati
# 
# Robotek se lahko premika le gor, dol, levo in desno ter ima omejeno količino
# goriva. V zbirki programov že ima funkcijo `moznost_pobega(soba, vrsta,
# stolpec, koraki)`, ki pove ali je pobeg možen.
# 
# Napišite funkcijo `pot_pobega(soba, vrsta, stolpec, koraki)`, ki sprejme
# matriko sobe, začetno pozicijo in število korakov ter izračuna pot po kateri
# robotek pobegne (če to ni možno vrne `None`). Pot zakodiramo s seznamom
# ukazov `'gor'`, `'dol'`, `'levo'` in `'desno'`.
# 
# Na primer za laboratorij:
# 
#     [[0, 1, 0, 0, 2],
#      [0, 2, 2, 0, 0],
#      [0, 0, 2, 2, 0],
#      [2, 0, 0, 2, 0],
#      [0, 2, 2, 0, 0],
#      [0, 0, 0, 2, 2]]
# 
# robotek iz vrste 3 in stolpca 1 pri vsaj petih korakih pobegne z ukazi
# 
#      ['gor', 'levo', 'gor', 'gor', 'desno']
# 
# medtem ko iz vrste 5 in stolpca 0 ne more pobegniti.
# =============================================================================

def pobeg_rob(soba, vrsta, stolpec, koraki):
    max_vrsta = len(soba)
    max_stolpec = len(soba[0])

    @lru_cache(maxsize=None)
    def pobegni(vrsta, stolpec, koraki):
        # Padli smo iz sobe
        if not (0 <= vrsta < max_vrsta) or not (0 <= stolpec < max_stolpec):
            return False
        # Pobeg uspesen! All hail our robot overlords!!!
        elif soba[vrsta][stolpec] == 1:
            return True
        # Lahko bezimo naprej
        elif soba[vrsta][stolpec] == 0 and koraki > 0:
            return any(
                [pobegni(vrsta + 1, stolpec, koraki-1),
                 pobegni(vrsta - 1, stolpec, koraki-1),
                 pobegni(vrsta, stolpec + 1, koraki-1),
                 pobegni(vrsta, stolpec - 1, koraki-1)])
        # Pristali smo na oviri ali pa nam je zmanjkalo korakov
        else:
            return False
    return pobegni(vrsta, stolpec, koraki)

def pot_pobega(soba, vrsta, stolpec, koraki):

    @lru_cache(maxsize=None)
    def poti(vrsta, stolpec, koraki):
        st_vrstic = len(soba)
        st_stolpcev = len(soba[0])
        
        #ce smo ze pri vratih
        if soba[vrsta][stolpec] == 1:
            return []
        #ce zmanjka goriva in nismo pri vratih
        elif koraki == 0 and soba[vrsta][stolpec] != 1:
            return None
        #ce smo zunaj vrstice
        elif vrsta > st_vrstic - 1 or vrsta < 0:
            return None
        #smo zunaj stolpcev
        elif stolpec > st_stolpcev - 1 or stolpec < 0:
            return None

        #ce imamo se goriva in nismo na oviri
        
        elif soba[vrsta][stolpec] == 0 and koraki > 0:
            
            mozne_poti = []
            if poti(vrsta - 1, stolpec, koraki - 1) is not None:
                mozne_poti.append("gor")
                mozne_poti += poti(vrsta - 1, stolpec, koraki - 1)
            elif poti(vrsta + 1, stolpec, koraki - 1) is not None:
                mozne_poti.append("dol")
                mozne_poti += poti(vrsta + 1, stolpec, koraki - 1)
            elif poti(vrsta, stolpec + 1, koraki - 1) is not None:
                mozne_poti.append("desno")
                mozne_poti += poti(vrsta, stolpec + 1, koraki - 1)
            elif poti(vrsta, stolpec - 1, koraki - 1) is not None:
                mozne_poti.append("levo")
                mozne_poti += poti(vrsta, stolpec - 1, koraki - 1)
            #ce je povsod okoli nas ovira
            else:
                return None
            
            return mozne_poti

        #ce smo ze na zacetku na oviri
        else:
            return None
        
    return poti(vrsta, stolpec, koraki)

laboratorij =  [[0, 1, 0, 0, 2],
                [0, 2, 2, 0, 0],
                [0, 0, 2, 2, 0],
                [2, 0, 0, 2, 0],
                [0, 2, 2, 0, 0],
                [0, 0, 0, 2, 2]]

pot_pobega(laboratorij, 3, 1, 5)