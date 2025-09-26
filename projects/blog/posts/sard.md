---
title: Il teorema di Sard
tags: [math]
date: 2020-05-21
language: it
---

> **Note:** Questo post nasce pochi giorni dopo un seminario che ho dovuto tenere in Universitá, infatti, dopo aver approfondito l'argomento che sto per esporre, mi spiaceva lasciare tali concetti "al vento" e mi sembrava sensato trascriverli nero su bianco da qualche parte. L'idea iniziale era di scrivere un bel documento in $\LaTeX$ di cui farne un pdf da perdere in qualche remota cartella del pc, ma alla fine mi sono detto: perché non qui sul blog?

L'obiettivo di queste note é di fornire una semplice e concisa esposizione del teorema di Sard, noto risultato di geometria differenziale. La trattazione é carica di osservazioni e frequenti definizioni, in modo da essere digeribile anche ai non addetti ai lavori.

**Definition:** Siano $(a_1, \dots, a_n), (b_1, \dots, b_n) \in \R^n$ tali che $a_i < b_i$, chiamiamo *rettangolo solido n-dimensionale* l'insieme
$$
S(a, b)=\{(x_1, \dots, x_n) \in \mathbb{R}^n : a_i \lt x_i \lt b_i\}
$$

**Definition:** Chiamiamo *volume n-dimensionale* di un rettangolo solido n-dimensionale $S(a, b)$ la quantitá reale positiva
$$
Vol(S(a, b)) = \prod_{i=1}^n (b_i - a_i)
$$

**Definition:** Diciamo che $A \subset \R^n$ ha *misura nulla* se $\forall \epsilon \gt 0$ esiste un ricoprimento $\{S_i\}_{i \in \N}$ di $A$ (ovvero $\bigcup_{i \in \N} S_i \supset A$) formato da rettangoli solidi $S_i$ tale che
$$
\sum_{i \in \mathbb{N}} Vol(S_i) \lt \epsilon
$$

Osserviamo innanzitutto che, nella definizione di insieme a misura nulla, considerare cubi n-dimensionali (cioé rettangoli dove $b_i - a_i = b_j - a_j \forall i, j$) invece che rettangoli é del tutto equivalente. D'altronde é del tutto equivalente anche considerare le palle euclidee o in generale le altre bolle indotte da norme equivalenti (nel senso che generano la stessa topologia).

Inoltre, si segnala come tale nozione sia equivalente all'avere misura di Lebesgue nulla, ovvero $A$ ha misura nulla se e solo se $m_n(A) = 0$ dove $m_n$ é la misura di Lebesgue n-dimensionale. La dimostrazione di tale equivalenza, nonostante non di difficile raggiungimento, non é necessaria per i nostri obiettivi e pertanto verrá omessa.

Iniziamo con le due seguenti banali osservazioni:

**Observation:** Se $m \lt n$ allora $\R^m$ ha misura nulla in $\R^n$

**Proof:** Basta ricoprire $\R^m$ con una famiglia numerabile di cubi, tali cubi giacciono su un iano di $\R^n$ e quindi ognuno di essi puó essere schiacciato in una direzione ortogonale a questo pano. In questo modo l'unione dei rettangoli continua a contenere tutto $\R^m$ e il volume di ogni rettangolo é piccolo a piacere, questo implica che anche la somma di tutti i volumi é piccola a piacere.

**Observation:** $\bigcup_{i \in \N} A_i$ ha misura nulla se ogni $A_i$ ha misura nulla.

**Proof:** Unioni di famiglie numerabili sono ancora numerabili per l'assioma della scelta.

Vediamo ora una prorietá fondamentale che ci permettá in seguito di estendere la definizione di insiemi a misura nulla.

**Proposition:** Sia $F: U \to \R^n$ una mappa liscia, con $U \subset \R^n$ aperto. Se $A \subset U$ ha misura nulla allora anche l'immagine $F(A)$ ha misura nulla.

**Proof:** Iniziamo osservando che $U$ é ricopribile da una famiglia numerabile di palle chiuse per cui la restrizione di $F$ ad ognuna di queste palle é ancora liscia, ricordiamo che una mappa é liscia su un insieme generico (non aperto) se essa é estendibile ad una mappa liscia definita su un aperto contenente tale insieme. Sia $\bar{B}$ una di queste palle, siccome $\bar{B}$ é compatto e $F \in \mathscr{C}^1(\bar{B})$ allora $\exists c \gt 0$ tale che $\forall x, y \in \bar{B}$

$$
\Vert F(x) - F(y) \Vert \le c \Vert x-y \Vert
$$

Fissiamo $\delta \gt 0$, visto che $A \cap \bar{B}$ ha misura nulla possiamo considerare un suo ricoprimento numerabile $\{ B_k \}_{k \in \N}$ di palle tale che

$$
\sum_{k \in \N} Vol({B_k}) \lt \delta
$$

Per la diseguaglianza di prima sappiamo che $F(B_k \cap \bar{B})$ é contenuto in una palla di raggio al piú $c$ volte il raggio di $B_k$. Dunque $F(A \cap B_k)$ é ricoperto da una famiglia numerabile $\{ \tilde{B_k} \}_{k \in \N}$ di palle di volume complessivo inferiore a

$$
\sum_{k \in \N} Vol({\tilde{B_k}}) \lt c^n \delta
$$

Per arbitrarietá di $\delta$ segue che $F(A \cap \bar{B})$ ha misura nulla, e dunque per quanto osservato all'inizio che anche $F(A)$ ha misura nulla, cioé la tesi.

Ció implica che l'avere dimensione nulla é invariante per diffeomorfismi, siamo quindi ora pronti ad estendere la definizione di insiemi a misura nulla alle varietá differenziabili, prima di fare ció enunciamo e dimostriamo una versione piú debole del teorema di Sard. Per capire in che modo questo teorema é implicato dalla versione generale occorrerá attendere ancora un poco.

**Theorem (Mini-Sard):** Sia $F: U \sub \R^m \to \R^n$ una mappa liscia, con $U$ aperto e $m < n$. Allora l'immagine $F(\R^m)$ ha misura nulla in $\R^n$.

**Proof:** Sia $\pi: \R^n \to \R^m$ la proiezione sulle prime $m$ componenti, tale mappa é liscia. Consideriamo ora l'aperto $\tilde{U} = \pi^{-1}(U) \sub \R^n$ e $\tilde{F} = F \circ \pi: \R^n \to \R^n$, che é ancora liscia. A questo punto é sufficiente osservare che $F(U)$ non é nient'altro che l'immagine di $\tilde{U} \cap \R^m$ attraverso $\tilde{F}$, che, per la proposizione precedente, ha misura nulla siccome é l'immagine di un insieme a misura nulla (é tutto contenuto in un iperpiano!) attraverso una funzione liscia.

Come preannunciato, estendiamo la definizione di insieme a misura nulla sulle varietá differenziali.

**Definition (Insiemi a misura nulla su varietá differenziabili):** Sia $M$ una varietá differenziale, diciamo che $A \sub M$ ha misura nulla se $\varphi(A_i \cap U_i)$ ha misura nulla in $\R^{dim(M)}$ per ogni carta $(U, \varphi)$ dell'atlante di $M$.

Si osserva che, a causa della $\mathscr{C}^\infty$-compatibilitá delle carte dell'atlante, per affermare che un sottoinsieme della varietá ha misura nulla é sufficiente trovare una collezione numerabile di carte che ricoprano l'insieme candidato e che soddisfino l'enunciato della definizione. In particolare se l'insieme é tutto contenuto in una carta, per mostrare che ha misura nulla basta verificare che l'immagine attraverso la carta ha misura nulla.

Passiamo ora a definire un altro concetto che sará fondamentale per enunciare il Teorema di Sard.

**Definition (Punti critici e valori critici):** Sia $F: M \to N$ una mappa liscia tra varietá differenziali, diciamo che $p \in M$ é un **punto critico** se la mappa differenziale indotta $dF_p: T_P \to T_{F(p)}N$ non é suriettiva. In tal caso $F(p)$ si dice **valore critico**.

Denotiamo con $Crit(F)$ l'insieme dei punti critici di $F$.

**Definition (Punti regolari e valori regolari):** Sia $F: M \to N$ una mappa liscia tra varietá differenziali, diciamo che $p \in M$ é un **punto regolare** se non é critico, ovvero se $dF_p: T_p \to T_{F(p)}N$ é suriettiva (ovvero locamente $F$ é una sommersione). Se $p'$ é un punto regolare per ogni punto sulla fibra $F^{-1}(F(p))$ allora $F(p)$ si dice **valore regolare**.

Osserviamo come affinché un valore sia critico é sufficiente che esso sia l'immagine di un solo punto critico, mentre affinché sia regolare occorre che tutti i punti della sua controimmagine siano regolari.

La seguente osservazione ci sará utile durante la dimostrazione del teorema di Sard.

**Observation:** $Crit(F)$ é un chiuso di $M$

**Proof:** $Crit(F)=h^{-1}(0)$ dove $h: M \to \R$ é la mappa liscia tale che

$$h(p)=det(J(F)\bigr|_p \cdot (J(F)\bigr|_p)^t)$$

cioé la mappa che manda i punti della varietá nel determinante del prodotto della Jacobiana con la sua trasposta.

Prima di presentare il teorema di Sard occorre ancora dare una definizione ed enunciare il teorema di Fubini, di cui peró ometteremo la dimostrazione [^fubini]. Tale risultato sará fondamentale nella dimostrazione del teorema di Sard.

**Definition (Sezione verticale):** Sia $\R^n = \R^k \times \R^l$ e $a \in \R^k$, chiamiamo **sezione verticale** l'insieme $V_a = \{ a \} \times \R^l$.

Sempre adottando le notazioni della definizione, diremo che un insieme $A \sub \R^n$ ha sezione verticale nulla se la proiezione (sulle ultime l componenti) di $V_a \cap A$ in $\R^l$ ha misura nulla.

**Theorem (Teorema di Ruffini):** Sia $A \sub \R^n = \R^k \times \R^l$, se tutte le sezioni verticali $V_a$ hanno misura nulla (quindi $\forall a \in \R^k$) allora $A$ ha misura nulla in $\R^n$.

Enunciamo finalmente il teorema di Sard:

**Theorem (Teorema di Sard):** Sia $F: M \to N$ una mappa liscia tra varietá differenziabili, allora l'insieme dei valori critici $F(Crit(F))$ ha misura nulla in $N$.

Siccome per le varietá differenziabili vale il secondo assioma di numerabilitá ogni insieme é ricopribile con una collezione numerabile di carte, pertanto nell'enunciato del teorema é sufficiente chiedere che il dominio di F sia un singolo aperto $U \sub \R^m$, dove $m = dim(M)$. Inoltre, per lo stesso motivo, anche l'immagine $F(U)$ é ricopribile con una collezione numerabile di carte, pertanto anche qui si puó supporre senza perdita di generalitá che la carta sia una sola, ovvero che l'immagine $F(U)$ stia in $\R^n$, dove $n = dim(N)$.

Quanto appena scritto é sufficiente a giustificare la seguente formulazione equivalente del teorema di Sard.

**Theorem (Teorema di Sard, formulazione equivalente):** Sia $F: U \sub \R^m \to \R^n$ una mappa liscia, con $U$ aperto. Allora l'insieme dei valori critici $F(Crit(F))$ ha misura nulla in $\R^n$.

**Proof:** Se $m \lt n$ l'enunciato diventa una semplice conseguenza del Teorema Mini-Sard, supponiamo dunque $m \geq n$ e procediamo per induzione su $m$.

Se $m = 0$ allora l'immagine dei punti critici deve essere contenuta in un punto, e pertanto non puó che avere misura nulla. Supponiamo quindi ora il teorema valido per $m-1$ e dimostriamolo per $m$.

Chiamiamo ora per brevitá $C = Crit(F)$ e $C_i = \{ p \in U : \frac{\partial^k F}{\partial \dots} \bigr|_p = 0, \forall k \leq i \}$, ovvero l'insieme dei punti di $U$ in cui tutte le derivate di ordine inferiore a $i$ si annullano.

Osserviamo subito come $C$ e i $C_i$ sono chiusi (dimostrazione simile all'osservazione iniziale sulla chiusura di $C$), inoltre ha luogo la seguente catena di inclusioni:

$$C \supset C_1 \supset C_2 \supset \dots$$

Assumiamo ora i tre seguenti lemmi, rimandandone temporaneamente la dimostrazione, che ricordiamo avverrá per induzione su $m$.

- **Lemma a** $F(C \setminus C_1)$ ha misura nulla in $\R^n$
- **Lemma b** $F(C_i \setminus C_{i+1})$ ha misura nulla in $\R^n$
- **Lemma c** $F(C_k)$ ha misura nulla in $\R^n$ se $k \gt \frac{m}{n} - 1$

Per concludere il teorema ora é sufficiente osservare che

$$F(C) = F(C \setminus C_1) \cup \bigcup_{i=1}^{\floor{\frac{m}{n}-1}} F(C_i) \setminus F(C_{i+1}) \cup F(C_{\ceil{\frac{m}{n}-1}})$$

Ovvero che l'insieme dei valori critici é unione finita di insiemi che sono a misura nulla per i tre lemmi, e pertanto anch'esso ha misura nulla.

Seguono le dimostrazioni dei tre lemmi, ricordiamo che ci troviamo sotto ipotesi induttive, pertanto potremo assumere il teorema di Sard valido per $m-1$.

**Lemma (a):** $F(C \setminus C_1)$ ha misura nulla in $\R^n$

**Proof:** Osserviamo come sia sufficiente mostrare che per ogni punto in $C \setminus C_1$ esiste un intorno $V$ per cui $f(C \cap V)$ ha misura nulla. Infatti siccome $U$ é un aperto di $\R^m$, vale il secondo assioma di numerabilitá, e quindi é possibile ricoprire $C \setminus C_1$ di intorni la cui immagine ha misura nulla.

Consideriamo quindi $\tilde{x} \in C \setminus C_1$, visto che $\tilde{x} \not\in C_1$ possiamo assumere senza perdita di generalitá che $\frac{\partial f}{\partial x_1}$ sia non nulla in $\tilde{x}$, a questo punto definiamo una mappa $h: U \to \R^m$ tale che

$$h(x_1, \dots, x_m) = (f_1(x), x_2, \dots, x_m)$$

Questa mappa ha rango massimo in $\tilde{x}$ e quindi é un diffeomorfismo locale per un qualche intorno aperto di $\tilde{x}$, continuiamo a chiamare $h: V \sub U \to V' \sub \R^m$ il diffeomorfismo ottenuto dalla restrizione.

Definiamo ora la mappa composta $g = f \circ h^{-1} : V' \to \R^n$ e chiamiamo $C' = Crit(g)$, osserviamo subito che $C' = h(C \cap V)$; ma allora $g(C') = g(h(C \cap V)) = f(h(h^{-1}(C \cap V))) = f(C \cap V)$ e quindi per mostrare la tesi basta mostrare che $g(C')$ ha misura nulla.

Osservando che $g_1 = f_1 \circ h_1^{-1} = id$ si vede che $g$ é la funzione identitá sulla prima coordinata, questo permette di definire per ogni $t$ la mappa $g^t: ({t} \times \R^{m-1} \to {t} \times \R^{n-1})$ dove

$$g^t(x_2, x_3, \dots, x_m) = (g_2(t, x_2, \dots, x_m), \dots, g_m(t, x_2, \dots, x_m))$$

I punti critici di questa mappa coincidono coi punti critici della sezione verticale di $C'$, ovvero $C' \cap V_t = \{ t \} \times Crit(g^t)$. Questo implica che $g(C') \cap V_t = \{ t \} \times g^t(C)$, ovvero che le varie sezioni verticali dei valori critici di $g$ coincidono con i valori critici di $g^t$, che peró hanno misura nulla per ipotesi induttiva!

Questo basterebbe a concludere grazie al teorema di Fubini, se solo non fosse che mentre $C'$ é un chiuso non é detto che anche $g(C')$ sia un chiuso (serve che lo sia affinché sia possibile applicare il teorema di Fubini). Questo problema é facilmente superabile osservando che $C'$ é unione numerabile di compatti (é un chiuso di $U$) e quindi anche l'immagine $g(C')$ é unione numerabile di compatti, pertanto non assumere $C'$ chiuso non é lmitante.

**Lemma (b):** $F(C_i \setminus C_{i+1})$ ha misura nulla in $\R^n$

**Proof:** La dimostrazione é simile a quella del lemma precedente, infatti dimostreremo che $\forall x \in C_i \setminus C_{i+1}$ troviamo un intorno $V$ di $x$ tale che $f(C_i \cap V)$ ha misura nulla, per le stesse motivizaioni del lemma precedente questo é sufficiente a concludere la dimostrazione.

Sia $\tilde{x} \in C_i \setminus C_{i+1}$, siccome $\tilde{x} \not \in C_{i+1}$ significa che possiamo trovare una derivata $k+1$-esima di $f$ non nulla in $\tilde{x}$. Senza perdita di generalitá assumiamo quindi che esista una derivata $k$-esima $\rho: U \to \R^n$ tale che $\frac{\partial \rho_1}{\partial x_1}$ sia non nulla in $\tilde{x}$.

Definiamo a questo punto una mappa $h: U \to \R^m$ tale che

$$h(x_1, \dots, x_m) = (\rho_1(x_1, \dots, x_m), \dots, x_m)$$

Come nella dimostrazione del lemma precedente, siccome tale mappa ha rango massimo in $\tilde{x}$, esistono $x \in V \sub U \sub \R^m$ e $V' \sub \R^n$ aperti diffeomorfi attraverso la restrizione di $h$, che continueremo a chiamare $h$. Per costruzione $h(C_k \cap V)$ é contenuto nell'iperpiano $\{ 0 \} \times \R^{m-1}$, e quindi $g = f \circ h^{-1}$ ha i punti critici di tipo $C_k$ in tale iperpiano.

Definiamo $\tilde{g}$ come la restrizione di $g$ data da $\tilde{g}: (\{ 0 \} \times \R^{m-1}) \cap V' \to \R^n$, per induzione vediamo che i suoi valori critici hanno misura nulla, ma i suoi punti critici coincidono coi punti critici di tipo $C_k$ di $g$, e quindi l'immagini di tali punti, ovvero $f(C_k \cap V)$, ha misura nulla.

**Lemma (c):** $F(C_k)$ ha misura nulla in $\R^n$ se $k \gt \frac{m}{n} - 1$

**Proof:** Siccome $C_k$ é ricopribile da una collezione numerabile di cubi contenuti in $U$ di lato $\delta$, preso uno di questi cubi, diciamo $S \sub U$, é sufficiente mostrare che $f(C_k \cap S)$ ha misura nulla per $k$ sufficientemente grande.

Sia $x \in C_k \cap S$ e $x+h \in S$, scrivendo lo sviluppo in serie di Taylor di $f$ di ordine $k$ e ricordandoci della compattezza di $S$ e della definizione di $C_k$ otteniamo:

$$f(x, h) = f(x) + R(x, h)$$

dove vale la seguente maggiorazione per il resto $R$

$$R(x, h) \lt a ||h||^{k+1}$$

$a \in \R$ é costante e dipende solo da $f$ e $S$. A questo punto possiamo suddividere il cubo $S$ in $r^m$ cubi di lato $\frac{\delta}{r}$, sia $\tilde{S}$ uno di questi cubi e sia $x \in \tilde{S} \cap C_k$, osserviamo come ogni punto di $\tilde{S}$ sia della forma $x+h$ dove

$$||h|| \lt \sqrt{m} \cdot \frac{\delta}{r} = diam(\tilde{S})$$

Dalle diseguaglianze di prima otteniamo

$$||f(x,h) - f(x)|| = ||R(x,h)|| \lt a ||h||^{k+1} \lt a ( \sqrt{m} \frac{\delta}{r} )^{k+1}$$

Che significa che un $diam(f(\tilde{S})) \lt a ( \sqrt{m} \frac{\delta}{r} )^{k+1}$ e che quindi $f(\tilde{S})$ é contenuto in un cubo di lato $\frac{b}{r^{k+1}}$ dove $b = 2a (\sqrt{m} \delta)^{k+1}$.

Questo ragionamento non dipende da una particolare scelta del cubo $\tilde{S}$ e puó essere effettuato per ogni cubo della suddivisione, dunque $f(C_k \cap S)$ é ricopribile da una famiglia di $r^m$ cubi, ognuno di lato $\frac{b}{r^{k+1}}$. Ma allora la somma dei volumi é minore di

$$r^m (\frac{b}{r^{k+1}})^n = b^n r^{m - (k+1)n} \xrightarrow[r \rightarrow \infty]{} 0$$

Che é equivalente ad affermare che $\forall \epsilon \gt 0$ troviamo un $r_0$ sufficientemente grande per cui $\forall r \gt r_0$ la somma dei volumi dei cubi che ricoprono $f(C_k \cap S)$ é inferiore di $\epsilon$, ovvero che $f(C_k \cap S)$ ha misura nulla.

La dimostrazione di questo lemma termina la dimostrazione del teorema di Sard, seguono gli enunciati di alcuni notevoli risultati interpretabili come corollari.

**Observation:** Il gruppo di omotopia $\pi_q(S^n)$ é banale se $q \lt n$

**Proof:** (idea [^gruppo-omotopia]) Basta il teorema Mini-Sard, che usato in un certo modo permette di non considerare un punto da $S^n$ e retrarre (in modo $\mathscr{C}^{\infty}$) tramite una proiezione stereografica ad un aperto di $\R^n$.

**Theorem (Teorema del punto fisso di Brouwer [^hirsch]):** Sia $f: D^n \to D^n$ continua, dove $D^n$ é il disco $n$-dimensionale. Allora $f$ ammette un punto fisso cioé $\exists x_0 \in D^n$ tale che $f(x_0)=x_0$.

**Theorem (Teorema di Whitney [^whitney]):** Sia $M$ una varietá differenziabile $n$-dimensionale, allora essa puó essere realizzata come sottovarietá chiusa di $\R^{2n+1}$ o come sottovarietá immersa di $\R^{2n}$.

Equivalentemente esiste un embedding proprio di $M$ in $\R^{2n+1}$ e una immersione di $\R^{2n}$.

Per esporre il prossimo risultato (fondamentale nella teoria di Morse) occorre dare alcune definizioni.

**Definition (punto critico non degenere):** Sia $f: \R^k \to \R$ una funzione liscia, diciamo che $p \in \R^k$ é un **punto critico non degenere** se é un punto critico (cioé la mappa differenziale indotta ivi si annulla) e se l'Hessiana nel punto é non singolare, ovvero

$$det(H(f)\bigr|_p) = det(\Big(\frac{\partial^2 f}{\partial x_i \partial x_j}\Big)_{i, j}) \not = 0$$

**Definition (funzione di Morse):** Sia $f: \R^k \to \R$ una funzione liscia, diciamo é una **funzione di Morse** se tutti i suoi punti non degeneri.

Si puó mostrare [^lemma-morse] (lemma di Morse) che le funzioni di Morse hanno la proprietá di essere localmente descrivibili come polinomi di secondo grado, ovvero che esiste sempre un cambio di coordinate per cui

$$f(x_1, \dots, x_k) = f(p) + \bold{x} \cdot H(f)\bigr|_p \cdot \bold{x}^t$$

Diagonalizzando la matrice si riesce addirittura a riscrivere la precedente relazione come

$$f(x_1, \dots, x_k) = f(p) + \sum_{i = 1}^k \epsilon_i x_i^2$$

dove $\epsilon_i = \pm 1$.

Il teorema di Sard ci permette di affermare [^teo-morse] che queste (belle) funzioni di Morse sono quasi tutte le funzioni liscie, in termini piú precisi

**Theorem:** Sia $f: M \to \R$ una funzione liscia definita su una varietá differenziabile $k$-dimensionale $M$, tramite le sue carte possiamo definire sempre $M$ la funzione liscia

$$f_a(x_1, \dots, x_k) = f(x_1, \dots, x_k) + \sum_{i=1}^k a_i x_i$$

Allora il sottoinsieme di $\R^k$ degli $a \in \R^k$ tali che $f_a$ non é funzione di Morse ha misura nulla.

---

[^fubini]: V. Guillemin, A. Pollack - Differential Topology (p. 204)
[^gruppo-omotopia]: L. W. Tu, R. Bott - Differential Forms in Algebraic Topology (pp. 214, 215)
[^lemma-morse]: V. Guillemin, A. Pollack - Differential Topology (p. 42)
[^teo-morse]: V. Guillemin, A. Pollack - Differential Topology (p. 43)
[^hirsch]: M. W. Hirsch - A proof of the non-retractability of a cell onto its boundary
[^whitney]: M. Abate, F. Tovena - Geometria Differenziale (pp. 109-115)