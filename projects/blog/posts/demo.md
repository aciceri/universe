---
title: Demo post
tags: [demo, org, org-mode, carousel]
date: 2021-02-02
language: it
---

Si può inserire il testo in **grassetto**, *corsivo* e ***grassetto e corsivo***, inoltre il testo può
essere testo `inline` o `verbatim`, <u>sottolineato</u>, ~~barrato~~ e a ped~ice~ o ap^ice^.

## Le liste sono testo

Questa è una lista non ordinata...

- Un elemento
- Un altro elemento
- Un altro ancora

...mentre questa qui è ordinata (e anche innestata).

1. Primo
2. Secondo
   - Prima parte
   - Seconda parte
3. Terzo
4. Quarto

### TODO ciao
### DONE mondo
### DONE finito

Inserire [link](https://google.it) è facile, si può anche omettere il nome: <https://google.it>.

Anche cambiare paragrafo è semplice, passiamo ora alle note e agli avvertimenti.

> **Note:** Questa è una nota, che bella...

> **Warning:** Stai attento, questo ~~non~~ è un avvertimento!

Ed ecco una citazione.

> Considerate la vostra semenza:
> fatti non foste a viver come bruti,
> ma per seguir virtute e canoscenza

È possibile mantere la formattazione in questo modo.

```
Considerate la vostra semenza:
fatti non foste a viver come bruti,
ma per seguir virtute e canoscenza
```

# Sorgenti

Si possono includere sorgenti senza evidenziazione della sintassi.

```
+------+.      +------+       +------+       +------+      .+------+
|`.    | `.    |\     |\      |      |      /|     /|    .' |    .'|
|  `+--+---+   | +----+-+     +------+     +-+----+ |   +---+--+'  |
|   |  |   |   | |    | |     |      |     | |    | |   |   |  |   |
+---+--+.  |   +-+----+ |     +------+     | +----+-+   |  .+--+---+
 `. |    `.|    \|     \|     |      |     |/     |/    |.'    | .'
   `+------+     +------+     +------+     +------+     +------+'
```

Altrimenti anche codice specifico di linguaggi di programmazione, con l'opzione per visualizzare il
numero di riga.

```scheme
(define fail
  (lambda ()
    (error "Amb tree exhausted")))

(define-syntax amb
  (syntax-rules ()
    ((AMB) (FAIL))                      ; Two shortcuts.
    ((AMB expression) expression)

    ((AMB expression ...)
     (LET ((FAIL-SAVE FAIL))
       ((CALL-WITH-CURRENT-CONTINUATION ; Capture a continuation to
	  (LAMBDA (K-SUCCESS)           ;   which we return possibles.
	    (CALL-WITH-CURRENT-CONTINUATION
	      (LAMBDA (K-FAILURE)       ; K-FAILURE will try the next
		(SET! FAIL K-FAILURE)   ;   possible expression.
		(K-SUCCESS              ; Note that the expression is
		 (LAMBDA ()             ;   evaluated in tail position
		   expression))))       ;   with respect to AMB.
	    ...
	    (SET! FAIL FAIL-SAVE)       ; Finally, if this is reached,
	    FAIL-SAVE)))))))            ;   we restore the saved FAIL.
```

# Tabelle

Si possono inserire tabelle, con allineamenti differenti colonna per colonna e una meravigliosa
idascalia.

| Questa       |   è l'    | intestazione |
|:-------------|:---------:|-------------:|
| Del          | Contenuto |       a caso |
| Non       so |   cosa    |     scrivere |

*Ma che bella questa tabella*

# Immagini

Includere immagini è semplice (didascalia opzionale):

![Bel paesaggio](/images/wallpaper.jpg)
*Bel paesaggio*

# Video

Ecco l'eversione della sfera[^eversione-sfera], ovvero come risvoltarla.

{video:sphere-eversion.mp4}

# Matematica

Si può scrivere matematica inline, per esempio lo sapevi che $\nexists a,b,c \in \mathbb{N}$
tali che

$$a^n+^n=c^n \forall n$$

dove $n \in \mathbb{N}$.

**Definition:** Ciao

**Theorem:** If an integer $n$ is greater than 2, then the equation $a^n + b^n = c^n$
has no solutions in non-zero integers $a$, $b$, and $c$.

**Proposition:** Proposizione

**Lemma:** Lemma

**Proof:** I have a truly *marvelous* proof of this proposition that this margin is too
narrow to contain.

# Youtube

Nonostante preferirei evitare di appoggiarmi troppo a servizi esterni di cui non approvo le
politiche ho predisposto una macro per includere video a [Youtube](https://youtube.com).

{youtube:dQw4w9WgXcQ}

# Aciinema

Personalmente mi piace molto [Asciinema](https://asciinema.org) e in generale l'idea di non dover usare gif animate per
raggiungere scopi analoghi. Mi piace meno l'idea di dovermi affidare anche a loro per l'hosting dei
miei cast, pertanto ho presisposto una macro per includere i cast hostandoli direttamente in questo
spazio.

{asciinema:neofetch}

# Proviamo Iosevka Comfy

```haskell
module Amb (AmbT, Amb, amb, cut, runAmbT, runAmb) where

import Control.Monad.Cont
import Control.Monad.State
import Control.Monad.Identity

newtype AmbT r m a = AmbT { unAmbT :: StateT [AmbT r m r] (ContT r m) a }
type Amb r = AmbT r Identity

instance MonadTrans (AmbT r) where
    lift = AmbT . lift . lift

instance (Monad m) => Monad (AmbT r m) where
    AmbT a >>= b = AmbT $ a >>= unAmbT . b
    return = AmbT . return

backtrack :: (Monad m) => AmbT r m a
backtrack = do xss <- AmbT get
               case xss of
                 [] -> fail "amb tree exhausted"
                 (f:xs) -> do AmbT $ put xs; f; return undefined

addPoint :: (Monad m) => (() -> AmbT r m r) -> AmbT r m ()
addPoint x = AmbT $ modify (x () :)

amb :: (Monad m) => [a] -> AmbT r m a
amb []     = backtrack
amb (x:xs) = ambCC $ \exit -> do
               ambCC $ \k -> addPoint k >> exit x
               amb xs
    where ambCC f = AmbT $ callCC $ \k -> unAmbT $ f $ AmbT . k

cut :: (Monad m) => AmbT r m ()
cut = AmbT $ put []

runAmbT :: (Monad m) => AmbT r m r -> m r
runAmbT (AmbT a) = runContT (evalStateT a []) return

runAmb :: Amb r r -> r
runAmb = runIdentity . runAmbT
```

[^eversione-sfera]: https://www.youtube.com/watch?v=iynrV-3I9CY
