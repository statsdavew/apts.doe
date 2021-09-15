## APTS Design of Experiments 2021-22
## Practical Sheet 1


## 2 blocking (fractional) factorial designs

library(FrF2)

2^10

2^5

## (a)
q2a_u <- FrF2(32, 5, randomize = F)
q2a_b <- FrF2(32, 5, blocks = 4, randomize = F, alias.info = 3)
design.info(q2a_b)$aliased.with.blocks


## (c)
q2c_u <- FrF2(16, 5, generator = list(c(1 ,2 ,3, 4)), randomize = F,, alias.info = 3)
## 5 = 1234 (E = ABCD); I = 12345 (I = ABCDE)
design.info(q2c_u)
q2c_b <- FrF2(16, 5, generator = list(c(1 ,2 ,3)), blocks = list(c(1, 2, 3), c(1, 4)), randomize = F, alias.info = 3, alias.block.2fis = T)
design.info(q2c_b)$aliased.with.blocks

123 = 45, 234 = 15, 14 = 235

4 = 2^2 blocks
4 - 1 = 3 comparisons

ABC = DE, BCD = AE, AD = BCE

I = ABCDE

E = ABCD
I = ABCDE

A = BCDE
B = ACDE
C = ABDE
D = ABCE
E = ABCD


E = ABC
I = ABCE

ABC = E


16 = 2^4
