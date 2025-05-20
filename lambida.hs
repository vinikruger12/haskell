{-1-}
{-
a) a b c d -> (a b) c d -> ((a b) c) d - > (((a b) c ) d)

b)
λq.λi.q -> λq.(λi.q) -> (λq.(λi.q))

c) λx.λy.λz.x z (y z) -> λx.λy.λz.(x z (y z)) -> λx.λy.(λz.(x z (y z)))
    -> λx.(λy.(λz.(x z (y z)))) -> (λx.(λy.(λz.(x z (y z)))))

-}

{-2-}
    {-
    a) λs.s z λq.s q -> as variáveis  'z' estão livres.

    b) (λs.s z) λq.w λw.w q z s -> as variáveis 'z' e 'w' e 's' estão livres.

    c) (λs.s) (λq.q s) -> as variáves livres são 's'.

    d) λz.((λs.s q) (λq.q z)) λz.z z -> as variáves livres são a 'q'.

-}

{-3-}
{-



-}


