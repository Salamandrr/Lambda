let pred=Abs ("n",Abs("f",Abs ("x",
    App (fst,
        App (Var "n",
            App (
                Abs ("p",App ( App (pair,App (snd,Var "p")),App (Var "f",App (snd,Var "p")))),Abs ("t",App (App (Var "t",Var "x"),Var "x"))))))));;

