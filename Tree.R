library(DiagrammeR)

graph <- DiagrammeR("
           graph TB
           A(( ))
           B
           C
           D
           E
           F
           G
           H
           I
           J
           K
           L
           M
           N
           O
           P
           Q
           R
           S
           T
           V
           W
           X
           Y
           Z
           AA
           AB
           A---B(( ))
           A---C(( ))
           B---D(( )) 
           B---E(( )) 
           D---F((1)) 
           D---O(( ))
           O---R(( ))
           O---AB(( ))
           R---P((2)) 
           R---Q(( ))
           Q---S((3)) 
           Q---T((4)) 
           E---G((5)) 
           E---H(( )) 
           H---Z((6))
           H---AA((7))
           C---I(( )) 
           C---J(( )) 
           I---K(( )) 
           K---X((8)) 
           K---Y((9)) 
           I---W((10))  
           J---L((11))  
           J---M(( )) 
           M---N((12))  
           M---V((13)) 
           
           linkStyle 8 fill:none , stroke:#D80000  ,stroke-width:2px;
           linkStyle 9 fill:none , stroke:#D80000  ,stroke-width:2px;
           linkStyle 10 fill:none , stroke:#D80000 ,stroke-width:2px;
           linkStyle default fill:none, stroke:#000 ,stroke-width:2px;
           style A fill:#fff, stroke:#2773ae, stroke-width:2px, height:5px, width:5;
           style B fill:#fff, stroke:#2773ae, stroke-width:2px;
           style C fill:#fff, stroke:#2773ae, stroke-width:2px;
           style D fill:#fff, stroke:#2773ae, stroke-width:2px;
           style E fill:#fff, stroke:#2773ae, stroke-width:2px;
           style F fill:#fff, stroke:#2773ae, stroke-width:2px;
           style G fill:#fff, stroke:#2773ae, stroke-width:2px;
           style H fill:#fff, stroke:#2773ae, stroke-width:2px;
           style I fill:#fff, stroke:#2773ae, stroke-width:2px;
           style J fill:#fff, stroke:#2773ae, stroke-width:2px;
           style K fill:#fff, stroke:#2773ae, stroke-width:2px;
           style L fill:#fff, stroke:#2773ae, stroke-width:2px;
           style M fill:#fff, stroke:#2773ae, stroke-width:2px;
           style N fill:#fff, stroke:#2773ae, stroke-width:2px;
           style O fill:#fff, stroke:#2773ae, stroke-width:2px;
           style P fill:#fff, stroke:#2773ae, stroke-width:2px;
           style Q fill:#fff, stroke:#2773ae, stroke-width:2px;
           style R fill:#fff, stroke:#2773ae, stroke-width:2px;
           style S fill:#fff, stroke:#2773ae, stroke-width:2px;
           style T fill:#fff, stroke:#2773ae, stroke-width:2px;
           style V fill:#fff, stroke:#2773ae, stroke-width:2px;
           style V fill:#fff, stroke:#2773ae, stroke-width:2px;
           style W fill:#fff, stroke:#2773ae, stroke-width:2px;
           style X fill:#fff, stroke:#2773ae, stroke-width:2px;
           style Y fill:#fff, stroke:#2773ae, stroke-width:2px;
           style Z fill:#fff, stroke:#2773ae, stroke-width:2px;
           style AA fill:#fff, stroke:#2773ae, stroke-width:2px;
           style AB fill:#fff, stroke:#2773ae, stroke-width:2px;
           ")
graph
