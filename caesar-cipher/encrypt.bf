START: (*0; 0; 0; 0; 0)

GET INPUT COUNT
>,                                          (0; *COUNT char; 0; 0; 0)
<++++++                                     (*6; COUNT char; 0; 0; 0)
[>--------<-]                               (*0; COUNT dec; 0; 0; 0)

CONVERT dash TO colon AND PRINT
,+++++++++++++.[-]>                         (*0; COUNT; 0; 0; 0)

MOVE COUNT
[>>>+<<<-]>>>                               (0; 0; 0; 0; *COUNT)

DECRYPT COUNT CHARACTERS AND PRINT
[<<<,                                       (0; *INPUT; 0; 0; *COUNT)
    >>+++++[<+++++>-]<+                     (0; INPUT; *26; 0; COUNT)
    <++++                                   (0; *INPUT plus 4; 26; 0; COUNT)
    [>->+<[>]>[<+>-]<<[<]>-]>[-]            (0; 0; *0; MOD; COUNT)
    <++++++++[<++++++++++++>-]<+            (*97; 0; 0; MOD; COUNT)
    [>>>+<<<-]                              (*0; 0; 0; MOD plus 97; COUNT)
    >>>.[-]                                 (0; 0; 0; *0; COUNT)
>-]                                         (0; 0; 0; 0; *0)

PRINT NEWLINE
>++[<+++++>-]<.                             (0; 0; 0; 0; *10)

FINAL: (0; 0; 0; 0; *10)
