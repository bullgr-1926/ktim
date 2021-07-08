       id division.
       program-id. pel01.
       author. temerzidis stavros.
       environment division.
       configuration section.
       source-computer. ibm-at.
       object-computer. ibm-at.
       input-output section.
       file-control.
           select optional pelates assign random file-name
           organization indexed
           access dynamic

           record key kodikos
           alternate record key epitheto with duplicates
           alternate record key dieythynsh with duplicates
           alternate record key afm 
           alternate record key telephone with duplicates
           status pel-stat.

       data division.
       file section.
       fd pelates.
       01 pelates-rec.
          02 index-fields.
             03 kodikos pic x(20).
             03 onoma pic x(15).
             03 epitheto pic x(25).
          02 pelates-rest.
             03 dieythynsh pic x(35).
             03 afm pic x(8).
             03 telephone pic x(15).

       working-storage section.
       77 chr-ch pic 9(3) comp-1.

       77 synexeia pic x.
          88 syn-n values "N" "n" "Œ" "¤".
          88 syn-o values "O" "o" "Ž" "¦".

       01  str-var.
           02 bc-line pic x(80) value all "°".
           02 inv-line pic x(80) value all spaces.
           02 rev-line pic x(80) value all "Û".
           02 rev-half pic x(80) value all "Ü".
           02 und-line pic x(80) value all "_".
           02 half-line pic x(80) value all "-".

       01  num-var.
           02 flag pic 9.
           02 flag2 pic x.
           02 answ pic 9.
           02 answ2 pic z.
           02 i pic 9(2).
           02 k pic 9(2).
           02 m pic 9(2).
           02 e pic 999.
           02 l pic 999.
           02 x pic 9999.
           02 y pic 9999.

       01  index-var.
           02 index-count pic 99.
           02 t-code pic x(20).

       01  save-var.
           02 s-code pic x(20).
           02 s-onoma pic x(15).
           02 s-epitheto pic x(25).
           02 s-dieythynsh pic x(35).
           02 s-afm pic x(8).
           02 s-telephone pic x(15).

       01  color-table.
           02  pic x(7) value "black  ".
           02  pic x(7) value "blue   ".
           02  pic x(7) value "green  ".
           02  pic x(7) value "cyan   ".
           02  pic x(7) value "red    ".
           02  pic x(7) value "magenta".
           02  pic x(7) value "brown  ".
           02  pic x(7) value "white  ".
       01  redefines color-table.
           02  c-name pic x(7) occurs 8 times.

       01  control-value.
           02  col-cnt.
               03 pic x(5) value "high,".
               03 pic x(7) value "fcolor=".
               03 fc pic x(7).
               03 pic x(8) value ",bcolor=".
               03 bc pic x(7).

       01  nor-cnt.
           02  pic x(47) value "low,fcolor=white,bcolor=black".

       01 acpt-col.
          02 pic x(47) value "fcolor=black,bcolor=black".

       01  index-table.
           02 pic x(60) value spaces.
       01  redefines index-table.
           02  idx-table pic x(60) occurs 1000 times.

       01  index-lines.
           02 pic x(20) value "09101112131415161718".
       01  redefines index-lines.
           02 idx-ln pic 9(2) occurs 10 times.

       01  index-pos.
           02 kodikos-2 pic x(20).
           02 filler pic x(2) value spaces.
           02 epitheto-2 pic x(25).
           02 filler pic x(2) value spaces.
           02 onoma-2 pic x(15).

       01  file-used.
           02 fpel pic x(12) value "pel.dat".

       01  file-name pic x(29).
       01  fn redefines file-name.
           03 fl-disk pic x(2).
           03 fl-rest pic x(15).
           03 fl-nam pic x(12).

       01  file-status.
           02 pel-stat pic xx.
           02 file-stat pic xx.

       01  invalid-mess.
           02 inval-stat pic xx.
           02 inval-mess pic x(20).
       01  valid-mess.
           02 val-stat pic 9.
           02 val-mess pic x(20).

       01  acpt-cnt pic x(30) value "update,echo,no beep".

       linkage section.
       01  linkages.
           02  paths.
               03  pr-path.
                  04 disk-path pic xx.
                   04 rest-path pic x(15).
                   04 prog pic x(12).
               03  fl-path.
                   04 fl-disk-path pic xx.
                   04 fl-rest-path pic x(15).

       procedure division using linkages.

       begin.
           move fl-disk-path to fl-disk. move fl-rest-path to fl-rest.
           move fpel to fl-nam. open i-o pelates.
       
       begin-2.
           initialize pelates-rec.
           perform code-box.

       a1.
           initialize pelates-rec.
           accept kodikos line 8 position 25 update 
           no beep on exception chr-ch
           evaluate chr-ch
              when 27 perform clean-box go telos
              when 1 go index-code
              when other go a1
           end-evaluate.

           if kodikos = spaces go a1.

       begin-3.
           perform values-box.
           perform find-rec.
           perform save-box-2.

       a2.
           accept epitheto line 8 position 31 prompt "." 
           control acpt-cnt on exception chr-ch
           evaluate chr-ch
              when 27 perform save-box perform clean-box go telos
              when 3 perform save-box perform clean-box go begin-2
              when 5 perform eis
              when 6 perform dia go begin-2
              when 52 perform disp-ln go a6
              when 53 perform disp-ln go a3
              when other go a2
           end-evaluate.
           
           perform disp-ln.

       a3.
           accept onoma line 9 position 31 prompt "." 
           control acpt-cnt on exception chr-ch
           evaluate chr-ch
              when 27 perform save-box perform clean-box go telos
              when 3 perform save-box perform clean-box go begin-2
              when 5 perform eis
              when 6 perform dia go begin-2
              when 52 perform disp-ln go a2
              when 53 perform disp-ln go a4
              when other go a3
           end-evaluate.

           perform disp-ln.       

       a4.
           accept dieythynsh line 10 position 31 prompt "." 
           control acpt-cnt on exception chr-ch     
           evaluate chr-ch
              when 27 perform save-box perform clean-box go telos
              when 3 perform save-box perform clean-box go begin-2
              when 5 perform eis
              when 6 perform dia go begin-2
              when 52 perform disp-ln go a3
              when 53 perform disp-ln go a5
              when other go a4
           end-evaluate.

           perform disp-ln. 

       a5.
           accept afm line 11 position 31 prompt "."  
           control acpt-cnt on exception chr-ch
           evaluate chr-ch
              when 27 perform save-box perform clean-box go telos
              when 3 perform save-box perform clean-box go begin-2
              when 5 perform eis
              when 6 perform dia go begin-2
              when 52 perform disp-ln go a4
              when 53 perform disp-ln go a6
              when other go a5
           end-evaluate.

           perform disp-ln. 

       A6.
           accept telephone line 12 position 31 prompt "."   
           control acpt-cnt on exception chr-ch
           evaluate chr-ch
              when 27 perform save-box perform clean-box go telos
              when 3 perform save-box perform clean-box go begin-2
              when 5 perform eis
              when 6 perform dia go begin-2
              when 52 perform disp-ln go a5
              when 53 perform disp-ln go a2
              when other go a6
           end-evaluate.

           perform disp-ln.
           go a6.

       eis.
           write pelates-rec invalid

                 rewrite pelates-rec invalid

                         move pel-stat to file-stat 
                         perform invalid-info

                 not invalid move 2 to val-stat perform valid-info

                 end-rewrite

           not invalid move 1 to val-stat perform valid-info
           end-write.

           perform telos. perform begin. perform save-box-2.

       dia.
           delete pelates invalid move pel-stat to file-stat
                                  perform invalid-info

                      not invalid move 3 to val-stat 
                                  perform valid-info

           end-delete.

           initialize pelates-rec.

           perform disp-ln. perform telos. perform begin. 
           perform clean-box.

       telos.
           close pelates.

       telos-1.
           goback.

       find-rec.
           read pelates invalid move pel-stat to file-stat
                                perform invalid-info
                                perform disp-ln

                    not invalid move 4 to val-stat
                                perform valid-info
                                perform disp-info
                                perform disp-ln

           end-read.

       disp-ln.
           move c-name(6) to bc. move c-name(8) to fc.

           if epitheto = spaces
           display und-line line 8 position 31 size 25 control col-cnt.

           if onoma = spaces 
           display und-line line 9 position 31 size 15 control col-cnt.

           if dieythynsh = spaces 
           display und-line line 10 position 31 size 35 control col-cnt.

           if afm = spaces
           display und-line line 11 position 31 size 8 control col-cnt.

           if telephone = spaces
           display und-line line 12 position 31 size 15 control col-cnt.

       disp-info.
           move c-name(6) to bc. move c-name(8) to fc.
           display epitheto line 8 position 31 control col-cnt
                   onoma position 31
                   dieythynsh position 31
                   afm position 31
                   telephone position 31.

       index-code.
           perform clean-table.

           initialize i. initialize flag. move 20 to i.

           perform varying x from 1 by 1 until x = 20 or flag = 1
           if kodikos (i:1) = space subtract 1 from i
           else move 1 to flag
           end-if 
           end-perform.

           move kodikos to t-code. move 1 to index-count.
           start pelates key not < kodikos invalid key go a1.

           if kodikos = spaces go index-code-3.

       index-code-2.
           read pelates next at end go index-disp-1. 

           if kodikos (1:i) > T-code (1:i) go index-disp-1.

           move index-fields to idx-table (index-count).

           add 1 to index-count.

           go index-code-2.

       index-code-3.
           read pelates next at end go index-disp-1. 

           move index-fields to idx-table (index-count).

           add 1 to index-count.

           go index-code-3.

       index-disp-1.
           if index-count not > 1 initialize pelates-rec go begin-2.

           move 1 to y. move 1 to k. subtract 1 from index-count.
           move 1 to e. move 1 to m.

           if index-count < 10 move 1 to l 

           else compute l rounded = ( index-count / 10) + .5

           end-if.
           
           perform index-box.

       index-disp-2.
           move c-name(3) to bc. move c-name(7) to fc.

           move idx-table(y) to index-fields.

           move kodikos to kodikos-2. move onoma to onoma-2.
           move epitheto to epitheto-2.

           if index-pos = spaces move 1 to k m go index-select.

           display index-pos line idx-ln(k) position 8 
                                control col-cnt.

           if k = 10 compute y = ( e * 10 ) - 9 move 1 to k 
                     go index-select.

           if y = index-count compute y = ( e * 10 ) - 9 move 1 to k
                              go index-select.
                  
           add 1 to y k m. 
                     
           go index-disp-2.

       index-select.
           move c-name(7) to bc. move c-name(3) to fc.

           move idx-table(y) to index-fields.

           move kodikos to kodikos-2. move onoma to onoma-2.
           move epitheto to epitheto-2.
           
           display index-pos line idx-ln(k) position 8 
                             control col-cnt.

           accept flag line 24 position 80 no beep control acpt-col
                               on exception chr-ch

           move c-name(3) to bc move c-name(7) to fc

           display index-pos line idx-ln(k) position 8 
                             control col-cnt

            evaluate chr-ch
              when 52 if y > ( e * 10 ) - 9 subtract 1 from y k

                      else move m to k 
                           compute y = ( ( e * 10 ) - 9 ) + ( m - 1 ) 
 
                      end-if go index-select

              when 53 if y < ( ( e * 10 ) - 9 ) + ( m - 1 ) add 1 to y k

                      else compute y = ( e * 10 ) - 9 move 1 to k
  
                      end-if go index-select
                      

              when 67  if e > 1 subtract 1 from e perform page-box 
                         perform index-clean
                         compute y = ( e * 10 ) - 9 move 1 to k m
                         go index-disp-2 

                      else go index-select

                      end-if

              when 68 if e < l add 1 to e perform page-box 
                         perform index-clean
                         compute y = ( e * 10 ) - 9 move 1 to k m 
                         go index-disp-2

                      else go index-select

                      end-if

              when 27 perform clean-box go begin-2

              when other go index-select

           end-evaluate.

           if flag = 0 and index-pos not = spaces

                      initialize pelates-rec

                       move idx-table(y) to index-fields

                       move kodikos to kodikos-2 move onoma to onoma-2
                       move epitheto to epitheto-2

                       move c-name(2) to bc move c-name(7) to fc

                       display index-pos line idx-ln(k) position 8 
                                            control col-cnt

                       perform clean-box go begin-3

           else go index-select

           end-if. 
       
       invalid-info.
           evaluate file-stat
              when "10"  move "   ’â¢¦ª ˜¨®œå¦¬   " to inval-mess
                                          perform invalid-message

              when "22"  move "   ƒ §¢ã œšš¨˜­ã   " to inval-mess
               perform message-error-box thru message-error-box-2

              when "23"  move "    Œâ˜ œšš¨˜­ã    " to inval-mess
                                          perform invalid-message

              when "24"  move "  ‚â£ ©œ «¦ ˜¨®œå¦ " to inval-mess
               perform message-error-box thru message-error-box-2

              when "30"  move "  ¢á™ž ©«¦ ›å©¡¦  " to inval-mess
               perform message-error-box thru message-error-box-2

              when "98"  move "•˜¢˜©£â¤¦ œ¬¨œ«ã¨ ¦" to inval-mess
               perform message-error-box thru message-error-box-2

              when other move "€¤˜­¦¨á ©œ SYS.OP. " to inval-mess                 
               perform message-error-box thru message-error-box-2

           end-evaluate.

       valid-info.
           evaluate val-stat
              when 1  move " † œ ©˜šàšã âš ¤œ  " to val-mess

              when 2  move " H ˜¢¢˜šã âš ¤œ    " to val-mess

              when 3  move " † › ˜š¨˜­ã âš ¤œ  " to val-mess

              when 4  move " ‹œ«˜™¦¢ã ©«¦ ®œåà¤" to val-mess.

           move 0 to val-stat.

           perform valid-message.

       invalid-message.
           perform message-box.
           display inval-mess line 19 position 10 control col-cnt.
           move c-name(6) to bc. move c-name(8) to fc.
           display " " line 16 position 69 control col-cnt.

       valid-message.
           perform message-box.
           display val-mess line 19 position 10 control col-cnt.
           move c-name(6) to bc. move c-name(8) to fc.
           display " " line 16 position 69 control col-cnt.

       code-box.
           move c-name(6) to bc. move c-name(8) to fc.
           display "ÚÄÄÄÄÄÄÄ ‰à› ¡æª œ¢á«ž ÄÄÄÄÄÄÄ¿" 
                   line 7 position 15 control col-cnt
                   "³ ¡à› ¡æª                      ³"
                   line 8 position 15
                   "³                              ³" 
                   line 9 position 15
                   "³  ESC           F1            ³" 
                   line 10 position 15
                   "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" 
                   line 11 position 15

                   bc-line line 12 position 17 size 32 
                           control nor-cnt.

           perform varying x from 8 by 1 until x > 11
           display bc-line line x position 47 size 2 control nor-cnt
           end-perform.

           move c-name(3) to bc. move c-name(8) to fc.
           display inv-line line 8 position 25 size 20 reverse 
                   control nor-cnt
                   " ˜¡ç¨à©ž " line 10 position 21 control col-cnt
                   " œ¬¨œ«ã¨ ¦ " line 10 position 34.

       values-box.
           move c-name(6) to bc. move c-name(8) to fc.
           display 
           "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ „ ©˜šàšã ‘«¦ ®œåà¤ ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
                   line 5 position 12 control col-cnt.

           perform varying x from 6 by 1 until x > 16
           display 
           "³                                                         ³"
                   line x position 12
           end-perform.
           display 
           "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ"
                   line 17 position 12

                   "‰à› ¡æª:" line 6 position 15
                   "„§åŸœ«¦....:" line 8 position 19
                   "î¤¦£˜......:" position 19
                   "ƒ œçŸ¬¤©ž..:" position 19
                   "€.”.‹......:" position 19
                   "’ž¢â­à¤¦...:" position 19
                   "ESC" line 15 position 15
                   "F3" line 15 position 29
                   "F5" line 15 position 42
                   "F6" line 15 position 57.

           move c-name(3) to bc. move c-name(8) to fc.
           display kodikos line 6 position 23 control col-cnt
                   "€¡ç¨à©ž" line 15 position 18
                   "‰à› ¡æª" line 15 position 31
                   "„ ©˜šàšã" line 15 position 45
                   "ƒ ˜š¨˜­ã" line 15 position 60.

           display bc-line line 18 position 14 size 59 control nor-cnt
           perform varying x from 6 by 1 until x > 17
           display bc-line line x position 71 size 2 control nor-cnt
           end-perform.

       index-box.
           move c-name(3) to bc. move c-name(8) to fc.
           display "ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ" 
                   line 6 position 6 control col-cnt

                   "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»" 
                   line 6 position 41.

           perform varying x from 7 by 1 until x > 21
           display "º                                    " 
                   line x position 6
                   "                                º" 
                   line x position 41
           end-perform.   

           display "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ" 
                   line 21 position 6 control col-cnt

                  "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼" 
                  line 21 position 41.

           display bc-line line 22 position 8 low size 68
                           control nor-cnt.

           perform varying x from 7 by 1 until x > 21
           display bc-line line x position 74 low size 2 
           end-perform.

           move c-name(3) to bc. move c-name(8) to fc.
           display "‰à› ¡æª" line 7 position 8 control col-cnt
                   "„§åŸœ«¦" line 7 position 30
                   "î¤¦£˜" line 7 position 57
                   half-line line 8 position 8 size 20
                   half-line line 8 position 30 size 25
                   half-line line 8 position 57 size 15
                   "PGUP-PGDN" line 20 position 8
                   "ESC" line 20 position 31
                   "" line 20 position 43
                   "ENTER " line 20 position 60.

           move c-name(2) to bc. move c-name(5) to fc.
           display "€¢¢.©œ¢å›˜ª" line 20 position 17 control col-cnt
                   "€¡ç¨à©ž" line 20 position 34
                   "‰å¤ž©ž £§á¨˜ª" line 20 position 45
                   "„§ ¢¦šã" line 20 position 65.

           perform page-box.

       page-box.
           move c-name(2) to bc. move c-name(7) to fc.
           display "‘œ¢å›˜    /   " line 19 position 7 control col-cnt
                   e line 19 position 14
                   l line 19 position 18.

       save-box.
           if pelates-rec not = save-var

              perform error-box

              display "   (!) ¨¦©¦®ã        " line 7 position 29 
                                             control col-cnt
                      "----------------------" line 8 position 29
                      "     Œ˜ ©àŸ¦ç¤ ¦      " line 10 position 29
                      "   ˜¢¢˜šâª;(N/O):     " line 12 position 29

              move "n" to synexeia

              accept synexeia line 12 position 46 update tab blink

              evaluate true
                 when syn-n perform eis
                 when syn-o continue
                 when other go save-box
              end-evaluate
            
           end-if.

       save-box-2.
           initialize save-var.
           move pelates-rec to save-var.

       clean-table.
           perform varying x from 1 by 1 until x > 1000
           initialize idx-table(x)
           end-perform.

       clean-box.
           move c-name(4) to bc. move c-name(2) to fc.

           perform varying x from 5 by 1 until x > 22 
           display bc-line line x position 5 size 71
                           control col-cnt
           end-perform.

       index-clean.
           move c-name(3) to bc. move c-name(3) to fc.
           perform varying x from 9 by 1 until x > 18
           display inv-line line x position 7 size 66 control col-cnt
           end-perform.

       error-box.
           move c-name(2) to bc. move c-name(3) to fc.
           display "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" line 6 position 28 
                   control col-cnt
                   "³                      ³" line 7 position 28
                   "³                      ³" line 8 position 28
                   "³                      ³" line 9 position 28
                   "³                      ³" line 10 position 28
                   "³                      ³" line 11 position 28                                         
                   "³                      ³" line 12 position 28
                   "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" line 13 position 28

           inv-line line 14 position 30 size 24 control nor-cnt.

           perform varying x from 7 by 1 until x > 13
           display inv-line line x position 52 size 2 control nor-cnt
           end-perform.

           if epitheto (22:2) = spaces
                  display und-line line 8 position 52 size 2 
                                   control nor-cnt

           else display epitheto (22:2) line 8 position 52 
                                   control nor-cnt
           end-if.

           if dieythynsh (22:2) = spaces
                  display und-line line 10 position 52 size 2 
                                   control nor-cnt

           else display dieythynsh (22:2) line 10 position 52 
                                   control nor-cnt
           end-if.

       message-box.
           move c-name(1) to bc. move c-name(8) to fc.
           display "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" line 18 position 14
                   control col-cnt.

           move c-name(2) to bc. move c-name(3) to fc.
           display "ÚÄÄÄÄÄÄÄÄ" line 18 position 5 control col-cnt
                   "³                            ³" line 19 position 5
                   control col-cnt
                   "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" line 20 position 5

                   bc-line line 21 position 7 size 30 control nor-cnt.

           perform varying x from 19 by 1 until x > 20
           display bc-line line x position 35 size 2 control nor-cnt
           end-perform.
         
       
       message-error-box.
           perform error-box.
           display  "<<                  >>" line 7 position 29
                     inval-mess line 7 position 30
                    "   ‚ ˜ §œ¨ ©©æ«œ¨œª   " line 8 position 29
                    "     §¢ž¨¦­¦¨åœª      " line 9 position 29
                    "    ˜¤˜«¨â¥«œ «¦      " line 10 position 29
                    "  §¨æš¨˜££˜ ™¦ãŸœ ˜ª  " line 11 position 29
                    "     ˜§æ «¦ ‹„ŒŽ“     " line 12 position 29.
                    
           accept flag line 24 position 80 control acpt-col.
           perform clean-box.

       message-error-box-2.
           exit.

       message-error-box-3.
           go begin-2.


           



