       id division.
       program-id. pel02.
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
           alternate record key afm with duplicates
           alternate record key telephone with duplicates
           status pel-stat.

           select pel-prt assign print "printer".

           select optional word assign random file-name
           organization indexed
           access dynamic
           
           record key word-num.

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

       fd pel-prt label records omitted.
       01 prt-rec pic x(80).

       fd word.
       01 word-rec.
          02 word-num pic x(10).
          02 word-var pic x(1224).

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
           02 t-code1 pic x(20).
           02 t-code2 pic x(20).

       01  word-var.
           02 ctab pic 99.
           02 editor-count pic 99.

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

       01  editor-lines.
           02 pic x(34) value "0405060708091011121314151617181920".
       01  redefines editor-lines.
           02 ed-ln pic 9(2) occurs 17 times.  

       01  editor-vars.
           02 pic x(1224).
       01  redefines editor-vars.
           02 ed-var pic x(72) occurs 17 times.

       01  file-used.
           02 fpel pic x(12) value "pel.dat".
           02 fpel-2 pic x(12) value "word.dat".

       01  file-name pic x(29).
       01  fn redefines file-name.
           03 fl-disk pic x(2).
           03 fl-rest pic x(15).
           03 fl-nam pic x(12).

       01  file-status.
           02 pel-stat pic xx.
           02 file-stat pic xx.
        
       01  prt-1.
           02 filler pic x(2) value spaces.
           02 today-date pic x(8).
           02 today-day pic xx. 
           02 filler pic x(12) value spaces.
           02 filler pic x(32) value "‰ € ’ € ‘ ’ € ‘ †   „ Š € ’ — Œ".

       01  prt-2.
           02 filler pic x(80) value all ".".

       01  prt-3.
           02 filler pic x(5) value spaces.
           02 filler pic x(7) value "‰à› ¡æª".
           02 filler pic x(18) value spaces.
           02 filler pic x(7) value "„§åŸœ«¦".
           02 filler pic x(23) value spaces.
           02 filler pic x(5) value "î¤¦£˜".

       01  prt-4.
           02 filler pic x(5) value spaces.
           02 filler pic x(20) value all "-".
           02 filler pic x(5) value spaces.
           02 filler pic x(25) value all "-".
           02 filler pic x(5) value spaces.
           02 filler pic x(15) value all "-".

       01  prt-5.
           02 filler pic x(5) value spaces.
           02 kodikos-3 pic x(20).
           02 filler pic x(5) value spaces.
           02 epitheto-3 pic x(25).
           02 filler pic x(5) value spaces.
           02 onoma-3 pic x(15).
 
       01  prt-6.
           02 filler pic x(13) value "BULL THE BEST" .

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
           02  today-day pic xx.
           02 today-date.
               03 t-dd pic 99. 03 t-mm pic 99. 03 t-yy pic 99.
           02  ed-date.
               03 e-dd pic zz/. 03 e-mm pic zz/. 03 e-yy pic zz.

       procedure division using linkages.

       begin.
           move fl-disk-path to fl-disk. move fl-rest-path to fl-rest.
           move fpel to fl-nam. open i-o pelates.

       begin-2.
           initialize pelates-rec.
           initialize t-code1 t-code2.
           perform code-box.

       condition-1.       
           accept t-code1 line 9 position 31 update 
           no beep on exception chr-ch
           evaluate chr-ch
             when 27 perform clean-box go telos-2
             when 53 go condition-2
             when 1 move t-code1 to t-code move 1 to flag2 go index-code
             when other go condition-1
           end-evaluate.

       condition-2.       
           accept t-code2 line 11 position 31 update 
           no beep on exception chr-ch
           evaluate chr-ch
             when 27 perform clean-box go telos-2
             when 52 go condition-1
             when 1 move t-code2 to t-code move 2 to flag2 go index-code
             when other go condition-2
           end-evaluate.

           if t-code1 > t-code2 
              initialize t-code1 t-code2 go condition-1.

           move t-code1 to t-code.
           go edit-box.

       print-out.
           perform rec-begin.
           if kodikos = spaces perform header-prt go print-out-3.

       print-out-2.
           read pelates next at end perform header-prt go print-out-4. 

           if kodikos (1:i) > T-code2 (1:i) perform header-prt
                                              go print-out-4.

           move index-fields to idx-table (index-count).

           add 1 to index-count.

           go print-out-2.

       print-out-3.
           read pelates next at end write prt-rec from prt-6 
                                     perform telos go begin-2.

           move kodikos to kodikos-3 move epitheto to epitheto-3
           move onoma to onoma-3.
 
           write prt-rec from prt-5.

           go print-out-3.

       print-out-4.
           move 1 to y. subtract 1 from index-count.

       print-out-4b.           
           move idx-table(y) to index-fields.

           read pelates invalid perform telos go begin-2.

           move kodikos to kodikos-3 move epitheto to epitheto-3
           move onoma to onoma-3.

           write prt-rec from prt-5.

           add 1 to y. if y > index-count perform telos go begin-2.
           go print-out-4b.

       header-prt.
           open extend pel-prt.
           write prt-rec from prt-1.
           write prt-rec from prt-2. 
           write prt-rec from prt-3 after 3.
           write prt-rec from prt-4.

       index-code.       
           perform rec-begin.
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

                       perform clean-box perform code-box 
                       go cond-disp 

           else go index-select

           end-if. 

       rec-begin.
           perform clean-table.

           initialize i. initialize flag. move 20 to i.

           perform varying x from 1 by 1 until x = 20 or flag = 1
           if t-code (i:1) = space subtract 1 from i
           else move 1 to flag
           end-if 
           end-perform.

           move t-code to kodikos. move 1 to index-count.
           start pelates key not < kodikos invalid key perform brk.

       cond-disp.
           evaluate flag2
               when 1 move kodikos to t-code1 
                           display t-code1 line 9 position 31
                                   t-code2 line 11 position 31 
                                   go condition-1

               when 2 move kodikos to t-code2 
                           display t-code1 line 9 position 31
                                   t-code2 line 11 position 31 
                                   go condition-2
           end-evaluate.

       code-box.
           move c-name(6) to bc. move c-name(8) to fc.
           display "ÚÄÄÄÄÄÄÄÄ ‘¬¤Ÿã¡ž „¡«ç§à©žª ÄÄÄÄÄÄÄÄ¿" 
                   line 7 position 15 control col-cnt
                   "³                                   ³"
                   line 8 position 15
                   "³           €§æ                     ³"
                   line 9 position 15
                   "³ ‰à› ¡æª                           ³" 
                   line 10 position 15
                   "³           „éª                     ³" 
                   line 11 position 15
                   "³                                   ³"
                   line 12 position 15
                   "³  ESC             F1               ³" 
                   line 13 position 15
                   "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" 
                   line 14 position 15

                   bc-line line 15 position 17 size 37 
                           control nor-cnt.

           perform varying x from 8 by 1 until x > 14
           display bc-line line x position 52 size 2 control nor-cnt
           end-perform.

           move c-name(3) to bc. move c-name(8) to fc. 
           display " ˜¡ç¨à©ž " line 13 position 22 control col-cnt
                   " œ¬¨œ«ã¨ ¦ " line 13 position 37.

           move c-name(8) to bc. move c-name(8) to fc.
           display inv-line line 9 position 31 size 20  
                   control col-cnt
                   inv-line line 11 position 31 size 20.

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

       edit-box.
           move c-name(6) to bc. move c-name(8) to fc.
           display "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" 
                   line 17 position 25 control col-cnt
                   "³  ‹œ œ¡«ç§à©ž ¡œ £â¤¦¬; (N/O):   ³"
                   line 18 position 25
                   "³                                 ³"
                   line 19 position 25
                   "³ ESC           F2                ³" 
                   line 20 position 25
                   "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" 
                   line 21 position 25

                   bc-line line 22 position 27 size 35 
                           control nor-cnt.

           perform varying x from 18 by 1 until x > 21
           display bc-line line x position 60 size 2 control nor-cnt
           end-perform.

           move c-name(3) to bc. move c-name(8) to fc. 
           display " ˜¡ç¨à©ž " line 20 position 30 control col-cnt
                   " œ§œ¥/¡œ £â¤¦¬ " line 20 position 43.

           move c-name(6) to bc. move c-name(8) to fc.
           move "n" to synexeia.

           accept synexeia line 18 position 55 update tab blink 
                 control col-cnt
                 on exception chr-ch
           evaluate chr-ch
             when 27 move c-name(4) to bc move c-name(2) to fc 

                     perform varying x from 17 by 1 until x > 22
                        display bc-line line x position 25 size 37
                                control col-cnt
                     end-perform

                     go condition-1
                      
             when 2 go editor 

             when other go edit-box
           end-evaluate.
 
             evaluate true
                 when syn-n move 1 to flag
                 when syn-o move 0 to flag
                 when other go edit-box
              end-evaluate.
           go print-out.

       check-code.
           move c-name(2) to bc move c-name(4) to fc.

           display "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" line 17 position 10
                                                control col-cnt
                   "³                       ³" line 18 position 10
                   "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" line 19 position 10
                   "€›ç¤˜«ž ‘¬¤Ÿã¡ž" line 18 position 15 blink

                   bc-line line 20 position 12 size 25 
                            control nor-cnt.

           perform varying x from 18 by 1 until x > 19
             display bc-line line x position 35 size 2 control nor-cnt
           end-perform.

           accept flag2 line 24 position 80 control acpt-col.

           move c-name(4) to bc. move c-name(2) to fc.
           perform varying x from 17 by 1 until x > 20
             display bc-line line x position 10 size 30 control col-cnt
           end-perform. 

       page-box.
           move c-name(2) to bc. move c-name(7) to fc.
           display "‘œ¢å›˜    /   " line 19 position 7 control col-cnt
                   e line 19 position 14
                   l line 19 position 18.

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

       editor-open.
           move fl-disk-path to fl-disk. move fl-rest-path to fl-rest.
           move fpel-2 to fl-nam. open i-o word.
           read word invalid go editor.

       editor-open-2.
           perform index-box.    
           move word-var to editor-vars.

       editor.
           move c-name(2) to bc. move c-name(8) to fc.
           display "ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ"
                   line 3 position 3 control col-cnt
                   "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»" line 3 position 51.

           perform varying x from 4 by 1 until x > 20
           display "º                                               "
                   line x position 3 
                   "                         º" line x position 51
           end-perform.

           display "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ"
                   line 21 position 3 control col-cnt
                   "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼" line 21 position 51.

           perform varying x from 4 by 1 until x > 22
           display bc-line line x position 77 size 2 control nor-cnt
           end-perform.
           display bc-line line 22 position 5 size 72 control nor-cnt.
           move c-name(2) to bc. move c-name(7) to fc.
           move 1 to editor-count ctab.
       
       editor-2.
           accept ed-var(editor-count) line ed-ln(editor-count) 
            position 4 cursor ctab update no beep control col-cnt 
             on exception chr-ch go editor-chr.
           if editor-count = 17 go editor-2
           else add 1 to editor-count.
           go editor-2.

       editor-chr.       
           evaluate chr-ch
              when 9 add 9 to ctab
              when 56 subtract 9 from ctab
              when 27 move 0 to flag perform editor-clean go print-out
              when 52 if editor-count = 1 go editor-2 
                        else subtract 1 from editor-count  
                      end-if
              when 53 if editor-count = 17 go editor-2
                        else add 1 to editor-count 
                      end-if
              when 54 move 1 to ctab
              when 82 move 74 to ctab
              when 2 perform editor-save
              when 5 perform editor-delete
              when other go editor-2
           end-evaluate.
           go editor-2.
           
       editor-clean.
           move c-name(4) to bc. move c-name(2) to fc.
           perform varying x from 22 by -1 until x < 3
           display bc-line line x position 2 size 78 control col-cnt
           end-perform.

       editor-save.
           move 1 to wd-num.
           move editor-vars to word-var.
           write word-rec invalid rewrite word-rec.
           close word.

       telos.
           close pel-prt.

       telos-2.
           close pelates.
       telos-3.
           goback.

       brk.
           exit.

       brk-2.
           go begin-2.


      



