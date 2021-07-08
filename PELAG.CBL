       id division.
       program-id. pelag.
       author. temerzidis stavros.
       environment division.
       configuration section.
       source-computer. pentium-100.
       object-computer. pentium-100.
       input-output section.
       file-control.
           select optional agores assign random file-name
           organization indexed
           access dynamic

           record key kodikos-a
           alternate record key epitheto-a with duplicates
           status ag-stat.

       data division.
       file section.
       fd agores.
       01 agores-rec.
          02 index-fields.
             03 kodikos-a pic x(20).
             03 onoma-a pic x(15).
             03 epitheto-a pic x(25).
          02 agores-rest.
             03 dieythynsh-a pic x(35).
             03 epagelma-a   pic x(20).
             03 afm-a pic x(8).
             03 telephone1-a pic x(15).
             03 telephone2-a pic x(15).

       working-storage section.
       77 chr-ch pic 9(3) comp-1.

       77 synexeia pic x.
          88 syn-n values "N" "n" "å" "§".
          88 syn-o values "O" "o" "é" "¶".

       01  str-var.
           02 bc-line pic x(80) value all "∞".
           02 inv-line pic x(80) value all spaces.
           02 rev-line pic x(80) value all "€".
           02 rev-half pic x(80) value all "‹".
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
           02 s-epagelma   pic x(20).
           02 s-afm pic x(8).
           02 s-telephone-1 pic x(15).
           02 s-telephone-2 pic x(15).

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
           02  idx-table pic x(60) occurs 999 times.

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
           02 fag pic x(12) value "pelag.dat".

       01  file-name pic x(29).
       01  fn redefines file-name.
           03 fl-disk pic x(2).
           03 fl-rest pic x(15).
           03 fl-nam pic x(12).

       01  file-status.
           02 ag-stat pic xx.
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
           move fag to fl-nam. open i-o agores.
       
       begin-2.
           initialize agores-rec.
           perform code-box.

       a1.
           initialize agores-rec.
           accept kodikos-a line 8 position 25 update 
           no beep on exception chr-ch
           evaluate chr-ch
              when 27 perform clean-box go telos
              when 1 go index-code
              when other go a1
           end-evaluate.

           if kodikos-a = spaces go a1.

       begin-3.
           perform values-box.
           perform find-rec.
           perform save-box-2.

       a2.
           accept epitheto-a line 6 position 31 prompt "." 
           control acpt-cnt on exception chr-ch
           evaluate chr-ch
              when 27 perform save-box perform clean-box go begin-2
              when 3 perform save-box perform clean-box go begin-2
              when 4 perform eis
              when 6 perform dia if syn-n go begin-2 end-if
              when 52 perform disp-ln go a8
              when 53 perform disp-ln go a3
              when other go a2
           end-evaluate.
           
           perform disp-ln.

       a3.
           accept onoma-a line 7 position 31 prompt "." 
           control acpt-cnt on exception chr-ch
           evaluate chr-ch
              when 27 perform save-box perform clean-box go begin-2
              when 3 perform save-box perform clean-box go begin-2
              when 4 perform eis
              when 6 perform dia if syn-n go begin-2 end-if
              when 52 perform disp-ln go a2
              when 53 perform disp-ln go a4
              when other go a3
           end-evaluate.

           perform disp-ln.       

       a4.
           accept dieythynsh-a line 8 position 31 prompt "." 
           control acpt-cnt on exception chr-ch     
           evaluate chr-ch
              when 27 perform save-box perform clean-box go begin-2
              when 3 perform save-box perform clean-box go begin-2
              when 4 perform eis
              when 6 perform dia if syn-n go begin-2 end-if
              when 52 perform disp-ln go a3
              when 53 perform disp-ln go a5
              when other go a4
           end-evaluate.

           perform disp-ln. 

       a5.
           accept epagelma-a line 9 position 31 prompt "." 
           control acpt-cnt on exception chr-ch     
           evaluate chr-ch
              when 27 perform save-box perform clean-box go begin-2
              when 3 perform save-box perform clean-box go begin-2
              when 4 perform eis
              when 6 perform dia if syn-n go begin-2 end-if
              when 52 perform disp-ln go a4
              when 53 perform disp-ln go a6
              when other go a5
           end-evaluate.

           perform disp-ln. 


       a6.
           accept afm-a line 10 position 31 prompt "."  
           control acpt-cnt on exception chr-ch
           evaluate chr-ch
              when 27 perform save-box perform clean-box go begin-2
              when 3 perform save-box perform clean-box go begin-2
              when 4 perform eis
              when 6 perform dia if syn-n go begin-2 end-if
              when 52 perform disp-ln go a5
              when 53 perform disp-ln go a7
              when other go a6
           end-evaluate.

           perform disp-ln. 

       A7.
           accept telephone1-a line 11 position 31 prompt "."   
           control acpt-cnt on exception chr-ch
           evaluate chr-ch
              when 27 perform save-box perform clean-box go begin-2
              when 3 perform save-box perform clean-box go begin-2
              when 4 perform eis
              when 6 perform dia if syn-n go begin-2 end-if
              when 52 perform disp-ln go a6
              when 53 perform disp-ln go a8
              when other go a7
           end-evaluate.

           perform disp-ln.

       A8.
           accept telephone2-a line 12 position 31 prompt "."   
           control acpt-cnt on exception chr-ch
           evaluate chr-ch
              when 27 perform save-box perform clean-box go begin-2
              when 3 perform save-box perform clean-box go begin-2
              when 4 perform eis
              when 6 perform dia if syn-n go begin-2 end-if
              when 52 perform disp-ln go a7
              when 53 perform disp-ln go a2
              when other go a8
           end-evaluate.

           perform disp-ln.
           go a2.


       eis.
           write agores-rec invalid

                 rewrite agores-rec invalid

                         move ag-stat to file-stat 
                         perform invalid-info

                 not invalid move 2 to val-stat perform valid-info

                 end-rewrite

           not invalid move 1 to val-stat perform valid-info
           end-write.

           perform telos. perform begin. perform save-box-2.
             
       dia.
           perform error-box.

           display "   (!) è®¶©¶Æ„        " line 7 position 29 
                                         control col-cnt
                   "----------------------" line 8 position 29
                   "    åò Æòü¶Á§ ´ò      " line 10 position 29
                   "   ©´¶†ÆúÂò;(N/O):    " line 12 position 29.

           move "o" to synexeia.

           accept synexeia line 12 position 46 update tab blink.

           evaluate true
              when syn-n perform dia-2 
              when syn-o perform values-box perform find-rec 
                         perform save-box-2 continue
              when other go dia
           end-evaluate.
              
       dia-2.
           delete agores invalid move ag-stat to file-stat
                                  perform invalid-info

                      not invalid move 3 to val-stat 
                                  perform valid-info

           end-delete.

           initialize agores-rec.

           perform disp-ln. perform telos. perform begin. 
           perform clean-box.

       telos.
           close agores.

       telos-1.
           goback.

       find-rec.
           read agores invalid move ag-stat to file-stat
                                perform invalid-info
                                perform disp-ln

                    not invalid move 4 to val-stat
                                perform valid-info
                                perform disp-info
                                perform disp-ln

           end-read.

       disp-ln.
           move c-name(6) to bc. move c-name(8) to fc.

           if epitheto-a = spaces
           display und-line line 6 position 31 size 25 control col-cnt.

           if onoma-a = spaces 
           display und-line line 7 position 31 size 15 control col-cnt.

           if dieythynsh-a = spaces 
           display und-line line 8 position 31 size 35 control col-cnt.

           if epagelma-a = spaces 
           display und-line line 9 position 31 size 20 control col-cnt.

           if afm-a = spaces
           display und-line line 10 position 31 size 8 control col-cnt.

           if telephone1-a = spaces
           display und-line line 11 position 31 size 15 control col-cnt.

           if telephone2-a = spaces
           display und-line line 12 position 31 size 15 control col-cnt.

       disp-info.
           move c-name(6) to bc. move c-name(8) to fc.
           display epitheto-a line 6 position 31 control col-cnt
                   onoma-a position 31
                   dieythynsh-a position 31
                   epagelma-a position 31
                   afm-a position 31
                   telephone1-a position 31
                   telephone2-a position 31.

       index-code.
           perform clean-table.

           initialize i. initialize flag. move 20 to i.

           perform varying x from 1 by 1 until x = 20 or flag = 1
           if kodikos-a (i:1) = space subtract 1 from i
           else move 1 to flag
           end-if 
           end-perform.

           move kodikos-a to t-code. move 1 to index-count.
           start agores key not < kodikos-a invalid key go a1.

           if kodikos-a = spaces go index-code-3.

       index-code-2.
           read agores next at end go index-disp-1. 

           if kodikos-a (1:i) > T-code (1:i) go index-disp-1.

           move index-fields to idx-table (index-count).

           add 1 to index-count.

           go index-code-2.

       index-code-3.
           read agores next at end go index-disp-1. 

           move index-fields to idx-table (index-count).

           add 1 to index-count.

           go index-code-3.

       index-disp-1.
           if index-count not > 1 initialize agores-rec go begin-2.

           move 1 to y. move 1 to k. subtract 1 from index-count.
           move 1 to e. move 1 to m.

           if index-count < 10 move 1 to l 

           else compute l rounded = ( index-count / 10) + .5

           end-if.
           
           perform index-box.

       index-disp-2.
           move c-name(3) to bc. move c-name(7) to fc.

           move idx-table(y) to index-fields.

           move kodikos-a to kodikos-2. move onoma-a to onoma-2.
           move epitheto-a to epitheto-2.

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

           move kodikos-a to kodikos-2. move onoma-a to onoma-2.
           move epitheto-a to epitheto-2.
           
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

                      initialize agores-rec

                       move idx-table(y) to index-fields

                       move kodikos-a to kodikos-2 
                       move onoma-a to onoma-2
                       move epitheto-a to epitheto-2

                       move c-name(2) to bc move c-name(7) to fc

                       display index-pos line idx-ln(k) position 8 
                                            control col-cnt

                       perform clean-box go begin-3

           else go index-select

           end-if. 
       
       invalid-info.
           evaluate file-stat
              when "10"  move "   í‚¢¶™ ò®ÆúÂ¶¨   " to inval-mess
                                          perform invalid-message

              when "22"  move "   É†ß¢„ úöö®ò≠„   " to inval-mess
               perform message-error-box thru message-error-box-2

              when "23"  move "    å‚ò úöö®ò≠„    " to inval-mess
                                          perform invalid-message

              when "24"  move "  Ç‚£†©ú ´¶ ò®ÆúÂ¶ " to inval-mess
               perform message-error-box thru message-error-box-2

              when "30"  move "  Å¢·ôû ©´¶ õÂ©°¶  " to inval-mess
               perform message-error-box thru message-error-box-2

              when "98"  move "ïò¢ò©£‚§¶ ú¨®ú´„®†¶" to inval-mess
               perform message-error-box thru message-error-box-2

              when other move "Ä§ò≠¶®· ©ú SYS.OP. " to inval-mess                 
               perform message-error-box thru message-error-box-2

           end-evaluate.

       valid-info.
           evaluate val-stat
              when 1  move " Ü ú†©òö‡ö„ ‚ö†§ú  " to val-mess

              when 2  move " H ò¢¢òö„ ‚ö†§ú    " to val-mess

              when 3  move " Ü õ†òö®ò≠„ ‚ö†§ú  " to val-mess

              when 4  move " ãú´òô¶¢„ ©´¶†ÆúÂ‡§" to val-mess.

           move 0 to val-stat.

           perform valid-message.

       invalid-message.
           perform message-box.
           display inval-mess line 20 position 10 control col-cnt.
           move c-name(6) to bc. move c-name(8) to fc.
           display " " line 16 position 69 control col-cnt.

       valid-message.
           perform message-box.
           display val-mess line 20 position 10 control col-cnt.
           move c-name(6) to bc. move c-name(8) to fc.
           display " " line 16 position 69 control col-cnt.

       code-box.
           move c-name(6) to bc. move c-name(8) to fc.
           display "⁄ƒƒƒƒƒƒƒ â‡õ†°Ê™ èú¢·´û ƒƒƒƒƒƒƒø" 
                   line 7 position 15 control col-cnt
                   "≥ °‡õ†°Ê™                      ≥"
                   line 8 position 15
                   "≥                              ≥" 
                   line 9 position 15
                   "≥  ESC           F1            ≥" 
                   line 10 position 15
                   "¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ" 
                   line 11 position 15

                   bc-line line 12 position 17 size 32 
                           control nor-cnt.

           perform varying x from 8 by 1 until x > 11
           display bc-line line x position 47 size 2 control nor-cnt
           end-perform.

           move c-name(3) to bc. move c-name(8) to fc.
           display inv-line line 8 position 25 size 20 reverse 
                   control nor-cnt
                   " ò°Á®‡©û " line 10 position 21 control col-cnt
                   " ú¨®ú´„®†¶ " line 10 position 34.

       values-box.
           move c-name(6) to bc. move c-name(8) to fc.
           display 
           "⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ Ñ†©òö‡ö„ ë´¶†ÆúÂ‡§ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø"
                   line 3 position 12 control col-cnt.

           perform varying x from 4 by 1 until x > 19
           display 
           "≥                                                         ≥"
                   line x position 12
           end-perform.
           display 
           "¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ"
                   line 18 position 12

                   "â‡õ†°Ê™:" line 4 position 15
                   "ÑßÂüú´¶....:" line 6 position 19
                   "Ó§¶£ò......:" position 19
                   "É†úÁü¨§©û..:" position 19
                   "Ñß·ööú¢£ò..:" position 19
                   "Ä.î.ã......:" position 19
                   "íû¢‚≠‡§¶-1.:" position 19
                   "íû¢‚≠‡§¶-2.:" position 19 
                   "ESC" line 16 position 15
                   "F3" line 16 position 29
                   "F4" line 16 position 42
                   "F6" line 16 position 57.

           move c-name(3) to bc. move c-name(8) to fc.
           display kodikos-a line 4 position 23 control col-cnt
                   "Ä°Á®‡©û" line 16 position 18
                   "â‡õ†°Ê™" line 16 position 31
                   "Ñ†©òö‡ö„" line 16 position 45
                   "É†òö®ò≠„" line 16 position 60.

           display bc-line line 19 position 14 size 59 control nor-cnt
           perform varying x from 4 by 1 until x > 18
           display bc-line line x position 71 size 2 control nor-cnt
           end-perform.

       index-box.
           move c-name(3) to bc. move c-name(8) to fc.
           display "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ" 
                   line 6 position 6 control col-cnt

                   "ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª" 
                   line 6 position 41.

           perform varying x from 7 by 1 until x > 21
           display "∫                                    " 
                   line x position 6
                   "                                ∫" 
                   line x position 41
           end-perform.   

           display "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ" 
                   line 21 position 6 control col-cnt

                  "ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº" 
                  line 21 position 41.

           display bc-line line 22 position 8 low size 68
                           control nor-cnt.

           perform varying x from 7 by 1 until x > 21
           display bc-line line x position 74 low size 2 
           end-perform.

           move c-name(3) to bc. move c-name(8) to fc.
           display "â‡õ†°Ê™" line 7 position 8 control col-cnt
                   "ÑßÂüú´¶" line 7 position 30
                   "Ó§¶£ò" line 7 position 57
                   half-line line 8 position 8 size 20
                   half-line line 8 position 30 size 25
                   half-line line 8 position 57 size 15
                   "PGUP-PGDN" line 20 position 8
                   "ESC" line 20 position 31
                   "" line 20 position 43
                   "ENTER " line 20 position 60.

           move c-name(2) to bc. move c-name(5) to fc.
           display "Ä¢¢.©ú¢Âõò™" line 20 position 17 control col-cnt
                   "Ä°Á®‡©û" line 20 position 34
                   "âÂ§û©û £ß·®ò™" line 20 position 45
                   "Ñß†¢¶ö„" line 20 position 65.

           perform page-box.

       page-box.
           move c-name(2) to bc. move c-name(7) to fc.
           display "ëú¢Âõò    /   " line 19 position 7 control col-cnt
                   e line 19 position 14
                   l line 19 position 18.

       save-box.
           if agores-rec not = save-var

              perform error-box

              display "   (!) è®¶©¶Æ„        " line 7 position 29 
                                             control col-cnt
                      "----------------------" line 8 position 29
                      "     åò ©‡ü¶Á§ ¶†     " line 10 position 29
                      "   ò¢¢òö‚™;(N/O):     " line 12 position 29

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
           move agores-rec to save-var.

       clean-table.
           perform varying x from 1 by 1 until x > 999
           initialize idx-table(x)
           end-perform.

       clean-box.
           move c-name(4) to bc. move c-name(2) to fc.

           perform varying x from 3 by 1 until x > 22 
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
           display "⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø" line 6 position 28 
                   control col-cnt
                   "≥                      ≥" line 7 position 28
                   "≥                      ≥" line 8 position 28
                   "≥                      ≥" line 9 position 28
                   "≥                      ≥" line 10 position 28
                   "≥                      ≥" line 11 position 28                                         
                   "≥                      ≥" line 12 position 28
                   "¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ" line 13 position 28

           inv-line line 14 position 30 size 24 control nor-cnt.

           perform varying x from 7 by 1 until x > 13
           display inv-line line x position 52 size 2 control nor-cnt
           end-perform.

           if epitheto-a (22:2) = spaces
                  display und-line line 8 position 52 size 2 
                                   control nor-cnt

           else display epitheto-a (22:2) line 8 position 52 
                                   control nor-cnt
           end-if.

           if dieythynsh-a (22:2) = spaces
                  display und-line line 10 position 52 size 2 
                                   control nor-cnt

           else display dieythynsh-a (22:2) line 10 position 52 
                                   control nor-cnt
           end-if.

       message-box.
           move c-name(1) to bc. move c-name(8) to fc.
           display "ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø" line 19 position 14
                   control col-cnt.

           move c-name(2) to bc. move c-name(3) to fc.
           display "⁄ƒƒƒƒƒƒƒƒ" line 19 position 5 control col-cnt
                   "≥                            ≥" line 20 position 5
                   control col-cnt
                   "¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ" line 21 position 5

                   bc-line line 22 position 7 size 30 control nor-cnt.

           perform varying x from 19 by 1 until x > 21
           display bc-line line x position 35 size 2 control nor-cnt
           end-perform.
         
       
       message-error-box.
           perform error-box.
           display  "<<                  >>" line 7 position 29
                     inval-mess line 7 position 30
                    "   Ç†ò ßú®†©©Ê´ú®ú™   " line 8 position 29
                    "     ß¢û®¶≠¶®Âú™      " line 9 position 29
                    "    ò§ò´®‚•´ú ´¶      " line 10 position 29
                    "  ß®Êö®ò££ò ô¶„üú†ò™  " line 11 position 29
                    "     òßÊ ´¶ ãÑåéì     " line 12 position 29.
                    
           accept flag line 24 position 80 control acpt-col.
           perform clean-box.

       message-error-box-2.
           exit.

       message-error-box-3.
           go begin-2.


           



