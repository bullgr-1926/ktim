       id division.
       program-id. dcars00.
       author. temerzidis stavros.
       environment division.
       configuration section.
       source-computer. ibm-at.
       object-computer. ibm-at.
       input-output section.
       file-control.
           select optional mn-file assign random mn-nam.
       data division.
       file section.
       fd  mn-file.
       01  mn-rec pic x(2).
       working-storage section.
       77  chr-ch pic 999 comp-1.

       01  date-var.
           02 date-ok pic x.
              88 y-ok values "N" "n" "Œ" "¤".
              88 n-ok values "O" "o" "Ž" "¦".
           02 leap-y pic 9.
           02 temp pic 99.
           02 ok-flag pic 9.
           02 temp-1 pic s9(5).
           02 temp-2 pic s9(5).
           02 temp-3 pic s9(5).
           02 factor pic s9(5).

       01  inv-date.
           02 inv-yy pic 9(2). 02 inv-mm pic 9(2). 02 inv-dd pic 9(2).

       01  str-var.
           02 flag2 pic x.
           02 bc-line pic x(80) value all "°".
           02 inv-line pic x(80) value all spaces.
           02 rev-line pic x(80) value all "Û".
           02 rev-half pic x(80) value all "Ü".

       01  num-var.
           02 flag pic 9.
           02 answ pic 99.
           02 answ2 pic z.
           02 i pic 9(2).
           02 k pic 9(2).
           02 l pic 9(4).
           02 x pic 99.
           02 y pic 99.

       01 mnu-var.
          02 a pic 99.
          02 t pic 99.
          02 rt pic 99.
          02 mn pic 99 value 1.
          02 g pic 99 value 1.
          02 e pic 99 value 3.
          02 c pic 99 value 12.
          02 p pic 99 value 6.
          02 b pic 99 value 9.
          02 h pic 99 value 11.

       01  tables-1.
           02 pic x(24) value "312831303130313130313031".
       01  redefines tables-1.
           02 dim pic 99 occurs 12 times.

       01  w-day-of-week-area.
           03 pic x(4) value "1-‰¬".
           03 pic x(4) value "2-ƒœ".
           03 pic x(4) value "3-’¨".
           03 pic x(4) value "4-’œ".
           03 pic x(4) value "5-â".
           03 pic x(4) value "6-˜".
           03 pic x(4) value "7-‘á".
       01  redefines w-day-of-week-area.
           03 w-day-of-week-item occurs 7 times.
               05 pic xx.
               05 w-day pic xx.

       01  files-used.
           02 mn-nam pic x(24).

       01  file-name pic x(26).
       01  fn redefines file-name.
           03 fl-disk pic xx.
           03 fl-rest pic x(12).
           03 fl-nam pic x(12).

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
           02  c-name   pic x(7) occurs 8 times.

       01  control-value.
           02  col-cnt.
               03 pic x(5) value "high,".
               03 pic x(7) value "fcolor=".
               03 fc pic x(7).
               03 pic x(8) value ",bcolor=".
               03 bc pic x(7).

       01  nor-cnt.
           02  pic x(47) value "fcolor=white,bcolor=black".

       01 acpt-col.
          02 pic x(47) value "fcolor=black,bcolor=black".

       01  menu-options.
           02 pic x(20)
                value " œ¨å               ".
           02 pic x(20)
                value " ë¥¦›¦ª ©«¦ DOS     ".
           02 pic x(20)
                value " ‘«˜Ÿœ¨á ©«¦ ®œå˜  ".
           02 pic x(20)
                value " €š¦¨˜©«âª          ".
           02 pic x(20)
                value " à¢ž«âª            ".
           02 pic x(20)
                value " €¬«¦¡ ¤ã«à¤        ".
           02 pic x(20)
                value " €š¦¨˜©«é¤          ".
           02 pic x(20)
                value " à¢ž«é¤            ".
           02 pic x(20)
                value " BACKUP ˜¨®œåà¤     ".
           02 pic x(20)
                value " €¢¢˜šã †£œ¨¦£ž¤å˜ª ".
           02 pic x(20)
                value " ¦ãŸœ ˜            ".
           02 pic x(20)
                value " ‹á¨¡œª ˜¬«¦¡ ¤ã«à¤ ".
           02 pic x(20)
                value " ‹¦¤«â¢˜ ˜¬«¦¡¤ã«à¤ ".
       01  redefines menu-options.
           02 m-opt pic x(20) occurs 13 times.

       01  opt-lns.
           02 pic x(26) value "03050305070305070305030608".
       01  redefines opt-lns.
           02 o-ln pic 9(2) occurs 13 times.
       01  opt-pos.
           02 pic x(26) value "05051515153030304444572525".
       01  redefines opt-pos.
           02 o-pos pic 9(2) occurs 13 times.
       01  opt-sizes.
           02 pic x(26) value "06162011091311091620092020".
       01  redefines opt-sizes.
           02 o-sz pic 9(2) occurs 13 times.


       01 mnu-options.
           02 pic x(13)
                value " ‚œ¤ ¡á      ".
           02 pic x(13)
                value " „§œ¥œ¨š˜©å˜ ".
           02 pic x(13)
                value " „¡«¬§é©œ ª  ".
           02 pic x(13)
                value " ¦žŸž« ¡á   ".
           02 pic x(13)
                value " ¦ãŸœ ˜     ".
       01 redefines mnu-options.
          02 mnu-opt pic x(13) occurs 5 times.

       01 mnu-pos.
          02 pic x(10) value "0414294356".
       01 redefines mnu-pos.
          02 m-pos pic 9(2) occurs 5 times.
       01 mnu-sizes.
          02 pic x(10) value "0813121109".
       01 redefines mnu-sizes.
          02 m-sz pic 9(2) occurs 5 times.

       01  linkages.
           02  paths.
               03  pr-path.
                   04 disk-path pic xx value "c:".
                   04 rest-path pic x(15) value "\rmcobol\prg\".
                   04 prog pic x(12).
               03  fl-path.
                   04 fl-disk-path pic xx value "c:".
                   04 fl-rest-path pic x(15) value "\rmcobol\prg\".
           02  today-day pic xx.
           02 today-date.
               03 t-dd pic 99. 03 t-mm pic 99. 03 t-yy pic 99.
           02  ed-date.
               03 e-dd pic zz/. 03 e-mm pic zz/. 03 e-yy pic zz.

       procedure division.
       maska.
           display spaces erase control nor-cnt.
           move c-name(4) to bc. move c-name(2) to fc.
           display inv-line line 1 position 1 reverse
                   control col-cnt
                   inv-line line 24 position 1 reverse size 75
                   " bull" line 24 position 75 control nor-cnt.

           perform varying x from 2 by 1 until x > 23
           display bc-line line x position 1 control col-cnt
           end-perform.

           move c-name(2) to bc. move c-name(8) to fc.
           display mnu-opt(1) line 1 position m-pos(1) control col-cnt
                   mnu-opt(2) line 1 position m-pos(2)
                   mnu-opt(3) line 1 position m-pos(3)
                   mnu-opt(4) line 1 position m-pos(4)                   
                   mnu-opt(5) line 1 position m-pos(5)
                   " " line 24 position 80 control acpt-col.

           move c-name(8) to bc. move c-name(4) to fc. 
           display " F1.¦ãŸœ ˜    F10.‹œ¤¦ç    CONTROL-END.’â¢¦ª " 
                   line 24 position 1 low control col-cnt.

           perform get-date thru get-date-ex.
           perform about-box.

       begin.
           accept flag line 24 position 80 no beep control acpt-col
           on exception chr-ch
           evaluate chr-ch
              when 10 go mnu-boxes
              when 83 display " " erase stop run
              when other go begin
           end-evaluate.
           go begin.

       begin-2.
           move c-name(8) to bc. move c-name(1) to fc.
           display " F10.‹œ¤¦ç    CONTROL-END.’â¢¦ª " 
                   line 24 position 14 low control col-cnt.
                   
           display " " line 24 position 80 control acpt-col.

           evaluate answ
              when 1 perform about-box
              when 2 display " " erase stop run
              when 3 go const-box 
              when 4 move "dcars01" to prog call pr-path using linkages
              when 5 move "dcars02" to prog call pr-path using linkages
              when 6 move "dcars03" to prog call pr-path using linkages
              when 7 move "dcars04" to prog call pr-path using linkages
              when 8 move "dcars05" to prog call pr-path using linkages
              when 9 move "A:backup.flg" to mn-nam
                     open output mn-file close mn-file
                     display spaces line 1 position 1 erase
                     stop run

              when 10 move "o" to date-ok
                      perform get-date-2 thru get-date-ex
                      go begin

              when 11 move "help" to prog call pr-path using linkages
              when 12 move "const01" to prog call pr-path using linkages
                      move e to c move 3 to e 
                      
              when 13 move "const02" to prog call pr-path using linkages
                      move e to c move i to e   
                      
              when other
                    go begin

           end-evaluate.

           cancel pr-path.

           move c-name(8) to bc. move c-name(4) to fc. 
           display " F1.¦ãŸœ ˜    F10.‹œ¤¦ç    CONTROL-END.’â¢¦ª " 
                   line 24 position 1 low control col-cnt.

           go begin.

       mnu-boxes.
           move c-name(3) to bc. move c-name(8) to fc.
           display mnu-opt(mn) line 1 position m-pos(mn) size m-sz(mn)
                               control col-cnt.
           accept flag line 24 position 80 no beep control acpt-col
                     on exception chr-ch
           move c-name(2) to bc move c-name(8) to fc
           display mnu-opt(mn) line 1 position m-pos(mn) size m-sz(mn)
                               control col-cnt
           evaluate chr-ch
              when 56 if mn > 1 subtract 1 from mn
                      else move 5 to mn end-if go mnu-boxes
              when 9 if mn < 5 add 1 to mn
                      else move 1 to mn end-if go mnu-boxes
              when 53 go mnu-boxes-2
              when 27 perform clean-box go begin
              when other go mnu-boxes
           end-evaluate.

           if flag = 0 go mnu-boxes-2
           else go mnu-boxes.

       mnu-boxes-2.
           evaluate mn
              when 1 go gen-box
              when 2 go epe-box
              when 3 go ekt-box
              when 4 go boh-box
              when 5 go hlp-box
           end-evaluate.

       gen-box.
           move c-name(2) to bc. move c-name(8) to fc.

           display "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" line 2 position m-pos(1)
                                        control col-cnt
                   "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" line 6 position m-pos(1).

           perform varying x from 3 by 1 until x > 5
           display "³                ³" line x position m-pos(1)
	   end-perform.

           display bc-line line 7 position 6 size 18 low 
                           control nor-cnt.
           perform varying x from 3 by 1 until x > 6
           display bc-line line x position 22 size 2 low
           end-perform.

           perform varying x from 1 by 1 until x > 2
           display m-opt(x) line o-ln(x) position o-pos(x) size o-sz(x)
                            control col-cnt
	   end-perform.

           move g to rt. move 1 to a. move 2 to t.
           go routin-box.

       
       epe-box.
           move c-name(2) to bc. move c-name(8) to fc.
           display "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" line 2 position m-pos(2)
                                            control col-cnt
                   "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" line 8 position m-pos(2).

           perform varying x from 3 by 1 until x > 7
           display "³                    ³" line x position m-pos(2)
	   end-perform.

           display bc-line line 9 position 16 size 22 low
                           control nor-cnt.
           perform varying x from 3 by 1 until x > 8
           display bc-line line x position 36 size 2 low
           end-perform.
           
           perform varying x from 3 by 1 until x > 5
           display m-opt(x) line o-ln(x) position o-pos(x) size o-sz(x)
                            control col-cnt
           end-perform.

           move e to rt. move 3 to a. move 5 to t.
           go routin-box.

         
       const-box.
           move c-name(2) to bc. move c-name(8) to fc.

           display "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" line 5 position 24
                                        control col-cnt
                   "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" line 9 position 24.

           perform varying x from 6 by 1 until x > 8
           display "³                    ³" line x position 24
	   end-perform.

           display bc-line line 10 position 26 size 22 low 
                           control nor-cnt.
           perform varying x from 6 by 1 until x > 9
           display bc-line line x position 46 size 2 low
           end-perform.

           perform varying x from 12 by 1 until x > 13
           display m-opt(x) line o-ln(x) position o-pos(x) size o-sz(x)
                            control col-cnt
	   end-perform.

           move c to rt. move 12 to a. move 13 to t.
           go routin-box.
  

       ekt-box.
           move c-name(2) to bc. move c-name(8) to fc.
           display "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" line 2 position m-pos(3)
                                            control col-cnt
                   "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" line 8 position m-pos(3).

           perform varying x from 3 by 1 until x > 7
           display "³                    ³" line x position m-pos(3)
	   end-perform.

           display bc-line line 9 position 31 size 22 low
                           control nor-cnt.
           perform varying x from 3 by 1 until x > 8
           display bc-line line x position 51 size 2 low
           end-perform.
           
           perform varying x from 6 by 1 until x > 8
           display m-opt(x) line o-ln(x) position o-pos(x) size o-sz(x)
                            control col-cnt
           end-perform.

           move p to rt. move 6 to a. move 8 to t.
           go routin-box.


       boh-box.
           move c-name(2) to bc. move c-name(8) to fc.
           display "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿" line 2 position m-pos(4)
                                        control col-cnt
                   "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" line 6 position m-pos(4).

           perform varying x from 3 by 1 until x > 5
           display "³                    ³" line x position m-pos(4)
	   end-perform.

           display bc-line line 7 position 45 size 22 low
                           control nor-cnt.
           perform varying x from 3 by 1 until x > 7
           display bc-line line x position 65 size 2 low
           end-perform.
           
           perform varying x from 9 by 1 until x > 10
           display m-opt(x) line o-ln(x) position o-pos(x) size o-sz(x)
                            control col-cnt
           end-perform.

           move b to rt. move 9 to a. move 10 to t.
           go routin-box.

       hlp-box.
           move c-name(2) to bc. move c-name(8) to fc.
           display "ÚÄÄÄÄÄÄÄÄÄ¿" line 2 position m-pos(5)
                                  control col-cnt
                   "³         ³" line 3 position m-pos(5)
                   "ÀÄÄÄÄÄÄÄÄÄÙ" line 4 position m-pos(5).

           display bc-line line 5 position 58 size 11 low
                           control nor-cnt.
           perform varying x from 3 by 1 until x > 4
           display bc-line line x position 67 size 2 low
           end-perform.

           move h to rt. move 11 to a. move 11 to t.
           go routin-box.

       routin-box.
           move c-name(3) to bc. move c-name(8) to fc.
           display mnu-opt(mn) line 1 position m-pos(mn) size m-sz(mn)
                               control col-cnt
                   m-opt(rt) line o-ln(rt) position o-pos(rt)
                             size o-sz(rt).

           accept flag line 24 position 80 no beep control acpt-col
                       on exception chr-ch
           move c-name(2) to bc move c-name(8) to fc
           display m-opt(rt) line o-ln(rt) position o-pos(rt)
                             size o-sz(rt) control col-cnt
           evaluate chr-ch
              when 56 perform clean-box perform routin-box-2
                                       if mn > 1 subtract 1 from mn
                                       else move 5 to mn end-if
                                       go mnu-boxes-2
              when 9 perform clean-box perform routin-box-2
                                       if mn < 5 add 1 to mn
                                       else move 1 to mn end-if
                                       go mnu-boxes-2
              when 52 if rt > a subtract 1 from rt
                      else move t to rt end-if go routin-box
              when 53 if rt < t add 1 to rt
                      else move a to rt end-if go routin-box
              when 27 perform clean-box go begin
              when other go routin-box
           end-evaluate.

           if flag = 0 move rt to answ
                     if answ not = 3 perform clean-box end-if
                       perform routin-box-2
                       go begin-2
           else go routin-box.

       routin-box-2.
           evaluate mn
              when 1 move rt to g
              when 2 move rt to e
              when 3 move rt to p
              when 4 move rt to b
              when 5 move rt to h
           end-evaluate.

       get-date.
           perform get-sys-clock.
           if inv-date not = 880101 perform get-clock-2
                                    go get-date-ex.

       get-date-2.
           move c-name(5) to bc. move c-name(8) to fc.
           display
           "ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿"
                 line 15 position 27 control col-cnt
           "³                                         ³" position 27
           "³                                         ³" position 27
           "ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ" position 27
           display
             "ƒ—‘„ ’„•Ž“‘€ †‹„Ž‹†Œˆ€:" line 16 position 30

             bc-line line 19 position 29 size 43 low
                     control nor-cnt.

           perform varying x from 16 by 1 until x > 18
           display bc-line line x position 70 size 2 low
           end-perform.

           move 0 to t-dd t-mm t-yy.
           perform get-day until t-dd > 0 and t-dd < 32.
           perform get-mon until t-mm > 0 and t-mm < 13.
           perform get-yea until t-yy > 87.

           perform get-day-name.
           move t-dd to e-dd. move t-mm to e-mm. move t-yy to e-yy.
           move w-day(i) to today-day.
           display ed-date line 16 position 56
                   today-day line 16 position 65.

           perform get-ver. if n-ok go get-date-2.

       get-date-ex.
           move t-dd to e-dd. move t-mm to e-mm. move t-yy to e-yy.
           move w-day(i) to today-day.
           move c-name(3) to bc. move c-name(1) to fc.
           display ed-date line 1 position 70
                   control col-cnt
                   " " line 1 position 78
                   today-day line 1 position 79.
             move c-name(4) to bc. move c-name(2) to fc.
             perform varying x from 15 by 1 until x > 19
             display bc-line line x position 27 size 50 control col-cnt
             end-perform.

       get-day-name.
           divide t-yy by 4 giving temp remainder leap-y.
           perform check-date. if ok-flag = 1 go get-date.

           if t-mm = 1 or = 2
              compute temp-1 = (t-yy - 1) / 4
              compute temp-2 = (t-yy - 1) / 100 + 1
              compute temp-2 = 0.75 * temp-2
              compute factor = 365 * t-yy + t-dd + 31 * (t-mm - 1)
                               + temp-1 - temp-2
           else
              compute temp-1 = 0.4 *  t-mm + 2.3
              compute temp-2 = t-yy / 4
              compute temp-3 = t-yy / 100 + 1
              compute temp-3 = 0.75 * temp-3
              compute factor = 365 * t-yy + t-dd + 31 * (t-mm - 1)
                               - temp-1 + temp-2 - temp-3.

           compute temp-1 = (factor * -1) / 7.
           compute temp-2 = temp-1 * 7.
           compute i = (factor + temp-2) + 1.

       get-day.
           move c-name(5) to bc. move c-name(1) to fc.
           display "  /  /  " line 16 position 56 control col-cnt
           accept t-dd line 16 position 56 prompt echo no beep
             on exception chr-ch perform get-sys-clock
                                 perform get-clock-2 go get-date-ex.
           if t-dd not numeric go get-day.
       get-mon.
           accept t-mm line 16 position 59 prompt echo no beep.
           if t-mm not numeric go get-day.
       get-yea.
           accept t-yy line 16 position 62 prompt echo no beep.
           if t-yy not numeric go get-day.

       get-ver.
           move c-name(5) to bc. move c-name(8) to fc.
           display "„ˆ„€ˆ—‘† †‹„Ž‹†Œˆ€‘:" line 17 position 30
                control col-cnt
           move c-name(5) to bc. move c-name(1) to fc.
           move "n" to date-ok.
           accept date-ok line 17 position 55 update echo no beep blink
                control col-cnt.

       get-sys-clock.
           accept inv-date from date.
       get-clock-2.
           move inv-dd to t-dd. move inv-mm to t-mm.
           move inv-yy to t-yy. perform get-day-name.

       check-date.
           move 0 to ok-flag.
           if leap-y = 0 move 29 to dim(2)
           else move 28 to dim(2) end-if.
           if t-dd > dim(t-mm) move 1 to ok-flag.

       about-box.
           move c-name(8) to bc. move c-name(8) to fc.
           display
             "ÉÍÍÍÍÍÍÍÍÍÍÍ œ¨å ÍÍÍÍÍÍÍÍÍÍÍ»" line 5 position 25 control
             col-cnt.
           perform varying x from 6 by 1 until x > 17
           display
             "º                            º" line x position 25
           end-perform.
           display
             "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼" line 18 position 25.

           display bc-line line 19 position 27 size 30 low
                   control nor-cnt.
           perform varying x from 6 by 1 until x > 18
           display bc-line line x position 55 size 2 low
           end-perform.

           move c-name(8) to bc. move c-name(1) to fc.
           display "     ƒˆ€•„ˆ†‘† ‹€Œ’€‘     " line 7 position 26 
                                                   control col-cnt
                   "_________€“’Ž‰ˆŒ†’—Œ________" line 8 position 26
                   "ë¡›¦©ž   1.0" line 10 position 33
                   "Copyright (c) Januar '93" line 12 position 28
                   "Tœ£œ¨«å›žª   ‘«˜ç¨¦ª" line 14 position 29
                   "«ž¢. (0523) 61254" line 16 position 31.

           move c-name(1) to bc. move c-name(1) to fc.
           accept answ2 line 24 position 80 no beep control acpt-col.

           move c-name(4) to bc. move c-name(2) to fc.
           perform varying x from 5 by 1 until x > 19
           display bc-line line x position 25 size 32 control col-cnt
           end-perform.


       clean-box.
           evaluate mn
              when 1 move 04 to i
              when 2 move 14 to i
              when 3 move 29 to i
              when 4 move 43 to i
              when 5 move 56 to i
           end-evaluate.
            move c-name(2) to bc. move c-name(8) to fc.
            display mnu-opt(mn) line 1 position m-pos(mn) size m-sz(mn)
                                control col-cnt.
            move c-name(4) to bc. move c-name(2) to fc.
            perform varying x from 2 by 1 until x > 11
            display bc-line line x position i size 35 control col-cnt
            end-perform.





