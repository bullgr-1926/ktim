       id division.
       program-id. spol.
       author. temerzidis stavros.
       environment division.
       configuration section.
       source-computer. pentium-100.
       object-computer. pentium-100.
       input-output section.
       file-control.
           select optional polites assign random file-name
             organization indexed access dynamic record key kodikos-p
             alternate record key epitheto-p with duplicates
             status pol-stat.
           select optional spolites assign random file-name
             organization indexed access dynamic record key pcode
             status spol-stat.
           select optional const assign random file-name
           organization indexed
           access dynamic
           record key const-code
           status const-stat.


       data division.
       file section.
       fd polites.
       01 polites-rec.
          02 index-fields.
             03 kodikos-p pic x(20).
             03 onoma-p pic x(15).
             03 epitheto-p pic x(25).
          02 polites-rest.
             03 dieythynsh-p pic x(35).
             03 epagelma-p   pic x(20).
             03 afm-p pic x(8).
             03 telephone1-p pic x(15).
             03 telephone2-p pic x(15).

       fd spolites.
       01 spolites-rec.
          02 pcode pic 9(3).
          02 visual-values.
             03 pkod  pic x(20).
             03 ponoma pic x(15).
             03 pepitheto pic x(25).
             03 peidos-code pic 9(2).
             03 idx-fields.
                04 pcode-idx pic 9(3).
                04 peidos-per pic x(15).
                04 pperioxi pic x(15).
                04 pdieythynsh pic x(20).
             03 ppros-code pic 9(2).
             03 ppros-per pic x(15).
             03 pmetra-num pic 9(5).
             03 pmetra-code pic 9(2).
             03 pmetra pic x(15).
             03 ptimi pic 9(9).
             03 pparat pic x(40).
             03 pparat2 pic x(40).
             03 pparat3 pic x(40).

       fd const.
       01 const-rec.
          02 const-code.
             03 type-code pic 99.
          02 type-descr pic x(15).

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
           02 y pic 999.
           02 c-count pic 999.
           02 const-count pic 99.            
           02 const-end pic 99.

       01 mnu-var.
           02 a pic 9 value 1.
           02 t pic 9 value 3.
           02 rt pic 9.

       01  menu-options.
           02 pic x(11)
                value " E†©òö‡ö„ ".
           02 pic x(11)
                value " ãú´òô¶¢„ ".
           02 pic x(11)
                value " É†òö®ò≠„ ".

       01  redefines menu-options.
           02 m-opt pic x(11) occurs 3 times.

       01  opt-lns.
           02 pic x(6) value "081012".
       01  redefines opt-lns.
           02 o-ln pic 9(2) occurs 3 times.
       01  opt-pos.
           02 pic x(6) value "525252".
       01  redefines opt-pos.
           02 o-pos pic 9(2) occurs 3 times.
       01  opt-sizes.
           02 pic x(6) value "111111".
       01  redefines opt-sizes.
           02 o-sz pic 9(2) occurs 3 times.

       01  index-var.
           02 index-count pic 999.
           02 t-code pic x(20).

       01 save-var.
           02 s-pkod  pic x(20).
           02 s-ponoma pic x(15).
           02 s-pepitheto pic x(25).
           02 s-peidos-code pic 9(2).
           02 s-pcode-idx pic 9(3). 
           02 s-peidos-per pic x(15).
           02 s-pperioxi pic x(15).
           02 s-pdieythynsh pic x(20).
           02 s-ppros-code pic 9(2).
           02 s-ppros-per pic x(15).
           02 s-pmetra-num pic 9(5). 
           02 s-pmetra-code pic 9(2).
           02 s-pmetra pic x(15).
           02 s-ptimi pic 9(9).
           02 s-pparat pic x(40).
           02 s-pparat2 pic x(40).
           02 s-pparat3 pic x(40).

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

       01  idx-pos.
           02 pcode-idx-2 pic 9(3).
           02 filler pic x(3) value spaces. 
           02 peidos-per-2 pic x(15).
           02 filler pic x(2) value spaces.
           02 pperioxi-2 pic x(15).
           02 filler pic x(2) value spaces.
           02 pdieythynsh-2 pic x(20).

       01  index-table-2.
           02 pic x(17) value spaces.
       01  redefines index-table-2.
           02  idx-table-2 pic x(20) occurs 40 times.

       01  index-lines-2.
           02 pic x(30) value "04050607080910111213".
       01  redefines index-lines-2.
           02 idx-ln-2 pic 9(2) occurs 10 times.

       01  index-pos-2.
           02 type-code-2 pic 99.
           02 filler pic x value spaces.
           02 type-descr-2 pic x(15).


       01  file-used.
           02 fpol pic x(12) value "pol.dat".
           02 fspol pic x(12) value "spol.dat".
           02 fconst pic x(12) value "const.dat". 

       01  file-name pic x(29).
       01  fn redefines file-name.
           03 fl-disk pic x(2).
           03 fl-rest pic x(15).
           03 fl-nam pic x(12).

       01  file-status.
           02 pol-stat pic xx.
           02 spol-stat pic xx.
           02 const-stat pic xx.
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
           move fspol to fl-nam. open i-o spolites.
           move fpol to fl-nam. open i-o polites.
           move fconst to fl-nam. open i-o const. 

       begin-2.
           initialize polites-rec.
           initialize spolites-rec.
           initialize const-rec.
           perform code-box.

       a1.
           initialize polites-rec.
           initialize spolites-rec.
           initialize const-rec. 
           accept kodikos-p line 8 position 25 update 
           no beep on exception chr-ch
           evaluate chr-ch
              when 27 perform clean-box go telos
              when 1 go index-code
              when other go a1
           end-evaluate.

           if kodikos-p = spaces go a1.
            
       begin-3.
           read polites invalid go a1
                    not invalid continue
           end-read.

       begin-4.
           go mnu-box.

       begin-5. 
           evaluate answ
              when 1 move 1 to pcode c-count
                     go find-code
              when 2 perform clean-table 
                     move 1 to index-count
                     move 1 to pcode
                     go index-rec
              when 3 perform clean-table 
                     move 1 to index-count
                     move 1 to pcode
                     go index-rec
              when other go a1
           end-evaluate.

       a2.
           accept peidos-code line 8 position 26 prompt "." 
           control acpt-cnt on exception chr-ch
           evaluate chr-ch
              when 27 perform save-box perform clean-box 
                      perform telos go begin
              when 1 move 1 to const-count go index-const 
              when 4 perform eis perform clean-box 
                     perform telos go begin
              when 52 perform disp-ln go a11
              when 53 perform disp-ln go a3
              when other go a2
           end-evaluate.
           
           perform disp-ln.

       a3.
           accept ppros-code line 9 position 26 prompt "." 
           control acpt-cnt on exception chr-ch     
           evaluate chr-ch
              when 27 perform save-box perform clean-box 
                      perform telos go begin
              when 1 move 2 to const-count go index-const 
              when 4 perform eis perform clean-box 
                     perform telos go begin
              when 52 perform disp-ln go a2
              when 53 perform disp-ln go a4
              when other go a3
           end-evaluate.

           perform disp-ln.
 
       a4.
           accept pperioxi line 10 position 23 prompt "." 
           control acpt-cnt on exception chr-ch     
           evaluate chr-ch
              when 27 perform save-box perform clean-box 
                      perform telos go begin
              when 4 perform eis perform clean-box 
                     perform telos go begin
              when 52 perform disp-ln go a3
              when 53 perform disp-ln go a5
              when other go a4
           end-evaluate.

           perform disp-ln. 

       a5.
           accept pdieythynsh line 10 position 49 prompt "." 
           control acpt-cnt on exception chr-ch     
           evaluate chr-ch
              when 27 perform save-box perform clean-box 
                      perform telos go begin
              when 4 perform eis perform clean-box 
                     perform telos go begin
              when 52 perform disp-ln go a4
              when 53 perform disp-ln go a6
              when other go a5
           end-evaluate.

           perform disp-ln. 

       a6.
           accept pmetra-code line 11 position 32 prompt "."  
           control acpt-cnt on exception chr-ch
           evaluate chr-ch
              when 27 perform save-box perform clean-box 
                      perform telos go begin
              when 1 move 3 to const-count go index-const 
              when 4 perform eis perform clean-box 
                     perform telos go begin
              when 52 perform disp-ln go a5
              when 53 perform disp-ln go a7
              when other go a6
           end-evaluate.

           perform disp-ln. 


       A7.
           accept pmetra-num line 11 position 36 prompt "."   
           control acpt-cnt on exception chr-ch
           evaluate chr-ch
              when 27 perform save-box perform clean-box 
                      perform telos go begin
              when 4 perform eis perform clean-box 
                     perform telos go begin
              when 52 perform disp-ln go a6
              when 53 perform disp-ln go a8
              when other go a7
           end-evaluate.

           perform disp-ln.

       A8.
           accept ptimi line 12 position 32 prompt "."   
           control acpt-cnt on exception chr-ch
           evaluate chr-ch
              when 27 perform save-box perform clean-box 
                      perform telos go begin
              when 4 perform eis perform clean-box 
                     perform telos go begin
              when 52 perform disp-ln go a7
              when 53 perform disp-ln go a9
              when other go a8
           end-evaluate.

           perform disp-ln.

       A9.
           accept pparat line 13 position 28 prompt "."   
           control acpt-cnt on exception chr-ch
           evaluate chr-ch
              when 27 perform save-box perform clean-box 
                      perform telos go begin
              when 4 perform eis perform clean-box 
                     perform telos go begin
              when 52 perform disp-ln go a8
              when 53 perform disp-ln go a10
              when other go a9
           end-evaluate.

           perform disp-ln.

       A10.
           accept pparat2 line 14 position 28 prompt "."   
           control acpt-cnt on exception chr-ch
           evaluate chr-ch
              when 27 perform save-box perform clean-box 
                      perform telos go begin
              when 4 perform eis perform clean-box 
                     perform telos go begin
              when 52 perform disp-ln go a9
              when 53 perform disp-ln go a11
              when other go a10
           end-evaluate.

           perform disp-ln.

       a11.
           accept pparat3 line 15 position 28 prompt "." 
           control acpt-cnt on exception chr-ch     
           evaluate chr-ch
              when 27 perform save-box perform clean-box 
                      perform telos go begin
              when 4 perform eis perform clean-box 
                     perform telos go begin
              when 52 perform disp-ln go a10
              when 53 perform disp-ln go a2
              when other go a11
           end-evaluate.

           perform disp-ln. 
           go a2.

       eis.
           write spolites-rec invalid

                 rewrite spolites-rec invalid

                         move spol-stat to file-stat 
                         perform invalid-info

                 not invalid move 2 to val-stat perform valid-info

                 end-rewrite

           not invalid move 1 to val-stat perform valid-info
           end-write.

           perform save-box-2.
             
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
              when syn-n perform dia-2 go begin
              when syn-o perform clean-box perform telos go begin
              when other go dia
           end-evaluate.
              
       dia-2.
           delete spolites invalid move spol-stat to file-stat
                                  perform invalid-info

                      not invalid move 3 to val-stat 
                                  perform valid-info

           end-delete.

           initialize spolites-rec.

           perform disp-ln. perform telos. 
           perform clean-box.

       telos.
           close polites.
           close spolites. 

       telos-1.
           goback.

       disp-ln.
           move c-name(6) to bc. move c-name(8) to fc.

           if peidos-code = spaces
           display und-line line 8 position 26 size 2 control col-cnt.
           if peidos-per = spaces 
           display und-line line 8 position 41 size 15 control col-cnt.

           if ppros-code = spaces 
           display und-line line 9 position 26 size 2 control col-cnt.
           if ppros-per = spaces 
           display und-line line 9 position 41 size 15 control col-cnt.

           if pperioxi = spaces 
           display und-line line 10 position 23 size 15 control col-cnt.
           if pdieythynsh = spaces 
           display und-line line 10 position 49 size 20 control col-cnt.

           if pmetra-code = spaces
           display und-line line 11 position 32 size 2 control col-cnt.
           if pmetra-num = spaces
           display und-line line 11 position 36 size 5 control col-cnt.
           if pmetra = spaces
           display und-line line 11 position 55 size 15 control col-cnt.

           if ptimi = spaces
           display und-line line 12 position 32 size 9 control col-cnt.

           if pparat = spaces
           display und-line line 13 position 28 size 40 control col-cnt.
           if pparat2 = spaces
           display und-line line 14 position 28 size 40 control col-cnt.
           if pparat3 = spaces
           display und-line line 15 position 28 size 40 control col-cnt.


       disp-info.
           move c-name(6) to bc. move c-name(8) to fc.
           display peidos-code line 8 position 26 control col-cnt
                   peidos-per  line 8 position 41      
                   ppros-code line 9 position 26
                   ppros-per  line 9 position 41  
                   pperioxi line 10 position 23
                   pdieythynsh line 10 position 49
                   pmetra-code line 11 position 32
                   pmetra-num line 11 position 36
                   pmetra line 11 position 55
                   ptimi line 12 position 32
                   pparat line 13 position 28
                   pparat2 line 14 position 28
                   pparat3 line 15 position 28.

       index-code.
           perform clean-table.

           initialize i. initialize flag. move 20 to i.

           perform varying x from 1 by 1 until x = 20 or flag = 1
           if kodikos-p (i:1) = space subtract 1 from i
           else move 1 to flag
           end-if 
           end-perform.

           move kodikos-p to t-code. move 1 to index-count.
           start polites key not < kodikos-p invalid key go a1.

           if kodikos-p = spaces go index-code-3.

       index-code-2.
           read polites next at end go index-disp-1. 

           if kodikos-p (1:i) > T-code (1:i) go index-disp-1.

           move index-fields to idx-table (index-count).

           add 1 to index-count.

           go index-code-2.

       index-code-3.
           read polites next at end go index-disp-1. 

           move index-fields to idx-table (index-count).

           add 1 to index-count.

           go index-code-3.

       index-disp-1.
           if index-count not > 1 initialize polites-rec go begin-2.

           move 1 to y. move 1 to k. subtract 1 from index-count.
           move 1 to e. move 1 to m.

           if index-count < 10 move 1 to l 

           else compute l rounded = ( index-count / 10) + .5

           end-if.
           
           perform index-box.

       index-disp-2.
           move c-name(3) to bc. move c-name(7) to fc.

           move idx-table(y) to index-fields.

           move kodikos-p to kodikos-2. move onoma-p to onoma-2.
           move epitheto-p to epitheto-2.

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

           move kodikos-p to kodikos-2. move onoma-p to onoma-2.
           move epitheto-p to epitheto-2.
           
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

                      initialize polites-rec

                       move idx-table(y) to index-fields

                       move kodikos-p to kodikos-2 
                       move onoma-p to onoma-2
                       move epitheto-p to epitheto-2

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
           if answ = 1 display 
           "⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ Ñ†©òö‡ö„ ë´¶†ÆúÂ‡§ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø"
                   line 3 position 12 control col-cnt.
           if answ = 2 display 
           "⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ ãú´òô¶¢„ ë´¶†ÆúÂ‡§ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø"
                   line 3 position 12 control col-cnt.           
           if answ = 3 display 
           "⁄ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒ É†òö®ò≠„ ë´¶†ÆúÂ‡§ ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒø"
                   line 3 position 12 control col-cnt.
           perform varying x from 4 by 1 until x > 18
           display 
           "≥                                                         ≥"
                   line x position 12
           end-perform.
           display 
           "¿ƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒƒŸ"
                   line 18 position 12

                   "â‡õ†°Ê™ èú¢·´û:" line 4 position 15
                   "ÑßÂüú´¶.......:" position 15
                   "Ó§¶£ò.........:" position 15
                   "â‡õ.ÑÂõ¶¨™:" line 8 position 15
                   "ÑÂõ¶™:" line 8 position 35
                   "â‡õ.è®¶™..:" line 9 position 15
                   "è®¶™.:" line 9 position 35 
                   "èú®†¶Æ„:" line 10 position 15
                   "É†úÁü¨§©û:" line 10 position 39
                   "â‡õ.ã¶§.ã‚´®û©û™:" line 11 position 15
                   "ã¶§.ã‚´®û©û™:" line 11 position 42
                   "í†£„........:" line 12 position 19
                   "èò®ò´û®„©ú†™:" line 13 position 15
                   "ESC" line 17 position 25
                   "F4" line 17 position 45.

           move  kodikos-p to pkod. move epitheto-p to pepitheto.
           move onoma-p to ponoma.

           display pkod line 4 position 30 control col-cnt
                   pepitheto position 30
                   ponoma position 30.
           move c-name(3) to bc. move c-name(8) to fc.
           display "Ä°Á®‡©û" line 17 position 28 control col-cnt
                   "Ñ†©òö‡ö„" line 17 position 48

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
           if visual-values not = save-var

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
           move 1 to pcode-idx.
           move visual-values to save-var.

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

           if epitheto-p (22:2) = spaces
                  display und-line line 8 position 52 size 2 
                                   control nor-cnt

           else display epitheto-p (22:2) line 8 position 52 
                                   control nor-cnt
           end-if.

           if dieythynsh-p (22:2) = spaces
                  display und-line line 10 position 52 size 2 
                                   control nor-cnt

           else display dieythynsh-p (22:2) line 10 position 52 
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

       mnu-box.
           move c-name(6) to bc. move c-name(8) to fc.

           display "⁄ƒƒƒƒƒƒƒƒƒƒƒø" line 7 position 51
                                        control col-cnt
                   "¿ƒƒƒƒƒƒƒƒƒƒƒŸ" line 13 position 51.

           perform varying x from 8 by 1 until x > 12
           display "≥           ≥" line x position 51
	   end-perform.

           display bc-line line 14 position 53 size 13 low 
                           control nor-cnt.
           perform varying x from 8 by 1 until x > 13
           display bc-line line x position 64 size 2 low
           end-perform.

           perform varying x from 1 by 1 until x > 3
           display m-opt(x) line o-ln(x) position o-pos(x) size o-sz(x)
                            control col-cnt
	   end-perform.
           compute rt = 1.
           go routin-box.

       routin-box.
           move c-name(3) to bc. move c-name(8) to fc.
           display m-opt(rt) line o-ln(rt) position o-pos(rt)
                             size o-sz(rt) control col-cnt.

           accept flag line 24 position 80 no beep control acpt-col
                       on exception chr-ch
           move c-name(6) to bc move c-name(8) to fc
           display m-opt(rt) line o-ln(rt) position o-pos(rt)
                             size o-sz(rt) control col-cnt
           evaluate chr-ch
              when 52 if rt > a subtract 1 from rt
                      else move t to rt end-if go routin-box
              when 53 if rt < t add 1 to rt
                      else move a to rt end-if go routin-box
              when 27 perform clean-box perform telos go begin
              when other go routin-box
           end-evaluate.

           if flag = 0 move rt to answ
                       perform clean-box
                       go begin-5 
           else go routin-box.

       find-code.
           read spolites next at end go find-code-3.
           if pcode = c-count add 1 to c-count 
                              go find-code              
           else go find-code-3.
            

       find-code-2.
           read spolites invalid move kodikos-p to pkod 
                                      move onoma-p to ponoma
                                      move epitheto-p to pepitheto
                                      perform disp-info 
                                      perform disp-ln go a2
                         not invalid display "skata" line 7 position 1
           end-read. 

       find-code-3.
           initialize visual-values 
           move c-count to pcode
           move 1 to pcode-idx
           perform values-box 
           perform save-box-2 
           display pcode line 6 position 1
           perform find-code-2
           display spolites-rec line 23 position 1 
           go a2.   

       message-error-box-2.
           exit.

       message-error-box-3.
           go begin-2.

       index-rec.
           read spolites next at end go index-disp1. 
           if kodikos-p = pkod
              move pcode to pcode-idx            
              move idx-fields to idx-table (index-count)
              add 1 to index-count
              go index-rec
           end-if.
           go index-rec.

       index-disp1.
           if index-count not > 1 perform telos 
                                  go begin                                   
           end-if.

           move 1 to y. move 1 to k. subtract 1 from index-count.
           move 1 to e. move 1 to m.

           if index-count < 10 move 1 to l 

           else compute l rounded = ( index-count / 10) + .5

           end-if.
           
           perform index-box.

       index-disp2.
           move c-name(3) to bc. move c-name(7) to fc.

           move idx-table(y) to idx-fields.

           move pcode-idx to pcode-idx-2. 
           move peidos-per to peidos-per-2. 
           move pperioxi to pperioxi-2.
           move pdieythynsh to pdieythynsh-2.

           if idx-pos = spaces move 1 to k m go indexselect.

           display idx-pos line idx-ln(k) position 8 
                                control col-cnt.

           if k = 10 compute y = ( e * 10 ) - 9 move 1 to k 
                     go indexselect.

           if y = index-count compute y = ( e * 10 ) - 9 move 1 to k
                              go indexselect.
                  
           add 1 to y k m. 
                     
           go index-disp2.

       indexselect.
           move c-name(7) to bc. move c-name(3) to fc.

           move idx-table(y) to idx-fields.

           move pcode-idx to pcode-idx-2.
           move peidos-per to peidos-per-2. 
           move pperioxi to pperioxi-2.
           move pdieythynsh to pdieythynsh-2.
           
           display idx-pos line idx-ln(k) position 8 
                             control col-cnt.

           accept flag line 24 position 80 no beep control acpt-col
                               on exception chr-ch

           move c-name(3) to bc move c-name(7) to fc

           display idx-pos line idx-ln(k) position 8 
                             control col-cnt

            evaluate chr-ch
              when 52 if y > ( e * 10 ) - 9 subtract 1 from y k

                      else move m to k 
                           compute y = ( ( e * 10 ) - 9 ) + ( m - 1 ) 
 
                      end-if go indexselect

              when 53 if y < ( ( e * 10 ) - 9 ) + ( m - 1 ) add 1 to y k

                      else compute y = ( e * 10 ) - 9 move 1 to k
  
                      end-if go indexselect
                      

              when 67  if e > 1 subtract 1 from e perform page-box 
                         perform index-clean
                         compute y = ( e * 10 ) - 9 move 1 to k m
                         go index-disp2 

                      else go indexselect

                      end-if

              when 68 if e < l add 1 to e perform page-box 
                         perform index-clean
                         compute y = ( e * 10 ) - 9 move 1 to k m 
                         go index-disp2

                      else go indexselect

                      end-if

              when 27 perform clean-box perform telos go begin

              when other go indexselect

           end-evaluate.

           if flag = 0 and idx-pos not = spaces

                      initialize spolites-rec

                       move idx-table(y) to idx-fields
                       move pcode-idx to pcode
                       move pcode-idx to pcode-idx-2  
                       move peidos-per to peidos-per-2 
                       move pperioxi to pperioxi-2
                       move pdieythynsh to pdieythynsh-2

                       move c-name(2) to bc move c-name(7) to fc

                       display idx-pos line idx-ln(k) position 8 
                                            control col-cnt

                       perform clean-box 
                       perform values-box 
                       perform find-code-2 
                       perform save-box-2
                       perform disp-info  
                       perform disp-ln
                       if answ = 2 go a2 
                       else if answ = 3 go dia

           else go indexselect

           end-if. 

       index-const.
           perform clean-box.
           move 1 to index-count.
           if const-count = 1 move 1 to type-code 
                              move 9 to const-end
           end-if. 
           if const-count = 2 move 30 to type-code
                              move 32 to const-end
           end-if. 
           if const-count = 3 move 20 to type-code
                              move 21 to const-end
           end-if.      

       index-const-3.
           read const next at end go index-disp-const. 
           if type-code > const-end go index-disp-const.
           move const-rec to idx-table-2 (index-count).
           add 1 to index-count.
           go index-const-3.

       index-disp-const.
           move 1 to y. move 1 to k. subtract 1 from index-count.
           move 1 to e. move 1 to m.

           if index-count < 10 move 1 to l 

           else compute l rounded = ( index-count / 10) + .5

           end-if.
           
           perform index-box-2.

       index-disp-code-2.
           move c-name(3) to bc. move c-name(7) to fc.

           move idx-table-2(y) to const-rec.

           move type-code to type-code-2. 
           move type-descr to type-descr-2.

           display index-pos-2 line idx-ln-2(k) position 58 
                                control col-cnt.

           if k = 10 compute y = ( e * 10 ) - 9 move 1 to k 
                     go index-select-2.

           if y = index-count compute y = ( e * 10 ) - 9 move 1 to k
                              go index-select-2.
                  
           add 1 to y k m. 
                     
           go index-disp-code-2.

       index-select-2.
           move c-name(7) to bc. move c-name(3) to fc.

           move idx-table-2(y) to const-rec.

           move type-code to type-code-2. 
           move type-descr to type-descr-2.

           display index-pos-2 line idx-ln-2(k) position 58 
                             control col-cnt.

           accept flag line 24 position 80 no beep control acpt-col
                               on exception chr-ch

           move c-name(3) to bc move c-name(7) to fc

           display index-pos-2 line idx-ln-2(k) position 58 
                             control col-cnt

            evaluate chr-ch
              when 52 if y > ( e * 10 ) - 9 subtract 1 from y k

                      else move m to k 
                           compute y = ( ( e * 10 ) - 9 ) + ( m - 1 ) 
 
                      end-if go index-select-2

              when 53 if y < ( ( e * 10 ) - 9 ) + ( m - 1 ) 
                           add 1 to y k

                      else move 1 to k
                           compute y = ( e * 10 ) - 9 
  
                      end-if go index-select-2
                      

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

                      else go index-select-2

                      end-if

              when 27 perform clean-index-box 
                      if const-count = 1 go a2 end-if
                      if const-count = 2 go a4 end-if
                      if const-count = 3 go a8 end-if   

              when other go index-select-2

           end-evaluate.

           if flag = 0 and index-pos-2 not = spaces

                       initialize const-rec

                       move idx-table-2(y) to const-rec

                       move type-code to type-code-2 
                       move type-descr to type-descr-2

                       move c-name(2) to bc move c-name(7) to fc

                       display index-pos-2 line idx-ln-2(k) position 58 
                                            control col-cnt

                       perform clean-index-box 
                       if const-count = 1 move type-code to peidos-code
                                          move type-descr to peidos-per
                                          perform disp-ln
                                          go a2
                       end-if 
                       if const-count = 2 move type-code to ppros-code
                                          move type-descr to ppros-per 
                                          perform disp-ln
                                          go a4
                       end-if
                       if const-count = 3 move type-code to pmetra-code
                                          move type-descr to pmetra-num
                                          perform disp-ln
                                          go a8
                       end-if  
           else 
              go index-select-2

           end-if. 

       index-box-2.
           move c-name(3) to bc. move c-name(8) to fc.
           display "…ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕª" 
                   line 3 position 56 control col-cnt
                    
           perform varying x from 4 by 1 until x > 14
           display "∫                    ∫" line x position 56 
           end-perform.   

           display "»ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº" line 15 position 56
  
           display und-line line 13 position 57 size 20.

           display bc-line line 16 position 58 control nor-cnt size 22.

           perform varying x from 4 by 1 until x > 15
           display bc-line line x position 78 size 2 control nor-cnt
           end-perform.

           perform page-box-2.


       page-box-2.
           move c-name(3) to bc. move c-name(7) to fc.
           display "ëú¢Âõò   /  " line 14 position 62 control col-cnt
                   e line 14 position 69
                   l line 14 position 72.

       clean-index-box.
           move c-name(4) to bc. move c-name(2) to fc.

           perform varying x from 3 by 1 until x > 22 
           display bc-line line x position 56 size 24
                           control col-cnt
           end-perform. 

