       id division.
       program-id. const01.
       author. temerzidis stavros.
       environment division.
       configuration section.
       source-computer. ibm-at.
       object-computer. ibm-at.
       input-output section.
       file-control.
           select optional const assign random file-name
           organization indexed
           access dynamic

           record key const-code
           status const-stat.

       data division.
       file section.
       fd const.
       01 const-rec.
          02 const-code.
             03 type-code pic 99.
          02 type-descr pic x(15).

       working-storage section.
       77 chr-ch pic 9(3) comp-1.

       77 synexeia pic x.
          88 syn-n values "N" "n" "�" "�".
          88 syn-o values "O" "o" "�" "�".

       01  str-var.
           02 bc-line pic x(80) value all "�".
           02 inv-line pic x(80) value all spaces.
           02 rev-line pic x(80) value all "�".
           02 rev-half pic x(80) value all "�".
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
           02 e pic 99.
           02 l pic 99.
           02 x pic 9999.
           02 y pic 99.

       01  index-var.
           02 index-count pic 99.
           02 t-code pic 99.

       01  save-var pic x(15).

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

       01  acpt-col.
           02 pic x(47) value "fcolor=black,bcolor=black".
       
       01  acpt2-col.
           02 pic x(47) value "fcolor=white,bcolor=white".

       01  index-table.
           02 pic x(17) value spaces.
       01  redefines index-table.
           02  idx-table pic x(20) occurs 50 times.

       01  index-lines.
           02 pic x(30) value "040506070809101112131415161718".
       01  redefines index-lines.
           02 idx-ln pic 9(2) occurs 15 times.

       01  index-pos.
           02 type-code-2 pic 99.
           02 filler pic x value spaces.
           02 type-descr-2 pic x(15).

       01  file-used.
           02 fconst pic x(12) value "const.dat".

       01  file-name pic x(29).
       01  fn redefines file-name.
           03 fl-disk pic x(2).
           03 fl-rest pic x(15).
           03 fl-nam pic x(12).

       01  file-name-2 pic x(17).
       01  file-name-3 pic x(2).

       01  file-status.
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
           move fconst to fl-nam. open i-o const.
           perform code-box.


       begin-2. 
           initialize const-rec.
           

       a1.
           accept type-code line 9 position 30 echo no beep tab
               control acpt2-col on exception chr-ch
           evaluate chr-ch
              when 27 perform clean-box-1 go telos-1
              when 21 go index-code
              when 1 perform help-box-1 go a1
              when other go a1
           end-evaluate.

           if type-code = zeros go a1.
           perform find-rec.
           perform save-box-2.

       a2.
           accept type-descr line 11 position 29 update no beep 
              control acpt2-col on exception chr-ch
           evaluate chr-ch
              when 27 perform save-box perform clean-box-1 go telos-1
              when 1 perform help-box-2 go a2
              when 22 perform save-routin
              when 23 perform chance-code-routin go a1
              when other go a2
           end-evaluate.
           move type-code to file-name-3.
           move file-name-3 to file-name-2
           move type-descr to file-name-2(3: ).
           display file-name-2 line 23.
           go a2.           


       save-routin.
           initialize answ. 
           write const-rec invalid

                 rewrite const-rec invalid

                         move const-stat to file-stat 
                         perform invalid-info
                         move 1 to answ

                 not invalid move 2 to val-stat perform valid-info

                 end-rewrite

           not invalid move 1 to val-stat perform valid-info
           end-write.

           if file-stat = 23 and answ = 1 
              move 22 to file-stat 
              perform invalid-info
           end-if.


       chance-code-routin.
           perform save-box. initialize const-rec.
           display type-code  line 9  position 30 control acpt2-col
                   type-descr line 11 position 29.


       telos-1.
           close const.

       telos-2.
           goback.


       find-rec.
           read const invalid  move const-stat to file-stat
                                perform invalid-info

                    not invalid move 4 to val-stat
                                perform valid-info
                                perform disp-info

           end-read.


       disp-info.
           display type-code  line 9  position 30 control acpt2-col
                   type-descr line 11 position 29. 


       index-code.
           perform clean-table.
           move type-code to t-code. move 1 to index-count.
           start const key not < type-code invalid key go a1.


       index-code-3.
           read const next at end go index-disp-1. 

           move const-rec to idx-table (index-count).

           add 1 to index-count.

           go index-code-3.

       index-disp-1.
           if index-count not > 1 initialize const-rec go a1.

           move 1 to y. move 1 to k. subtract 1 from index-count.
           move 1 to e. move 1 to m.

           if index-count < 15 move 1 to l 

           else compute l rounded = ( index-count / 15) + .5

           end-if.
           
           perform index-box.

       index-disp-2.
           move c-name(3) to bc. move c-name(7) to fc.

           move idx-table(y) to const-rec.

           move type-code to type-code-2. 
           move type-descr to type-descr-2.

           if index-pos = spaces move 1 to k m go index-select.

           display index-pos line idx-ln(k) position 58 
                                control col-cnt.

           if k = 15 compute y = ( e * 15 ) - 14 move 1 to k 
                     go index-select.

           if y = index-count compute y = ( e * 15 ) - 14 move 1 to k
                              go index-select.
                  
           add 1 to y k m. 
                     
           go index-disp-2.

       index-select.
           move c-name(7) to bc. move c-name(3) to fc.

           move idx-table(y) to const-rec.

           move type-code to type-code-2. 
           move type-descr to type-descr-2.

           display index-pos line idx-ln(k) position 58 
                             control col-cnt.

           accept flag line 24 position 80 no beep control acpt-col
                               on exception chr-ch

           move c-name(3) to bc move c-name(7) to fc

           display index-pos line idx-ln(k) position 58 
                             control col-cnt

            evaluate chr-ch
              when 52 if y > ( e * 15 ) - 14 subtract 1 from y k

                      else move m to k 
                           compute y = ( ( e * 15 ) - 14 ) + ( m - 1 ) 
 
                      end-if go index-select

              when 53 if y < ( ( e * 15 ) - 14 ) + ( m - 1 ) 
                           add 1 to y k

                      else move 1 to k
                           compute y = ( e * 15 ) - 14 
  
                      end-if go index-select
                      

              when 67  if e > 1 subtract 1 from e perform page-box 
                         perform index-clean
                         compute y = ( e * 15 ) - 14 move 1 to k m
                         go index-disp-2 

                      else go index-select

                      end-if

              when 68 if e < l add 1 to e perform page-box 
                         perform index-clean
                         compute y = ( e * 15 ) - 14 move 1 to k m 
                         go index-disp-2

                      else go index-select

                      end-if

              when 27 perform clean-index-box go begin-2
    
              when 1 perform help-box-3 go index-disp-2

              when other go index-select

           end-evaluate.

           if flag = 0 and index-pos not = spaces

                      initialize const-rec

                       move idx-table(y) to const-rec

                       move type-code to type-code-2 
                       move type-descr to type-descr-2

                       move c-name(2) to bc move c-name(7) to fc

                       display index-pos line idx-ln(k) position 58 
                                            control col-cnt

                       perform clean-index-box 
                       perform find-rec perform save-box-2 go a2

           else 
              go index-select

           end-if. 
       
       invalid-info.
           evaluate file-stat
              when "10"  move "   �⢦� ����妬   " to inval-mess
                                          perform invalid-message

              when "22"  move "   ����� �������   " to inval-mess
               perform message-error-box thru message-error-box-2

              when "23"  move "    �� �������    " to inval-mess
                                          perform invalid-message

              when "24"  move "  �⣠�� �� ����� " to inval-mess
               perform message-error-box thru message-error-box-2

              when "30"  move "  ��ᙞ ��� �婡�  " to inval-mess
               perform message-error-box thru message-error-box-2

              when "98"  move "������⤦ �����㨠�" to inval-mess
               perform message-error-box thru message-error-box-2

              when other move "������� �� SYS.OP. " to inval-mess                 
               perform message-error-box thru message-error-box-2

           end-evaluate.

       valid-info.
           evaluate val-stat
              when 1  move " � �������� ⚠��  " to val-mess

              when 2  move " H ������ ⚠��    " to val-mess

              when 3  move " � �������� ⚠��  " to val-mess

              when 4  move " �������� ��������" to val-mess

              when 5  move " ��礘�� ��������  " to val-mess.
           move 0 to val-stat.

           perform valid-message.

       invalid-message.
           perform message-box.
           display inval-mess line 4 position 29 control col-cnt.

       valid-message.
           perform message-box.
           display val-mess line 4 position 29 control col-cnt.

       code-box.
           move c-name(6) to bc. move c-name(8) to fc.
           display "�������� ������ �ᨡ�� ������Ŀ" 
                   line 8 position 15 control col-cnt
                   "� ������                      �"
                   line 9 position 15
                   "�                              �" 
                   line 10 position 15
                   "� ���������                    �" 
                   line 11 position 15
                   "��������������������������������" 
                   line 12 position 15

                   bc-line line 13 position 17 size 32 
                           control nor-cnt.

           perform varying x from 9 by 1 until x > 12
           display bc-line line x position 47 size 2 control nor-cnt
           end-perform.

           display "*  *" line 9 position 29 control acpt2-col
                   "               " line 11 position 29.                         


       index-box.
           move c-name(3) to bc. move c-name(8) to fc.
           display "��������������������ͻ" 
                   line 3 position 56 control col-cnt
                    
           perform varying x from 4 by 1 until x > 19
           display "�                    �" line x position 56 
           end-perform.   

           display "��������������������ͼ" line 20 position 56
  
           display und-line line 18 position 57 size 20.

           display bc-line line 21 position 58 control nor-cnt size 22.

           perform varying x from 4 by 1 until x > 20
           display bc-line line x position 78 size 2 control nor-cnt
           end-perform.

           perform page-box.


       page-box.
           move c-name(3) to bc. move c-name(7) to fc.
           display "���囘   /  " line 19 position 62 control col-cnt
                   e line 19 position 69
                   l line 19 position 72.

       help-box.     
           move c-name(2) to bc. move c-name(3) to fc.
           display "�����������������������Ŀ" 
                   line 10 position 50 control col-cnt.

           perform varying x from 11 by 1 until x > 21
             display "�                       �" line x position 50 
                                           control col-cnt
           end-perform.
                                           
           display "�������������������������" line 21 position 50

                   bc-line line 22 position 52 size 25 
                           control nor-cnt.

           perform varying x from 11 by 1 until x > 21
           display bc-line line x position 75 size 2 control nor-cnt
           end-perform.
 
       help-box-1.
           perform help-box.
           display " �������.: 1-99        " line 11 position 51 
                                               control col-cnt
                   " ESC.....: 륦���      " line 13 position 51
                   " ENTER �: ��������ਫ਼ " line 15 position 51
                   " CRTL-F1.: �����㨠�   " line 17 position 51
           accept flag2 line 24 position 80 no beep control acpt-col.
           perform clean-help-box.      
               
    
       help-box-2.
           perform help-box.
           display " ���������.: 1-15 ���. " line 11 position 51
                                               control col-cnt
                   " ESC.....: 륦���      " line 13 position 51
                   " CRTL-F2.: �����騠��  " line 15 position 51
                   " CRTL-F3.: ���.������� " line 17 position 51
                   " CRTL-F5.: ��������    " line 19 position 51.
           accept flag2 line 24 position 80 no beep control acpt-col.
           perform clean-help-box.                


       help-box-3.
           perform help-box.
           perform varying x from 11 by 1 until x > 19
           display inv-line line x position 75 size 2 control nor-cnt
           end-perform.
           display "��" line 20 position 75 low.
           display " PGUP...: �����.���囘 " line 11 position 51
                                               control col-cnt 
                   " PGDN...: �����.���囘 " line 13 position 51
                   " .....: �夞�� ��ᨘ�" line 15 position 51
                   " ENTER�: �������      " line 17 position 51
                   " ESC....: ���ਫ਼      " line 19 position 51.
           accept flag2 line 24 position 80 no beep control acpt-col.
           perform varying x from 10 by 1 until x > 22
                move c-name(4) to bc move c-name(2) to fc
                display bc-line line x position 50 size 6
                                control col-cnt
                move c-name(3) to bc move c-name(8) to fc
                display "�                    " line x position 56
                                control col-cnt
           end-perform.
           display "���������������������" line 20 position 56
                   bc-line line 21 position 58 size 22 control nor-cnt.
           move c-name(4) to bc. move c-name(2) to fc.
           display bc-line line 21 position 56 size 2 control col-cnt
                   bc-line line 22 position 56 size 21.
           compute y = ( e * 15 ) - 14 move 1 to k m.
           perform page-box.


       save-box.
           if type-descr not = save-var

              perform error-box

              display "   (!) �������        " line 16 position 29 
                                             control col-cnt
                      "----------------------" line 17 position 29
                      "     �� ����� ��     " line 18 position 29
                      "   ������;(N/O):     " line 20 position 29

              move "n" to synexeia

              accept synexeia line 20 position 46 update tab blink

              evaluate true
                 when syn-n perform save-routin
                 when syn-o continue
                 when other go save-box
              end-evaluate
              perform clean-error-box
           end-if.

       save-box-2.
           initialize save-var.
           move type-descr to save-var.

       clean-table.
           perform varying x from 1 by 1 until x > 50
           initialize idx-table(x)
           end-perform.

       clean-box-1.
           move c-name(4) to bc. move c-name(2) to fc.

           perform varying x from 3 by 1 until x > 22 
           display bc-line line x position 5 size 71
                           control col-cnt
           end-perform.


       clean-help-box.
           move c-name(4) to bc. move c-name(2) to fc.

           perform varying x from 10 by 1 until x > 22 
           display bc-line line x position 50 size 27
                           control col-cnt
           end-perform.
 

       clean-index-box.
           move c-name(4) to bc. move c-name(2) to fc.

           perform varying x from 3 by 1 until x > 22 
           display bc-line line x position 56 size 24
                           control col-cnt
           end-perform. 

       clean-error-box.
           move c-name(4) to bc. move c-name(2) to fc.

           perform varying x from 15 by 1 until x > 23 
           display bc-line line x position 28 size 30
                           control col-cnt
           end-perform. 


       index-clean.
           move c-name(3) to bc. move c-name(3) to fc.
           perform varying x from 5 by 1 until x > 18
           display inv-line line x position 58 size 18 control col-cnt
           end-perform.


       error-box.
           move c-name(2) to bc. move c-name(3) to fc.
           display "����������������������Ŀ" line 15 position 28 
                   control col-cnt
                   "�                      �" line 16 position 28
                   "�                      �" line 17 position 28
                   "�                      �" line 18 position 28
                   "�                      �" line 19 position 28
                   "�                      �" line 20 position 28                                         
                   "�                      �" line 21 position 28
                   "������������������������" line 22 position 28

           bc-line line 23 position 30 size 24 control nor-cnt.

           perform varying x from 16 by 1 until x > 22
           display bc-line line x position 52 size 2 control nor-cnt
           end-perform.


       message-box.
           move c-name(2) to bc. move c-name(3) to fc.
           display "����������������������������Ŀ" line 3 position 24 
                                                     control col-cnt
                   "�                            �" line 4 position 24
                   control col-cnt
                   "������������������������������" line 5 position 24

                   bc-line line 6 position 26 size 30 control nor-cnt.

           perform varying x from 4 by 1 until x > 5
           display bc-line line x position 54 size 2 control nor-cnt
           end-perform.
         
       
       message-error-box.
           perform error-box.
           move c-name(2) to bc. move c-name(4) to fc.
           display  "<<                  >>" line 16 position 29
                                             control col-cnt
                     inval-mess line 16 position 30
                    "   ��� ������櫜���   " line 17 position 29
                    "     ��������圪      " line 18 position 29
                    "    �����⥫� ��      " line 19 position 29
                    "  ��暨���� ��㟜���  " line 20 position 29
                    "     ��� �� �����     " line 21 position 29.
                    
           accept flag line 24 position 80 control acpt-col.
           perform clean-error-box.
           initialize type-descr.
           display type-code  line 9  position 30 control acpt2-col
                   type-descr line 11 position 29.
           

       message-error-box-2.
           exit.


       message-error-box-3.
           go begin-2.


      


           



