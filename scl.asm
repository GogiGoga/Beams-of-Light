    device zxspectrum128

entrya=$100
    macro   sectors datab,datae
 IF low (datae-datab)=0
   db high (datae-datab)
 ELSE
   db (1+high (datae-datab))
 ENDIF
    endm

    org entrya-14*1-9
scl_b:
;header_b:
 db "SINCLAIR"
 db 1; filez
;-1st
file1:
;    12345678t
 db "Beams   B"
 dw basic_e-basic_b-4;start
 dw basic_e-basic_b-4;length?
 sectors basic_b,scl_e;sectors


 org entrya
basic_b:
 db #00,#01;номер строки
 DW EndLine1 - Line1
Line1:
 db #EA
 
    res 4,(iy+1)
    xor a:out ($FE),a
    ld hl,$5AFF,de,$5AFe,bc,$1B00-1,(hl),0:lddr
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; L O A D E R ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    macro trload mema,secs
 ld hl,mema
 ld bc,5+secs*256
 ld de,(#5CF4)
 call $3d13
    endm

    macro u where,jumpto;unpack
 ld hl,(23635)
 push de
 ld de,.patch-entrya
 add hl,de
 pop de
 ld a,$C3,(where),a,(where+1),hl
 jp jumpto
.patch:
    endm

    macro pokeb addr,byt
        ld a,byt,(addr),a
    endm

    macro pokew addr,wrd
        ld hl,wrd,(addr),hl
    endm
    
    macro bkey addr
    	ld hl,$3daf,(addr),hl
    endm

x2:
 di
    ld a,$10,bc,$7FFD:out (c),a
; xor a
; ld (23693),a
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	ld sp,$5fff

	trload $8000,$0C
	call $8000

	trload $6000,43
	call $6000

;myjump equ $6000;$5b14
; ld hl,(23635)
; ld de,my_b-entrya
; add hl,de
; ld de,myjump
; ld bc,my_e-my_b
; ldir
; call myjump
;
;my_b:
;basiclen=$0400
;;	jr $
;	trload $4000,basiclen/256; - для васика
;	di
;	ld hl,$4000,de,$5d40,bc,basiclen:ldir
; ld hl,myjump+pa1-my_b
; ld de,$5d9e
; ld bc,pa2-pa1
; ldir
;	jp 23882
;pa1:
;	bkey $6d64
;	jp $5dec
;pa2:
;	jr $
;patchplace=$5e02
; ld hl,mypab-my_b+myjump,(patchplace+1),hl
;   ld a,$C3,(patchplace),a
;   jp $5D93
;mypab:
;;	jp $9FFE
;patchplace2=$a094
;   ld hl,mypab2-my_b+myjump,(patchplace2+1),hl
;   ld a,$C3,(patchplace2),a
;	jp $9ffe
;mypab2:
;	bkey $a178
;	jp $A000

;атрибутики
;   ld hl,$5800,de,$5801,bc,$02ff,(hl),$47:ldir

; ----------------------------------------------------
; ld hl,$4000,de,$4001,bc,$6143,(hl),l:ldir

 jr $
my_e:
;------------------------------
 db #0D
EndLine1:
 db #00,#02
 DW EndLine2 - Line2
Line2:
;border not pi
 db 231,195,167
 db #3A; :
 
 db #20,#FD,#B0,#22,#32,#34,#35,#37,#35,#22;clear val "24575"
 db #3A; :
 db #F9,#C0,#28,#35;randomize usr (5+256*peek val "23635"+peek val "23636"
 db #0E,#00,#00,#05,#00,#00,#2B
 db #32,#35,#36
 db #0E,#00,#00,#00,#01,#00,#2A,#BE

 db #B0
 db #22,#32,#33,#36,#33,#36,#22;"23635"
 db #2B;???
 db #BE
 db #B0
 db #22,#32,#33,#36,#33,#35,#22;"23636"
 db #29,#0D;)
 db #80
 db #AA,1,0;;;;;;;;;;;;;autorun line,change program length to -4, e.g. 83-4=79
EndLine2:
basic_e:

 align 256
 incbin "title.code"
 align 256
 incbin "beams.code"
 align 256

scl_e:
    savebin "huj.scl",scl_b,scl_e-scl_b

    LUA

    local fp
    local checksum
-- never rename file
    fp = assert(io.open("huj.scl", "rb"))
    checksum=0
    while true do
        local byte = fp:read(1)
        if byte==nil then
            break
        end
        checksum=checksum+string.byte(byte)
    end
    assert(fp:close())
    print("writing",string.format("%08X",checksum))
    fp = assert(io.open("huj.scl", "a"))

    for i=1,4 do
        fp:write(string.char(checksum%256))
        checksum=math.floor(checksum/256)
    end

--  assert(fp:flush())
    assert(fp:close())

--  os.rename("huj.scl", "Xtermin.scl")

    local ss=""
    for i=0,7,1 do
        ss=ss..string.char( sj.get_byte( sj.get_label("file1")+i ) )
    end

    ss=ss:match "^%s*(.-)%s*$"
--  print(ss)
    os.rename("huj.scl", ss..".scl")

    ENDLUA
