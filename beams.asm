;+при отрисовке сбрасываются подсветки начальных точек
	device zxspectrum128
        ORG #4000
 incbin "title_0.scr"
 
        ORG #6000
begin
;  1
; 2 8
;  4
    res 4,(iy+1)
	ld a,0:out ($FE),a
	ld a,$10,bc,$7FFD:out (c),a
	ld sp,$5fff
;*************************************************
	di
;	ld sp,$5FE0
;	ld a,$5F,i,a
;	im 2
;	ld hl,intvec
;	ld ($5FFF),hl

	ld hl,$b000,de,$b001,bc,256,(hl),$b1:ldir
	ld a,$b0,i,a
	im 2
	ld a,$C3,($b1b1),a
	ld hl,intvec,($b1b2),hl
	ld hl,song:call INIT
	ei
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	call wk
	call fade

	ld a,8
	ld b,fadt/256
fal:
	ei
	halt
	halt
	halt
	ld hl,$5800
	push af
	call fade_r
	pop af
	dec a
	jp nz,fal

;	ld a,$10,bc,$7FFD:out (c),a
	call generate_corners
	
;	ld hl,$5800,de,$5801,bc,767,(hl),$47:ldir
;	jr $
	call blackscr

;печать номера уровней
	ld ix,levelsn
	call print
	call print
	call print
	call print

	call print
	call print
	call print

	call print

;	ld hl,$C000
;	ld de,$4000
;	ld ixl,16
;co_p:
;	push hl,de
;	ld bc,32
;	ldir
;	pop de,hl
;	inc h
;	call nbde
;	dec ixl
;	jr nz,co_p
;	jr $
;*************************************************
;debug info
;	ld c,0
;drw1:
;	ld h,0,l,c
;	ld de,hl
;	add hl,hl
;	add hl,hl
;	add hl,de;&5
;	ld de,ven+130
;	add hl,de
;	push hl;number
;	
;	ld h,0,l,c
;	add hl,hl
;	add hl,hl
;	add hl,hl
;	add hl,hl
;	add hl,hl
;	ld de,tiles+8
;	add hl,de
;	
;	pop de
;	ld b,5
;drw2:
;	ld a,(de):inc de
;	;or (hl)
;;	ld (hl),a
;	inc hl,hl
;	djnz drw2
;	inc c
;	ld a,c
;	cp 9
;	jp nz,drw1

;выбор уровня
	ld de,0;D.E=y.x
chl:
	call crs2
ch_k:
	ei
	halt
	halt
	halt
	halt
	call wk
;5-left  f7fe,b4
;6-down  effe,b4
;7-up    effe,b3
;8-right effe,b2
;0-fire  effe,b0
keyright:
	ld bc,$f7fe:in a,(c)
	and 16:jr nz,keyleft
	ld a,e:cp 0:jr z,ch_k;keylp
	call crs2
	dec e
	jp chl;game_lp
keyleft:
	ld bc,$effe:in a,(c)
	and 4:jr nz,keyup
	ld a,e:cp 7:jr z,ch_k;keylp
	call checkm
	sub 8
	cp 49;+1
	jp nc,ch_k

	call crs2
	inc e
	jp chl;game_lp
keyup:
	ld a,$ef:in a,($FE)
	and 8:jr nz,keydown
	ld a,d:cp 0:jr z,ch_k;keylp
	call crs2
	dec d
	jp chl;game_lp

keydown:
	ld a,$ef:in a,($FE)
	and 16:jr nz,keyfire
;	ld a,d:cp 6:jr z,ch_k;keylp
	call checkm
;	ld a,d
;	add a,a
;	add a,a
;	add a,a
;	add a,e
;	add a,9
;	cp 49+1
	jp nc,ch_k

	call crs2
	inc d
	jp chl;game_lp
keyfire:
	ld a,$bf:in a,($fe)
	rra:jp nc,gotcha
	ld a,$ef:in a,($fe)
	rra:jp c,ch_k;keylp
gotcha:
;	jp chl
	ld a,d
	add a,a
	add a,a
	add a,a
	add a,e
	inc a
	ld (level),a
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
start_level:
	call blackscr

	call clearmem

	ld de,$4048,hl,$5848:call drawstripe
	ld de,$5048,hl,$5A48:call drawstripe

	ld a,(level)
	ld hl,l_n
	call printnum

	ld ix,mes1
	call print

	;ld hl,ch1

	ld a,(level),l,a,h,0
	add hl,hl
	ld de,level_t_b
	add hl,de
	ld a,(hl):inc hl:ld h,(hl),l,a
	call coplvl


	call bpath

crxy:	ld de,$0101;y.x=D.E
;game_lp:
;	push de
;	jr $
	call drawmap

game_lp:
	call crs
	
	ei
	halt
	halt
	halt
	halt
;keyloop - only 1,2,4,8 can rotate a*2
;5-left  f7fe,b4
;6-down  effe,b4
;7-up    effe,b3
;8-right effe,b2
;0-fire  effe,b0
keylp:
	ei
	halt
	xor a
	in a,($FE)
	cpl
	and 31
	jr z,keylp
	ld bc,$f7fe:in a,(c)
	and 16:jr nz,tesleft
	ld a,e:cp 1:jr z,keylp
	call crs
	dec e
	jp game_lp
tesleft:
	ld bc,$effe:in a,(c)
	and 4:jr nz,testup
	ld a,e:cp 7:jr z,keylp
	call crs
	inc e
	jp game_lp
testup:
	ld a,$ef:in a,($FE)
	and 8:jr nz,testdown
	ld a,d:cp 0:jr z,keylp
	call crs
	dec d
	jp game_lp

testdown:
	ld a,$ef:in a,($FE)
	and 16:jr nz,testfire
	ld a,d:cp 6:jr z,keylp
	call crs
	inc d
	jp game_lp
testfire:
	ld a,$bf:in a,($fe)
	rra:jp nc,testrot
	ld a,$ef:in a,($fe)
	rra:jp c,keylp
testrot:
;	jr $
	ld a,d
	or $B8
	ld h,a
	ld l,e
	ld a,(hl)
	and 127
	cp 1:jp z,rot
	cp 2:jp z,rot
	cp 4:jp z,rot
	cp 8:jp z,rot
	jp keylp;game_lp
rot:
	ld a,(hl)
	and $F0
	ld c,a
	ld a,(hl)
	and $0F
	add a,a
	cp 16:jr nz,no_lr
	ld a,1
no_lr:
	or c
	ld (hl),a
	push de
	call bpath
	push af
	ei:halt
	call drawmap
	pop af
	pop de
	jp nc,game_lp
	ld ix,mes2
	call print
;мерцание надписи
	ld hl,blink_t
bl_1:
	ld ix,$5808
	ld b,18
	ld a,(hl):inc hl
	ei:halt
bl_2:
	ld (ix+0),a
	ld (ix+32),a
	inc ix
	djnz bl_2
	ld a,(hl)
	cp $FF
	jp nz,bl_1
;след. уровень
	ld a,(level)
	inc a
	cp (level_t_e-level_t_b)/2; +1
	jr nz,no_l_1
	ld a,1
no_l_1:
	ld (level),a
	jp start_level
	jr $
blink_t:
	db $00,$00,$00,$00
	db $47,$47,$47,$47

	db $00,$00,$00,$00
	db $47,$47,$47,$47

	db $00,$00,$00
	db $47,$47,$47

	db $00,$00,$00
	db $47,$47,$47

	db $00,$00
	db $47,$47

	db $00,$00
	db $47,$47

	db $00,$47

	db $00,$00
	db $47,$47

	db $00,$00,$00
	db $47,$47,$47

	db $00,$00,$00,$00
	db $47,$47,$47,$47

	db $00,$47
	db $FF; конец таблицы
;!!!!!!!!!!!!!!!!!!build path!!!!!!!!!!!!!!!!!!!!!!!!!!!
reset_field:
	ld h,$b8
	ld d,$B8
rf1:
	ld l,1
	ld e,64
rf2:
	xor a
	ld (de),a
	ld a,(hl)
	and 127
	cp 3:jr z,rese
	cp 5:jr z,rese
	cp 7:jr nz,put_rf
rese:
	xor a
put_rf:
	ld (hl),a
	inc e
	inc l
	ld a,l
	cp 9
	jp nz,rf2

;-	inc e
	xor a
	ld (de),a

	inc h
	inc d
	ld a,h
	cp $C0
	jp nz,rf1
	ret
bpath:
	call reset_field
bxy:ld de,0
	ld iyl,8; начинаем с поля
btune:
;	call crs
;	ei:halt
	ld c,iyl;куда движется
	ld b,0
	ld hl,direct
	add hl,bc
	ld a,(hl);тайл-направление - или |
	ld iyh,a;
	ld hl,dxy
	add hl,bc
	ld a,(hl)
	ld (dx),a

	ld hl,d_y
	add hl,bc
	ld a,(hl)
	ld (dy),a

b_lp:
;отметить путь здесь
	ld a,d
	or $B8
	ld h,a
	ld l,e
	set 6,l
	ld a,(hl):or iyl:ld (hl),a

dx:nop
;check for X
	ld a,e
	or a:jp z,end_bp
;	cp 8+1:jp z,end_bp
	cp 8:jp nz,dy
;грань справа?
	ld a,d
	or $B8
	ld h,a
	ld l,e
	ld a,(hl)
	and 127
	cp 2:jp nz,end_bp
	set 7,(hl)
	set 6,l
		ld a,iyl
	exx
	ld h,0,l,a
	ld de,huj
	add hl,de
	ld a,(hl)
	exx
	or (hl):ld (hl),a
;проверка на все засвеченные линии
; check 2
	ld h,$B8
ch_1:
	ld l,0
ch_2:
	ld a,(hl)
;	and 127
	cp 1:jp z,end_bp
	cp 2:jp z,end_bp
	cp 4:jp z,end_bp
	cp 8:jp z,end_bp
	inc l
	ld a,l
	cp 8
	jr nz,ch_2
	inc h
	ld a,h
	cp $BF
	jp nz,ch_1
	scf;CY=1 game done
	ret
m1
	ld a,r
	and 7
	out ($FE),a
	jr m1
dy:nop
;check for Y
	ld a,d
	cp $FF:jp z,end_bp
	cp 7:jp z,end_bp

; что в следующей клетке?
	ld a,d
	or $B8
	ld h,a
	ld l,e
;3,5,7 - лучи
;0,6-пусто
;1,2,4,8 - кристаллы
	ld a,(hl)
;	set 7,(hl)
	and 127
	cp 3:jp z,put_dir
	cp 5:jp z,put_dir
	cp 7:jp z,put_dir
	or a:jr nz,no_case_0
put_dir:
	ld a,iyh
	or (hl)
	or 128;подсветка
	ld (hl),a
	jp b_lp
no_case_0:
;столкновение 1 и 4, 2 и 8, or сработает
;1,2,4,8-продолжение пути
;  1
; 2 8
;  4
;
	ld a,(hl)
	and 127
	or iyl
;?подсветить встречный?
	cp 5:jp z,end_bp1
	cp 10:jp z,end_bp1
;поворот, если другой кристалл
	ld a,(hl)
;натолкнулись на тот же подсвеченный кристалл?
	bit 7,a:jp nz,end_bp
	and 127
;	ld iyl,a
	set 7,(hl)
	push hl,af;еще маркер
	ld a,d
	or $B8
	ld h,a
	ld l,e
	set 6,l

	ld a,iyl
	exx
	ld h,0,l,a
	ld de,huj
	add hl,de
	ld a,(hl)
	exx
;	ld a,(hl)
;	or iyl
	or (hl)
	ld (hl),a

	pop af,hl
	ld iyl,a
	jp btune
end_bp1:
	set 7,(hl)
;здесь же добавить
	set 6,l
	ld a,iyl

	exx
	ld h,0,l,a
	ld de,huj
	add hl,de
	ld a,(hl)
	exx

	or (hl)
	ld (hl),a

end_bp:
	xor a
	ret
huj:
;	db $00,$01,$02,$03,$04,$05,$06,$07,$08
	db $00,$04,$08,$03,$01,$05,$06,$07,$02

;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;table for build
;  1
; 2 8
;  4

direct:;тайлы для рисования, 3- 5|,7+
;	db $00,$01,$02,$03,$04,$05,$06,$07,$08
	db $00,$05,$03,$00,$05,$00,$00,$00,$03

dxy:
	nop;0
	nop;1
	dec e;2
	nop;3
	nop;4
	nop;5
	nop;6
	nop;7
	inc e;8
d_y:
	nop;0
	dec d;1
	nop;2
	nop;3
	inc d;4
	nop;5
	nop;6
	nop;7
	nop;8
crs:
	ld h,0
	ld l,d
	add hl,hl;*2
	add hl,hl;*4
	add hl,hl;*8
	add hl,hl;*16
	add hl,hl;*32
	add hl,hl;*64
	ld bc,$5800+4*32+8
	add hl,bc
	ld b,0
	ld c,e
	add hl,bc
	add hl,bc
crscol=8
	ld a,(hl):xor crscol:ld (hl),a
	inc hl
	ld a,(hl):xor crscol:ld (hl),a
	ld bc,31:add hl,bc
	ld a,(hl):xor crscol:ld (hl),a
	inc hl
	ld a,(hl):xor crscol:ld (hl),a
	ret
clearmem:
	ld hl,$b700; $b700-для уголка, $B800 - сама карта
cm:
	ld (hl),0
	inc l
	jr nz,cm
	inc h
	ld a,h
	cp $C0
	jr nz,cm

	ld ix,$B800
	ld b,7
cm2:
	ld (ix+0),9
	ld (ix+8),9
	inc ixh
	djnz cm2
	ret

coplvl:
	ld a,(hl):inc hl
	ld (crxy+2),a
	ld (bxy+2),a
	or $B8
	ld d,a
	ld e,0
	ld a,$88
	ld (de),a

	ld a,(hl):inc hl
	or $B8
	ld d,a
	ld e,8
	ld a,2
	ld (de),a

	ld de,$B801
	ld a,8-1
cp:
	push de
	ld bc,7
	ldir
	pop de
	inc d
	dec a
	jr nz,cp
	ret

nbde    INC D:LD A,D:AND 7:RET NZ
        LD A,E:ADD A,#20:LD E,A:RET C
        LD A,D:SUB 8:LD D,A:RET
nbhl    INC h:LD A,h:AND 7:RET NZ
        LD A,L:ADD A,#20:LD L,A:RET C
        LD A,H:SUB 8:LD H,A:RET

sc=4;path
lc=$46;lightning
dcols:
;	db $00,$01,$02,$03,$04,$05,$06,$07,$08,$09
	db $00, sc, sc, lc, sc, lc, lc,00, sc, 05
drawmap:
	push de
	ld ixh,$B8
	ld de,$4008+4*32
	ld iy,$5808+4*32
dm1:
	push iy
	push de
	ld ixl,0
dm2:
	ld l,(ix+0);:inc ix
	
	ld a,l
	and 127;reset light bit
	ld l,a
	exx
	ld l,a,h,0
	ld bc,dcols
	add hl,bc
	ld a,(hl)
;учесть, что подсветка
	bit 7,(ix+0)
	jr z,nolight
	ld a,lc
nolight:
	ld (iy+0),a
	ld (iy+1),a
	ld (iy+32),a
	ld (iy+33),a
	inc iy,iy
	exx
	
	ld h,0
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	ld bc,tiles
	add hl,bc
	push de
	
	ld a,16
dm3:
	exa
	ldi:ldi
	dec e,e
	call nbde
	exa
	dec a
	jr nz,dm3
;;;;;;;; уголок;;;;;;;;;;;;;;;
	pop de
	push de
	push ix
	ld a,ixl
	or 64
	ld ixl,a
	ld c,(ix+0)
	pop ix
	
	ld a,c:or a:jp z,skp
	
	ld l,c
	ld h,$C0/2
	add hl,hl
	ld a,16
cornl:
	exa
	ld a,(de):or (hl):ld (de),a
	inc e,l
	ld a,(de):or (hl):ld (de),a
	dec e,l
	inc h
	call nbde
	exa
	dec a
	jp nz,cornl
skp:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	pop de
	inc e,e
	inc ixl
	ld a,ixl
	cp 9
	jp nz,dm2
	pop de

	pop iy
	ld bc,64
	add iy,bc

	inc ixh
	ld a,e:add a,64:ld e,a
	jr nc,nod8
	ld a,d:add a,8:ld d,a
nod8:
	ld a,ixh
	cp $BF:jp nz,dm1
	pop de
	ret

level:db 1

level_t_b:
	dw 0
	dw ch1,ch2,ch3,ch4,ch5,ch6,ch7,ch8,ch9,ch10
	dw ch11,ch12,ch13,ch14,ch15,ch16,ch17,ch18,ch19,ch20
	dw ch21,ch22,ch23,ch24,ch25,ch26,ch27,ch28,ch29,ch30
	dw ch31,ch32,ch33,ch34,ch35,ch36,ch37,ch38,ch39,ch40
	dw ch41,ch42,ch43,ch44,ch45,ch46,ch47,ch48,ch49;,ch50
level_t_e:

ch1: ;7x7
;	db 2;start
;	db 3;end
;  db 0,4,0,2,8,4,0
;  db 0,8,4,0,0,0,0
;  db 0,4,8,0,1,0,0
;  db 0,8,0,1,4,0,0
;  db 0,4,0,0,1,0,0
;  db 0,0,8,2,0,0,0
;  db 0,1,2,1,0,2,0

	db 2;start
	db 3;end
  db 0,1,0,1,1,1,0
  db 0,1,1,0,0,0,0
  db 0,1,1,0,1,0,0
  db 0,1,0,1,1,0,0
  db 0,1,0,0,1,0,0
  db 0,0,1,1,0,0,0
  db 0,1,1,1,0,1,0

ch2:
	db 2;start
	db 4;end
  db 0,0,1,1,1,1,0
  db 0,1,0,1,0,0,0
  db 0,0,1,0,1,0,1
  db 0,1,0,1,0,0,0
  db 1,0,1,0,1,0,0
  db 0,0,1,1,1,0,1
  db 1,0,0,0,0,1,0

ch3:
	db 4;start left
	db 2;end right
  db 1,0,1,1,1,1,1
  db 1,0,0,1,1,0,1
  db 0,1,1,1,1,0,1
  db 0,0,1,1,0,0,0
  db 0,0,1,1,1,0,0
  db 0,0,1,1,1,1,0
  db 0,1,1,0,1,0,1

ch4:
	db 5;start left
	db 0;end right
  db 0,0,0,1,0,1,1
  db 1,0,1,0,1,1,0
  db 1,1,1,1,1,0,1
  db 1,1,1,0,0,0,1
  db 1,1,0,1,0,0,1
  db 0,0,0,0,0,1,0
  db 0,1,1,1,0,1,0

ch5:
	db 2;start left
	db 4;end right
  db 0,1,0,1,1,0,1
  db 1,0,1,0,0,0,0
  db 0,1,0,1,0,1,0
  db 0,1,0,0,0,1,0
  db 1,1,1,0,1,1,0
  db 0,1,0,0,1,1,0
  db 0,1,0,0,1,1,1

ch6:
	db 5;start left
	db 0;end right
  db 1,1,1,0,0,0,0
  db 0,1,1,1,0,0,1
  db 0,1,0,1,1,1,0
  db 1,1,1,1,0,0,0
  db 1,1,0,0,0,0,0
  db 1,0,0,0,0,0,0
  db 0,1,1,1,1,1,1

ch7:
	db 1;start left
	db 0;end right
  db 0,0,0,0,1,0,0
  db 0,1,1,1,1,0,1
  db 0,1,1,0,0,1,1
  db 1,0,0,1,1,1,0
  db 1,1,1,1,1,0,1
  db 1,1,0,0,1,1,0
  db 1,0,1,1,1,1,1

ch8:
	db 3;start left
	db 3;end right
  db 0,1,0,0,0,1,0
  db 0,1,0,0,1,0,0
  db 1,0,1,1,1,0,0
  db 0,1,0,1,1,1,0
  db 1,0,0,1,0,0,0
  db 0,1,0,1,0,0,0
  db 0,0,1,0,1,0,0

ch9:
	db 5;start left
	db 5;end right
  db 1,0,0,1,1,1,0
  db 0,1,1,0,0,1,1
  db 1,1,1,0,0,0,1
  db 0,1,1,0,0,1,1
  db 1,0,0,1,0,0,0
  db 0,1,0,1,1,1,0
  db 1,0,1,1,0,0,1

ch10:
	db 0;start left
	db 5;end right
  db 0,1,0,0,1,0,1
  db 1,0,0,1,0,0,0
  db 1,1,1,0,0,0,0
  db 0,1,0,0,1,0,0
  db 1,0,1,0,1,0,1
  db 0,1,0,1,1,1,1
  db 0,0,0,0,0,1,1

ch11:
	db 0;start left
	db 6;end right
  db 1,1,1,1,1,0,0
  db 1,0,1,0,0,0,0
  db 1,1,1,1,0,1,1
  db 1,0,1,1,1,0,0
  db 0,1,0,1,1,1,0
  db 1,1,1,1,0,1,1
  db 1,0,1,1,1,1,0

ch12:
	db 2;start left
	db 4;end right
  db 0,1,0,1,1,0,1
  db 1,0,1,0,0,0,0
  db 0,1,0,1,0,1,0
  db 0,1,0,0,0,1,0
  db 1,1,1,0,1,1,0
  db 0,1,0,0,1,1,0
  db 0,1,0,0,1,1,1

ch13:
	db 1;start left
	db 4;end right
  db 1,0,0,0,1,0,0
  db 0,0,1,0,1,0,1
  db 1,1,1,1,0,1,1
  db 1,1,1,1,0,1,1
  db 0,1,0,0,0,0,0
  db 0,0,1,0,1,1,1
  db 1,1,0,0,1,1,0

ch14:
	db 1;start left
	db 1;end right
  db 0,0,1,0,0,0,1
  db 0,1,1,1,1,0,0
  db 1,0,0,0,0,1,0
  db 0,0,1,0,1,0,0
  db 0,0,1,0,0,1,0
  db 1,0,0,0,0,0,1
  db 0,1,0,1,0,0,0

ch15:
	db 0;start left
	db 3;end right
  db 0,0,0,1,1,0,1
  db 1,0,1,1,0,1,0
  db 0,0,1,0,1,0,0
  db 0,1,1,1,0,0,0
  db 0,0,1,1,1,0,1
  db 0,1,0,0,1,1,1
  db 1,0,0,0,0,0,1

ch16:
	db 5;start left
	db 4;end right
  db 0,1,1,0,1,0,1
  db 0,1,0,1,0,1,1
  db 1,1,0,1,1,1,1
  db 0,1,1,1,0,0,1
  db 0,1,1,1,0,1,1
  db 0,1,0,1,0,1,0
  db 1,0,1,1,0,0,1

ch17:
	db 0;start left
	db 4;end right
  db 0,0,0,1,0,1,1
  db 0,1,0,0,1,0,0
  db 0,1,0,1,0,0,0
  db 1,1,1,1,1,0,1
  db 0,0,0,1,0,0,0
  db 0,1,1,0,1,0,1
  db 1,0,0,0,1,1,1

ch18:
	db 3;start left
	db 5;end right
  db 1,0,0,0,1,0,0
  db 0,1,1,0,0,0,0
  db 0,1,0,1,0,0,0
  db 0,1,0,1,1,1,0
  db 1,0,0,0,1,1,1
  db 0,1,0,1,0,0,1
  db 0,0,1,0,1,0,0

ch19:
	db 1;start left
	db 0;end right
  db 0,0,0,0,1,1,1
  db 0,1,1,1,0,0,0
  db 0,1,1,0,1,0,1
  db 1,0,0,1,1,1,0
  db 0,1,0,0,0,0,1
  db 0,0,0,0,1,0,1
  db 1,1,0,0,0,0,0

ch20:
	db 5;start left
	db 0;end right
  db 1,0,1,1,1,0,1
  db 1,1,1,1,0,1,1
  db 1,0,1,1,1,1,1
  db 0,1,1,1,1,1,1
  db 1,1,0,0,0,1,1
  db 0,0,1,1,0,1,0
  db 0,1,1,1,1,1,1

ch21:
	db 2;start left
	db 5;end right
  db 0,1,1,0,1,1,0
  db 1,0,0,0,0,0,1
  db 0,1,1,0,1,1,1
  db 1,0,1,1,1,1,1
  db 1,1,0,1,0,0,1
  db 0,1,0,1,0,0,1
  db 1,0,1,1,1,1,1

ch22:
	db 4;start left
	db 5;end right
  db 1,1,0,1,1,1,1
  db 0,1,1,0,1,1,0
  db 0,1,1,1,1,1,1
  db 0,0,1,1,0,1,1
  db 0,0,1,0,0,1,1
  db 0,1,0,1,0,0,1
  db 1,0,0,0,1,1,1

ch23:
	db 2;start left
	db 4;end right
  db 1,1,0,1,0,0,1
  db 0,1,1,0,1,1,0
  db 0,0,0,0,0,1,0
  db 0,0,1,1,1,0,1
  db 1,1,1,1,0,0,1
  db 1,1,0,0,1,1,0
  db 1,0,1,1,1,1,1

ch24:
	db 3;start left
	db 4;end right
  db 0,1,1,1,0,0,1
  db 1,1,0,0,0,1,1
  db 0,1,0,0,1,1,1
  db 1,0,0,1,1,1,1
  db 1,1,0,1,0,1,1
  db 1,1,0,0,1,1,0
  db 0,1,1,1,1,1,1

ch25:
	db 5;start left
	db 6;end right
  db 0,0,0,0,1,0,1
  db 0,0,0,1,0,1,0
  db 0,0,0,0,0,1,1
  db 0,1,0,1,1,0,1
  db 0,0,1,0,0,0,1
  db 0,1,1,0,0,1,0
  db 0,0,0,0,0,1,0

ch26:
	db 3;start left
	db 2;end right
  db 0,0,1,1,1,0,1
  db 1,0,1,1,0,0,1
  db 1,0,0,0,0,0,0
  db 0,0,0,0,0,0,1
  db 0,1,0,0,0,1,0
  db 0,0,0,0,1,1,0
  db 0,1,0,0,0,0,1

ch27:
	db 2;start left
	db 1;end right
  db 1,1,1,1,0,1,1
  db 0,0,1,0,1,1,0
  db 0,0,0,0,0,1,0
  db 0,1,0,1,1,1,0
  db 1,1,1,1,0,1,1
  db 1,1,0,1,1,0,0
  db 1,0,1,0,1,1,0

ch28:
	db 3;start left
	db 0;end right
  db 0,0,1,0,1,1,0
  db 0,1,0,1,0,1,1
  db 1,0,0,0,0,1,0
  db 0,1,1,0,0,1,0
  db 1,1,1,0,1,0,1
  db 1,0,1,0,0,0,0
  db 0,1,0,1,0,0,0

ch29:
	db 3;start left
	db 5;end right
  db 0,0,0,0,0,1,1
  db 1,1,0,1,1,0,0
  db 0,1,1,0,1,1,0
  db 0,0,0,1,1,1,0
  db 0,0,1,0,0,0,1
  db 1,1,0,1,1,0,1
  db 0,1,0,1,0,1,1

ch30:
	db 6;start left
	db 3;end right
  db 1,1,0,1,0,0,1
  db 0,0,1,0,0,1,0
  db 1,0,0,0,0,1,0
  db 1,0,0,0,0,0,0
  db 0,0,0,1,0,1,0
  db 1,1,1,0,0,1,0
  db 0,0,0,0,0,0,1

ch31:
	db 1;start left
	db 3;end right
  db 0,1,0,1,0,0,0
  db 0,1,1,0,0,1,0
  db 1,0,0,0,0,0,1
  db 0,1,0,0,0,0,0
  db 0,1,0,0,0,0,1
  db 0,1,1,0,0,0,0
  db 1,1,0,1,0,1,0

ch32:
	db 0;start left
	db 0;end right
  db 0,0,0,0,0,1,1
  db 1,0,1,0,0,0,0
  db 0,0,0,1,0,1,0
  db 0,1,0,0,0,0,1
  db 0,1,1,1,0,0,1
  db 1,0,0,0,1,1,1
  db 0,0,0,0,1,1,0

ch33:
	db 1;start left
	db 4;end right
  db 1,0,0,0,1,1,1
  db 0,0,0,1,0,0,0
  db 1,0,0,0,0,0,1
  db 0,0,1,0,0,1,0
  db 0,1,0,0,1,1,0
  db 0,0,1,0,0,1,0
  db 0,1,0,1,0,0,0

ch34:
	db 5;start left
	db 3;end right
  db 0,0,0,0,0,1,1
  db 1,1,0,0,1,1,0
  db 0,1,1,0,1,1,0
  db 0,0,1,1,0,1,0
  db 0,1,1,1,1,0,0
  db 0,1,1,1,1,1,0
  db 1,0,0,1,0,1,1

ch35:
	db 0;start left
	db 5;end right
  db 1,1,0,1,1,1,0
  db 0,0,1,0,0,0,1
  db 0,1,1,0,1,0,1
  db 1,0,0,1,0,1,1
  db 1,0,0,0,0,1,0
  db 0,1,0,1,1,0,0
  db 1,1,0,1,1,1,1

ch36:
	db 5;start left
	db 3;end right
  db 0,0,0,0,0,1,1
  db 1,1,0,0,1,1,0
  db 0,1,1,0,1,1,0
  db 0,0,1,1,0,1,0
  db 0,1,1,1,1,0,0
  db 0,1,1,1,1,1,0
  db 1,0,0,1,0,1,1

ch37:
	db 1;start left
	db 4;end right
  db 1,1,1,0,0,0,1
  db 1,0,0,1,0,1,0
  db 1,1,0,0,1,1,0
  db 1,0,1,1,0,0,1
  db 1,1,1,0,0,0,0
  db 0,1,1,1,0,0,1
  db 1,0,0,1,1,0,1

ch38:
	db 5;start left
	db 5;end right
  db 1,1,0,0,1,1,1
  db 0,1,0,1,0,0,0
  db 1,1,1,0,0,1,0
  db 0,0,1,1,0,1,1
  db 0,0,1,1,1,0,0
  db 0,1,1,0,1,1,1
  db 0,0,1,1,1,1,1

ch39:
	db 2;start left
	db 3;end right
  db 0,1,0,1,0,1,1
  db 0,0,1,0,1,0,0
  db 1,1,0,0,1,0,0
  db 1,0,1,0,1,1,1
  db 0,1,0,1,0,0,0
  db 1,0,1,0,1,1,0
  db 1,1,1,0,0,1,0

ch40:
	db 2;start left
	db 1;end right
  db 1,1,1,1,0,1,1
  db 0,0,1,0,1,1,0
  db 0,0,0,0,0,1,0
  db 0,1,0,1,1,1,0
  db 1,1,1,1,0,1,1
  db 1,1,0,1,1,0,0
  db 1,0,1,0,1,1,0

ch41:
	db 3;start left
	db 5;end right
  db 1,0,0,0,1,1,1
  db 0,1,0,1,1,1,0
  db 0,0,1,0,1,0,0
  db 0,0,0,0,1,1,1
  db 0,1,1,0,1,1,0
  db 1,0,0,1,1,1,1
  db 0,0,0,0,0,1,1

ch42:
	db 5;start left
	db 5;end right
  db 1,1,0,0,1,1,1
  db 0,1,0,1,0,0,0
  db 1,1,1,0,0,1,0
  db 0,0,1,1,0,1,1
  db 0,0,1,1,1,0,0
  db 0,1,1,0,1,1,1
  db 0,0,1,1,1,1,1

ch43:
	db 5;start left
	db 1;end right
  db 1,0,1,0,1,0,1
  db 1,1,1,1,1,0,0
  db 0,1,1,1,0,0,1
  db 1,0,1,1,0,0,1
  db 0,1,1,0,1,1,0
  db 0,0,0,0,0,1,0
  db 1,1,1,1,1,0,1

ch44:
	db 3;start left
	db 6;end right
  db 1,0,0,0,0,1,0
  db 0,1,0,1,0,0,0
  db 1,1,0,1,1,0,0
  db 0,0,0,1,1,0,1
  db 1,1,1,0,1,0,0
  db 0,1,1,0,0,0,0
  db 1,0,0,1,1,1,1

ch45:
	db 4;start left
	db 1;end right
  db 0,1,0,0,0,1,0
  db 1,0,0,1,1,0,0
  db 0,0,1,0,1,1,0
  db 1,0,0,0,0,0,1
  db 0,0,0,0,0,1,0
  db 0,0,1,0,1,1,1
  db 0,1,0,1,1,1,0

ch46:
	db 3;start left
	db 4;end right
  db 0,1,1,1,0,0,1
  db 1,1,0,0,0,1,1
  db 0,1,0,0,1,1,1
  db 1,0,0,1,1,1,1
  db 1,1,0,1,0,1,1
  db 1,1,0,0,1,1,0
  db 0,1,1,1,1,1,1

ch47:
	db 3;start left
	db 4;end right
  db 0,0,1,1,1,1,0
  db 1,1,0,1,0,1,0
  db 0,1,0,1,0,0,0
  db 0,1,1,1,0,1,1
  db 0,0,0,0,0,0,1
  db 0,0,1,0,1,1,1
  db 1,1,1,0,0,1,1

ch48:
	db 5;start left
	db 0;end right
  db 1,0,1,1,1,0,1
  db 1,1,1,1,0,1,1
  db 1,0,1,1,1,1,1
  db 0,1,1,1,1,1,1
  db 1,1,0,0,0,1,1
  db 0,0,1,1,0,1,0
  db 0,1,1,1,1,1,1

ch49:
	db 5;start left
	db 6;end right
  db 1,1,1,1,1,0,1
  db 1,0,1,0,0,1,1
  db 0,0,0,1,1,1,1
  db 1,1,0,1,1,1,1
  db 1,1,1,0,1,1,1
  db 0,0,0,1,1,0,1
  db 0,1,1,0,1,0,0

;ch47:
;	db 0;start left
;	db 0;end right
;  db 0,0,0,0,0,0,0
;  db 0,0,0,0,0,0,0
;  db 0,0,0,0,0,0,0
;  db 0,0,0,0,0,0,0
;  db 0,0,0,0,0,0,0
;  db 0,0,0,0,0,0,0
;  db 0,0,0,0,0,0,0

tiles:
 incbin "00empty.bin"
 incbin "01up.bin"
 incbin "02left.bin"
 incbin "03horiz.bin"
 incbin "04down.bin"
 incbin "05vert.bin"
 incbin "06empty.bin"
 incbin "07cross.bin"
 incbin "08right.bin"
 incbin "09bord2.bin"
;***************************************************************
intvec:
	push iy,ix
	push hl,de,bc,af
	exx:exa
	push hl,de,bc,af
	call PLAY
	pop af,bc,de,hl
	exx:exa
	pop af,bc,de,hl
	pop ix,iy
	ei:ret

plyr:include "PTxPlay.a80"
song:incbin "arf_tune.pt3";;"bb_tune.pt3"
;ven:incbin "ve1.bin"
;***************************************************************
;для уголков:
;  1 
; 2 8
;  4
;
;0123
;1248
generate_corners:
	ld ixl,0 ; счетчик
gc_lp:
	ld a,ixl
	ld l,a
	ld h,$C0/2
	add hl,hl;*2
	ld de,0
	bit 0,a:jr z,no_b0
	ld de,%0000000110000000
no_b0:
;верхняя часть
	ld b,8
gc2:
	ld (hl),d:inc l
	ld (hl),e:dec l
	inc h
	djnz gc2
;нижняя
	push hl
	ld a,ixl
	ld de,0
	bit 2,a:jr z,no_b2
	ld de,%0000000110000000
no_b2:
	ld b,8
gc3:
	ld (hl),d:inc l
	ld (hl),e:dec l
	inc h
	djnz gc3
	pop hl
	dec h;$C700

;лево
	ld a,ixl
	bit 1,a:jr z,no_b1
	ld a,(hl):or $FF:ld (hl),a
	inc h
	ld a,(hl):or $FF:ld (hl),a
	dec h
no_b1:
	inc l
;право
	ld a,ixl
	bit 3,a:jr z,no_b3
	ld a,(hl):or $FF:ld (hl),a
	inc h
	ld a,(hl):or $FF:ld (hl),a
	dec h
no_b3:
	inc ixl
	ld a,ixl
	cp 16
	jp nz,gc_lp
	ret
;=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
print: ;ix=adr
	ld e,(ix+0):inc ix
	ld d,(ix+0):inc ix
pr1
	ld a,(ix+0):inc ix
	or a:ret z
	
	sub 32
	ld l,a,h,0

	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	add hl,hl
	ld bc,bfnt
	add hl,bc
	push de
	ld a,16
pr2
	exa
	ldi:ldi
	dec de,de
	call nbde
	exa
	dec a
	jp nz,pr2
	pop de
	inc e,e
	jp pr1

;;;dec2ascii
printnum:
DispA:
;	ld hl,txtbuf
;	ld	c,-100
;	call	Na1
	ld	c,-10
	call	Na1
	ld	c,-1
Na1:	ld	b,'0'-1
Na2:	inc	b
	add	a,c
	jr	c,Na2
	sub	c		;works as add 100/10/1
	push af		;safer than ld c,a
	ld	a,b		;char is in b
	ld (hl),a:inc hl;CALL	PUTCHAR	;plot a char. Replace with bcall(_PutC) or similar.
	pop af		;safer than ld a,c
	ret
;txtbuf:
; db 0,0,0,0

wk:
	ei
	halt
	xor a
	in a,($FE)
	cpl
	and 31
	jr z,wk
	ret

selcrs=2*8+1;+$40
crs2:
	ld l,d,h,0,b,h
	add hl,hl;*2
	add hl,hl;*4
	add hl,hl;*8
	add hl,hl;*16
	add hl,hl;*32
	add hl,hl;*64
	ld c,e
	add hl,bc
	add hl,bc
	add hl,bc
	add hl,bc

	ld bc,$5800
	add hl,bc
	push hl
	pop ix
	ld c,selcrs
	ld a,(ix+0):xor c:ld (ix+0),a
	ld a,(ix+1):xor c:ld (ix+1),a
	ld a,(ix+2):xor c:ld (ix+2),a
	ld a,(ix+3):xor c:ld (ix+3),a
	
	ld a,(ix+32):xor c:ld (ix+32),a
	ld a,(ix+33):xor c:ld (ix+33),a
	ld a,(ix+34):xor c:ld (ix+34),a
	ld a,(ix+35):xor c:ld (ix+35),a
	ret
checkm:
	ld a,d
	add a,a
	add a,a
	add a,a
	add a,e
	add a,9
	cp 49+1
	ret
;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
drawstripe:
	push de
;attributes
	push hl
	pop ix
	ld b,18;32
ds3:
	ld (ix+0),5
	ld (ix+32),5
	inc ix:djnz ds3

	ld hl, hor

	ld ixl,16
ds1:
	push de
	ld bc,4:ldir
	pop de
	call nbde
	dec ixl:jp nz,ds1
	
	pop de
	ld ixl,16
ds2:
	push de
	ld hl,4
	add hl,de
	ex de,hl
	ld bc,15-1
	ldir
	pop de
	call nbde
	dec ixl
	jp nz,ds2
	ret
hor:incbin "sq.bin"

blackscr:
	ld hl,$5aff,de,$5afe,bc,6911,(hl),0:lddr
	ld hl,$5800,de,$5801,bc,767,(hl),$47:ldir
	ret

;&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
fade:
fadt=$BF00
fade_r=$C000
	ld hl,fadt
bu1:
	ld a,l
	and $C0
	ld c,a
	ld a,l
	and 7
	jr z,noink
	dec a
noink
	or c
	ld c,a
	ld a,l
	and %00111000
	jr z,nopap
	sub 8
nopap:
;	or c
	ld c,a
	ld (hl),a
	inc l
	jp nz,bu1
;nxt
	ld hl,fade_r
	ld c,3
bu2:
	ld b,0
bu3:
	ld (hl),$4e:inc hl ;ld c,(hl)
	ld (hl),$0A:inc hl ;ld a,(bc)
	ld (hl),$77:inc hl ;ld (hl),a
	ld (hl),$2C:inc hl ;inc l
	dec b
	jp nz,bu3
	dec hl
	ld (hl),$23:inc hl ;inc hl
	dec c
	jp nz,bu2
	ld (hl),$C9:inc hl ;ret
	ret

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mes2:
	dw $4008;+32
 db "C#NGRATS!",0

mes1:
	dw $4008;+32
 db " LEVEL:"
l_n:
 db "00 ",0

bfnt:incbin "binfont.bin"
levelsn:
	dw $4000
	db "0102030405060708",0

	dw $4040
	db "0910111213141516",0

	dw $4080
	db "1718192021222324",0

	dw $40C0
	db "2526272829303132",0

	dw $4800
	db "3334353637383940",0

	dw $4840
	db "4142434445464748",0

	dw $4880
	db "49",0

	dw $5000+4
	db "CHOOSE LEVEL",0

end
	display end
	display /d,end-begin
	savesna "!beams.sna",begin
	savebin "beams.code",begin,end-begin

