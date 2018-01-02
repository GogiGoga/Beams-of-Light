	device zxspectrum128
        ORG #8000
begin
	ld hl,picp,de,$4000
zx7:include "zx7.a80"
picp:incbin "title_00.scr.zx7"

end
	display /d,end-begin
	;savesna "!void.sna",begin
	savebin "title.code",begin,end-begin


