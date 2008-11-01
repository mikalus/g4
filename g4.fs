0 [if]                                                              mk 10/2008 
***************************  Macro assembler g4 *******************************
             Translating amforth source code into assembler (AVRA) 

Todo: 
-- state recogniton: 
   allot ok 
   cells ok 
   ?
-- test control structures 
   do loop ... --> ok
   labels must use names und numers within definition only. ok 
-- test if simple words should be immediate or not. 

Code like this: 
    variable foo   10 allot 
    create foo  10 cells allot 
will work now.

Use >>> to force string into assembler file. 
Example:
>>> .set pc = pc + $10


Help: 

Invoce g4 like this:
    ../michael$ gforth amforth.macros 
    include <file>      \ <file> is your amforth sourcce code. 

Or include amforth.macros in sourcefile, then run: 
    gforth <file>.frt  >  <file>.asm 




Work arround: 
Use _set-immediate-on in source to force the next : definition to be immediate.
We do not handel the forthstyle immediate placed after those definitions jet.  
*******************************************************************************
[then] 


only forth also definitions
: g4off     only forth definitions cr order ; 

  vocabulary g4voc
: g4on      only forth also g4voc definitions cr order ; 

g4voc definitions  cr .( ; g4 macro definitions: ) .s 


\ Interpret forth source code, expand into assembler macros. 
\ A polyForth-like :-compiler in ANS-Forth will do it.

   variable g4mode 
:  g4mode-on    true   g4mode ! ;   g4mode-on 
:  g4mode-off   false  g4mode ! ; 

defer _comma

: _]    ( -- )
   ] g4mode @ 0= IF exit THEN 
   BEGIN 
     BEGIN 
       bl word         \ cr ." word: " .s  ( testing) 
       dup c@
     WHILE ( c-addr ) 
     find ?dup 
        IF  -1 = IF 
                   execute 
                ELSE 
                   execute  
                     state @ 0= IF [compile] [ exit THEN
                THEN 
        ELSE 
          count snumber?  ( -- n ) 
            IF  _comma ELSE type true abort" ?" THEN 
        THEN 
     REPEAT 
     drop 
     source-id 0= IF cr ." _] " THEN 
     refill 0= 
   UNTIL ; 

: ]]          g4mode-on            _]   ; 
: [[          g4mode-off  [compile] [   ; 

cr .( ; some alias ) .s 
\ Now we need some alias to build g4 in g4voc 
\ using the forth funktion instead of the g4 funktion. 
: _immediate    [compile] immediate ; 
: _zero         0           ; 
: _cr           cr          ; 
: _words        words       ; 
\ : _parse-word   parse-word  ; 
: _.s           .s          ; 
: _hex          hex         ; 
: _@            @           ; 
: _.            .           ; 
: _dup          dup         ; 
: _swap         swap        ; 
: _type         type        ; 

: _."  ( -- ) \ ccc"
  state @ if 
  [char] " parse  POSTPONE SLiteral postpone type 
  else 
  [char] " parse type 
  then ; immediate 



: _;         [compile] ;    ; immediate 
: _:         [compile] :    ; immediate 
: _(         [compile] (    ; immediate 
: _\         [compile] \    ; immediate 
: _state-off    0 state !   ; 

cr .( ; label ; headers ) .s 
\ We will need a counter for labels in control structures. 
    Variable label#      0 label# ! 
: _nextlabel    ( -- x )  label# @  dup 1+ label# ! ; 

\ Exit a g4 macro definition. 
: ;             _cr _."     .dw XT_EXIT " 
                0 label# !
                _cr _cr [compile] [   _; immediate 



\ This I found usefull while in debugging g4.
: .. bye  _; \ I use this *very* often to leave gforth and recompile my source.
: \ok     _; \ just for a human to see that this word was tested ok. 
: \???    _; \ offene Frage(n). 
: \oki?   _; \ Geht, aber ggf.immediate setzen? 


\ Often uses phrases:
: "emit     [char] " emit   _; 
: :emit     [char] : emit   _; 
: ,emit     [char] , emit   _; 
: ;emit     [char] ; emit   _; 
: (emit     [char] ( emit   _; 
: )emit     [char] ) emit   _; 
: $emit     [char] $ emit   _; 

: _type-name    ( adr n -- )    
                0 do dup i + c@  toupper emit loop drop      _; \ok 
: _type-label   ( adr n -- )    _type-name :emit             _; 
: add0                          dup 1+ 1 and if _." ,0" then _; 


\ Handle immediate words. 
   variable g4immediate
: g4immediate-on    true  g4immediate !          _; 
: g4immediate-off   false g4immediate !          _; 
  g4immediate-off 

: .$$       ( n -- ) base @ >r hex $emit . r> base ! _; 
: .header-count    ( n -- ) 
            g4immediate @ if 
                $80 or .$$ 
            else 
                .$$ 
            then g4immediate-off _; 

\ Build header like this one: 
\ VE_XXX: 
\     .db $03, "xxx" 
\     .dw VE_HEAD 
\     .set VE_HEAD = VE_XXX 
\ XT_XXX: 
\     .dw DO_COLON 
\ PFA_XXX: 

\ Some often used phrases for those macros to build. 
: _type-head    ( adr n -- ) 
                _cr _."     .db " dup .header-count 
                ,emit "emit 2dup type "emit add0 
                _cr _."     .dw VE_HEAD" 
                _cr _."     .set VE_HEAD = VE_" _type-name           _; \ok 
: _type$    ( n -- )    _cr _."     .dw " .$$                        _; 
: _docomma  ( n -- )    _cr _."     .dw XT_DOLITERAL" _type$         _; 
 ' _docomma is _comma 

\ Different headers. 
: _colon:       ( adr n -- ) 
            _cr _." XT_" 2dup _type-label 
            _cr _."     .dw DO_COLON "
            _cr _." PFA_"     _type-label                  _; \ok 
: _variable:    ( adr n -- ) 
            _cr _." XT_" 2dup _type-label 
            _cr _."     .dw DO_VARIABLE " 
            _cr _." PFA_"     _type-label                  _; \ok 
: _constant:    ( adr n -- ) 
            _cr _." XT_" 2dup _type-label 
            _cr _."     .dw DO_CONSTANT " 
            _cr _." PFA_"     _type-label                  _; \ok 
: _user:        ( adr n -- ) 
            _cr _." XT_" 2dup _type-label 
            _cr _."     .dw DO_USER "
            _cr _." PFA_"     _type-label                  _; \ok 
: _header       ( adr n -- ) 
            _cr _." VE_" 2dup _type-label 
                              _type-head                   _; \ok 



_\ Make all following words immediate, so they will execute while compiling. 
_\ This makes them akt as an assembler macro.

_cr .( ; compiling words ) _.s  
_\ That is: Create a g4 definitions and type its macro when interpreted. 
_\ On runtime type created name. 

_: :noname      create latest 
                dup , ( save name-token) 
                name>string  _colon:
                [compile] immediate ]] 
                does> ( -- adr )
                _cr _."     .dw XT_" @  name>string _type-name  _; \oki?

_: :            create latest 
                dup , ( save name-token) 
                name>string 2dup _header _colon:
                [compile] immediate ]] 
                does> ( -- adr )
                _cr _."     .dw XT_" @  name>string _type-name  _; \oki?

_: constant     ( n -- )
                create latest 
                dup , ( save name-token) 
                _cr _." ; defining a constant: " 
                name>string 2dup _header _variable:  ( n -- ) _type$ _cr 
                [compile] immediate 
                does> ( -- adr )
                _cr _."     .dw XT_" @  name>string _type-name  _; \ok 

_: variable     create latest 
                dup , ( save name-token) 
                _cr _." ; defining a variable: " 
                name>string 2dup _header _user: 
                _cr _."     .dw heap "
                _cr _."     .set heap = heap + CELLSIZE " _cr 
                [compile] immediate 
                does> ( -- adr )
                _cr _."     .dw XT_" @  name>string _type-name  _; \ok 

_: [char]       parse-word drop c@
                _cr _."     .dw XT_DOLITERAL " 
                _cr _."     .dw " .$$                _; immediate  \ok 

_: [']          parse-word 
                _cr _."     .dw XT_DOLITERAL " 
                _cr _."     .dw XT_" _type-name      _; immediate  \ok 

_: does>        _cr _."     .dw XT_DODOES " 
                _cr _."     .dw $940e      ; code for call" 
                _cr _."     .dw DO_DODOES "          _; \ok 

_: user       ( ccc" n --- )
        state @ if
                _cr _."     .dw XT_USER "
        else
                create latest 
                _cr _." ; user variable: " 
                name>string  2dup _header 
                _cr _." XT_" 2dup _type-label 
                _cr _."     .dw PFA_DOUSER " 
                _cr _." PFA_"     _type-label 
                _cr _."     .dw " .$$ _cr 
        then                                        _; \ok 
_\ creates a RAM based defer vector

_: Rdefer        ( n <name> -- ) 
    state @ if 
        _cr _."     .dw XT_RDEFER " 
    else        _cr _." ; create a RAM based defer vector: " 
                create latest  
                name>string 2dup _header 
                _cr _." XT_" 2dup _type-label
                _cr _."     .dw PFA_DODEFER " 
                _cr _." PFA_"     _type-label
                _cr _."     .dw heap "
                _cr _."     .set heap = heap + CELLSIZE  ; cell allot "
                _cr _."     .dw XT_RDEFERFETCH " 
                _cr _."     .dw XT_RDEFERSTORE " _cr 
    then                                            _; \??? 


_: Edefer       _cr _."     .dw XT_EDEFEER "         _; \???



_: create     ( ccc"   -- ) 
        state @ if
                _cr _."     .dw XT_CREATE "
        else
                create latest 
                dup , ( save name-token) 
                _cr _." ; create: " 
                name>string 2dup _header _constant:  _cr 
        then                                         _; \ok 

_cr .( ; some state smart words )
_: s"   ( ccc"   -- adr n ) 
        state @ if
          _cr _."     .dw XT_SLITERAL " 
    [[   $22 parse   
          _cr _."     .db " _dup .$$ ,emit "emit type "emit ]] 
        else 
          _cr _."     .dw DOTQUOTE " 
        then                                         _; _immediate \ok

_: ."   ( ccc"   -- ) 
        state @ if
          _cr _."     .dw XT_SLITERAL " 
    [[   $22 parse   
          _cr _."     .db " _dup .$$ ,emit "emit type "emit  
          _cr _."     .dw XT_ITYPE " ]]
        else 
          _cr _."     .dw DOTQUOTE " 
        then                                         _; _immediate \ok

_: allot        ( n -- ) 
    state @ if 
        _cr _."     .dw XT_ALLOT " 
    else 
        _cr _." ; allot ram "
        _cr _." .set heap = heap + " .$$ 
        _cr _cr 
    then  _; 

_: cells        ( n -- cell*n )
                _cr _."     .dw XT_CELLS " 
    state @ if 
        _cr _."     .dw XT_CELLS " 
    else 2* 
    then  _; 



_cr .( ; control structures ) _.s
_\ see VARIABLE _nextlabel above  

_: _label       latest name>string _type-name  _; 

_: ?do          _( --- x0 x1 )
                _cr _."     .dw XT_DOQDO " 
                _cr _."     .dw PFA_" _label  _nextlabel _dup 1 .r 
                _cr _." PFA_" _label  _nextlabel _dup 1 .r :emit _; _immediate \ok 

_: leave        _cr _."     .dw XT_LEAVE "           _; \ok 

_: +loop        _( xo x1 -- ) 
                _cr _."     .dw XT_DOPLUSLOOOP " 
                _cr _."     .dw PFA_"  _label _. 
                _cr _." PFA_" _label _.                 _; _immediate \ok 

_: loop         _( x0 x1 - ) 
                _cr _."     .dw XT_DOLOOP " 
                _cr _."     .dw PFA_" _label  _. 
                _cr _." PFA_" _label  _.                 _; _immediate \ok 

_: do           _( -- x0 x1 ) 
                _cr _."     .dw XT_DODO" 
                _cr _."     .dw PFA_"  _label  _nextlabel _dup 1 .r 
                _cr _." PFA_"  _label  _nextlabel _dup 1 .r :emit _; _immediate \ok 

_: begin        _( -- x ) 
                _cr _." PFA_"  _label  
                _nextlabel _dup 1 .r :emit          _; _immediate \ok 

_: again        _( x -- ) 
                _cr _."     .dw XT_DOBRANCH" 
                _cr _."     .dw PFA_" _label  _.        _; _immediate \ok 

_: until        _( x -- ) 
                _cr _."     .dw XT_DOCONDBRANCH" 
                _cr _."     .dw PFA_" _label  _.        _; _immediate \ok 

_: while        _( x0 -- x0 x1 ) 
                _cr _."     .dw XT_DOCONDBRANCH" 
                _cr _."     .dw PFA_" _label 
                _nextlabel _dup _.                      _; _immediate \ok 

_: repeat       _( x0 x1 -- ) 
                _swap 
                _cr _."     .dw XT_DOBRANCH"
                _cr _."     .dw PFA_" _label  _.   
                _cr _." PFA_" _label  1 .r :emit        _; _immediate \ok 

_: then         _( x -- ) 
                _cr _." PFA_" _label  1 .r :emit        _; _immediate \ok 

_: else         _( x0 -- x1 ) 
                _cr _."     .dw XT_DOBRANCH" 
                _cr _."     .dw PFA_" _label  
                          _nextlabel _dup _. 
                _swap [compile] then                _; _immediate \ok 

_: if           _( -- x) 
                _cr _."     .dw XT_DOCONDBRANCH" 
                _cr _."     .dw PFA_" _label  
                          _nextlabel _dup _.        _; _immediate \ok 


_cr .( ; exceptions utils ) _.s 
_: immediate   
             _cr _." Error: set-immediate-on befor : to build immediate word" 
   _cr  -260 throw                                  _; 



_\ Konvert forth stack comments to assembler comments. 
_: (        8 spaces ;emit space (emit space 
            [compile] .(  )emit space               _; _immediate \ok
_\ Print line \-comments as well. 
_: \       ;emit space   $0D parse type             _; _immediate \ok

_\ Use >>> in your forth code to force string into assembler file. 
_: >>>       $0D parse type _; 


_cr .( ; Simple words ) _.s 
_\ And here are all other simple words, they just type their XT_label. 
_\ Some amforth words missing? Go ahead and include them. 
_: d2/          _cr _."     .dw XT_D2SLASH "         _; 
_: s>d          _cr _."     .dw XT_STOD "            _; 
_: up!          _cr _."     .dw XT_UPSTORE "         _; 
_: up@          _cr _."     .dw XT_UPFETCH "         _; 
_: 0            _cr _."     .dw XT_ZERO "            _; 
_: 1ms          _cr _."     .dw XT_1MS "             _; 
_: ><           _cr _."     .dw XT_BYTESWAP "        _; 
_: cmove>       _cr _."     .dw XT_CMOVE> "          _; 
_: i!           _cr _."     .dw XT_ISTORE "          _; 
_: i@           _cr _."     .dw XT_IFETCH "          _; 
_: unloop       _cr _."     .dw XT_UNLOOP "          _; 
_: i            _cr _."     .dw XT_I "               _; 
_: sp!          _cr _."     .dw XT_SPSTORE "         _; 
_: sp@          _cr _."     .dw XT_SPFETCH "         _; 
_: rp!          _cr _."     .dw XT_RPSTORE "         _; 
_: rp@          _cr _."     .dw XT_RPFETCH "         _; 
_: +!           _cr _."     .dw XT_PLUSSTORE "       _; 
_: rshift       _cr _."     .dw XT_RSHIFT "          _; 
_: lshift       _cr _."     .dw XT_LSHIFT "          _; 
_: 1-           _cr _."     .dw XT_1MINUS "          _; 
_: 1+           _cr _."     .dw XT_1PLUS  "          _; 
_: xor          _cr _."     .dw XT_XOR "             _; 
_: or           _cr _."     .dw XT_OR "              _; 
_: and          _cr _."     .dw XT_AND "             _; 
_: 2*           _cr _."     .dw XT_2STAR "           _; 
_: 2/           _cr _."     .dw XT_2SLASH "          _; 
_: invert       _cr _."     .dw XT_INVERT "          _; 
_: um*          _cr _."     .dw XT_UMSTAR "          _; 
_: um/mod       _cr _."     .dw XT_UMSLASHMOD "      _; 
_: m*           _cr _."     .dw XT_MSTAR "           _; 
_: +            _cr _."     .dw XT_PLUS "            _; 
_: -            _cr _."     .dw XT_MINUS "           _; 
_: log2         _cr _."     .dw XT_LOG2 "            _; 
_: d<           _cr _."     .dw XT_DSMALER "         _; 
_: d>           _cr _."     .dw XT_DGREATER "        _; 
_: u>           _cr _."     .dw XT_UGREATER "        _; 
_: u<           _cr _."     .dw XT_ULESS "           _; 
_: 0>           _cr _."     .dw XT_ZEROGRATER "      _; 
_: 0<           _cr _."     .dw XT_ZEROLESS "        _; 
_: >            _cr _."     .dw XT_GRATER "          _; 
_: <            _cr _."     .dw XT_LESS "            _; 
_: 0=           _cr _."     .dw XT_ZEROEQUAL "       _; 
_: =            _cr _."     .dw XT_EQUAL "           _; 
_: <>           _cr _."     .dw XT_NOTEQUAL "        _; 
_: r@           _cr _."     .dw XT_R-FETCH "         _; 
_: >r           _cr _."     .dw XT_TO-R "            _; 
_: r>           _cr _."     .dw XT_R-FROM "          _; 
_: rot          _cr _."     .dw XT_ROT "             _; 
_: drop         _cr _."     .dw XT_DROP "            _; 
_: over         _cr _."     .dw XT_OVER "            _; 
_: swap         _cr _."     .dw XT_SWAP "            _; 
_: ?dup         _cr _."     .dw XT_QDUP "            _; 
_: dup          _cr _."     .dw XT_DUP "             _; 
_: c@           _cr _."     .dw XT_CFETCH "          _; 
_: c!           _cr _."     .dw XT_CSTORE "          _; 
_: !            _cr _."     .dw XT_STORE "           _; 
_: @            _cr _."     .dw XT_FETCH "           _; 
_: e@           _cr _."     .dw XT_EFETCH "          _; 
_: e!           _cr _."     .dw XT_ESTORE "          _; 
_: execute      _cr _."     .dw XT_EXECUTE "         _; 
_: exit         _cr _."     .dw XT_EXIT "            _; 
_: -int         _cr _."     .dw XT_MINUSINT "        _; 
_: +int         _cr _."     .dw XT_PLUSINT "         _; 
_: postpone     _cr _."     .dw XT_POSTPONE "        _; 
_: .errorx      _cr _."     .dw XT_DOTERRORX "       _; 
_: xoff         _cr _."     .dw XT_XOFF "            _; 
_: xon          _cr _."     .dw XT_XON "             _; 
_: d-           _cr _."     .dw XT_DMINUS "          _; 
_: d+           _cr _."     .dw XT_DPLUS "           _; 
_: spirw        _cr _."     .dw XT_SPIRW "           _; 
_: sleep        _cr _."     .dw XT_SLEEP "           _; 
_: wdr          _cr _."     .dw XT_WDR "             _; 
_: -wdt         _cr _."     .dw XT_MINUSWDT "        _; 
_: -jtag        _cr _."     .dw XT_MINUSJTAG "       _; 
_: end-code     _cr _."     .dw XT_ENDCODE "         _; 
_: code         _cr _."     .dw XT_CODE "            _; 
_: abort        _cr _."     .dw XT_ABORT "           _; 
_: abort"       _cr _."     .dw XT_ABORTQUOTE "      _; 
_: recurse      _cr _."     .dw XT_RECURSE "         _; 
_: [            _cr _."     .dw XT_LBRACKET "        _; 
_: ]            _cr _."     .dw XT_RBRACKET "        _; 
_: literal      _cr _."     .dw XT_LITERAL "         _; 
_: int@         _cr _."     .dw XT_INIFETCH "        _; 
_: int!         _cr _."     .dw XT_INTSTORE "        _; 
_: is           _cr _."     .dw XT_IS "              _; 
_: words        _cr _."     .dw XT_WORDS "           _; 
_: .s           _cr _."     .dw XT_DOTS "            _; 
_: applturnkey  _cr _."     .dw XT_APPLTURNKEY "     _; 
_: .$           _cr _."     .dw XT_DOTDOLLAR "       _; 
_: u0.r         _cr _."     .dw XT_UZERODOTR "       _; 
_: u.r          _cr _."     .dw XT_UDOTR "           _; 
_: u.           _cr _."     .dw XT_UDOT "            _; 
_: ud.          _cr _."     .dw XT_UDDOT "           _; 
_: ud.r         _cr _."     .dw XT_UDDOTR "          _; 
_: .            _cr _."     .dw XT_DOT "             _; 
_: d.           _cr _."     .dw XT_DDOT "            _; 
_: .r           _cr _."     .dw XT_DOTR "            _; 
_: d.r          _cr _."     .dw XT_DDOTR "           _; 
_: ud/mod       _cr _."     .dw XT_UDSLASHMOD "      _; 
_: dabs         _cr _."     .dw XT_DABS "            _; 
_: dnegate      _cr _."     .dw XT_DNEGATE "         _; 
_: dinvert      _cr _."     .dw XT_DINVERT "         _; 
_: >usart0      _cr _."     .dw XT_TOUSART0 "        _; 
_: +usart0      _cr _."     .dw XT_PLUSUSART0 "      _; 
_: baud0        _cr _."     .dw XT_BAUD0 "           _; 
_: tx0?         _cr _."     .dw XT_TX0Q "            _; 
_: tx0          _cr _."     .dw XT_TX0 "             _; 
_: rx0?         _cr _."     .dw XT_RX0Q "            _; 
_: rx0          _cr _."     .dw XT_RX0 "             _; 
_: d>s          _cr _."     .dw XT_D2S "             _; 
_: j            _cr _."     .dw XT_J "               _; 
_: *            _cr _."     .dw XT_STAR "            _; 
_: defer@       _cr _."     .dw XT_DEFEREFETCH "     _; 
_: defer!       _cr _."     .dw XT_DEFERESTORE "     _; 
_: icompare     _cr _."     .dw XT_ICOMPARE "        _; 
_: find         _cr _."     .dw XT_FIND "            _; 
_: to           _cr _."     .dw XT_TO "              _; 
_: value        _cr _."     .dw XT_VALUE "           _; 
_: unused       _cr _."     .dw XT_UNUSED "          _; 
_: noop         _cr _."     .dw XT_NOOP "            _; 
_: ver          _cr _."     .dw XT_VER "             _; 
_: interpret    _cr _."     .dw XT_INTERPRET "       _; 
_: depth        _cr _."     .dw XT_DEPTH "           _; 
_: rp0          _cr _."     .dw XT_RP0 "             _; 
_: sp           _cr _."     .dw XT_SP "              _; 
_: sp0          _cr _."     .dw XT_SP0 "             _; 
_: cold         _cr _."     .dw XT_COLD "            _; 
_: pause        _cr _."     .dw XT_PAUSE "           _; 
_: quit         _cr _."     .dw XT_QUIT "            _; 
_: place        _cr _."     .dw XT_PLACE "           _; 
_: word         _cr _."     .dw XT_WORD "            _; 
_: /string      _cr _."     .dw XT_SLASHSTRING "     _; 
_: source       _cr _."     .dw XT_SOURCE "          _; 
_: cscan        _cr _."     .dw XT_CSCAN "           _; 
_: parse        _cr _."     .dw XT_PARSE "           _; 
_: number       _cr _."     .dw XT_NUMBER "          _; 
_: char         _cr _."     .dw XT_CHAR "            _; 
_: refill       _cr _."     .dw XT_REFILL "          _; 
_: accept       _cr _."     .dw XT_ACCEPT "          _; 
_: cskip        _cr _."     .dw XT_CSKIP "           _; 
_: throw        _cr _."     .dw XT_THROW "           _; 
_: catch        _cr _."     .dw XT_CATCH "           _; 
_: handler      _cr _."     .dw XT_HANDLER "         _; 
_: '            _cr _."     .dw XT_TICKS "           _; 
_: type         _cr _."     .dw XT_TYPE "            _; 
_: count        _cr _."     .dw XT_COUNT "           _; 
_: spaces       _cr _."     .dw XT_SPACES "          _; 
_: space        _cr _."     .dw XT_SPACE "           _; 
_: cr           _cr _."     .dw XT_CR "              _; 
_: icount       _cr _."     .dw XT_ICOUNT "          _; 
_: itype        _cr _."     .dw XT_ITYPE "           _; 
_: s,           _cr _."     .dw XT_SKOMMA "          _; 
_: digit        _cr _."     .dw XT_DIGIT "           _; 
_: sign         _cr _."     .dw XT_SIGN "            _; 
_: #>           _cr _."     .dw XT_SHARP_G "         _; 
_: #s           _cr _."     .dw XT_SHARP_S "         _; 
_: <#           _cr _."     .dw XT_L_SHARP "         _; 
_: hold         _cr _."     .dw XT_HOLD "            _; 
_: hld          _cr _."     .dw XT_HLD "             _; 
_: max          _cr _."     .dw XT_MAX "             _; 
_: min          _cr _."     .dw XT_MIN "             _; 
_: abs          _cr _."     .dw XT_ABS "             _; 
_: mod          _cr _."     .dw XT_MOD "             _; 
_: /            _cr _."     .dw XT_SLASH "           _; 
_: negate       _cr _."     .dw XT_NEGATE "          _; 
_: u/mod        _cr _."     .dw XT_USLASHMOD "       _; 
_: */           _cr _."     .dw XT_STARSLASH "       _; 
_: /mod         _cr _."     .dw XT_SLASHMOD "        _; 
_: */mod        _cr _."     .dw XT_STARSLASHMOD "    _; 
_: turnkey      _cr _."     .dw XT_TURNKEY "         _; 
_: heap         _cr _."     .dw XT_HEAP "            _; 
_: edp          _cr _."     .dw XT_EDP "             _; 
_: bl           _cr _."     .dw XT_BL "              _; 
_: hex          _cr _."     .dw XT_HEX "             _; 
_: decimal      _cr _."     .dw XT_DECIMAL "         _; 
_: ,            _cr _."     .dw XT_COMMA "           _; 
_: compile      _cr _."     .dw XT_COMPILE "         _; \???
_: here         _cr _."     .dw XT_HERE "            _; 
_: head         _cr _."     .dw XT_HEAD "            _; 
_: /key         _cr _."     .dw XT_SLASHKEY "        _; 
_: key?         _cr _."     .dw XT_KEYQ "            _; 
_: key          _cr _."     .dw XT_KEY "             _; 
_: emit?        _cr _."     .dw XT_EMITQ "           _; 
_: emit         _cr _."     .dw XT_EMIT "            _; 
_: pad          _cr _."     .dw XT_PAD "             _; 
_: tibsize      _cr _."     .dw XT_TIBSIZE "         _; 
_: tib          _cr _."     .dw XT_TIB "             _; 
_: #tib         _cr _."     .dw XT_SHARPTIB "        _; 
_: >in          _cr _."     .dw XT_G_IN "            _; 
_: cell+        _cr _."     .dw XT_CELLPLUS "        _; 
_: base         _cr _."     .dw XT_BASE "            _; 
_: state        _cr _."     .dw XT_STATE "           _; 
_: f_cpu        _cr _."     .dw XT_F-CPU "           _; 


_\ _words
_cr .( ; finis ) _.s _cr _cr _cr 
