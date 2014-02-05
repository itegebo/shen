\*                                                   

**********************************************************************************
*                           The License						*
* 										*
* The user is free to produce commercial applications with the software, to 	*
* distribute these applications in source or binary  form, and to charge monies *
* for them as he sees fit and in concordance with the laws of the land subject 	*
* to the following license.							*
*										* 
* 1. The license applies to all the software and all derived software and 	*
*    must appear on such.							*
*										*
* 2. It is illegal to distribute the software without this license attached	*
*    to it and use of the software implies agreement with the license as such.  *
*    It is illegal for anyone who is not the copyright holder to tamper with 	*
*    or change the license.							*
*										*
* 3. Neither the names of Lambda Associates or the copyright holder may be used *
*    to endorse or promote products built using the software without specific 	*
*    prior written permission from the copyright holder.			*
*										*
* 4. That possession of this license does not confer on the copyright holder 	*
*    any special contractual obligation towards the user. That in no event 	* 
*    shall the copyright holder be liable for any direct, indirect, incidental, *   
*    special, exemplary or consequential damages (including but not limited     *
*    to procurement of substitute goods or services, loss of use, data, 	* 
*    interruption), however caused and on any theory of liability, whether in	* 
*    contract, strict liability or tort (including negligence) arising in any 	*
*    way out of the use of the software, even if advised of the possibility of 	*
*    such damage.								* 
*										*
* 5. It is permitted for the user to change the software, for the purpose of 	*
*    improving performance, correcting an error, or porting to a new platform, 	*
*    and distribute the derived version of Shen provided the resulting program 	*
*    conforms in all respects to the Shen standard and is issued under that     * 
*    title. The user must make it clear with his distribution that he/she is 	*
*    the author of the changes and what these changes are and why. 		*
*										*
* 6. Derived versions of this software in whatever form are subject to the same *
*    restrictions. In particular it is not permitted to make derived copies of  *
*    this software which do not conform to the Shen standard or appear under a  *
*    different title.								*
*										*
*    It is permitted to distribute versions of Shen which incorporate libraries,*
*    graphics or other facilities which are not part of the Shen standard.	*
*										*
* For an explication of this license see www.shenlanguage.org/license.htm which *
* explains this license in full. 
*				 						*
*********************************************************************************

*\

(package shen. []

(define read-file-as-bytelist
 File -> (let Stream (open File in)
               Byte (read-byte Stream)
               Bytes (read-file-as-bytelist-help Stream Byte [])
               Close (close Stream)
               (reverse Bytes)))  

(define read-file-as-bytelist-help
  Stream -1 Bytes -> Bytes
  Stream Byte Bytes -> (read-file-as-bytelist-help Stream (read-byte Stream) [Byte | Bytes]))

(define read-file-as-string
   File -> (let Stream (open File in) 
               (rfas-h Stream (read-byte Stream) "")))
               
(define rfas-h
  Stream -1 String -> (do (close Stream) String)
  Stream N String -> (rfas-h Stream (read-byte Stream) (cn String (n->string N))))  

(define input
  Stream -> (eval-kl (read Stream)))

(define input+ 
  Type Stream -> (let Mono? (monotype Type)
                      Input (read Stream)                      
                      (if (= false (typecheck Input Type))
                          (error "type error: ~R is not of type ~R~%" Input Type)
                          (eval-kl Input))))

(define monotype
  [X | Y] -> (map (function monotype) [X | Y])
  X -> (if (variable? X) (error "input+ expects a monotype: not ~A~%" X) X))

(define read
  Stream -> (hd (read-loop Stream (read-byte Stream) [])))

(define read-loop
  _ -1 Bytes -> (if (empty? Bytes)
                    (simple-error "error: empty stream")
                    (compile (function <st_input>) Bytes (/. E E)))
  Stream Byte Bytes -> (let AllBytes (append Bytes [Byte])
                            Read (compile (function <st_input>) AllBytes (/. E nextbyte))
                            (if (or (= Read nextbyte) (empty? Read))
                                (read-loop Stream (read-byte Stream) AllBytes)
                                Read))    where (terminator? Byte) 
  Stream Byte Bytes -> (read-loop Stream (read-byte Stream) (append Bytes [Byte])))   
  
(define terminator?
  Byte -> (element? Byte [9 10 13 32 34 41 93]))           

(define lineread
  Stream -> (lineread-loop (read-byte Stream) [] Stream))

(define lineread-loop
  -1 Bytes Stream -> (if (empty? Bytes)
                         (simple-error "empty stream")
                         (compile (function <st_input>) Bytes (/. E E)))
  Byte _ Stream -> (error "line read aborted")  where (= Byte (hat))
  Byte Bytes Stream -> (let Line (compile (function <st_input>) Bytes (/. E nextline))
                           (if (or (= Line nextline) (empty? Line))
                               (lineread-loop (read-byte Stream) (append Bytes [Byte]) Stream)
                               Line))	where (element? Byte [(newline) (carriage-return)])
  Byte Bytes Stream -> (lineread-loop (read-byte Stream) (append Bytes [Byte]) Stream))

(define read-file
  File -> (let Bytelist (read-file-as-bytelist File)
               (compile (function <st_input>) Bytelist (function read-error))))

(define read-from-string
  S -> (let Ns (map (function string->n) (explode S))
            (compile (function <st_input>) 
                     Ns 
                     (function read-error))))

(define read-error
  [[Byte | Bytes] _] -> (error "read error here:~%~% ~A~%" (compress-50 50 [Byte | Bytes]))
  _ -> (error "read error~%"))

(define compress-50
  _ [] -> ""
  0 _ -> ""
  N [Byte | Bytes] -> (cn (n->string Byte) (compress-50 (- N 1) Bytes)))

(defcc <st_input>
  <lsb> <st_input1> <rsb> <st_input2> 
    := [(macroexpand (cons_form <st_input1>)) | <st_input2>];
  <lrb>  <st_input1> <rrb> <st_input2> 
   := (package-macro (macroexpand <st_input1>) <st_input2>);
  <lcurly> <st_input> := [{ | <st_input>];
  <rcurly> <st_input> := [} | <st_input>];    
  <bar> <st_input> := [bar! | <st_input>];  
  <semicolon> <st_input> := [; | <st_input>];
  <colon> <equal> <st_input> := [:= | <st_input>];
  <colon> <minus> <st_input> := [:- | <st_input>];
  <colon> <st_input> := [: | <st_input>];
  <comma> <st_input> := [(intern ",") | <st_input>];
  <comment> <st_input> := <st_input>;
  <atom> <st_input> := [(macroexpand <atom>) | <st_input>];
  <whitespaces> <st_input> := <st_input>;
  <e> := [];)
  
(defcc <lsb>
   91 := skip;)  
   
(defcc <rsb>
   93 := skip;)     
  
(defcc <lcurly>
  123 := skip;)
  
(defcc <rcurly>
  125 := skip;)
  
(defcc <bar>
  124 := skip;)  
  
(defcc <semicolon>
  59 := skip;) 
  
(defcc <colon>
  58 := skip;)     
      
(defcc <comma>
  44 := skip;)  
  
(defcc <equal>
  61 := skip;)     
   
(defcc <minus>
  45 := skip;)      
  
(defcc <lrb>
  40 := skip;)
  
(defcc <rrb>
  41 := skip;)   
  
(defcc <atom>
  <str> := (control-chars <str>); 
  <number> := <number>; 
  <sym> := (if (= <sym> "<>")
               [vector 0]
               (intern <sym>));)

(define control-chars
  [] -> ""
  ["c" "#" | Ss]
   -> (let CodePoint (code-point Ss)
           AfterCodePoint (after-codepoint Ss)
           (@s (n->string (decimalise CodePoint)) (control-chars AfterCodePoint)))  
  [S | Ss] -> (@s S (control-chars Ss)))
                          
(define code-point
  [";" | _] -> ""
  [S | Ss] -> [S | (code-point Ss)]  
                     where (element? S ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "0"])
  S -> (error "code point parse error ~A~%" S))                          
                          
(define after-codepoint
   [] -> []
   [";" | Ss] -> Ss
   [_ | Ss] -> (after-codepoint Ss))                          
                          
(define decimalise
  S -> (pre (reverse (digits->integers S)) 0))

(define digits->integers
  ["0" | S] -> [0 | (digits->integers S)]
  ["1" | S] -> [1 | (digits->integers S)]
  ["2" | S] -> [2 | (digits->integers S)]
  ["3" | S] -> [3 | (digits->integers S)]
  ["4" | S] -> [4 | (digits->integers S)]
  ["5" | S] -> [5 | (digits->integers S)]
  ["6" | S] -> [6 | (digits->integers S)]
  ["7" | S] -> [7 | (digits->integers S)]
  ["8" | S] -> [8 | (digits->integers S)]
  ["9" | S] -> [9 | (digits->integers S)]
   _ -> [])

(defcc <sym>
  <alpha> <alphanums> := (@s <alpha> <alphanums>);)

(defcc <alphanums>
   <alphanum> <alphanums> := (@s <alphanum> <alphanums>);
   <e> := "";)  
   
(defcc <alphanum>
    <alpha> := <alpha>;
    <num> := <num>;) 	

(defcc <num>
  Byte := (n->string Byte)    where (numbyte? Byte);)

(define numbyte?
  48 -> true
  49 -> true
  50 -> true
  51 -> true 
  52 -> true
  53 -> true 
  54 -> true 
  55 -> true
  56 -> true 
  57 -> true
  _ -> false)
   
(defcc <alpha>
  Byte := (n->string Byte)	  where (symbol-code? Byte);)

(define symbol-code?
  N -> (or (= N 126)
           (and (> N 94) (< N 123))
           (and (> N 59) (< N 91))
           (and (> N 41) (< N 58) (not (= N 44)))
           (and (> N 34) (< N 40))
           (= N 33)))
  
(defcc <str>
  <dbq> <strcontents> <dbq> := <strcontents>;)
  
(defcc <dbq>
  Byte := Byte	where (= Byte 34);)    
  
(defcc <strcontents>
  <strc> <strcontents> := [<strc> | <strcontents>];
  <e> := [];)
  
(defcc <byte>
  Byte := (n->string Byte);)  
  
(defcc <strc>
  Byte := (n->string Byte)	where (not (= Byte 34));)
  
(defcc <number>
   <minus> <number> := (- 0 <number>);
   <plus> <number> := <number>;
   <predigits> <stop> <postdigits> <E> <log10> 
   := (* (expt 10 <log10>) (+ (pre (reverse <predigits>) 0) (post <postdigits> 1)));
   <digits> <E> <log10> := (* (expt 10 <log10>) (pre (reverse <digits>) 0));
   <predigits> <stop> <postdigits> 
   := (+ (pre (reverse <predigits>) 0) (post <postdigits> 1));
   <digits> := (pre (reverse <digits>) 0);)

(defcc <E>
   101 := skip;)

(defcc <log10>
  <minus> <digits> := (- 0 (pre (reverse <digits>) 0));
  <digits> := (pre (reverse <digits>) 0);)
   
(defcc <plus>
  Byte := Byte 	where (= Byte 43);)
  
(defcc <stop>
  Byte := Byte 	where (= Byte 46);)      
   
(defcc <predigits>
    <digits> := <digits>;
    <e> := [];)
    
(defcc <postdigits>
  <digits> := <digits>;)

(defcc <digits>
   <digit> <digits> := [<digit> | <digits>];
   <digit> := [<digit>];)
 
(defcc <digit>
  X := (byte->digit X)  where (numbyte? X);)
  
(define byte->digit  
  48 -> 0   
  49 -> 1  
  50 -> 2  
  51 -> 3  
  52 -> 4  
  53 -> 5  
  54 -> 6
  55 -> 7   
  56 -> 8   
  57 -> 9)
  
(define pre
  [] _ -> 0
  [N | Ns] Expt -> (+ (* (expt 10 Expt) N) (pre Ns (+ Expt 1))))
  
(define post
  [] _ -> 0
  [N | Ns] Expt -> (+ (* (expt 10 (- 0 Expt)) N) (post Ns (+ Expt 1)))) 
    
(define expt
  _ 0 -> 1
  Base Expt -> (* Base (expt Base (- Expt 1)))  where (> Expt 0)
  Base Expt -> (* 1.0 (/ (expt Base (+ Expt 1)) Base)))  
  
(defcc <st_input1>
  <st_input> := <st_input>;)

(defcc <st_input2>
  <st_input> := <st_input>;)

(defcc <comment>
  <singleline> := skip;
  <multiline> := skip;)

(defcc <singleline>
  <backslash> <backslash> <anysingle> <return> := skip;)

(defcc <backslash>
  92 := skip;)

(defcc <anysingle>
   <non-return> <anysingle> := skip;
   <e> := skip;)

(defcc <non-return>
   X :=  skip   where (not (element? X [10 13]));)

(defcc <return>
  X  := skip  where (element? X [10 13]);)
 
(defcc <multiline>
  <backslash> <times> <anymulti> := skip;)

(defcc <times>
  42 := skip;)       

(defcc <anymulti>
  <comment> <anymulti> := skip;
  <times> <backslash> := skip;
  X <anymulti> := skip;)

(defcc <whitespaces>
  <whitespace> <whitespaces> := skip;
  <whitespace> := skip;)

(defcc <whitespace>
  X := skip     where (let Case X
                      (or (= Case 32) 
                          (= Case 13) 
                          (= Case 10) 
                          (= Case 9)));)                      

(define cons_form
  [] -> []
  [X Bar Y] -> [cons X Y]	  where (= Bar bar!)
  [X | Y] -> [cons X (cons_form Y)])  
 
(define package-macro
    [$ S] Stream -> (append (explode S) Stream)
    [package null _ | Code] Stream -> (append Code Stream)
    [package PackageName Exceptions | Code] Stream
     -> (let ListofExceptions (eval-without-macros Exceptions)
             Record (record-exceptions ListofExceptions PackageName)
             PackageNameDot (intern (cn (str PackageName) "."))
             (append (packageh PackageNameDot ListofExceptions Code) Stream))
    X Stream -> [X | Stream])  

(define record-exceptions 
  ListofExceptions PackageName 
   -> (let CurrExceptions (trap-error (get PackageName external-symbols) (/. E []))
           AllExceptions (union ListofExceptions CurrExceptions)
           (put PackageName external-symbols AllExceptions)))   
       
(define packageh
    PackageName Exceptions [X | Y] 
      -> [(packageh PackageName Exceptions X) | (packageh PackageName Exceptions Y)]
    PackageName Exceptions X -> X  
                 where (or (sysfunc? X) (variable? X) (element? X Exceptions)
                           (doubleunderline? X) (singleunderline? X))
    PackageName Exceptions X -> (concat PackageName X)   
             where (and (symbol? X) (not (prefix? [($ shen.)] (explode X))))
    _ _ X -> X) )

