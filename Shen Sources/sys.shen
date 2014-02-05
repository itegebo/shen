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

(define thaw 
  F -> (F))

(define eval
  X -> (let Macroexpand (walk (function macroexpand) X)
            (if (packaged? Macroexpand)
                (map (function eval-without-macros) (package-contents Macroexpand))
                (eval-without-macros Macroexpand))))

(define eval-without-macros 
  X -> (eval-kl (elim-def (proc-input+ X)))) 

(define proc-input+ 
  [input+ Type Stream] -> [input+ (rcons_form Type) Stream]
  [read+ Type Stream] -> [read+ (rcons_form Type) Stream]
  [X | Y] -> (map (function proc-input+) [X | Y])
  X -> X) 
  
(define elim-def
  [define F | Rest] -> (shen->kl F Rest)
  [defmacro F | Rest] -> (let Default [(protect X) -> (protect X)]
                              Def (elim-def [define F | (append Rest Default)]) 
                              MacroAdd (add-macro F) 
                              Def)
  [defcc F | X] -> (elim-def (yacc [defcc F | X]))
  [X | Y] -> (map (function elim-def) [X | Y])
  X -> X)

(define add-macro
  F -> (set *macros* (adjoin F (value *macros*))))
                
(define packaged?
  [package P E | _] -> true
  _ -> false)

(define external
  Package -> (trap-error (get Package external-symbols) 
                         (/. E (error "package ~A has not been used.~%" Package))))
  
(define package-contents
  [package null _ | Contents] -> Contents
  [package P E | Contents] -> (packageh P E Contents))
              
(define walk
  F [X | Y] -> (F (map (/. Z (walk F Z)) [X | Y]))
  F X -> (F X))              

(define compile
   F X Err -> (let O (F [X []])
                   (if (or (= (fail) O) (not (empty? (hd O))))
                       (Err O)
                       (hdtl O))))

(define fail-if
  F X -> (if (F X) (fail) X))  

(define @s
  X Y -> (cn X Y))

(define tc?
  -> (value *tc*))
 
(define ps
  Name -> (trap-error (get Name source) (/. E (error "~A not found.~%" Name))))

(define stinput 
  -> (value *stinput*))

(define +vector? 
 X -> (and (absvector? X) (> (<-address X 0) 0)))

(define vector
   N -> (let Vector (absvector (+ N 1))
             ZeroStamp (address-> Vector 0 N)
             Standard (if (= N 0) ZeroStamp (fillvector ZeroStamp 1 N (fail)))
             Standard))
                            
(define fillvector 
  Vector N N X -> (address-> Vector N X)
  Vector Counter N X -> (fillvector (address-> Vector Counter X) (+ 1 Counter) N X))

(define vector? 
  X -> (and (absvector? X) (trap-error (>= (<-address X 0) 0) (/. E false))))

(define vector-> 
  Vector N X -> (if (= N 0) 
                    (error "cannot access 0th element of a vector~%")
                    (address-> Vector N X)))

(define <-vector 
  Vector N -> (if (= N 0) 
                  (error "cannot access 0th element of a vector~%")
                  (let VectorElement (<-address Vector N)
                      (if (= VectorElement (fail))
                          (error "vector element not found~%")
                          VectorElement))))

(define posint? 
  X -> (and (integer? X) (>= X 0)))

(define limit 
  Vector -> (<-address Vector 0))

(define symbol?
  X -> false where (or (boolean? X) (number? X) (string? X))
  X -> (trap-error (let String (str X)
                        (analyse-symbol? String)) (/. E false)))

(define analyse-symbol?
  (@s S Ss) -> (and (alpha? S)
                    (alphanums? Ss)))

(define alpha?
  S ->  (element? S ["A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
                     "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
                     "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m" 
                     "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
                     "=" "*" "/" "+" "-" "_" "?" "$" "!" "@" "~" ">" "<" 
                     "&" "%" "{" "}" ":" ";" "`" "#" "'" "."])) 
                     
(define alphanums?
  "" -> true
  (@s S Ss) -> (and (alphanum? S) (alphanums? Ss)))

(define alphanum?
  S -> (or (alpha? S) (digit? S)))

(define digit?
  S -> (element? S ["1" "2" "3" "4" "5" "6" "7" "8" "9" "0"]))
                              
(define variable?
  X -> false where (or (boolean? X) (number? X) (string? X))
  X -> (trap-error (let String (str X)
                        (analyse-variable? String)) (/. E false)))

(define analyse-variable?
  (@s S Ss) -> (and (uppercase? S)
                   (alphanums? Ss)))

(define uppercase?
  S ->  (element? S ["A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
                     "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"]))   
                      
(define gensym
  Sym -> (concat Sym (set *gensym* (+ 1 (value *gensym*)))))
  
(define concat
  S1 S2 -> (intern (cn (str S1) (str S2))))  

(define @p 
  X Y -> (let Vector (absvector 3)
              Tag (address-> Vector 0 tuple)
              Fst (address-> Vector 1 X)
              Snd (address-> Vector 2 Y)
              Vector))

(define fst 
  X -> (<-address X 1))
           
(define snd 
  X -> (<-address X 2))

(define tuple? 
  X -> (trap-error (and (absvector? X) (= tuple (<-address X 0))) (/. E false)))

(define append
  [] X -> X
  [X | Y] Z -> [X | (append Y Z)])

(define @v
  X Vector -> (let Limit (limit Vector)
                   NewVector (vector (+ Limit 1))
                   X+NewVector (vector-> NewVector 1 X)
                   (if (= Limit 0) 
                       X+NewVector
                       (@v-help Vector 1 Limit X+NewVector))))

(define @v-help
  OldVector N N NewVector -> (copyfromvector OldVector NewVector N (+ N 1))
  OldVector N Limit NewVector -> (@v-help OldVector (+ N 1) Limit 
                                     (copyfromvector OldVector NewVector N (+ N 1))))

(define copyfromvector
  OldVector NewVector From To -> (trap-error (vector-> NewVector To (<-vector OldVector From)) (/. E NewVector))) 

(define hdv
  Vector -> (trap-error (<-vector Vector 1) (/. E (error "hdv needs a non-empty vector as an argument; not ~S~%" Vector))))

(define tlv
  Vector -> (let Limit (limit Vector)
                 (cases (= Limit 0) (error "cannot take the tail of the empty vector~%")
                        (= Limit 1) (vector 0)
                        true (let NewVector (vector (- Limit 1))
                                  (tlv-help Vector 2 Limit (vector (- Limit 1)))))))

(define tlv-help
  OldVector N N NewVector -> (copyfromvector OldVector NewVector N (- N 1))
  OldVector N Limit NewVector -> (tlv-help OldVector (+ N 1) Limit 
                                     (copyfromvector OldVector NewVector N (- N 1))))

(define assoc
  _ [] -> []
  X [[X | Y] | _] -> [X | Y]
  X [_ | Y] -> (assoc X Y))

(define boolean?
  true -> true
  false -> true
  _ -> false)

(define nl
  0 -> 0
  N -> (do (output "~%") (nl (- N 1))))

(define difference
  [] _ -> []
  [X | Y] Z -> (if (element? X Z) (difference Y Z) [X | (difference Y Z)]))

(define do
  X Y -> Y)

(define element?
  _ [] -> false
  X [X | _] -> true
  X [_ | Z] -> (element? X Z))

(define empty?
  [] -> true
  _ -> false)

(define fix
  F X -> (fix-help F X (F X)))

(define fix-help
  _ X X -> X
  F _ X -> (fix-help F X (F X)))

(define put
  X Pointer Y Vector -> (let N (hash X (limit Vector))
                             Entry (trap-error (<-vector Vector N) (/. E []))
                             Change (vector-> Vector N (change-pointer-value X Pointer Y Entry))
                             Y))

(define change-pointer-value 
  X Pointer Y [] -> [[[X Pointer] | Y]]
  X Pointer Y [[[X Pointer] | _] | Entry] -> [[[X Pointer] | Y] | Entry]
  X Pointer Y [Z | Entry] -> [Z | (change-pointer-value X Pointer Y Entry)])

(define get
  X Pointer Vector -> (let N (hash X (limit Vector))
                           Entry (trap-error (<-vector Vector N) 
                                      (/. E (error "pointer not found~%")))
                           Result (assoc [X Pointer] Entry)
                           (if (empty? Result) (error "value not found~%") (tl Result)))) 

(define hash
  S Limit -> (let Hash (mod (sum (map (function string->n) (explode S))) Limit)
                  (if (= 0 Hash)
                      1
                      Hash)))
                      
(define mod
  N Div -> (modh N (multiples N [Div])))
  
(define multiples
  N [M | Ms] ->  Ms   where (> M N)
  N [M | Ms] -> (multiples N [(* 2 M) M | Ms]))
  
(define modh
   0 _ -> 0
   N [] -> N
   N [M | Ms] -> (if (empty? Ms)
                           N
                           (modh N Ms))   where (> M N)
   N [M | Ms] -> (modh (- N M) [M | Ms]))

(define sum
  [] -> 0
  [N | Ns] -> (+ N (sum Ns)))

(define head
  [X | _] -> X
  _ -> (error "head expects a non-empty list"))

(define tail
  [_ | Y] -> Y
  _ -> (error "tail expects a non-empty list"))

(define hdstr
  S -> (pos S 0))

(define intersection
  [] _ -> []
  [X | Y] Z -> (if (element? X Z) [X | (intersection Y Z)] (intersection Y Z)))

(define reverse
  X -> (reverse_help X []))

(define reverse_help
  [] R -> R
  [X | Y] R -> (reverse_help Y [X | R]))
  
(define union
  [] X -> X
  [X | Y] Z -> (if (element? X Z) (union Y Z) [X | (union Y Z)]))

(define y-or-n?
  String -> (let Message (output String)
                 Y-or-N (output " (y/n) ")
                 Input (make-string "~S" (read (stinput)))
                 (cases (= "y" Input) true 
                        (= "n" Input) false 
                        true (do (output "please answer y or n~%")
                                 (y-or-n? String)))))

(define not
  X -> (if X false true))

(define subst
  X Y Y -> X
  X Y [W | Z] -> [(subst X Y W) | (subst X Y Z)]
  _ _ Z -> Z)

(define explode
  X -> (explode-h (make-string "~A" X)))

(define explode-h
  "" -> []
  (@s S Ss) -> [S | (explode-h Ss)])

(define cd 
  Path -> (set *home-directory* (if (= Path "") "" (make-string "~A/" Path))))

(define map
  F X -> (map-h F X []))

(define map-h
  _ [] X -> (reverse X)
  F [X | Y] Z -> (map-h F Y [(F X) | Z]))

(define length
  X -> (length-h X 0))

(define length-h
  [] N -> N
  X N -> (length-h (tl X) (+ N 1)))

(define occurrences
  X X -> 1
  X [Y | Z] -> (+ (occurrences X Y) (occurrences X Z))
  _ _ -> 0)

(define nth 
  1 [X | _] -> X
  N [_ | Y] -> (nth (- N 1) Y))
  
(define integer? 
  N -> (and (number? N) (let Abs (abs N) (integer-test? Abs (magless Abs 1)))))

(define abs
  N -> (if (> N 0) N (- 0 N)))

(define magless
  Abs N -> (let Nx2 (* N 2)
                (if (> Nx2 Abs)
                    N
                    (magless Abs Nx2))))

(define integer-test?
  0 _ -> true
  Abs _ -> false    where (> 1 Abs)  
  Abs N -> (let Abs-N (- Abs N)
                (if (> 0 Abs-N)
                    (integer? Abs)
                    (integer-test? Abs-N N))))  

(define mapcan
  _ [] -> []
  F [X | Y] -> (append (F X) (mapcan F Y)))

(define ==
  X X -> true
  _ _ -> false)

(define abort
  -> (simple-error ""))
    
(define bound? 
  Sym -> (and (symbol? Sym) 
              (let Val (trap-error (value Sym) (/. E this-symbol-is-unbound))
                          (if (= Val this-symbol-is-unbound)
                              false
                              true))))
                              
(define string->bytes
  "" -> []
  S -> [(string->n (pos S 0)) | (string->bytes (tlstr S))])

(define maxinferences
  N -> (set *maxinferences* N))

(define inferences 
  -> (value *infs*))

(define protect
  X -> X)

(define stoutput
  -> (value *stoutput*))

(define string->symbol
  S -> (let Symbol (intern S)
          (if (symbol? Symbol)
              Symbol
              (error "cannot intern ~S to a symbol" S))))

(define optimise
  + -> (set *optimise* true)
  - -> (set *optimise* false)
  _ -> (error "optimise expects a + or a -.~%")))

(define os
  -> (value *os*))

(define language
  -> (value *language*))

(define version
  -> (value *version*))

(define port
  -> (value *port*))

(define porters
  -> (value *porters*))

(define implementation
  -> (value *implementation*))

(define release
  -> (value *release*))

