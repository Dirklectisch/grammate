(ns grammate.edn
  (:require [clojure.string :refer [join]]))

; The EDN spec can be found here: https://github.com/edn-format/edn 

(defn re-join [& coll]
  (re-pattern (join coll)))
        
(def patterns
  (let [lbl (re-join #"(?:[\-+][:#.*+!\-_?$%&=A-Za-z]|[.*+!\-_?$%&=A-Za-z])"
                     #"[:#.*+!\-_?$%&=A-Za-z0-9]*")
        sym (re-join "(" lbl ")?" "(/)?" "(" lbl ")?")]
    { :literals { :patterns (mapv (fn [x] { :include x }) ["#nil" 
                                                           "#boolean" 
                                                           "#character"
                                                           "#floating-point-number"
                                                           "#integer"
                                                           "#keyword"
                                                           "#string"]) }
      :nil { :name "constant.language.nil.edn"
             :match #"nil" }
      :boolean { :name "constant.language.boolean.edn"
                 :match #"true|false" }
      :character { :name "constant.character.edn"
                   :match #"(\\)(?:newline|return|space|tab|\S)"
                   :captures {1 { :name "punctuation.definition.character.begin.edn" }}}
      :keyword { :name "constant.other.keyword.edn"
                 :match (re-join ":" sym)
                 :captures { 1 { :name "constant.other.keyword.prefix.edn" }
                             2 { :name "punctuation.definition.keyword.seperator.edn" }
                             3 { :name "constant.other.keyword.name.edn" } }}
      :symbol { :name "variable.other.symbol.edn"
                :match sym
                :captures { 1 { :name "variable.other.symbol.prefix.edn" }
                            2 { :name "punctuation.definition.symbol.seperator.edn" }
                            3 { :name "variable.other.symbol.name.edn" } }}}))
  
(def grammar
  { :patterns [ { :include "#boolean" } 
                { :include "#keyword" }
                { :include "#symbol" } ]
    :repository patterns })

    ; {  patterns = (
    ;     {  name = 'punctuation.whitespace.edn';
    ;       match = '\s|,';
    ;     },
    ;     {  name = 'comment.line.semicolon.edn';
    ;       begin = ';';
    ;       end = '\n';
    ;     },
    ;     {  include = '#tag'; },
    ;     {  include = '#value'; },
    ;   );
    ;   repository = {
    ;     value = {
    ;       patterns = (
    ;         {  include = '#literal'; },
    ;         {  include = '#sequence'; },
    ;         {  include = '#symbol'; },
    ;       );
    ;     };
    ;     character = {
    ;       name = 'constant.character.edn';
    ;       match = '(?x:                              # turn on extended mode
    ;                  (\\)                            # a literal backslash
    ;                  (?:                             # ...followed by...
    ;                    newline|return|space|tab      # one of these words
    ;                    |                             # ...or...
    ;                    \S                            # a non whitespace character
    ;                  )
    ;                )';
    ;       captures = { 1 = { name = 'punctuation.definition.character.begin.edn'; }; };
    ;     };
    ;     floating-point-numbers = {
    ;       name = 'constant.numeric.float';
    ;       comment = 'TODO: refactor expression to be more DRY';
    ;       match = '(?x:
    ;                   (?:
    ;                     [+-]?
    ;                     \d
    ;                     |
    ;                     [1-9](?:\d)*
    ;                   )
    ;                   (?:
    ;                     M
    ;                     |
    ;                     (e | e\+ | e\- | E | E\+ | E\-)\d+
    ;                     |
    ;                     \.\d+ (e | e\+ | e\- | E | E\+ | E\-) \d+
    ;                     |
    ;                     \.\d+
    ;                   )
    ;                )';
    ;     };
    ;     integer = {
    ;       name = 'constant.numeric.integer.edn';
    ;       match = '(?x:
    ;                  (?:
    ;                    [+-]?
    ;                    \d
    ;                    |
    ;                    [1-9](?:\d)*
    ;                  )
    ;                    N?
    ;                )';
    ;     };
    ;     list = {
    ;       name = 'meta.structure.list.edn';
    ;       begin = '\(';
    ;       end = '\)';
    ;       beginCaptures = { 0 = { name = 'punctuation.definition.list.begin.edn'; }; };
    ;       endCaptures = { 0 = { name = 'punctuation.definition.list.end.edn'; }; };
    ;       patterns = ( { include = '#value'; } );
    ;     };
    ;     map = {
    ;       name = 'meta.structure.map.edn';
    ;       comment = 'TODO: Values must be included in pairs';
    ;       begin = '\{';
    ;       end = '\}';
    ;       beginCaptures = { 0 = { name = 'punctuation.definition.map.begin.edn'; }; };
    ;       endCaptures = { 0 = { name = 'punctuation.definition.map.end.edn'; }; };
    ;       patterns = ( { include = '#value'; } );
    ;     };
    ;     sequence = {
    ;       patterns = (
    ;         {  include = '#map'; },
    ;         {  include = '#set'; },
    ;         {  include = '#vector'; },
    ;       );
    ;     };
    ;     set = {
    ;       name = 'meta.structure.set.edn';
    ;       begin = '#\{';
    ;       end = '\}';
    ;       beginCaptures = { 0 = { name = 'punctuation.definition.set.begin.edn'; }; };
    ;       endCaptures = { 0 = { name = 'punctuation.definition.set.end.edn'; }; };
    ;       patterns = ( { include = '#value'; } );
    ;     };
    ;     string = {
    ;       name = 'string.quoted.double.edn';
    ;       begin = '"';
    ;       end = '"';
    ;       beginCaptures = { 0 = { name = 'punctuation.definition.string.begin.edn'; }; };
    ;       endCaptures = { 0 = { name = 'punctuation.definition.string.end.edn'; }; };
    ;       patterns = (
    ;         {  name = 'constant.character.escape.edn';
    ;           match = '(?x:                # turn on extended mode
    ;                      \\                # a literal backslash
    ;                      (?:               # ...followed by...
    ;                        ["\\/bfnrt]     # one of these characters
    ;                        |               # ...or...
    ;                        u               # a u
    ;                        [0-9a-fA-F]{4}  # and four hex digits
    ;                      )
    ;                    )';
    ;         },
    ;         {  name = 'invalid.illegal.unrecognized-string-escape.edn';
    ;           match = '\\.';
    ;         },
    ;       );
    ;     };
    ;     tag = {
    ;       name = 'variable.other.tag.edn';
    ;       match = '#[_A-Za-z0-9][:#.*+!\-_?$%&=A-Za-z0-9]*';
    ;     };
    ;     vector = {
    ;       name = 'meta.structure.vector.edn';
    ;       begin = '\[';
    ;       end = '\]';
    ;       beginCaptures = { 0 = { name = 'punctuation.definition.vector.begin.edn'; }; };
    ;       endCaptures = { 0 = { name = 'punctuation.definition.vector.end.edn'; }; };
    ;       patterns = ( { include = '#value'; } );
    ;     };
    ;   };
    ; }