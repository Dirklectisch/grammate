(ns grammate.edn
  "Home of some really crazy but well tested regular expressions"
  (:require [clojure.string :refer [join]]))

; The EDN spec can be found here: https://github.com/edn-format/edn
; Java's regex documentation can be found here: https://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html

(defn re-join [& coll]
  (re-pattern (join coll)))

(defn re-wb [r]
  "Encloses pattern r using custom word boundries"
  (let [wc #"\\.|[-+:#.*!?$%&=><\w\\/]"
        ws (re-join "(?<!" wc ")(?=" wc ")" )
        we (re-join "(?<=" wc ")(?!" wc ")" )]
    (re-join ws "(?:" r ")" we)))  
        
(def patterns
  (let [lbl (re-join #"(?:[-+](?:[-+:#.*!?$%&=><A-Za-z_]|(?!\\.|[-+:#.*!?$%&=><\w\\/]))|[.*!?$%&=><A-Za-z_])[-+:#.*!?$%&=><\w]*")
        sym (re-join "(?:(?:(" lbl ")/)?" "(" lbl ")|/)")
        int #"[+-]?(0|[1-9][0-9]*)"]
    { :literals { :patterns (mapv (fn [x] { :include x }) ["#nil" 
                                                           "#boolean" 
                                                           "#character"
                                                           "#floating-point-number"
                                                           "#integer"
                                                           "#keyword"
                                                           "#string"]) }
      :nil { :name "constant.language.nil.edn"
             :match (re-wb #"nil")}
      :boolean { :name "constant.language.boolean.edn"
                 :match (re-wb #"true|false") }
      :character { :name "constant.character.edn"
                   :match (re-wb #"(\\)(?:newline|return|space|tab|[\x00-\x7F])")
                   :captures {1 { :name "punctuation.definition.character.begin.edn" }}}
      :integer { :name "constant.numeric.integer.edn"
                 :match (re-wb #"[+-]?(0|[1-9][0-9]*)N?") }
      :floating-point-number { :name "constant.numeric.float"
                               :match (re-wb (re-join int #"(?:M|\.\d+(?:[eE][+-]?\d+)?M?|[eE][+-]?\d+M?)"))}
      :keyword { :name "constant.other.keyword.edn"
                 :match (re-wb #":[-+#.*!?$%&=><\w][-+:#.*!?$%&=><\w]*(?:/[-+:#.*!?$%&=><\w]+)?" )}
      :symbol { :name "variable.other.symbol.edn"
                :match (re-wb (re-join sym "(?<!nil|true|false)"))
                :captures { 1 { :name "variable.other.symbol.prefix.edn" }
                            2 { :name "punctuation.definition.symbol.seperator.edn" }
                            3 { :name "variable.other.symbol.name.edn" } }}
      :string { :name "string.quoted.double.edn"
                :begin "\""
                :end "\"" }}))
  
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