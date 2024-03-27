# Single gloss lines are printed

    [1] "(@label) <style>.gloss__line--original {font-style:normal;font-weight:normal} .gloss__word .gloss__line:first-child {font-style:normal;font-weight:normal} .gloss__word .gloss__line--2 {font-style:normal;font-weight:normal} .gloss__word .gloss__line--3 {font-style:normal;font-weight:normal} .gloss__line--free {font-style:normal;font-weight:normal}</style><div data-gloss=\"\">\n  <p class=\"gloss__line--original\">Some source</p>\n  <p>First line</p>\n  <p>Second line</p>\n  <p class=\"gloss__line--free\">\"This is the translation\"</p>\n</div>\n"
    attr(,"class")
    [1] "knit_asis"
    attr(,"knit_meta")
    attr(,"knit_meta")[[1]]
    List of 10
     $ name      : chr "leipzig"
     $ version   : chr "0.8.0"
     $ src       :List of 1
      ..$ file: chr "leipzig"
     $ meta      : NULL
     $ script    : chr "leipzig.js"
     $ stylesheet: chr "leipzig.css"
     $ head      : NULL
     $ attachment: NULL
     $ package   : chr "glossr"
     $ all_files : logi TRUE
     - attr(*, "class")= chr "html_dependency"
    
    attr(,"knit_cacheable")
    [1] NA

---

    [1] "\\lingset{exskip=0pt,belowglpreambleskip=0pt,aboveglftskip=0pt,extraglskip=0pt,everyglpreamble=,everygla=,everyglb=,everyglc=,everyglft=}"             
    [2] "\\ex\\label{label} \\begingl \\glpreamble Some source// \\gla First line// \\glb Second line// \\glft \"This is the translation\"// \n \\endgl \\xe \n"
    attr(,"class")
    [1] "knit_asis"
    attr(,"knit_meta")
    attr(,"knit_meta")[[1]]
    $name
    [1] "expex"
    
    $options
    NULL
    
    $extra_lines
    [1] "\\let\\expexgla\\gla"                   
    [2] "\\AtBeginDocument{\\let\\gla\\expexgla}"
    
    attr(,"class")
    [1] "latex_dependency"
    
    attr(,"knit_cacheable")
    [1] NA

# Single gloss lines without source are printed

    [1] "(@label) <style>.gloss__line--original {font-style:normal;font-weight:normal} .gloss__word .gloss__line:first-child {font-style:normal;font-weight:normal} .gloss__word .gloss__line--2 {font-style:normal;font-weight:normal} .gloss__word .gloss__line--3 {font-style:normal;font-weight:normal} .gloss__line--free {font-style:normal;font-weight:normal}</style><div data-gloss=\"\">\n  <p class=\"gloss__line--original\">&#160;</p>\n  <p>First line</p>\n  <p>Second line</p>\n  <p class=\"gloss__line--free\">\"This is the translation\"</p>\n</div>\n"
    attr(,"class")
    [1] "knit_asis"
    attr(,"knit_meta")
    attr(,"knit_meta")[[1]]
    List of 10
     $ name      : chr "leipzig"
     $ version   : chr "0.8.0"
     $ src       :List of 1
      ..$ file: chr "leipzig"
     $ meta      : NULL
     $ script    : chr "leipzig.js"
     $ stylesheet: chr "leipzig.css"
     $ head      : NULL
     $ attachment: NULL
     $ package   : chr "glossr"
     $ all_files : logi TRUE
     - attr(*, "class")= chr "html_dependency"
    
    attr(,"knit_cacheable")
    [1] NA

---

    [1] "\\lingset{exskip=0pt,belowglpreambleskip=0pt,aboveglftskip=0pt,extraglskip=0pt,everyglpreamble=,everygla=,everyglb=,everyglc=,everyglft=}"
    [2] "\\ex\\label{label} \\begingl \\gla First line// \\glb Second line// \\glft \"This is the translation\"// \n \\endgl \\xe \n"              
    attr(,"class")
    [1] "knit_asis"
    attr(,"knit_meta")
    attr(,"knit_meta")[[1]]
    $name
    [1] "expex"
    
    $options
    NULL
    
    $extra_lines
    [1] "\\let\\expexgla\\gla"                   
    [2] "\\AtBeginDocument{\\let\\gla\\expexgla}"
    
    attr(,"class")
    [1] "latex_dependency"
    
    attr(,"knit_cacheable")
    [1] NA

# Series of gloss lines are printed

    [1] "(@feel-icelandic) <style>.gloss__line--original {font-style:normal;font-weight:normal} .gloss__word .gloss__line:first-child {font-style:normal;font-weight:normal} .gloss__word .gloss__line--2 {font-style:normal;font-weight:normal} .gloss__word .gloss__line--3 {font-style:normal;font-weight:normal} .gloss__line--free {font-style:normal;font-weight:normal}</style><div data-gloss=\"\">\n  <p class=\"gloss__line--original\">Einarsson 1945:170</p>\n  <p>Mér er heitt/kalt</p>\n  <p>1SG.DAT COP.1SG.PRS hot/cold.A</p>\n  <p>Icelandic</p>\n  <p class=\"gloss__line--free\">\"I am hot/cold.\"</p>\n</div>\n(@amb-spanish) <style>.gloss__line--original {font-style:normal;font-weight:normal} .gloss__word .gloss__line:first-child {font-style:normal;font-weight:normal} .gloss__word .gloss__line--2 {font-style:normal;font-weight:normal} .gloss__word .gloss__line--3 {font-style:normal;font-weight:normal} .gloss__line--free {font-style:normal;font-weight:normal}</style><div data-gloss=\"\">\n  <p class=\"gloss__line--original\">Pustet 2015:908</p>\n  <p>Hace calor/frío</p>\n  <p>make.3SG.PRS heat/cold..N.A</p>\n  <p>Spanish</p>\n  <p class=\"gloss__line--free\">\"It is hot/cold; literally: it makes heat/cold.\"</p>\n</div>\n(@feel-dutch) <style>.gloss__line--original {font-style:normal;font-weight:normal} .gloss__word .gloss__line:first-child {font-style:normal;font-weight:normal} .gloss__word .gloss__line--2 {font-style:normal;font-weight:normal} .gloss__word .gloss__line--3 {font-style:normal;font-weight:normal} .gloss__line--free {font-style:normal;font-weight:normal}</style><div data-gloss=\"\">\n  <p class=\"gloss__line--original\">Ross 1996:204</p>\n  <p>Ik heb het koud</p>\n  <p>1SG have 3SG COLD.A</p>\n  <p>Dutch</p>\n  <p class=\"gloss__line--free\">\"I am cold; literally: I have it cold.\"</p>\n</div>\n(@heartwarming-jp) <style>.gloss__line--original {font-style:normal;font-weight:normal} .gloss__word .gloss__line:first-child {font-style:normal;font-weight:normal} .gloss__word .gloss__line--2 {font-style:normal;font-weight:normal} .gloss__word .gloss__line--3 {font-style:normal;font-weight:normal} .gloss__line--free {font-style:normal;font-weight:normal}</style><div data-gloss=\"\">\n  <p class=\"gloss__line--original\">Shindo 2015:660</p>\n  <p>Kotae-nagara otousan to okaasan wa honobonoto atatakai2 mono ni tsutsum-areru kimochi ga shi-ta.</p>\n  <p>reply-while father and mother TOP heartwarming warm thing with surround-PASS feeling NOM do-PST</p>\n  <p>Japanese</p>\n  <p class=\"gloss__line--free\">\"While replying (to your question), Father and Mother felt like they were surrounded by something heart warming.\"</p>\n</div>\n(@languid-jp) <style>.gloss__line--original {font-style:normal;font-weight:normal} .gloss__word .gloss__line:first-child {font-style:normal;font-weight:normal} .gloss__word .gloss__line--2 {font-style:normal;font-weight:normal} .gloss__word .gloss__line--3 {font-style:normal;font-weight:normal} .gloss__line--free {font-style:normal;font-weight:normal}</style><div data-gloss=\"\">\n  <p class=\"gloss__line--original\">Shindo 2015:660</p>\n  <p>Ainiku sonna shumi wa nai. Tsumetai-none. Kedaru-souna koe da-tta.</p>\n  <p>unfortunately such interest TOP not.exist cold-EMPH languid-seem voice COP-PST</p>\n  <p>Japanese</p>\n  <p class=\"gloss__line--free\">\"Unfortunately I never have such an interest. You are so cold. (Her) voice sounded languid.\"</p>\n</div>\n"
    attr(,"class")
    [1] "knit_asis"
    attr(,"knit_meta")
    attr(,"knit_meta")[[1]]
    List of 10
     $ name      : chr "leipzig"
     $ version   : chr "0.8.0"
     $ src       :List of 1
      ..$ file: chr "leipzig"
     $ meta      : NULL
     $ script    : chr "leipzig.js"
     $ stylesheet: chr "leipzig.css"
     $ head      : NULL
     $ attachment: NULL
     $ package   : chr "glossr"
     $ all_files : logi TRUE
     - attr(*, "class")= chr "html_dependency"
    
    attr(,"knit_cacheable")
    [1] NA

---

    [1] "\\lingset{exskip=0pt,belowglpreambleskip=0pt,aboveglftskip=0pt,extraglskip=0pt,everyglpreamble=,everygla=,everyglb=,everyglc=,everyglft=}"                                                                                                                                                                                                                                                                                                                                                      
    [2] "\\ex\\label{feel-icelandic} \\begingl \\glpreamble Einarsson 1945:170// \\gla Mér er heitt/kalt// \\glb \\textsc{1sg.dat} \\textsc{cop.1sg.prs} hot/cold.\\textsc{a}// \\glc Icelandic// \\glft \"I am hot/cold.\"// \n \\endgl \\xe \n"                                                                                                                                                                                                                                                        
    [3] "\\ex\\label{amb-spanish} \\begingl \\glpreamble Pustet 2015:908// \\gla Hace calor/frío// \\glb make.\\textsc{3sg.prs} heat/cold.\\textsc{.n.a}// \\glc Spanish// \\glft \"It is hot/cold; literally: it makes heat/cold.\"// \n \\endgl \\xe \n"                                                                                                                                                                                                                                               
    [4] "\\ex\\label{feel-dutch} \\begingl \\glpreamble Ross 1996:204// \\gla Ik heb het koud// \\glb \\textsc{1sg} have \\textsc{3sg} \\textsc{cold.a}// \\glc Dutch// \\glft \"I am cold; literally: I have it cold.\"// \n \\endgl \\xe \n"                                                                                                                                                                                                                                                           
    [5] "\\ex\\label{heartwarming-jp} \\begingl \\glpreamble Shindo 2015:660// \\gla Kotae-nagara otousan to okaasan wa honobonoto atatakai2 mono ni tsutsum-areru kimochi ga shi-ta.// \\glb reply-while father and mother \\textsc{top} heartwarming warm thing with surround-\\textsc{pass} feeling \\textsc{nom} do-\\textsc{pst}// \\glc Japanese// \\glft \"While replying (to your question), Father and Mother felt like they were surrounded by something heart warming.\"// \n \\endgl \\xe \n"
    [6] "\\ex\\label{languid-jp} \\begingl \\glpreamble Shindo 2015:660// \\gla Ainiku sonna shumi wa nai. Tsumetai-none. Kedaru-souna koe da-tta.// \\glb unfortunately such interest \\textsc{top} not.exist cold-\\textsc{emph} languid-seem voice \\textsc{cop-pst}// \\glc Japanese// \\glft \"Unfortunately I never have such an interest. You are so cold. (Her) voice sounded languid.\"// \n \\endgl \\xe \n"                                                                                   
    attr(,"class")
    [1] "knit_asis"
    attr(,"knit_meta")
    attr(,"knit_meta")[[1]]
    $name
    [1] "expex"
    
    $options
    NULL
    
    $extra_lines
    [1] "\\let\\expexgla\\gla"                   
    [2] "\\AtBeginDocument{\\let\\gla\\expexgla}"
    
    attr(,"class")
    [1] "latex_dependency"
    
    attr(,"knit_cacheable")
    [1] NA

---

    [1] "\\lingset{exskip=0pt,belowglpreambleskip=0pt,aboveglftskip=0pt,extraglskip=0pt,everyglpreamble=,everygla=,everyglb=,everyglc=,everyglft=}"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
    [2] "\\pex \\a\\label{feel-icelandic} \\begingl \\glpreamble Einarsson 1945:170// \\gla Mér er heitt/kalt// \\glb \\textsc{1sg.dat} \\textsc{cop.1sg.prs} hot/cold.\\textsc{a}// \\glc Icelandic// \\glft \"I am hot/cold.\"// \n \\endgl \n\\a\\label{amb-spanish} \\begingl \\glpreamble Pustet 2015:908// \\gla Hace calor/frío// \\glb make.\\textsc{3sg.prs} heat/cold.\\textsc{.n.a}// \\glc Spanish// \\glft \"It is hot/cold; literally: it makes heat/cold.\"// \n \\endgl \n\\a\\label{feel-dutch} \\begingl \\glpreamble Ross 1996:204// \\gla Ik heb het koud// \\glb \\textsc{1sg} have \\textsc{3sg} \\textsc{cold.a}// \\glc Dutch// \\glft \"I am cold; literally: I have it cold.\"// \n \\endgl \n\\a\\label{heartwarming-jp} \\begingl \\glpreamble Shindo 2015:660// \\gla Kotae-nagara otousan to okaasan wa honobonoto atatakai2 mono ni tsutsum-areru kimochi ga shi-ta.// \\glb reply-while father and mother \\textsc{top} heartwarming warm thing with surround-\\textsc{pass} feeling \\textsc{nom} do-\\textsc{pst}// \\glc Japanese// \\glft \"While replying (to your question), Father and Mother felt like they were surrounded by something heart warming.\"// \n \\endgl \n\\a\\label{languid-jp} \\begingl \\glpreamble Shindo 2015:660// \\gla Ainiku sonna shumi wa nai. Tsumetai-none. Kedaru-souna koe da-tta.// \\glb unfortunately such interest \\textsc{top} not.exist cold-\\textsc{emph} languid-seem voice \\textsc{cop-pst}// \\glc Japanese// \\glft \"Unfortunately I never have such an interest. You are so cold. (Her) voice sounded languid.\"// \n \\endgl  \\xe \n"
    attr(,"class")
    [1] "knit_asis"
    attr(,"knit_meta")
    attr(,"knit_meta")[[1]]
    $name
    [1] "expex"
    
    $options
    NULL
    
    $extra_lines
    [1] "\\let\\expexgla\\gla"                   
    [2] "\\AtBeginDocument{\\let\\gla\\expexgla}"
    
    attr(,"class")
    [1] "latex_dependency"
    
    attr(,"knit_cacheable")
    [1] NA

