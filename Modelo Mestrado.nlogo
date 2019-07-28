globals [ max-sheep patch-data] ; don't let sheep population grow too large ;; Sheep and wolves are both breeds of turtle.
breed [ sheepone a-sheepone ] ; sheep is its own plural, so we use "a-sheep" as the singular.
breed [ sheeptwo a-sheeptwo ] ; especie 2 de ovelha
breed [ sheepthree a-sheepthree ] ; especie 3 de ovelha
breed [ sheepfour a-sheepfour ] ; especie 4 de ovelha
breed [ wolvesone wolfone ] ; especie 1 de lobo
breed [ wolvestwo wolftwo ] ; especie 2 de lobo
breed [ wolvesthree wolfthree ]; especie 3 de lobo
breed [ wolvesfour wolffour ] ; especie 4 de lobo
turtles-own [ energy trophic-level age ] ; both wolves and sheep have energy, trophic-level and age
patches-own [ countdown cell-impacted ] ; contagem regressiva para nascimento das gramíneas

to setup ; configuração inicial do sistema
  clear-all
  ifelse netlogo-web?
   [  set max-sheep 10000  ]
   [  set max-sheep 30000  ]
  ; nascimento das gramíneas
    ask patches [
    set cell-impacted 0
    set pcolor one-of [ green gray violet sky ]
     if pcolor != brown
       [ set countdown grass-regrowth-time ]
    ]
  ask turtles [set age 0] ; initial age wolves and sheeps
  create-sheepone initial-number-sheep  ; create the first sheep, then initialize their variables
  [
    set shape "sheep"
    set color white
    set size 1.5  ; easier to see
    set energy random (2 * sheep-gain-from-food)
    setxy random-xcor random-ycor ; aparecem aleatoriamente
    set trophic-level "consumer"
  ]

  create-sheeptwo initial-number-sheep
  [
    set shape "sheep"
    set color red
    set size 1.5  ; easier to see
    set energy random (2 * sheep-gain-from-food)
    setxy random-xcor random-ycor
    set trophic-level "consumer"
  ]
   create-sheepthree initial-number-sheep
  [
    set shape "sheep"
    set color blue
    set size 1.5  ; easier to see
    set energy random (2 * sheep-gain-from-food)
    setxy random-xcor random-ycor
    set trophic-level "consumer"
  ]
   create-sheepfour initial-number-sheep
  [
    set shape "sheep"
    set color orange
    set size 1.5  ; easier to see
    set energy random (2 * sheep-gain-from-food)
    setxy random-xcor random-ycor
    set trophic-level "consumer"
  ]
  create-wolvesone initial-number-wolves  ; create the first wolves, then initialize their variables
  [
    set shape "wolf"
    set color black
    set size 1.5  ; easier to see
    set energy random (2 * wolf-gain-from-food)
    setxy random-xcor random-ycor
    set trophic-level "predator"
  ]
   create-wolvestwo initial-number-wolves
  [
    set shape "wolf"
    set color pink
    set size 1.5  ; easier to see
    set energy random (2 * wolf-gain-from-food)
    setxy random-xcor random-ycor
    set trophic-level "predator"
  ]
  create-wolvesthree initial-number-wolves
  [
    set shape "wolf"
    set color magenta
    set size 1.5  ; easier to see
    set energy random (2 * wolf-gain-from-food)
    setxy random-xcor random-ycor
    set trophic-level "predator"
  ]
  create-wolvesfour initial-number-wolves
  [
    set shape "wolf"
    set color yellow
    set size 1.5  ; easier to see
    set energy random (2 * wolf-gain-from-food)
    setxy random-xcor random-ycor
    set trophic-level "predator"
  ]
  reset-ticks


end

to go ; faz individuos se moveram e fazer as açoes criadas
  if ticks = 1100 [ impact ]
  ; stop the simulation of no wolves or sheep
  if not any? turtles [ stop ]
  ; stop the model if there are no wolves and the number of sheep gets very large
  if not any? turtles with [ shape = "wolf"] and count turtles with [shape = "sheep"] > max-sheep [ user-message "The sheeps have inherited the earth" stop ]
  ask sheepone
  [
      move
     ; sheep eat grass, grass grows and it costs sheep energy to move
      set energy energy - ( 1 + cost-plasticity-sheep)
      if energy < 10 [
      eat-grass green 1
      ]
      death ; sheep die from starvation
      reproduce-sheep  ; sheep reproduce at random rate governed by slider
  ]
  ask sheeptwo
  [
      move
     ; sheep eat grass, grass grows and it costs sheep energy to move
      set energy energy - ( 1 + cost-plasticity-sheep)
      if energy < 10 [
      eat-grass green 2
      eat-grass gray  2
      ]
      death ; sheep die from starvation
      reproduce-sheep  ; sheep reproduce at random rate governed by slider
  ]
   ask sheepthree
  [
      move
     ; sheep eat grass, grass grows and it costs sheep energy to move
      set energy energy - ( 1 + cost-plasticity-sheep)
      if energy < 10 [
      eat-grass gray   3
      eat-grass violet 3
      eat-grass sky    3
      ]
      death ; sheep die from starvation
      reproduce-sheep  ; sheep reproduce at random rate governed by slider
  ]
   ask sheepfour
  [
      move
     ; sheep eat grass, grass grows and it costs sheep energy to move
      set energy energy - ( 1 + cost-plasticity-sheep)
      if energy < 10 [
      eat-grass green  4
      eat-grass gray   4
      eat-grass violet 4
      eat-grass sky    4
      ]
      death ; sheep die from starvation
      reproduce-sheep  ; sheep reproduce at random rate governed by slider
  ]
  ask wolvesone
  [
    move
    set energy energy - ( 1 + cost-plasticity-wolf); wolves lose energy as they move
    if energy < 10 [
    eat-sheep sheepfour  1
    ] ; wolves eat a sheep on their patch
    death ; wolves die if our of energy
    reproduce-wolves ; wolves reproduce at random rate governed by slider
  ]
   ask wolvestwo
  [
    move
    set energy energy - ( 1 + cost-plasticity-wolf)  ; wolves lose energy as they move
    if (energy < 10) AND (any?  other turtles-here with [shape = "sheep"])[

    let context [0] ;;always can move
    if any? sheepthree-here [ set context lput 3 context]
    if any? sheepfour-here  [ set context lput 4 context]
    set context remove 0 context
      ifelse(empty? context)[]
       [ let x one-of context
          if (x = 3)[eat-sheep sheepthree 2]
          if (x = 4)[eat-sheep sheepfour  2]
        ]
    ] ; wolves eat a sheep on their patch
    death ; wolves die if our of energy
    reproduce-wolves ; wolves reproduce at random rate governed by slider
  ]
   ask wolvesthree
  [
    move
    set energy energy - ( 1 + cost-plasticity-wolf) ; wolves lose energy as they move
    if ( energy < 10) AND (any?  other turtles-here with [shape = "sheep"])[
    let context [0] ;;always can move
    if any? sheeptwo-here [ set context lput 2 context]
    if any? sheepthree-here [ set context lput 3 context]
    if any? sheepfour-here  [ set context lput 4 context]
    set context remove 0 context
      ifelse(empty? context)[]
       [
    let x one-of context
    if (x = 2)[eat-sheep sheeptwo 3]
    if (x = 3)[eat-sheep sheepthree 3]
    if (x = 4)[eat-sheep sheepfour 3]
      ]
    ]; wolves eat a sheep on their patch
    death ; wolves die if our of energy
    reproduce-wolves ; wolves reproduce at random rate governed by slider
  ]
   ask wolvesfour
  [
    move
    set energy energy - ( 1 + cost-plasticity-wolf)  ; wolves lose energy as they move
    if ( energy < 10) AND (any?  other turtles-here with [shape = "sheep"])[
    let context [0] ;;always can move
    if any? sheepone-here   [ set context lput 1 context]
    if any? sheeptwo-here   [ set context lput 2 context]
    if any? sheepthree-here [ set context lput 3 context]
    if any? sheepfour-here  [ set context lput 4 context]
    set context remove 0 context
      ifelse(empty? context)[]
       [
    let x one-of context
    if (x = 1)[eat-sheep sheepone   4]
    if (x = 2)[eat-sheep sheeptwo   4]
    if (x = 3)[eat-sheep sheepthree 4]
    if (x = 4)[eat-sheep sheepfour  4]
      ]
    ] ; wolves eat a sheep on their patch
    death ; wolves die if our of energy
    reproduce-wolves ; wolves reproduce at random rate governed by slider
  ]
  ask turtles [set age age + 1]
  ask patches [ grow-grass ]
  tick
end

to move  ; turtle procedure
  rt random 50
  lt random 50
  let x salto
  fd 1 + x
  set energy energy - (x / 10)
end

to eat-grass [grasstype number-of-foods] ; sheep procedure
  ; sheep eat grass, turn the patch brown
  if pcolor = grasstype
   [
    set pcolor brown
    set energy energy + (sheep-gain-from-food / number-of-foods) ; sheep gain energy by eating
    set countdown grass-regrowth-time
    ;show grasstype
   ]
end

to eat-sheep [sheeptype number-of-foods]  ; wolf procedure
  let preyone one-of turtles-here with [breed = sheeptype]                     ; grab a random sheep
  if preyone != nobody                             ; did we get one?  if so,
  [
    ask preyone [ die ]                            ; kill it, and...
    set energy energy + (wolf-gain-from-food / number-of-foods )     ; get energy from eating
    ;show sheeptype
  ]
end

to reproduce-sheep  ; sheep procedure
 if energy > 10 [ ; mudar pra 15 em outra possibilidade
  if random-float 100 < sheep-reproduce ; throw "dice" to see if you will reproduce
  [
    set energy energy - 10                ; divide energy between parent and offspring
    hatch 1 [
      set energy 5 ; mudar pra 10 em outra rodada
      rt random-float 360
      fd 1
      set age 0
    ]   ; hatch an offspring and move it forward 1 step ;;
  ]
  ]
end

to reproduce-wolves  ; wolf procedure
  if energy > 10 [ ; mudar pra 15 em outra possibilidade
  if random-float 100 < wolf-reproduce ; throw "dice" to see if you will reproduce
  [
    set energy energy - 10              ; divide energy between parent and offspring
    hatch 1 [
      set energy 5 ; mudar pra 10 em outra rodada
      rt random-float 360
      fd 1
      set age 0
    ]  ; hatch an offspring and move it forward 1 step
  ]
  ]
end

to grow-grass  ; patch procedure
  ; countdown on brown patches: if reach 0, grow some grass
  if pcolor = brown
  [
    let neighbors-not-brown count neighbors with [pcolor != brown AND pcolor != black] ;;; AQUII
    ifelse (countdown <= 0) and neighbors-not-brown != 0
      [
        let greens count neighbors with [pcolor = green]
        let grays count neighbors with [pcolor = gray]
        let violets count neighbors with [pcolor  = violet]
        let skys count neighbors with [pcolor = sky]
           ;;;;show (word "vizinhos =" count neighbors)
        let percent-greens greens / neighbors-not-brown
           ;;;;show (word "percent-greens  =" percent-greens )
        let percent-grays grays /  neighbors-not-brown
           ;;;;show (word "percent-grays greens ="percent-grays)
        let percent-violets violets /  neighbors-not-brown
           ;;;;show (word "percent-violets  =" percent-violets)
        let percent-skys skys /  neighbors-not-brown
           ;;;;show (word "percent-skys =" percent-skys)


        let g1  0 + percent-greens
        ;;show g1
        let g2  g1 + percent-grays
        ;;show g2
        let g3  g2 + percent-violets
        ;;show g3
        let g4  g3 + percent-skys ;; tem que somar um
        ;;show g4
        ;;show percent-greens + percent-grays + percent-violets + percent-skys

        let x random-float 1
        if x <= g1 [set pcolor green
          set countdown grass-regrowth-time]
        if ( x > g1) AND (x <= g2)[set pcolor gray
          set countdown grass-regrowth-time]
        if ( x > g2) AND (x <= g3)[set pcolor violet
          set countdown grass-regrowth-time]
        if ( x > g3)[set pcolor sky
          set countdown grass-regrowth-time]
    ]
      [ set countdown countdown - 1 ]
  ]
end

to death  ; turtle procedure (i.e. both wolf nd sheep procedure)
  ; when energy dips below zero, die
  if energy < 0 [ die ]
  if age > max-age [die]
end

to-report grass
  report patches with [pcolor != brown] ; contagem de gramíneas
end

to-report plastic-jump
  let ps 0
   ifelse (trophic-level = "consumer")
    [set ps random-normal sheep-plasticity 0.2 ;;; MODIFIQUEI
    ;show (word "fui plastico ovelha " ps)
  ]
    [set ps random-normal wolf-plasticity 0.2 ;;; MODIFIQUEI
    ;show (word "fui plastico lobo " ps)
  ]
  ifelse(ps < 0)[report 0.1][report ps]
end

to-report salto
  let tamanho 0
  let not-food count neighbors with [pcolor = brown OR pcolor = black]
  ;;if ([pcolor] of patch-here = brown) AND energy < 10 [
    if ((not-food >= 5) AND energy < 5) [
       set tamanho plastic-jump
  ]
  report tamanho
end

to impact

  ;LOAD-PATCH-DATA

  ; We check to make sure the file exists first
   ;ifelse ( file-exists? "C:/Users/vrios/Google Drive/projetos/nuevo/emerson/MestradoEmerson-master/Perturbacoes/habitat_destruidof03p30.txt" )
   ifelse ( file-exists? "C:/Users/emers/Dropbox/Codigos/MestradoEmerson-master/MestradoEmerson-master/MestradoEmerson/Perturbações/habitat_destruidof03p30.txt" )
  [
    ; We are saving the data into a list, so it only needs to be loaded once.
    set patch-data []

    ; This opens the file, so we can use it.
     ;file-open "C:/Users/vrios/Google Drive/projetos/nuevo/emerson/MestradoEmerson-master/Perturbacoes/habitat_destruidof03p30.txt"
   file-open "C:/Users/emers/Dropbox/Codigos/MestradoEmerson-master/MestradoEmerson-master/MestradoEmerson/Perturbações/habitat_destruidof03p30.txt"

    ; Read in all the data in the file
    while [ not file-at-end? ]
    [
      ; file-read gives you variables.  In this case numbers.
      ; We store them in a double list (ex [[1 1 9.9999] [1 2 9.9999] ...
      ; Each iteration we append the next three-tuple to the current list
      set patch-data sentence patch-data (list file-read)
    ]
    ; Done reading in patch information.  Close the file.
    file-close
  ]
  [ ];user-message "There is no File IO Patch Data.txt file in current directory!" ]

let linha 0 ;y
let coluna 0 ;x
 while[linha != (max-pycor + 1)]
    [
      while[coluna != (max-pxcor + 1)]
      [
          ask patch coluna linha [set cell-impacted item (linha + ((max-pycor + 1) * coluna) ) patch-data ]
        ;"MORE CODE"
        set coluna coluna + 1
      ]
      set coluna 0
      set linha linha + 1
    ]

 ; IMPACT

  ask patches with [cell-impacted = 1]
  [
    ask turtles-here [die]
    set pcolor black
    set countdown 100000
  ]

end

to output

  ; RELATIVE ABUNDANCE

  let abundance-turtles count turtles
  let abundance-patches count patches with [pcolor != black AND pcolor != brown] ;; AQUIII!!!
  let abundance-total abundance-turtles + abundance-patches
  let abundance-relative-patchesgreen count patches with [ pcolor = green ] / abundance-total
  let abundance-relative-patchesviolet count patches with [ pcolor = violet ] / abundance-total
  let abundance-relative-patchesgray count patches with [ pcolor = gray ] / abundance-total
  let abundance-relative-patchessky count patches with [ pcolor = sky ] / abundance-total
  let abundance-relative-sheepone count sheepone / abundance-total
  let abundance-relative-sheeptwo count sheeptwo / abundance-total
  let abundance-relative-sheepthree count sheepthree / abundance-total
  let abundance-relative-sheepfour count sheepfour / abundance-total
  let abundance-relative-wolvesone count wolvesone / abundance-total
  let abundance-relative-wolvestwo count wolvestwo / abundance-total
  let abundance-relative-wolvesthree count wolvesthree / abundance-total
  let abundance-relative-wolvesfour count wolvesfour / abundance-total

  ; RICHNESS

  let richness1 0
  ifelse any? sheepone [ set richness1 1][ set richness1 0]
  let richness2 0
  ifelse any? sheeptwo [ set richness2 1] [ set richness2 0]
  let richness3 0
  ifelse any? sheepthree [ set richness3 1] [ set richness3 0]
  let richness4 0
  ifelse any? sheepfour [ set richness4 1] [ set richness4 0]
  let richness5 0
  ifelse any? wolvesone [ set richness5 1] [ set richness5 0]
  let richness6 0
  ifelse any? wolvestwo [ set richness6 1] [ set richness6 0]
  let richness7 0
  ifelse any? wolvesthree [ set richness7 1] [ set richness7 0]
  let richness8 0
  ifelse any? wolvesfour [ set richness8 1] [ set richness8 0]
  let richness9 0
  ifelse any? patches with [pcolor = green] [ set richness9 1] [ set richness9 0]
  let richness10 0
  ifelse any? patches with [pcolor = violet] [ set richness10 1] [ set richness10 0]
  let richness11 0
  ifelse any? patches with [pcolor = gray] [ set richness11 1] [ set richness11 0]
  let richness12 0
  ifelse any? patches with [pcolor = sky] [ set richness12 1] [ set richness12 0]
  let richness richness1 + richness2 + richness3 + richness4 + richness5 + richness6 + richness7 + richness8 + richness9 + richness10 + richness11 + richness12

    ; Shannon (soma final depois de fazer para cada especie) = AbundanceRelative (OK) . LogNaturalAbundanceRelative

   let shannon 0
   if abundance-relative-patchesgreen   > 0 [ set shannon shannon + (abundance-relative-patchesgreen  * ln abundance-relative-patchesgreen )]
   if abundance-relative-patchesviolet  > 0 [ set shannon shannon + (abundance-relative-patchesviolet * ln abundance-relative-patchesviolet )]
   if abundance-relative-patchesgray    > 0 [ set shannon shannon + (abundance-relative-patchesgray   * ln abundance-relative-patchesgray)]
   if abundance-relative-patchessky     > 0 [ set shannon shannon + (abundance-relative-patchessky    * ln abundance-relative-patchessky)]
   if abundance-relative-sheepone       > 0 [ set shannon shannon + (abundance-relative-sheepone      * ln abundance-relative-sheepone)]
   if abundance-relative-sheeptwo       > 0 [ set shannon shannon + (abundance-relative-sheeptwo      * ln abundance-relative-sheeptwo )]
   if abundance-relative-sheepthree     > 0 [ set shannon shannon + (abundance-relative-sheepthree    * ln abundance-relative-sheepthree)]
   if abundance-relative-sheepfour      > 0 [ set shannon shannon + (abundance-relative-sheepfour     * ln abundance-relative-sheepfour )]
   if abundance-relative-wolvesone      > 0 [ set shannon shannon + (abundance-relative-wolvesone     * ln abundance-relative-wolvesone)]
   if abundance-relative-wolvestwo      > 0 [ set shannon shannon + (abundance-relative-wolvestwo     * ln abundance-relative-wolvestwo )]
   if abundance-relative-wolvesthree    > 0 [ set shannon shannon + (abundance-relative-wolvesthree   * ln  abundance-relative-wolvesthree)]
   if abundance-relative-wolvesfour     > 0 [ set shannon shannon + (abundance-relative-wolvesfour    * ln abundance-relative-wolvesfour) ]
   set shannon (-1 * shannon)

  ; EVENESSS (Pielou)

  let Evenness ( shannon / ln richness) ;report evenness

  ;impact report
  let sizeimp 0
  let imp count patches with [ pcolor = black ]
  let little "little"
  let big "big"
  ifelse imp < 50000 [ set sizeimp little ] [set sizeimp big]

  ;output

  let filename (word "plasticity" "s" sheep-plasticity "w" wolf-plasticity "_" "cost" "s" cost-plasticity-sheep "w" cost-plasticity-wolf "_"
                     "foodsheep" sheep-gain-from-food "_" "foodwolf" wolf-gain-from-food"_" "reproduce" "s" sheep-reproduce "w" wolf-reproduce "_"
                     "grassreg" grass-regrowth-time "_"  "_" "sizeimp" sizeimp "_" "rep" replicate-number ".csv")
  ;set-current-directory "C:/Users/vrios/Google Drive/projetos/nuevo/emerson/MestradoEmerson-master/"
  set-current-directory "C:/Users/emers/Dropbox/Codigos/MestradoEmerson-master/MestradoEmerson-master/MestradoEmerson/behavioralspace"
  if (file-exists? filename) [file-delete filename]
  file-open filename
  file-print
      (word "Richness; Shannon; Evenness; AbRelPgre; AbRelPvio; AbRelPgray; AbRelPsky; AbRelSone; AbRelStwo; AbRelSthree; AbRelSfou; AbRelWone; AbRelWtwo; AbRelWthree; AbRelWfour ")
  file-print
      (word richness ";" shannon ";" Evenness ";" abundance-relative-patchesgreen ";" abundance-relative-patchesviolet ";" abundance-relative-patchesgray ";"
            abundance-relative-patchessky ";" abundance-relative-sheepone ";" abundance-relative-sheeptwo ";" abundance-relative-sheepthree ";" abundance-relative-sheepfour ";"
            abundance-relative-wolvesone ";" abundance-relative-wolvestwo ";" abundance-relative-wolvesthree ";" abundance-relative-wolvesfour)
  file-close
end
@#$#@#$#@
GRAPHICS-WINDOW
345
10
848
514
-1
-1
4.9505
1
14
1
1
1
0
1
1
1
0
99
0
99
1
1
1
ticks
30.0

SLIDER
-5
10
169
43
initial-number-sheep
initial-number-sheep
0
300
150.0
1
1
NIL
HORIZONTAL

SLIDER
-5
80
169
113
sheep-gain-from-food
sheep-gain-from-food
0.0
100.0
10.0
1.0
1
NIL
HORIZONTAL

SLIDER
0
115
170
148
sheep-reproduce
sheep-reproduce
1.0
50.0
25.0
1.0
1
%
HORIZONTAL

SLIDER
175
10
340
43
initial-number-wolves
initial-number-wolves
0
300
150.0
1
1
NIL
HORIZONTAL

SLIDER
175
80
340
113
wolf-gain-from-food
wolf-gain-from-food
0.0
100.0
20.0
1.0
1
NIL
HORIZONTAL

SLIDER
175
115
340
148
wolf-reproduce
wolf-reproduce
0.0
50.0
5.0
1.0
1
%
HORIZONTAL

SLIDER
-5
45
170
78
grass-regrowth-time
grass-regrowth-time
0
100
10.0
1
1
NIL
HORIZONTAL

BUTTON
75
230
144
263
setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
165
230
235
263
go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

PLOT
850
245
1190
475
Turtles
time
pop
0.0
100.0
0.0
400.0
true
true
"" ""
PENS
"sheepone" 1.0 0 -1513240 true "" "plot count sheepone"
"sheeptwo" 1.0 0 -2674135 true "" "plot count sheeptwo"
"sheepthree" 1.0 0 -13345367 true "" "plot count sheepthree"
"sheepfour" 1.0 0 -955883 true "" "plot count sheepfour"
"wolvesone" 1.0 0 -16777216 true "" "plot count wolvesone"
"wolvestwo" 1.0 0 -2064490 true "" "plot count wolvestwo"
"wolvesthree" 1.0 0 -5825686 true "" "plot count wolvesthree"
"wolvesfour" 1.0 0 -1184463 true "" "plot count wolvesfour"

MONITOR
75
275
130
320
sheep
count turtles with [ shape = \"sheep\"]
3
1
11

MONITOR
135
275
190
320
wolves
count turtles with [ shape = \"wolf\"]
3
1
11

MONITOR
195
275
245
320
grass
count grass with [pcolor != black]
0
1
11

SLIDER
0
150
172
183
sheep-plasticity
sheep-plasticity
0
10
2.0
1
1
NIL
HORIZONTAL

SLIDER
175
45
340
78
max-age
max-age
0
100
100.0
1
1
NIL
HORIZONTAL

SLIDER
175
185
340
218
cost-plasticity-wolf
cost-plasticity-wolf
0
1
0.3
0.1
1
NIL
HORIZONTAL

SLIDER
175
150
340
183
wolf-plasticity
wolf-plasticity
0
10
3.0
1
1
NIL
HORIZONTAL

SLIDER
0
185
172
218
cost-plasticity-sheep
cost-plasticity-sheep
0
1
0.2
0.1
1
NIL
HORIZONTAL

PLOT
850
10
1190
240
Grasses
time
pop
0.0
100.0
0.0
3000.0
true
true
"" ""
PENS
"grass 1" 1.0 0 -10899396 true "" "plot count patches with [ pcolor = green ]"
"grass 2" 1.0 0 -8630108 true "" "plot count patches with [ pcolor = violet]"
"grass 3" 1.0 0 -7500403 true "" "plot count patches with [ pcolor = gray]"
"grass 4" 1.0 0 -13791810 true "" "plot count patches with [ pcolor = sky] "

SLIDER
25
350
197
383
replicate-number
replicate-number
0
100
1.0
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

This model explores the stability of predator-prey ecosystems. Such a system is called unstable if it tends to result in extinction for one or more species involved.  In contrast, a system is stable if it tends to maintain itself over time, despite fluctuations in population sizes.

## HOW IT WORKS

There are two main variations to this model.

In the first variation, the "sheep-wolves" version, wolves and sheep wander randomly around the landscape, while the wolves look for sheep to prey on. Each step costs the wolves energy, and they must eat sheep in order to replenish their energy - when they run out of energy they die. To allow the population to continue, each wolf or sheep has a fixed probability of reproducing at each time step. In this variation, we model the grass as "infinite" so that sheep always have enough to eat, and we don't explicitly model the eating or growing of grass. As such, sheep don't either gain or lose energy by eating or moving. This variation produces interesting population dynamics, but is ultimately unstable. This variation of the model is particularly well-suited to interacting species in a rich nutrient environment, such as two strains of bacteria in a petri dish (Gause, 1934).

The second variation, the "sheep-wolves-grass" version explictly models grass (green) in addition to wolves and sheep. The behavior of the wolves is identical to the first variation, however this time the sheep must eat grass in order to maintain their energy - when they run out of energy they die. Once grass is eaten it will only regrow after a fixed amount of time. This variation is more complex than the first, but it is generally stable. It is a closer match to the classic Lotka Volterra population oscillation models. The classic LV models though assume the populations can take on real values, but in small populations these models underestimate extinctions and agent-based models such as the ones here, provide more realistic results. (See Wilensky & Rand, 2015; chapter 4).

The construction of this model is described in two papers by Wilensky & Reisman (1998; 2006) referenced below.

## HOW TO USE IT

1. Set the model-version chooser to "sheep-wolves-grass" to include grass eating and growth in the model, or to "sheep-wolves" to only include wolves (black) and sheep (white).
2. Adjust the slider parameters (see below), or use the default settings.
3. Press the SETUP button.
4. Press the GO button to begin the simulation.
5. Look at the monitors to see the current population sizes
6. Look at the POPULATIONS plot to watch the populations fluctuate over time

Parameters:
MODEL-VERSION: Whether we model sheep wolves and grass or just sheep and wolves
INITIAL-NUMBER-SHEEP: The initial size of sheep population
INITIAL-NUMBER-WOLVES: The initial size of wolf population
SHEEP-GAIN-FROM-FOOD: The amount of energy sheep get for every grass patch eaten (Note this is not used in the sheep-wolves model version)
WOLF-GAIN-FROM-FOOD: The amount of energy wolves get for every sheep eaten
SHEEP-REPRODUCE: The probability of a sheep reproducing at each time step
WOLF-REPRODUCE: The probability of a wolf reproducing at each time step
GRASS-REGROWTH-TIME: How long it takes for grass to regrow once it is eaten (Note this is not used in the sheep-wolves model version)
SHOW-ENERGY?: Whether or not to show the energy of each animal as a number

Notes:
- one unit of energy is deducted for every step a wolf takes
- when running the sheep-wolves-grass model version, one unit of energy is deducted for every step a sheep takes

There are three monitors to show the populations of the wolves, sheep and grass and a populations plot to display the population values over time.

If there are no wolves left and too many sheep, the model run stops.

## THINGS TO NOTICE

When running the sheep-wolves model variation, watch as the sheep and wolf populations fluctuate. Notice that increases and decreases in the sizes of each population are related. In what way are they related? What eventually happens?

In the sheep-wolves-grass model variation, notice the green line added to the population plot representing fluctuations in the amount of grass. How do the sizes of the three populations appear to relate now? What is the explanation for this?

Why do you suppose that some variations of the model might be stable while others are not?

## THINGS TO TRY

Try adjusting the parameters under various settings. How sensitive is the stability of the model to the particular parameters?

Can you find any parameters that generate a stable ecosystem in the sheep-wolves model variation?

Try running the sheep-wolves-grass model variation, but setting INITIAL-NUMBER-WOLVES to 0. This gives a stable ecosystem with only sheep and grass. Why might this be stable while the variation with only sheep and wolves is not?

Notice that under stable settings, the populations tend to fluctuate at a predictable pace. Can you find any parameters that will speed this up or slow it down?

## EXTENDING THE MODEL

There are a number ways to alter the model so that it will be stable with only wolves and sheep (no grass). Some will require new elements to be coded in or existing behaviors to be changed. Can you develop such a version?

Try changing the reproduction rules -- for example, what would happen if reproduction depended on energy rather than being determined by a fixed probability?

Can you modify the model so the sheep will flock?

Can you modify the model so that wolves actively chase sheep?

## NETLOGO FEATURES

Note the use of breeds to model two different kinds of "turtles": wolves and sheep. Note the use of patches to model grass.

Note use of the ONE-OF agentset reporter to select a random sheep to be eaten by a wolf.

## RELATED MODELS

Look at Rabbits Grass Weeds for another model of interacting populations with different rules.

## CREDITS AND REFERENCES

Wilensky, U. & Reisman, K. (1998). Connected Science: Learning Biology through Constructing and Testing Computational Theories -- an Embodied Modeling Approach. International Journal of Complex Systems, M. 234, pp. 1 - 12. (The Wolf-Sheep-Predation model is a slightly extended version of the model described in the paper.)

Wilensky, U. & Reisman, K. (2006). Thinking like a Wolf, a Sheep or a Firefly: Learning Biology through Constructing and Testing Computational Theories -- an Embodied Modeling Approach. Cognition & Instruction, 24(2), pp. 171-209. http://ccl.northwestern.edu/papers/wolfsheep.pdf .

Wilensky, U., & Rand, W. (2015). An introduction to agent-based modeling: Modeling natural, social and engineered complex systems with NetLogo. Cambridge, MA: MIT Press.

Lotka, A. J. (1925). Elements of physical biology. New York: Dover.

Volterra, V. (1926, October 16). Fluctuations in the abundance of a species considered mathematically. Nature, 118, 558–560.

Gause, G. F. (1934). The struggle for existence. Baltimore: Williams & Wilkins.

## HOW TO CITE

If you mention this model or the NetLogo software in a publication, we ask that you include the citations below.

For the model itself:

* Wilensky, U. (1997).  NetLogo Wolf Sheep Predation model.  http://ccl.northwestern.edu/netlogo/models/WolfSheepPredation.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

Please cite the NetLogo software as:

* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## COPYRIGHT AND LICENSE

Copyright 1997 Uri Wilensky.

![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact Uri Wilensky at uri@northwestern.edu.

This model was created as part of the project: CONNECTED MATHEMATICS: MAKING SENSE OF COMPLEX PHENOMENA THROUGH BUILDING OBJECT-BASED PARALLEL MODELS (OBPML).  The project gratefully acknowledges the support of the National Science Foundation (Applications of Advanced Technologies Program) -- grant numbers RED #9552950 and REC #9632612.

This model was converted to NetLogo as part of the projects: PARTICIPATORY SIMULATIONS: NETWORK-BASED DESIGN FOR SYSTEMS LEARNING IN CLASSROOMS and/or INTEGRATED SIMULATION AND MODELING ENVIRONMENT. The project gratefully acknowledges the support of the National Science Foundation (REPP & ROLE programs) -- grant numbers REC #9814682 and REC-0126227. Converted from StarLogoT to NetLogo, 2000.

<!-- 1997 2000 -->
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.4
@#$#@#$#@
set model-version "sheep-wolves-grass"
set show-energy? false
setup
repeat 75 [ go ]
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experimentone" repetitions="1" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <final>output</final>
    <timeLimit steps="1000"/>
    <exitCondition>not any? turtles with [ shape = "wolf"]</exitCondition>
    <metric>count patches with [ pcolor = green ]</metric>
    <metric>count patches with [ pcolor = violet ]</metric>
    <metric>count patches with [ pcolor = gray ]</metric>
    <metric>count patches with [ pcolor = sky ]</metric>
    <metric>count turtles with [ breed = sheepone]</metric>
    <metric>count turtles with [ breed = sheeptwo]</metric>
    <metric>count turtles with [ breed = sheepthree]</metric>
    <metric>count turtles with [ breed = sheepfour]</metric>
    <metric>count turtles with [ breed = wolvesone]</metric>
    <metric>count turtles with [ breed = wolvestwo]</metric>
    <metric>count turtles with [ breed = wolvesthree]</metric>
    <metric>count turtles with [ breed = wolvesfour]</metric>
    <enumeratedValueSet variable="sheep-gain-from-food">
      <value value="10"/>
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-gain-from-food">
      <value value="20"/>
      <value value="30"/>
      <value value="40"/>
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-reproduce">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-reproduce">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="grass-regrowth-time">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="max-age">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-wolves">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-number-sheep">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-plasticity-sheep">
      <value value="0.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-plasticity-wolf">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sheep-plasticity">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="wolf-plasticity">
      <value value="3"/>
    </enumeratedValueSet>
    <steppedValueSet variable="replicate-number" first="1" step="1" last="1"/>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@
