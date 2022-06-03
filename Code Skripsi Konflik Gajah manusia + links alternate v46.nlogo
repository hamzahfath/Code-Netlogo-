extensions [matrix rnd py dbscan]

breed[elephants elephant]
breed[people person]
breed[plants plant]

globals [
  ada-seminar?
  collective-decision?
  kolektivitas
  tanggal-seminar
  saldo-kolektif
  pengetahuan-agregat
  kesadaran-agregat
  collective-fence-cost
  global-matrix
  skema-cicilan
  titik-serangan-gajah
  patches-diserang-gajah
  konflik-bulanan
  cicilan-berlaku
  pengetahuan-of-dominance-group
  dominance-group
  member-of-dominance-group
  ;patches-tidak-diserang-gajah


]

people-own[
  group
  any-people-nearby?
  people-nearby
  mean-kedekatan
  pekerjaan
  pengetahuan
  df-pengetahuan
  df-pengetahuan-old
  kesadaran
  conservation-attitude
  decision-individual-buy-fence
  saldo
  lahan
  jumlah-lahan
  punya-lahan-ilegal?
  jumlah-lahan-ilegal
  jumlah-lahan-produktif
  jumlah-lahan-nonproduktif
  keputusan-tanam
  last-decision-made
  estimated-cost-pagar-listrik
  perceived-benefits
  bayesian-beliefs
  decision-history
  beban-premi
  lahan-saya-diserang?
  membantu-lahan-warga?
  last-tick-membantu-lahan-warga
  orang-yang-dibantu
  lahan-yang-dibantu
  attitudes-pagar-listrik
  jumlah-premi-dibayar
  lagi-nyicil
  time-adopt
  my-patch-last-reduced-damage-history
  my-patch-last-reduced-damage



]

plants-own[
  jenis-tanaman
  waktu-panen
  potential-income
  ownership
  umur-tanaman
  yields
  siklus-panen
  yields-saat-panen
  price
  sedang-panen
  kondisi-tanaman
  last-adjusted-attack-damage
  reduced-damage
  palatability



]

patches-own [
kepemilikan
lahan-produktif?
  jenis-lahan
dikunjungi-gajah?
waktu-dikunjungi-gajah
frekuensi-lahan-diserang
jumlah-warga-membantu
dipasangi-pagar-listrik?
  probabilitas-diserang
  perimeter-length
  last-reduced-damage-history
  waktu-dikunjungi-gajah-history
]

links-own [
  kedekatan
belong-to
interaction-type
jumlah-bertemu]


to setup
  ca
  reset-ticks

  create-people num-people [setxy random-xcor random-ycor
    move-to-empty-patches
    set membantu-lahan-warga? False ]              ;cari tahu proporsi pekerjaan disana berapa persen berapa persen
setup-pekerjaan-dan-lahan
  update-color
  ;pola-kolektivitas
  create-link
  if innitial-adopter = True [setup-innitial-adopter]
  ask patches [set last-reduced-damage-history [] set waktu-dikunjungi-gajah-history [] ]

  if frontier-scenario = True [setup-frontier-innitial-adopter]


end




to setup-pekerjaan-dan-lahan
  ask people [
    set pekerjaan "petani"
    set decision-individual-buy-fence false
    set perceived-benefits random-normal 0 1
    set decision-history []
    set my-patch-last-reduced-damage-history []
    set saldo random-normal 200000000 1000000
    let k random 100
    ifelse k <= proporsi-pro/kontra [set attitudes-pagar-listrik "Pro" set pengetahuan (50 + random 35)][set attitudes-pagar-listrik "Kontra" set pengetahuan (50 - random 15)]
   ; set attitudes-pagar-listrik one-of ["Pro" "Kontra"]
    set conservation-attitude one-of ["pro" "kontra"]
    ifelse conservation-attitude = "pro" [set kesadaran 60 + random 15][set kesadaran 40 - random 15]
   ; ifelse attitudes-pagar-listrik = "pro"
   ; [set pengetahuan (50 + random 25)][set pengetahuan (50 - random 35)]


  ]
  ask people with [pekerjaan = "petani"][
    let prob [0.2 0.4 0.4]
    set punya-lahan-ilegal? one-of [True False]
    set keputusan-tanam (first rnd:weighted-one-of-list [["padi" .2] ["karet" .3] ["sawit" .5] ] [[p] -> last p ])
    ask patches in-radius 2 [

      ifelse [pxcor] of self < min [pxcor] of patches + (1 - proporsi-lahan-legal) * abs( min [pxcor] of patches)
        [set jenis-lahan "illegal"][set jenis-lahan "legal"]

      set dikunjungi-gajah? False
      set probabilitas-diserang random-float 10
      set pcolor green set kepemilikan myself set dipasangi-pagar-listrik? False      ; myself refers to the caller (yang nge ask pertama kali) which is the people dgn pekerjaan petani
      let z random 10
      if z < kekayaan-agrikultural [ if not any? plants-here = True [sprout-plants 1 [
        set kondisi-tanaman 100
        set shape "plant"
        set jenis-tanaman [keputusan-tanam] of ([kepemilikan] of patch-here)
        set ownership ([kepemilikan] of patch-here)]  ] ] ; tanaman akan ngambil value dari patch yang dia pijak
    ]
   set lahan [self] of patches with [kepemilikan = myself]  ;assigning me (tanaman dengan kepemilikan myself) to lahan
    set jumlah-lahan count patches with [kepemilikan = myself]
    set jumlah-lahan-produktif count patches with [kepemilikan = myself and lahan-produktif? = True]
    set jumlah-lahan-nonproduktif count patches with [kepemilikan = myself and lahan-produktif? = False]

    ]

  ask plants with [ownership != [kepemilikan] of patch-here][die]   ;agar tidak ada tanaman yang tumpang tindih di satu lahan
update-palatability

end

to create-link
  ask people [

    create-links-with other people[set belong-to myself set jumlah-bertemu 0]

  ]
   ask links [ set kedekatan (10 + random 9) ]
end

to setup-innitial-adopter
  ask n-of (proportion-of-innitial-adopter * num-people) people [
    set decision-individual-buy-fence True
    set kesadaran 20 + random 15
    set pengetahuan 70 + random 15
    ask patches with [kepemilikan = myself][ set dipasangi-pagar-listrik? True ]
  ]


end

to setup-frontier-innitial-adopter
  ask rnd:weighted-n-of (proportion-of-innitial-adopter * num-people) people [((max [xcor] of people  - xcor) / 2 * max [xcor] of people)]
  [set decision-individual-buy-fence True
    set kesadaran 20 + random 15
    set pengetahuan 70 + random 15
    ask patches with [kepemilikan = myself][ set dipasangi-pagar-listrik? True ]
  ]
end

to go


  ;konflik gajah
konflik-gajah
;membantu-lahan-warga
  gulung-tikar

;go procedure manusia

manusia-bergerak

seminar
proses-keputusan-menanam
individual-buy-fence2
if pagar-listrik-kolektif = True [
  if collective-type = "global consensus"
  [create-clusters
    kolektif-buying-fence ]

  if collective-type = "subgrouping"
  [create-clusters2
  skema-cicil-subgrouping]
  ]
;go procedure tanaman

pertumbuhan-tanaman
hitung-potential-income-tanaman
panen-tanaman






update-color


 ;  set konflik-bulanan lput patches-diserang-gajah konflik-bulanan

tick

  if count patches with [kepemilikan != 0 and dipasangi-pagar-listrik? = True] >= 0.9 * count patches with [kepemilikan != 0]
  [stop]
end


to go-non-stop


  ;konflik gajah
konflik-gajah
;membantu-lahan-warga
  gulung-tikar

;go procedure manusia

manusia-bergerak

seminar
proses-keputusan-menanam
individual-buy-fence2
kolektif-buying-fence

;go procedure tanaman

pertumbuhan-tanaman
hitung-potential-income-tanaman
panen-tanaman






update-color

 ;  set konflik-bulanan lput patches-diserang-gajah konflik-bulanan

tick
end
to manusia-bergerak

    ask people [
    if last-tick-membantu-lahan-warga < (ticks + 1) [set membantu-lahan-warga? False]  ;variable membantu lahan orang dari prosedur membantu-lahan-warga
  set people-nearby one-of other people in-radius radius-interaksi

   ; ifelse people-nearby = nobody and any-people-nearby? = True [fd 3 rt random 120 lt random 120  set people-nearby one-of other people in-radius radius-interaksi][
    ifelse people-nearby = nobody [ set any-people-nearby?  False][ifelse people-nearby = nobody and any-people-nearby? = True [ set any-people-nearby? False ]
      [set any-people-nearby?  True]]


          if any-people-nearby? = True
    [
      move-to people-nearby
      sharing-value  ;sharing value disini
     membangkitkan-kesadaran ;membangkitkan-kesadaran disini
      fd 1 rt random 120 lt random 120


  let z [who] of self
      let x [who] of people-nearby
      ask link z x [
        if kedekatan > 200 [set kedekatan 200]


        set jumlah-bertemu jumlah-bertemu + 1
        set interaction-type one-of ["positive" "negative"]
        ifelse [attitudes-pagar-listrik] of turtle x = [attitudes-pagar-listrik] of turtle z
        [ifelse interaction-type = "Positive" [set kedekatan kedekatan + (5 / abs(kedekatan + 1))][set kedekatan kedekatan + (0.2 / abs(kedekatan + 1)) ] ]
        [ifelse interaction-type = "positive"
          [set kedekatan kedekatan + (1 / abs(kedekatan + 1))][set kedekatan kedekatan - (1 / abs(kedekatan + 1))]]
    ]
    ]
   fd 1 rt random 120 lt random 120

  ]

end


to sharing-value    ;evaluate pola pertambahan pengetahuan nya
    ;value adjuster to maksimum value and minimum value
  ask people with [attitudes-pagar-listrik = "Pro"]
  [ if pengetahuan > 200 [set pengetahuan 200]]

  ask people with [attitudes-pagar-listrik = "Kontra"]
  [ if pengetahuan <= -200 [set pengetahuan -200]]

  ask people [
    if people-nearby = nobody and any-people-nearby? = True [ set any-people-nearby? False ]]

  ask people [
    ifelse pengetahuan >= 95 or pengetahuan <= 5 [fd 0][
     if any-people-nearby? = True
    [let value [kedekatan] of link [who] of self [who] of people-nearby
   ifelse value  > 70         ;(70 = treshold kedekatan, bisa diganti)   ;statement pertama kalo deket ; statement kedua kalo ga deket
      [ifelse [attitudes-pagar-listrik ] of people-nearby = [attitudes-pagar-listrik ] of self
        [ifelse [attitudes-pagar-listrik] of self = "Pro"
          [set pengetahuan pengetahuan + (inkremen4)
              ask people-nearby [set pengetahuan pengetahuan + inkremen4]]
          [set pengetahuan pengetahuan - (inkremen5)
              ask people-nearby [set pengetahuan pengetahuan - inkremen5]]
        ]   ;statement kalau sama sama pro atau sama sama kontra

        [ifelse [attitudes-pagar-listrik] of self = "Pro"                        ;statement kalau berbeda pandangan tpai masih deket
          [set pengetahuan pengetahuan - (inkremen7)
              ask people-nearby [set pengetahuan pengetahuan + inkremen7] ]  ;kalau self nya pro dan people-nearby nya kontra
          [set pengetahuan pengetahuan + (inkremen6 )
              ask people-nearby [set pengetahuan pengetahuan - inkremen6]]                                    ;kalau self nya kontra dan people-nearby nya pro
        ]
         ]
      [ifelse [attitudes-pagar-listrik ] of people-nearby = [attitudes-pagar-listrik ] of self
        [ifelse [attitudes-pagar-listrik] of self = "Pro"
          [set pengetahuan pengetahuan + (inkremen6 )
              ask people-nearby [set pengetahuan pengetahuan + inkremen6]]
          [set pengetahuan pengetahuan - (inkremen7 )
              ask people-nearby [set pengetahuan pengetahuan - inkremen7]]
        ]

          [ifelse [attitudes-pagar-listrik] of self = "Pro"
            [set pengetahuan pengetahuan + (inkremen4)
              ask people-nearby [set pengetahuan pengetahuan - inkremen4]]
            [set pengetahuan pengetahuan - (inkremen5)
              ask people-nearby [set pengetahuan pengetahuan + inkremen5]]
          ]

  ]

  ]
 ]
]
 ; memodelkan variable social influence --> ketika orang memiliki pengaruh social yang tinggi, maka orang akan cenderung percaya

;evaluate social setting nya (kedekatan)
 ;evaluate attittudes nya dia dan orang yang paling dekat
  ; kalo kedekatan nya tinggi --> value nya akan ngikut si kedekatan tinggi
  ;kalo kedekatan nya rendah --> value nya akan berlawanan dengan si kedekatan rendah
end

to membangkitkan-kesadaran
  ask people [
    ;skenario : kedekatan
    ; skenario kesadaran tinggi dan rendah
    ;kalo deket :
         ; tinggi ketemu tinggi --> dua dua nya nambah
         ; tinggi ketemu rendah ---> kompromi yang pro akan ngurang yang kontra akan nambah
    ; kalo gadeket : case nya sama kayak deket tapi inkremen nya ga gede

    if people-nearby = nobody and any-people-nearby? = True [ set any-people-nearby? False ]

      ;set people-nearby  one-of people in-radius 8

  if any-people-nearby? = True [
    let k [kedekatan] of link [who] of self [who] of people-nearby

      ifelse kesadaran >= 95 or kesadaran <= 5 [fd 0 ] [
    ifelse k > 70
    [;kalo deket
        ifelse [conservation-attitude] of self = [conservation-attitude] of people-nearby   ; menanyakan apakah conservation attitudes nya sama aau beda
       [ ifelse [conservation-attitude] of self = "pro"
        ;kalo sama sama pro
        [set kesadaran kesadaran  + 2
          ask people-nearby [
              set kesadaran kesadaran + 2 ]
          ]
        ;kalo sama sama kontra
        [set kesadaran kesadaran  - 2
          ask people-nearby [
              set kesadaran  kesadaran  - 2]
      ]
        ]
        [ifelse [conservation-attitude] of self = "pro"
          [;kalo saya pro dan dia kontra
            set kesadaran kesadaran - 1
              ask people-nearby [set kesadaran kesadaran + 1]
          ]
          [;kalo saya kontra dia pro
            set kesadaran kesadaran + 1
              ask people-nearby [set kesadaran kesadaran - 1]
          ]
      ]
    ]

    [;kalo gadeket
         ifelse [conservation-attitude] of self = [conservation-attitude] of people-nearby   ; menanyakan apakah conservation attitudes nya sama aau beda
       [ ifelse [conservation-attitude] of self = "pro"
        ;kalo sama sama pro
        [set kesadaran kesadaran  + 1
          ask people-nearby [
              set kesadaran kesadaran + 1 ]
          ]
        ;kalo sama sama kontra
        [set kesadaran kesadaran  - 1
          ask people-nearby [
              set kesadaran  kesadaran  - 1]
      ]
        ]
        [ifelse [conservation-attitude] of self = "pro"
          [;kalo saya pro dan dia kontra
            set kesadaran kesadaran - .5
              ask people-nearby [set kesadaran kesadaran + .5]
          ]
          [;kalo saya kontra dia pro
            set kesadaran kesadaran + .5
              ask people-nearby [set kesadaran kesadaran - .5]
          ]
      ]

          ]


      ]
    ]

  ]

  ;ask people [
   ; if any? patches in-radius 20 with [dikunjungi-gajah? = True and dipasangi-pagar-listrik? = True] = True
   ; [let B  patches in-radius 20 with [dikunjungi-gajah? = True and dipasangi-pagar-listrik? = True]
   ;   ask B [count plants-here with [last-adjusted-attack-damage < 10]]
   ;   let C count B with [last-adjusted-attack-damage < 10]
   ; set kesadaran kesadaran + (2 * C)
   ; ]



end


to seminar   ; cari jurnal mengenai pengaruh penyuluhan
  ifelse ticks mod frekuensi-seminar != 0 [set ada-seminar? false]

  [if ticks != 0
    [set ada-seminar? true
  ; set tanggal-seminar  lput (ticks) tanggal-seminar
    ask people [
      let r random 10 ; kehadiran by random occurences
      if r >= 8 [
          set pengetahuan pengetahuan + pengetahuan-seminar]  ; benefit seminar

      ]

    ]
  ]


end


to pertumbuhan-tanaman
  ask plants [set umur-tanaman (umur-tanaman + 1)]
  ask plants with [jenis-tanaman = "sawit"][
    if umur-tanaman > 1 and umur-tanaman mod 365 = 0 [
      ask ownership [
        set saldo saldo - sawit-expenditure]
    ]
  ]

   ask plants with [jenis-tanaman = "karet"][
    if umur-tanaman > 1 and umur-tanaman mod 365 = 0 [
      ask ownership [
        set saldo saldo - karet-expenditure]
    ]
  ]




  ask plants with [ownership = nobody][die]
end


to hitung-potential-income-tanaman
  ask plants with [jenis-tanaman = "sawit"] [set yields  2 * umur-tanaman + 1]  ;revisit
  ask plants with [jenis-tanaman = "padi"] [set yields  umur-tanaman + 7]   ;revisit
  ask plants with [jenis-tanaman = "karet"] [set yields  (2 * umur-tanaman) + 7]   ;revisit

  ask plants with [jenis-tanaman = "sawit"] [set yields-saat-panen  800 * 5 + random 500]  ;revisit
  ask plants with [jenis-tanaman = "padi"] [set yields-saat-panen 5800 * 5 + random 100]   ;revisit
  ask plants with [jenis-tanaman = "karet"] [set yields-saat-panen  150 * 5]   ;revisit


  ask people [ set decision-history lput calculate-total-losses decision-history   ]

end

to panen-tanaman    ;setelah panen maka income direset menjadi nol; untuk mereset butuh variable sedang-panen?
  ask plants with [ownership != nobody] [
    if jenis-tanaman = "padi" [
      ifelse umur-tanaman mod siklus-panen-padi = 0 [
        if ticks != 0
        [set sedang-panen true
          set waktu-panen ticks
          set potential-income yields-saat-panen * 5000 * kondisi-tanaman * .01
          ask ownership [set saldo (saldo + [potential-income] of myself)]
          set yields 0
        die ]
      ]

      [set sedang-panen false
      set potential-income 0]
    ]

    if jenis-tanaman = "sawit" [
      ifelse umur-tanaman mod siklus-panen-sawit = 0 [
        if ticks != 0
        [set sedang-panen true
          set waktu-panen ticks
          set potential-income  yields-saat-panen * 1700 * kondisi-tanaman * .01
          ask ownership [set saldo (saldo + [potential-income] of myself)]
          set yields 0]
      ]
      [set sedang-panen false
       set potential-income 0]
    ]

    if jenis-tanaman = "karet" [
      ifelse umur-tanaman mod siklus-panen-karet = 0 [
        if ticks != 0
        [set sedang-panen true
          set waktu-panen ticks
          set potential-income yields-saat-panen * 11000 * kondisi-tanaman * .01
          ask ownership [set saldo (saldo + [potential-income] of myself)]
          set yields 0]
 ]

      [set sedang-panen false
        set potential-income 0]
    ]

  ]

 ; ask people [ set saldo (saldo + [potential-income] of plants-on lahan) ]

;  ask people [set saldo (saldo + sum ([potential-income] of plants with [ownership = myself])) ] ; solved

ask plants with [jenis-tanaman = "padi"][set palatability 3]
end

to proses-keputusan-menanam
  ask people [
    set jumlah-lahan-nonproduktif (count patches with [kepemilikan = myself and not any? plants-here = True])
    set jumlah-lahan-produktif (jumlah-lahan - jumlah-lahan-nonproduktif)
    let capital-cost-sawit sawit-expenditure
    let capital-cost-karet karet-expenditure
    let capital-cost-padi  padi-expenditure

    let decision-beli (list "sawit" "karet" "padi")


    ifelse keputusan-tanam = 0
    [; set keputusan-tanam one-of decision-beli
     ;set last-decision-made ticks
    ]          ;ketika desicion dibuat, maka akan diberi buffer beberapa ticks sebelum petani mengubah keputusan nya

    [if ticks > (holding-decision-tanam + last-decision-made)
      [set last-decision-made (ticks)
        set keputusan-tanam one-of decision-beli
        ]]

    if jumlah-lahan-produktif = 0 [set keputusan-tanam 0
     set keputusan-tanam one-of decision-beli]


    if (keputusan-tanam = "sawit" and keputusan-tanam != "karet" and keputusan-tanam != "padi")
    [   if saldo > (sawit-expenditure + cash-reserve)
      [let k patches with [kepemilikan = myself and lahan-produktif? = False and (not any? plants-here = True)]
             if jumlah-lahan-nonproduktif != 0
        [ set saldo (saldo - sawit-expenditure)
          ask one-of k [
            sprout-plants 1 [

              set jenis-tanaman "sawit" set kondisi-tanaman 100 set ownership [kepemilikan] of myself
       ask patch-here [set lahan-produktif? True]
        ]]]
              ]
          ]

    if (keputusan-tanam = "karet" and keputusan-tanam != "sawit" and keputusan-tanam != "padi")
    [   if saldo > (karet-expenditure + cash-reserve)
      [let k patches with [kepemilikan = myself and lahan-produktif? = False and (not any? plants-here = True)]
        if jumlah-lahan-nonproduktif != 0
        [set saldo (saldo - karet-expenditure)
          ask one-of k [ sprout-plants 1 [

            set jenis-tanaman "karet" set kondisi-tanaman 100 set ownership [kepemilikan] of myself
      ask patch-here [set lahan-produktif? True]
      ]] ]]
    ]

    if (keputusan-tanam = "padi" and keputusan-tanam != "karet" and keputusan-tanam != "sawit")
    [   if saldo > (padi-expenditure + cash-reserve)
      [let k patches with [kepemilikan = myself and lahan-produktif? = False and (not any? plants-here = True)]
        if jumlah-lahan-nonproduktif != 0
        [set saldo (saldo - padi-expenditure)
          ask one-of k [ sprout-plants 1 [

            set jenis-tanaman "padi" set kondisi-tanaman 100 set ownership [kepemilikan] of myself
        ask patch-here [set lahan-produktif? True]
        ]]]
                         ]]

 set jumlah-lahan-produktif count patches with [kepemilikan = myself and lahan-produktif? = True]

  ]

  update-palatability

end

to update-palatability
  ask plants with [jenis-tanaman = "padi"][set palatability 3]
  ask plants with [jenis-tanaman = "sawit"][set palatability 2]
  ask plants with [jenis-tanaman = "karet"][set palatability 1]

end


to update-color
  ask people [set jumlah-lahan-nonproduktif (count patches with [kepemilikan = myself and not any? plants-here = True])
   set jumlah-lahan count patches with [kepemilikan = myself]
    set jumlah-lahan-produktif count patches with [kepemilikan = myself and lahan-produktif? = True]
    ;set jumlah-lahan-nonproduktif count patches with [kepemilikan = myself and lahan-produktif? = False]
    set jumlah-lahan-produktif (jumlah-lahan - jumlah-lahan-nonproduktif)]
  set patches-diserang-gajah count patches with [dikunjungi-gajah? = True and waktu-dikunjungi-gajah = ticks]

    ask patches with [kepemilikan != 0] [
    ifelse not any? plants-here = True
    [set pcolor brown set lahan-produktif? False]
    [ifelse jenis-lahan = "legal" [set pcolor green  set lahan-produktif? True][set pcolor green + 7 set lahan-produktif? True]  ]
  ]

  ask patches [
    if dikunjungi-gajah? = True [set pcolor pink]
     ]

  if ticks >= 1 [
  ask patches with [kepemilikan != 0] [ ifelse waktu-dikunjungi-gajah < ticks
    [ set dikunjungi-gajah? False][set dikunjungi-gajah? True] ]

  ]


  ask plants with [jenis-tanaman = "padi"][set shape "plant" set color red set siklus-panen siklus-panen-padi set price 4200 ]
  ask plants with [jenis-tanaman = "sawit"][set shape "plant" set color cyan set siklus-panen siklus-panen-sawit set price 3000]
  ask plants with [jenis-tanaman = "karet"][set shape "plant"set color brown set siklus-panen siklus-panen-karet set price 7200]

  ask patches with [kepemilikan != 0] [if dikunjungi-gajah? = True [
    set frekuensi-lahan-diserang frekuensi-lahan-diserang + 1]
  ]

;update value conservation attitudes
  ask people [
    ifelse pengetahuan > 50 [set attitudes-pagar-listrik "Pro"][set attitudes-pagar-listrik "Kontra"]
    if attitudes-pagar-listrik = "Pro" [set color white]
    if attitudes-pagar-listrik  = "Kontra" [set color red]
  ]

  ask plants [if kondisi-tanaman <= 10 [ask patch-here [set lahan-produktif? False] die ]]

   ask patches [
    if dikunjungi-gajah? = True and kepemilikan = 0  [set pcolor black]
     ]


end

to membantu-lahan-warga

 ask people [
    ifelse count patches with [kepemilikan = myself and dikunjungi-gajah? = True] = 0
    [set lahan-saya-diserang? False][set lahan-saya-diserang? True]]


  if patches-diserang-gajah > 0
  [
    ask patches with [dikunjungi-gajah? = True and kepemilikan != 0 and dipasangi-pagar-listrik? = False][
    ask [kepemilikan] of self [
      move-to myself
      ask my-links with [kedekatan > solidaritas-kedekatan and link-length < 2 * radius-interaksi]
      [ask other-end [

        ifelse decision-individual-buy-fence = False [
          if ((membantu-lahan-warga? = False and lahan-saya-diserang? = False ))
        [
        set membantu-lahan-warga? True
          set orang-yang-dibantu [other-end] of myself
         move-to orang-yang-dibantu
          set lahan-yang-dibantu patch-here
          set last-tick-membantu-lahan-warga ticks
          ]]
          [
          set membantu-lahan-warga? True
          set orang-yang-dibantu [other-end] of myself
         move-to orang-yang-dibantu
          set lahan-yang-dibantu patch-here
          set last-tick-membantu-lahan-warga ticks
          ]
        ]
      ]
    ]
      set jumlah-warga-membantu count people-here
  ]
  ]
;perlu revisit lagi karena orang yang ingin membantu belum sampai datang ke patch nya melainkan hanya datang ke orang yang butuh bantuan saja
  ;issue kedua --> orang yang dibantu masih beda patch dengan orang yang dibantuin lahan nya


end




to konflik-gajah    ;evaluasi : gajah nya nyerang border aja, gajah nya nyerang several at a time sama perlu dicaritahu gimana caranya si gajah cuma bisa nyerang border
  ask plants [if [dikunjungi-gajah?] of patch-here = False[set last-adjusted-attack-damage 0]]
  ask plants [if [dipasangi-pagar-listrik?] of patch-here = True [set last-adjusted-attack-damage 0]]
 let rate random-normal 0 1
  if rate < rate-serangan-gajah [
  let X count patches with [lahan-produktif? = True and (any? neighbors4 with [pcolor = black or pcolor = brown ])]
  ifelse jumlah-titik-serangan-gajah >= X [set titik-serangan-gajah X][set titik-serangan-gajah jumlah-titik-serangan-gajah]

  ask rnd:weighted-n-of titik-serangan-gajah patches with [any? plants-here = True and (any? neighbors4 with [pcolor = black or pcolor = brown ])] [((max-pxcor - pxcor) / 2 * max-pxcor) * item 0 [palatability] of plants-here]
  [


      set dikunjungi-gajah? True
      set frekuensi-lahan-diserang frekuensi-lahan-diserang + 1
      set waktu-dikunjungi-gajah ticks


ask plants-here [
          let attack-severity (10 + one-of [20 50 70 90])
     ifelse dipasangi-pagar-listrik? = True
          [set last-adjusted-attack-damage 0
            serang-sebelahnya
            if kondisi-tanaman > 10 [set kondisi-tanaman (kondisi-tanaman - last-adjusted-attack-damage)]
          ]
     [ membantu-lahan-warga
      let P count people-here       ;calculating efek bantuan orang terhadap severity rate tumbuhan
      set reduced-damage efek-bantuan-orang * P
            ifelse (attack-severity - reduced-damage) > 0 [set last-adjusted-attack-damage (attack-severity - reduced-damage)][set last-adjusted-attack-damage 0]
      if kondisi-tanaman > 10 [set kondisi-tanaman (kondisi-tanaman - last-adjusted-attack-damage)]
          ]

     ]




 ;   if waktu-dikunjungi-gajah < ticks + 1
  ;    [ set dikunjungi-gajah? False]

  ]
  ]

  ask patches [ifelse any? plants-here = True
    [set last-reduced-damage-history lput item 0 [last-adjusted-attack-damage] of plants-here last-reduced-damage-history
      if dikunjungi-gajah? = True [set waktu-dikunjungi-gajah-history lput waktu-dikunjungi-gajah waktu-dikunjungi-gajah-history ]]
    [set last-reduced-damage-history lput 0 last-reduced-damage-history ]
  ]

  ask people [set my-patch-last-reduced-damage-history lput sum [last last-reduced-damage-history] of patches with [kepemilikan = myself] my-patch-last-reduced-damage-history ]
  ask people [set my-patch-last-reduced-damage sum [last last-reduced-damage-history] of patches with [kepemilikan = myself] ]
end


to serang-sebelahnya   ;revisit --> ganti yang diserang jadi lahan orang terdekat ; problem --> bukan rekursif tapi patch akan memilih 2 patch lain ;bukan 1 patch --> milih 1 --> milih 1

    if any? other patches in-radius 6 with [(pcolor = green or pcolor = 62) and kepemilikan != [kepemilikan] of myself and (any? other patches in-radius 1 with [pcolor = black or pcolor = brown])] [
      ask one-of other patches in-radius 6 with [(pcolor = green or pcolor = 62) and kepemilikan != [kepemilikan] of myself and (any? other patches in-radius 1 with [pcolor = black or pcolor = brown])]
   ; if any? other patches in-radius 4 with [pcolor = black or pcolor = brown]
    [ let attack-severity (10 + one-of [20 50 70 90])
      set dikunjungi-gajah? True
      set frekuensi-lahan-diserang frekuensi-lahan-diserang + 1
      set waktu-dikunjungi-gajah ticks

      ask plants-here
        [ ifelse [dipasangi-pagar-listrik?] of patch-here = True [set last-adjusted-attack-damage 0 serang-sebelahnya2
          if kondisi-tanaman > 10 [set kondisi-tanaman (kondisi-tanaman - last-adjusted-attack-damage)] ]
        [membantu-lahan-warga
        let P count people-here
        set reduced-damage efek-bantuan-orang * P
         ifelse (attack-severity - reduced-damage) > 0 [set last-adjusted-attack-damage (attack-severity - reduced-damage)][set last-adjusted-attack-damage 0]
      if kondisi-tanaman > 10 [set kondisi-tanaman (kondisi-tanaman - last-adjusted-attack-damage)]
        ]
      ]
    ]

    ]

end

to serang-sebelahnya2

    if any? other patches in-radius 6 with [(pcolor = green or pcolor = 62) and kepemilikan != [kepemilikan] of myself and (any? other patches in-radius 1 with [pcolor = black or pcolor = brown])] [
      ask one-of other patches in-radius 6 with [(pcolor = green or pcolor = 62) and kepemilikan != [kepemilikan] of myself and (any? other patches in-radius 1 with [pcolor = black or pcolor = brown])]
   ; if any? other patches in-radius 4 with [pcolor = black or pcolor = brown]
    [ let attack-severity (10 + one-of [20 50 70 90])
      set dikunjungi-gajah? True
      set frekuensi-lahan-diserang frekuensi-lahan-diserang + 1
      set waktu-dikunjungi-gajah ticks

      ask plants-here
        [ ifelse [dipasangi-pagar-listrik?] of patch-here = True [set last-adjusted-attack-damage 0
            if kondisi-tanaman > 10 [set kondisi-tanaman (kondisi-tanaman - last-adjusted-attack-damage) ]
          ]  ;muncul error recursion to deep
        [membantu-lahan-warga
        let P count people-here
        set reduced-damage efek-bantuan-orang * P
         ifelse (attack-severity - reduced-damage) > 0 [set last-adjusted-attack-damage (attack-severity - reduced-damage)][set last-adjusted-attack-damage 0]
      if kondisi-tanaman > 10 [set kondisi-tanaman (kondisi-tanaman - last-adjusted-attack-damage)]
        ]
      ]
    ]

    ]


end



to gulung-tikar   ;revisit
  let a sawit-expenditure
  let b padi-expenditure
  let c karet-expenditure
  ask people with [jumlah-lahan-produktif = 0 and saldo < (min (list a b c) + cash-reserve) or saldo <= 0][
 ; ask people with [people-nearby = myself][set people-nearby one-of other people]
    ask plants with [ownership = myself][die]
    ask patches with [kepemilikan = myself][set kepemilikan 0 ]
    die

    ]


 ; ask people with [jumlah-lahan-produktif = 0] [
 ;   ask people with [people-nearby = myself][set people-nearby one-of other people]
  ;  if saldo > (sawit-expenditure + cash-reserve) [
   ;   die]
  ;]

  ask patches with [kepemilikan = 0][set pcolor black]

end




to individual-buy-fence2

   ask people [
    carefully
    [set bayesian-beliefs individual-bayesian-updating2]
    [set bayesian-beliefs 0]
  ]

  ask people with [decision-individual-buy-fence = False or decision-individual-buy-fence = "ready to adopt" ][
  set estimated-cost-pagar-listrik 18900000 + (cost-pagar-listrik * assess-perimeter)

ifelse kesadaran < treshold-kesadaran
  [if pengetahuan > treshold-pengetahuan
    [if saldo > estimated-cost-pagar-listrik + cash-reserve
      [set decision-individual-buy-fence final-decision

        ]
      ]
    ]

    [ if not any? patches with [kepemilikan = self and jenis-lahan = "illegal"]
      [if pengetahuan > treshold-pengetahuan
        [if saldo > estimated-cost-pagar-listrik + cash-reserve
          [set decision-individual-buy-fence final-decision

          ]
        ]
      ]
    ]


  if decision-individual-buy-fence = True
  [
    set saldo (saldo - estimated-cost-pagar-listrik)
    set time-adopt ticks
    ask patches with [kepemilikan = myself][ set dipasangi-pagar-listrik? True ]
  ]

  ]

  ; kalo kesadaran tinggi ditanyain :
     ;apakah lahan saya legal --> kalo ga legal pasang pagar listrik

end


;to pola-kolektivitas
;let m  from-row-list: [[
 ; let [solidaritas] of people (random 100)

 ; set global-matrix (matrix:from-row-list n-values  [list [kedekatan] of links])


  ;revisit --> cara bikin matrix nya menjadi dinamis berdasarkan frekuensi bertemu
;end

to skema-cicil


  if cicilan-berlaku = True [

  ask people with [decision-individual-buy-fence = "Kolektif"] [
      set beban-premi (collective-fence-cost / (6 * count people with [decision-individual-buy-fence = "Kolektif"]) )
      set lagi-nyicil True



    if lagi-nyicil = True [
    if beban-premi < saldo + cash-reserve
    [set saldo saldo - beban-premi
    set jumlah-premi-dibayar jumlah-premi-dibayar + 1
        set saldo-kolektif saldo-kolektif + [beban-premi] of self

      ]

    ]

  ]

  ]

  if saldo-kolektif >= collective-fence-cost
  [ ask patches with [kepemilikan != 0 and jenis-lahan = "legal"][set dipasangi-pagar-listrik? True]
    set saldo-kolektif saldo-kolektif - collective-fence-cost
    set cicilan-berlaku False]

  ask people [if jumlah-premi-dibayar >= 6
    [set lagi-nyicil False
     set time-adopt ticks] ]
end

to kolektif-buying-fence




  set collective-fence-cost (sum [assess-perimeter] of people with [decision-individual-buy-fence != True] * harga-grosir + 18900000 * count people with [decision-individual-buy-fence != True])


    if pengetahuan-of-dominance-group > treshold-pengetahuan [
  set pengetahuan-agregat sum [pengetahuan] of people
  set kesadaran-agregat sum [kesadaran] of people
   ; set saldo-kolektif (sum [saldo] of people with [decision-individual-buy-fence = 0])

    if  sum [calculate-total-losses] of people  > collective-fence-cost * (1 - bayesian-updating)
  [set collective-decision? True
    set skema-cicilan True
    ask people with [decision-individual-buy-fence = False or decision-individual-buy-fence = "ready to adopt"] [set decision-individual-buy-fence "Kolektif"]
   ; set collective-fence-cost (sum [jumlah-lahan] of people with [decision-individual-buy-fence = "kolektif"] * harga-grosir)
      if any? patches with [kepemilikan != 0 and dipasangi-pagar-listrik? = False] = True
      [
      set cicilan-berlaku True

      ]
     ]


    if collective-decision? = True [skema-cicil]

  ;if saldo-kolektif  >
    ]
    ;if collective-decision? = True [ask people with [decision-individual-buy-fence = False] [set decision-individual-buy-fence "Kolektif"]]

end

to bc-analysis
;let
end

to-report early-adopter
  let time-frame (max [time-adopt] of people - min [time-adopt] of people)
  let early-adopters (list 0)
  report ([who] of people with [time-adopt > 0.3 * time-frame])


end

to-report mid-adopter
  let time-frame (max [time-adopt] of people - min [time-adopt] of people)
  report [who] of people with [time-adopt > (0.3 * time-frame) and time-adopt < (0.6 * time-frame)]
end

to-report late-adopter
  let time-frame (max [time-adopt] of people - min [time-adopt] of people)
  report [who] of people with [ time-adopt > (0.6 * time-frame)]
end



to-report inkremen4

;kalo menambah pengetahuan
  ;set df-pengetahuan-old df-pengetahuan

set df-pengetahuan df-pengetahuan + .25
  if (df-pengetahuan + 0.5) = 0 [set df-pengetahuan df-pengetahuan + 1]
  ifelse pengetahuan <= 50
  [ report (0.25 / num-people) * df-pengetahuan ]
  [ report (18 / num-people) / (df-pengetahuan + 0.5) ]

 ; if pengetahuan >= 100 or pengetahuan <= 0 [set df-pengetahuan df-pengetahuan-old]
end


to-report inkremen5
  ;kalo mengurangi pengetahuan

  ;set df-pengetahuan-old df-pengetahuan

 set df-pengetahuan df-pengetahuan - 0.25
  if (df-pengetahuan + 1) = 0 [set df-pengetahuan df-pengetahuan + 1]
  ifelse pengetahuan <= 50
  [report (0.25 / num-people) * df-pengetahuan]
  [report  (18 / num-people) / (df-pengetahuan + 1)]

 ; if pengetahuan >= 100 or pengetahuan <= 0 [set df-pengetahuan 1]
end

to-report inkremen6
  ;kalo orang nya ga deket dan menambah pengetahuan
;set df-pengetahuan-old df-pengetahuan
  if (df-pengetahuan + 1) = 0 [set df-pengetahuan df-pengetahuan + 1]
  ifelse pengetahuan <= 50
  [set df-pengetahuan df-pengetahuan + .25   report (2 / num-people)  * df-pengetahuan ]  ;setengah dari kalau kedekatan nya tinggi

  [set df-pengetahuan df-pengetahuan + .25   report (9 / num-people) / (df-pengetahuan + 1) ]  ;setengah dari kalau kedekatan nya tinggi

 ; if pengetahuan >= 100 or pengetahuan <= 0 [set df-pengetahuan 1]
end


to-report inkremen7
  ;set df-pengetahuan-old df-pengetahuan
  ;kalo orangnya ga deket dan mengurangi pengetahuan

  set df-pengetahuan df-pengetahuan - .25
  if (df-pengetahuan + 1) = 0 [set df-pengetahuan df-pengetahuan + 1]
  ifelse pengetahuan <= 50
  [report (0.125 / num-people) * df-pengetahuan]
  [report ( 9 / num-people) / (df-pengetahuan + 1)]

  ;if pengetahuan >= 100 or pengetahuan <= 0 [set df-pengetahuan 1]

end


to map-function

  show map [ x -> 1 / 8 * x ^ 2] [1 2 3 4 5 6 7 8 9]
end

to test2
  let ll []
    set ll lput [yields] of plants with [kepemilikan = myself] ll
    print ll

  let l2 []
  set l2 lput [probabilitas-diserang] of patches with [kepemilikan = myself] l2


end

to test
  let ll [1 2 3 4]
  let tempsum 0
  let tempsum_list []
  foreach ll
  [ set tempsum (tempsum + 2 )
    set tempsum_list lput tempsum tempsum_list
  ]
  print tempsum_list
end


to-report record-attacks
  ;foreach lahan
  ;[the-lahan -> ask the-lahan [show [who] of plants-here]]
  let dampak-komoditas []
  foreach lahan
  [the-lahan -> ask the-lahan [set dampak-komoditas lput [jenis-tanaman] of plants-here dampak-komoditas]

  ]
  report dampak-komoditas
end

to-report potential-loses
  ;foreach lahan
  ;[the-lahan -> ask the-lahan [show [who] of plants-here]]
  let pot-loses []
  let refined-pot-loses []
  foreach lahan
  [the-lahan -> ifelse [dikunjungi-gajah?] of the-lahan = True [

    ask the-lahan [set pot-loses lput ([yields-saat-panen * price * 0.01 * (100 - kondisi-tanaman)] of plants-here ) pot-loses ] ]


    [ ask the-lahan [ifelse any? plants-here  = True
      [ask the-lahan [set pot-loses lput ([yields-saat-panen * price * 0.01 * (100 - kondisi-tanaman)] of plants-here ) pot-loses]   ]
      [ask the-lahan [set pot-loses lput [40000000] pot-loses]]]

    ]
  ]
  ;report pot-loses

  foreach pot-loses
  [the-pot-loses -> foreach the-pot-loses [ items -> set refined-pot-loses lput items refined-pot-loses]]

  report refined-pot-loses

end

to-report calculate-total-losses
  let pot-loses potential-loses
  let total-pot-loses sum pot-loses
  report total-pot-loses
end

to-report historical-total-losses
  let history []
  set history fput calculate-total-losses history
  report history
end

to-report final-decision
  let r perceived-benefits
  if innitial-adopter = True or frontier-scenario = True [set r bayesian-beliefs]
  ifelse (1 + r) * calculate-total-losses >  estimated-cost-pagar-listrik / 1 [report True][report "ready to adopt"]

  ;+ (1 - 0.89) * calculate-total-losses
end

to-report final-decision-without-utility
  let r bayesian-beliefs
  ifelse r * calculate-total-losses >  estimated-cost-pagar-listrik  [report True][report "ready to adopt"]

  ;+ (1 - 0.89) * calculate-total-losses
end

to move-to-empty-patches   ;; people innitiation procedure
 ; move-to one-of locations
  while [any? other turtles in-radius 4] [
    move-to one-of patches
  ]
end

to-report bayesian-updating
  let k 10
  let c_fence  count patches with [dipasangi-pagar-listrik? = True]
  let p_fence_low_attack  count patches with [any? plants-here and dipasangi-pagar-listrik? = True and item 0 [last-adjusted-attack-damage] of plants-here < k ]
  let p_noFence_low_attack count patches with [any? plants-here and dipasangi-pagar-listrik? = False and item 0 [last-adjusted-attack-damage] of plants-here < k ]

  report p_fence_low_attack / ( p_noFence_low_attack + p_fence_low_attack)
end

to-report individual-bayesian-updating

  let x my-out-links

  carefully [
    set x max-n-of 5 my-out-links [kedekatan] ]   ; evaluasi make proporsi persen, atau make treshold
  [set x my-out-links]


  let z []
  ask x [set z lput other-end z  ]

  let p turtle-set z
  ;foreach x [ the-x -> report the-x]

  let k 10
  let c_fence  count patches with [dipasangi-pagar-listrik? = True and kepemilikan = one-of p]
  let p_fence_low_attack  count patches with [ kepemilikan = one-of p and any? plants-here and dipasangi-pagar-listrik? = True and item 0 [last-adjusted-attack-damage] of plants-here < k ]
  let p_noFence_low_attack count patches with [kepemilikan = one-of p and any? plants-here and dipasangi-pagar-listrik? = False and item 0 [last-adjusted-attack-damage] of plants-here < k ]


   report p_fence_low_attack / ( p_noFence_low_attack + p_fence_low_attack)
end

;  carefully [ let w p_fence_low_attack / ( p_noFence_low_attack + p_fence_low_attack) ]
 ; [let w 0]

  to-report individual-bayesian-updating2

  let x my-out-links

  carefully [
    set x max-n-of 5 my-out-links [kedekatan] ]   ; evaluasi make proporsi persen, atau make treshold
  [set x my-out-links]


  let z []
  ask x [set z lput other-end z  ]

  let p turtle-set z
  ;foreach x [ the-x -> report the-x]

  let k 10
  let c_fence  count patches with [dipasangi-pagar-listrik? = True and kepemilikan = one-of p]
  let p_fence_low_attack  count patches with [ kepemilikan = one-of p and any? plants-here and dipasangi-pagar-listrik? = True and item 0 [last-adjusted-attack-damage] of plants-here < k ]
  let p_noFence_low_attack count patches with [kepemilikan = one-of p and any? plants-here and dipasangi-pagar-listrik? = False and item 0 [last-adjusted-attack-damage] of plants-here < k ]


  report (p_fence_low_attack / count patches with [dipasangi-pagar-listrik? = True] )
  / ( p_noFence_low_attack / count patches with [dipasangi-pagar-listrik? = False] + p_fence_low_attack / count patches with [dipasangi-pagar-listrik? = True])


end


to-report agregat-konflik-bulanan

 let k  konflik-bulanan


end

to-report assess-perimeter  ;;; must be called be a turtle

  let border-patches patches with [ kepemilikan = myself and any? neighbors4 with [ kepemilikan != myself ] ]
  let xmax max [pxcor] of border-patches
  let xmin min [pxcor] of border-patches
    let ymax max [pycor] of border-patches
      let ymin min [pycor] of border-patches


  report 2 * 5 * (xmax - xmin) + 2 * 5 * (ymax - ymin)
end


to-report report-matrix
  let n-cr count people
  let new-mat matrix:make-constant n-cr n-cr -1
  ask links [
    let from-t [who] of end1
    let to-t [who] of end2
    matrix:set new-mat from-t to-t kedekatan ]
report new-mat
end

to-report damage

  report list my-patch-last-reduced-damage people
end


to-report network-strength
  let temp []
  foreach (list links) [the-link -> ask the-link
    [set temp lput (list [pengetahuan ] of end1 [pengetahuan] of end2 kedekatan) temp
    ]]
  report temp
end

to-report calculate-influence
let temp network-strength


  foreach temp [the-temp ->  show abs (item 1 the-temp - item 0 the-temp) * item 2 the-temp]

end


to create-clusters
  ;set rata-rata kedekatan
  if ticks mod 10 = 0 [




  carefully [
  let ctr 1
  let clusters dbscan:cluster-by-variable people with [decision-individual-buy-fence = False or decision-individual-buy-fence = "ready to adopt"] "pengetahuan" 3 1
  foreach clusters
  [ x -> let aset people with [member? self x] ask aset [set group ctr  ]
  set ctr (ctr + 1)]

      ask people [set mean-kedekatan sum [kedekatan] of my-out-links with [[group] of end2 != [group] of myself]]
  let k []
  let r []
  foreach (range 1 (max [group] of people + 1))
  [xk -> set k lput mean [pengetahuan] of people with [group = xk ] k
    set r lput mean [mean-kedekatan] of people with [group = xk] r
  ]

  let max-kedekatan max r
  set dominance-group (position max-kedekatan r) + 1
  set pengetahuan-of-dominance-group item (dominance-group - 1) k



  ][ask people [set group 0]]

    set member-of-dominance-group  count people with [group = dominance-group]
  ]

;let ctr 1
;(foreach clusters (n-of (length clusters) base-colors)
 ; [ [ x y ] -> let aset turtles with [ member? self x ]
  ;  ask aset
   ;   [ set color y
    ;    set label (word "ID: " who ", Cluster: " ctr ", pengetahuan: " pengetahuan) ]
    ; Print agent cluster sets
   ; output-print (word "Cluster " ctr ": " aset)
   ; set ctr (ctr + 1) ])
end


to create-clusters2


  ;set rata-rata kedekatan

  ;if pengetahuan-agregat > num-people * treshold-pengetahuan and sum [calculate-total-losses] of people  > collective-fence-cost
  if ticks mod 10 = 0 [
    ask people [set mean-kedekatan sum [kedekatan] of my-out-links  ]

  carefully [
  let ctr 1
  let clusters dbscan:cluster-by-variable people with [decision-individual-buy-fence = False or decision-individual-buy-fence = "ready to adopt"] "pengetahuan" 2 1
  foreach clusters
  [ x -> let aset people with [member? self x] ask aset [set group ctr ]
  set ctr (ctr + 1)]


  foreach (range 1 (max [group] of people + 1))
      [xk -> if mean [pengetahuan] of people with [group = xk ] > treshold-pengetahuan [ask people with [group = xk]
        [
        let group-collective-fence-cost (sum [assess-perimeter] of people with [group = xk] * harga-grosir + 18900000 * count people with [group = xk])

        if sum [calculate-total-losses] of people with [group = xk] > (group-collective-fence-cost) / 6
          [set decision-individual-buy-fence  "Kolektif"
           set beban-premi (group-collective-fence-cost /( 6 * count people with [group = xk]))
           set lagi-nyicil True ]
        ]
        ]

  ]


  set dominance-group "NA"
  set pengetahuan-of-dominance-group "NA"



    ][]

    set member-of-dominance-group  count people with [group = dominance-group]
  ]

;let ctr 1
;(foreach clusters (n-of (length clusters) base-colors)
 ; [ [ x y ] -> let aset turtles with [ member? self x ]
  ;  ask aset
   ;   [ set color y
    ;    set label (word "ID: " who ", Cluster: " ctr ", pengetahuan: " pengetahuan) ]
    ; Print agent cluster sets
   ; output-print (word "Cluster " ctr ": " aset)
   ; set ctr (ctr + 1) ])
end


to skema-cicil-subgrouping

  ask people with [decision-individual-buy-fence = "Kolektif"]
  [if jumlah-premi-dibayar >= 6
    [
      set lagi-nyicil False
     set time-adopt ticks] ]



ask people with [decision-individual-buy-fence = "Kolektif"] [
    ask patches with [kepemilikan = myself] [set dipasangi-pagar-listrik? True]
    if lagi-nyicil = True [
    if beban-premi < saldo + cash-reserve
    [set saldo saldo - beban-premi
    set jumlah-premi-dibayar jumlah-premi-dibayar + 1]

    ]
  ]


end

;pola kolektivitas --> ada variable list yang ngestore orang orang yang memiliki kedekatan dengan diri nya; kedekatan itu berlangsung akibat sering nya melakukan interaksi yang bersifat positif
;untuk itu perlu ada matrix yang nyimpen informasi jumlah interaksi dari masing masing individu yang kemudian diranking untuk setiap individu dan akhirnya yang memenuhi treshold kedekatan akan
;distore ke variable kedekatan
;kemudian semakin banyak orang di dalam list kedekatan, maka semakin besar chance dia ngusir gajah


;step by step : membuktikan perintah gajah/orang --> conform dengan coding
;next di ODD
;berita tentang konflik
;trigger --> random value
;probabilitas yang dinamis berdasarkan waktu


;Karakter orang :
;1. orang yang termarginalkan --> gapunya relasi dan ga punya uang
;2. orang yang memiliki relasi kuat
;3. orang yang memiliki uang banyak relasi dikit

;lahan diasumsikan sama dulu --> setiap serangan akan belum tentu rusak total

;decision : ganti komoditas, ngusir, atau pasang pagar listrik


;kesadaran dapat diatur dengan memilih orang paling dekat 3 tertinggi, lalu semakin banyak people disekitarnya yang diserang; maka kesadaran dalam memasang semakin tinggi





;orang konservasi --> the best character
@#$#@#$#@
GRAPHICS-WINDOW
271
10
708
448
-1
-1
13.0
1
10
1
1
1
0
0
0
1
-16
16
-16
16
1
1
1
ticks
30.0

SLIDER
12
22
184
55
num-people
num-people
0
100
18.0
1
1
NIL
HORIZONTAL

BUTTON
722
11
788
44
NIL
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
796
12
859
45
NIL
go\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
727
168
899
201
frekuensi-seminar
frekuensi-seminar
0
100
30.0
1
1
NIL
HORIZONTAL

SLIDER
12
57
184
90
radius-interaksi
radius-interaksi
0
100
5.0
1
1
NIL
HORIZONTAL

SLIDER
723
69
898
102
treshold-kesadaran
treshold-kesadaran
0
100
90.0
1
1
NIL
HORIZONTAL

SLIDER
725
131
915
164
pengetahuan-seminar
pengetahuan-seminar
0
100
0.0
1
1
NIL
HORIZONTAL

SLIDER
27
847
199
880
siklus-panen-padi
siklus-panen-padi
0
100
90.0
1
1
NIL
HORIZONTAL

SLIDER
27
881
199
914
siklus-panen-sawit
siklus-panen-sawit
0
100
30.0
1
1
NIL
HORIZONTAL

SLIDER
27
916
199
949
siklus-panen-karet
siklus-panen-karet
0
100
7.0
1
1
NIL
HORIZONTAL

SLIDER
25
744
223
777
sawit-expenditure
sawit-expenditure
0
50000000
4.85E7
100000
1
NIL
HORIZONTAL

SLIDER
26
777
241
810
karet-expenditure
karet-expenditure
0
100000000
7.5E7
100000
1
NIL
HORIZONTAL

SLIDER
26
810
255
843
padi-expenditure
padi-expenditure
0
900000000
9.0E7
100000
1
Rp
HORIZONTAL

SLIDER
727
224
907
257
kekayaan-agrikultural
kekayaan-agrikultural
0
10
10.0
1
1
NIL
HORIZONTAL

SLIDER
25
710
197
743
cash-reserve
cash-reserve
0
5000000
2000000.0
2000000
1
NIL
HORIZONTAL

BUTTON
865
11
965
44
go-forever
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
729
412
1123
562
plot 1
hari
jumlah lahan
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Padi" 1.0 0 -2674135 true "" "plot count plants with [jenis-tanaman = \"padi\"]"
"Sawit" 1.0 0 -13791810 true "" "plot count plants with [jenis-tanaman = \"sawit\"]"
"Karet" 1.0 0 -13493215 true "" "plot count plants with [jenis-tanaman = \"karet\"]"
"lahan produktif" 1.0 0 -7500403 true "" "plot count patches with [lahan-produktif? = True]"
"lahan warga" 1.0 0 -955883 true "" "plot count patches with [kepemilikan != 0]"
"pagar listrik" 1.0 0 -6459832 true "" "plot count patches with [dipasangi-pagar-listrik? = True]"

SLIDER
25
676
221
709
holding-decision-tanam
holding-decision-tanam
0
1000
1000.0
50
1
NIL
HORIZONTAL

SLIDER
9
332
238
365
jumlah-titik-serangan-gajah
jumlah-titik-serangan-gajah
0
100
2.0
1
1
NIL
HORIZONTAL

SLIDER
972
12
1176
45
cost-pagar-listrik
cost-pagar-listrik
0
30000000
100.0
100000
1
NIL
HORIZONTAL

SLIDER
972
48
1187
81
harga-grosir
harga-grosir
0
10000000
800000.0
100000
1
NIL
HORIZONTAL

SLIDER
13
128
200
161
solidaritas-kedekatan
solidaritas-kedekatan
0
500
10.0
10
1
NIL
HORIZONTAL

SLIDER
14
163
194
196
efek-bantuan-orang
efek-bantuan-orang
0
100
4.0
1
1
NIL
HORIZONTAL

PLOT
1144
566
1344
716
jumlah lahan terpasang pagar listrik
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"orang pasang " 1.0 0 -3844592 true "" "plot count people with [decision-individual-buy-fence = True]"
"ready to adopt" 1.0 0 -14730904 true "" "plot count people with [decision-individual-buy-fence = \"ready to adopt\" or decision-individual-buy-fence = True ]"
"ga mampu beli" 1.0 0 -7500403 true "" "plot count people with [decision-individual-buy-fence = \"ready to adopt\" and saldo + cash-reserve < estimated-cost-pagar-listrik]"
"Kolektif" 1.0 0 -2674135 true "" "plot count people with [decision-individual-buy-fence = \"Kolektif\"]"

PLOT
291
455
491
605
agregat kepintaran/ketidakpintaran
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"kesadaran baik" 1.0 0 -16777216 true "" "plot count people with [pengetahuan > 50]"
"kesadaran kurang baik" 1.0 0 -2674135 true "" "plot count people with [pengetahuan <= 50]"
"agregat pengetahuan pro" 1.0 0 -15582384 true "" "plot sum [pengetahuan] of people with [attitudes-pagar-listrik = \"Pro\"]"
"agregat pengetahuan kontra" 1.0 0 -7171555 true "" "plot abs (200 * num-people - sum [pengetahuan] of people with [attitudes-pagar-listrik = \"Kontra\"])"

MONITOR
14
424
158
469
pengetahuan agregat
sum [pengetahuan] of people
17
1
11

SLIDER
726
101
949
134
treshold-pengetahuan
treshold-pengetahuan
0
100
50.0
1
1
NIL
HORIZONTAL

SLIDER
15
201
199
234
proporsi-pro/kontra
proporsi-pro/kontra
0
100
20.0
1
1
NIL
HORIZONTAL

MONITOR
17
513
151
558
rata-rata kedekatan
mean [kedekatan] of links
17
1
11

PLOT
502
456
702
606
kedekatan agregat
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot sum [kedekatan] of links "

MONITOR
18
561
145
606
agregat kesadaran
sum [kesadaran] of people
17
1
11

PLOT
730
567
930
717
crop raiding
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count patches with [dikunjungi-gajah? = True]"
"pen-1" 1.0 0 -8275240 true "" "plot patches-diserang-gajah"

SWITCH
989
96
1175
129
pagar-listrik-kolektif
pagar-listrik-kolektif
1
1
-1000

MONITOR
1193
517
1251
562
kolektif
count people with [decision-individual-buy-fence = \"Kolektif\"]
17
1
11

MONITOR
1258
516
1321
561
individu
count people with [decision-individual-buy-fence = True]
17
1
11

MONITOR
1259
466
1316
511
lahan
count patches with [kepemilikan != 0]
17
1
11

PLOT
933
568
1133
718
income 
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"fence" 1.0 0 -12345184 true "plot 0" "plot sum [saldo] of people with [decision-individual-buy-fence = True or decision-individual-buy-fence = \"Kolektif\"] / (count people with [decision-individual-buy-fence = True or decision-individual-buy-fence = \"Kolektif\"] + 1) "
"nofence" 1.0 0 -5298144 true "plot 0" "plot sum [saldo] of people with [decision-individual-buy-fence = False or decision-individual-buy-fence = \"ready to adopt\"] / (count people with [decision-individual-buy-fence = False or decision-individual-buy-fence = \"ready to adopt\"] + 1)"
"min no fence" 1.0 0 -1184463 true "" "plot min ([saldo] of people with [decision-individual-buy-fence = False or decision-individual-buy-fence = \"ready to adopt\"]) "

MONITOR
1189
417
1316
462
lahan-pagar listrik
count patches with [dipasangi-pagar-listrik? = True]
17
1
11

MONITOR
14
378
166
423
rata-rata pengetahuan
mean [pengetahuan] of people
17
1
11

PLOT
287
613
487
763
jumlah orang paham/tidak akan produk
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"Kontra" 1.0 0 -16777216 true "" "plot count people with [attitudes-pagar-listrik = \"Kontra\"]"
"Pro" 1.0 0 -2674135 true "" "plot count people with [attitudes-pagar-listrik = \"Pro\"]"

MONITOR
14
467
131
512
std pengetahuan
sqrt (variance [pengetahuan] of people)
17
1
11

SLIDER
733
261
917
294
proporsi-lahan-legal
proporsi-lahan-legal
0
1
1.0
0.001
1
NIL
HORIZONTAL

PLOT
502
613
702
763
kesadaran
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot sum [kesadaran] of people"

SWITCH
989
136
1143
169
innitial-adopter
innitial-adopter
0
1
-1000

SLIDER
940
170
1196
203
proportion-of-innitial-adopter
proportion-of-innitial-adopter
0
1
0.1
.001
1
NIL
HORIZONTAL

MONITOR
1188
367
1316
412
manusia-bangkrut
num-people - count people
17
1
11

MONITOR
1187
318
1316
363
bayesian
bayesian-updating
17
1
11

MONITOR
1186
269
1280
314
agg bayesian
mean [bayesian-beliefs] of people
17
1
11

SLIDER
9
298
221
331
rate-serangan-gajah
rate-serangan-gajah
0
5
0.0
0.01
1
NIL
HORIZONTAL

MONITOR
1186
221
1276
266
std bayesian
standard-deviation [bayesian-beliefs] of people
17
1
11

MONITOR
1216
172
1298
217
membantu 
count people with [membantu-lahan-warga? = True]
17
1
11

PLOT
710
746
956
896
dampak tanaman
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"all" 1.0 0 -7500403 true "" "plot sum [ last last-reduced-damage-history] of patches with [kepemilikan != 0] "
"falsee" 1.0 0 -2674135 true "" "plot sum [my-patch-last-reduced-damage] of people with [decision-individual-buy-fence = False ]"
"truee" 1.0 0 -955883 true "" "plot sum [my-patch-last-reduced-damage] of people with [decision-individual-buy-fence = True ] "
"ready to adopt" 1.0 0 -6459832 true "" "plot sum [my-patch-last-reduced-damage] of people with [decision-individual-buy-fence = \"ready to adopt\" ] "

MONITOR
18
609
131
654
mean kesadaran
mean [kesadaran] of people
17
1
11

PLOT
963
744
1163
894
dampak finansial
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [last-adjusted-attack-damage * price * yields-saat-panen] of plants"
"sawit" 1.0 0 -12345184 true "" "plot mean [last-adjusted-attack-damage * price * yields-saat-panen] of plants with [jenis-tanaman = \"sawit\"]"
"padi" 1.0 0 -2674135 true "" "plot mean [last-adjusted-attack-damage * price * yields-saat-panen] of plants with [jenis-tanaman = \"padi\"]"
"karet" 1.0 0 -8431303 true "" "plot mean [last-adjusted-attack-damage * price * yields-saat-panen] of plants with [jenis-tanaman = \"karet\"]"

SWITCH
993
214
1157
247
frontier-scenario
frontier-scenario
1
1
-1000

PLOT
1167
744
1367
894
dampak terhadap tanaman
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -12345184 true "" "plot mean [last-adjusted-attack-damage] of plants with [jenis-tanaman = \"sawit\"]"
"pen-1" 1.0 0 -2674135 true "" "plot mean [last-adjusted-attack-damage] of plants with [jenis-tanaman = \"padi\"]"
"pen-2" 1.0 0 -8431303 true "" "plot mean [last-adjusted-attack-damage] of plants with [jenis-tanaman = \"karet\"]"

PLOT
289
769
489
919
dampak per orang
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot sum [last last-reduced-damage-history] of patches with [kepemilikan != 0]    "
"pen-1" 1.0 0 -7500403 true "" "plot sum [ calculate-total-losses] of people with [decision-individual-buy-fence = False or decision-individual-buy-fence = \"ready to adopt\" ]"

BUTTON
203
12
266
45
go2
go-non-stop
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
989
254
1146
299
collective-type
collective-type
"global consensus" "subgrouping"
1

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
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
NetLogo 6.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="pagar-listrik dan serangan gajah" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>count patches with [dikunjungi-gajah? = True]</metric>
    <metric>count patches with [dipasangi-pagar-listrik? = True]</metric>
    <enumeratedValueSet variable="sawit-expenditure">
      <value value="10000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="jumlah-titik-serangan-gajah">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pengetahuan-seminar">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kekayaan-agrikultural">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-pro/kontra">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-kesadaran">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="holding-decision-tanam">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="padi-expenditure">
      <value value="13000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efek-bantuan-orang">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="karet-expenditure">
      <value value="10000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-padi">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frontier-scenario">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rate-serangan-gajah">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pagar-listrik-kolektif">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cash-reserve">
      <value value="2000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harga-grosir">
      <value value="1800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-lahan-legal">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="solidaritas-kedekatan">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="innitial-adopter">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proportion-of-innitial-adopter">
      <value value="0.207"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-pagar-listrik">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bisa-disamper">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-karet">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-sawit">
      <value value="34"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frekuensi-seminar">
      <value value="32"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-pengetahuan">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-people">
      <value value="18"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="kerugian rata rata terhadap tanaman" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>mean [last-adjusted-attack-damage * price * yields-saat-panen] of plants with [jenis-tanaman = "sawit"]</metric>
    <metric>mean [last-adjusted-attack-damage * price * yields-saat-panen] of plants with [jenis-tanaman = "padi"]</metric>
    <metric>mean [last-adjusted-attack-damage * price * yields-saat-panen] of plants with [jenis-tanaman = "karet"]</metric>
    <enumeratedValueSet variable="sawit-expenditure">
      <value value="10000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="jumlah-titik-serangan-gajah">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pengetahuan-seminar">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kekayaan-agrikultural">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-pro/kontra">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-kesadaran">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="holding-decision-tanam">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="padi-expenditure">
      <value value="13000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efek-bantuan-orang">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="karet-expenditure">
      <value value="10000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-padi">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frontier-scenario">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rate-serangan-gajah">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pagar-listrik-kolektif">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cash-reserve">
      <value value="2000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harga-grosir">
      <value value="1800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-lahan-legal">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="solidaritas-kedekatan">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="innitial-adopter">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proportion-of-innitial-adopter">
      <value value="0.207"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-pagar-listrik">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bisa-disamper">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-karet">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-sawit">
      <value value="34"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frekuensi-seminar">
      <value value="32"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-pengetahuan">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-people">
      <value value="18"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="mean yield loss per plant" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>mean [last-adjusted-attack-damage] of plants with [jenis-tanaman = "sawit"]</metric>
    <metric>mean [last-adjusted-attack-damage] of plants with [jenis-tanaman = "padi"]</metric>
    <metric>mean [last-adjusted-attack-damage] of plants with [jenis-tanaman = "karet"]</metric>
    <enumeratedValueSet variable="sawit-expenditure">
      <value value="10000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-people">
      <value value="18"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pengetahuan-seminar">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kekayaan-agrikultural">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-pro/kontra">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-kesadaran">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="holding-decision-tanam">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="padi-expenditure">
      <value value="13000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efek-bantuan-orang">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="karet-expenditure">
      <value value="10000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-padi">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frontier-scenario">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rate-serangan-gajah">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pagar-listrik-kolektif">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cash-reserve">
      <value value="2000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harga-grosir">
      <value value="1800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-lahan-legal">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="solidaritas-kedekatan">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="innitial-adopter">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proportion-of-innitial-adopter">
      <value value="0.207"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-pagar-listrik">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bisa-disamper">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-karet">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-sawit">
      <value value="34"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frekuensi-seminar">
      <value value="32"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-pengetahuan">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="jumlah-titik-serangan-gajah">
      <value value="2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="pagar-listrik dan loss" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>count people with [decision-individual-buy-fence = True]</metric>
    <metric>sum [last-adjusted-attack-damage] of plants</metric>
    <enumeratedValueSet variable="sawit-expenditure">
      <value value="10000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-people">
      <value value="18"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pengetahuan-seminar">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kekayaan-agrikultural">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-pro/kontra">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-kesadaran">
      <value value="80"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="holding-decision-tanam">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="padi-expenditure">
      <value value="13000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efek-bantuan-orang">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="karet-expenditure">
      <value value="10000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-padi">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frontier-scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rate-serangan-gajah">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pagar-listrik-kolektif">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cash-reserve">
      <value value="2000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harga-grosir">
      <value value="1800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-lahan-legal">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="solidaritas-kedekatan">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="innitial-adopter">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proportion-of-innitial-adopter">
      <value value="0.207"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-pagar-listrik">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bisa-disamper">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-karet">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-sawit">
      <value value="34"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frekuensi-seminar">
      <value value="32"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-pengetahuan">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="jumlah-titik-serangan-gajah">
      <value value="2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="scenario collective adopt" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>count people with [decision-individual-buy-fence = True or decision-individual-buy-fence = "Kolektif"]</metric>
    <enumeratedValueSet variable="sawit-expenditure">
      <value value="10000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-people">
      <value value="18"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pengetahuan-seminar">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-pro/kontra">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kekayaan-agrikultural">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-kesadaran">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="holding-decision-tanam">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="padi-expenditure">
      <value value="13000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efek-bantuan-orang">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="karet-expenditure">
      <value value="10000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-padi">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frontier-scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rate-serangan-gajah">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pagar-listrik-kolektif">
      <value value="false"/>
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cash-reserve">
      <value value="2000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harga-grosir">
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-lahan-legal">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="solidaritas-kedekatan">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="innitial-adopter">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proportion-of-innitial-adopter">
      <value value="0.204"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-pagar-listrik">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bisa-disamper">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-karet">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-sawit">
      <value value="34"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frekuensi-seminar">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-pengetahuan">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="jumlah-titik-serangan-gajah">
      <value value="2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="SA proportion-innitial-adopter" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>count people with [decision-individual-buy-fence = True]</metric>
    <enumeratedValueSet variable="jumlah-titik-serangan-gajah">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="sawit-expenditure">
      <value value="10000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pengetahuan-seminar">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-pro/kontra">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kekayaan-agrikultural">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-kesadaran">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="holding-decision-tanam">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="padi-expenditure">
      <value value="13000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efek-bantuan-orang">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="karet-expenditure">
      <value value="10000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-padi">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frontier-scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rate-serangan-gajah">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pagar-listrik-kolektif">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cash-reserve">
      <value value="2000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harga-grosir">
      <value value="1800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-lahan-legal">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="solidaritas-kedekatan">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="innitial-adopter">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="proportion-of-innitial-adopter" first="0.1" step="0.1" last="0.5"/>
    <enumeratedValueSet variable="cost-pagar-listrik">
      <value value="5000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bisa-disamper">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-karet">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-sawit">
      <value value="34"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frekuensi-seminar">
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-pengetahuan">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-people">
      <value value="18"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="efek seminar v20" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>count people with [decision-individual-buy-fence = True]</metric>
    <metric>sum [ last last-reduced-damage-history] of patches with [kepemilikan != 0]</metric>
    <enumeratedValueSet variable="sawit-expenditure">
      <value value="10000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-people">
      <value value="18"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pengetahuan-seminar">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-pro/kontra">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kekayaan-agrikultural">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-kesadaran">
      <value value="79"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="holding-decision-tanam">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="padi-expenditure">
      <value value="13000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efek-bantuan-orang">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="karet-expenditure">
      <value value="10000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-padi">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frontier-scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rate-serangan-gajah">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pagar-listrik-kolektif">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cash-reserve">
      <value value="2000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harga-grosir">
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-lahan-legal">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="solidaritas-kedekatan">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="innitial-adopter">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proportion-of-innitial-adopter">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-pagar-listrik">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bisa-disamper">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-sawit">
      <value value="34"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-karet">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-pengetahuan">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frekuensi-seminar">
      <value value="30"/>
      <value value="60"/>
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="jumlah-titik-serangan-gajah">
      <value value="2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="dampak vs adopter" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>count people with [decision-individual-buy-fence = True]</metric>
    <metric>sum [ last last-reduced-damage-history] of patches with [kepemilikan != 0]</metric>
    <enumeratedValueSet variable="sawit-expenditure">
      <value value="10000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-people">
      <value value="18"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pengetahuan-seminar">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-pro/kontra">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kekayaan-agrikultural">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-kesadaran">
      <value value="79"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="holding-decision-tanam">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="padi-expenditure">
      <value value="13000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efek-bantuan-orang">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="karet-expenditure">
      <value value="10000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-padi">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frontier-scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rate-serangan-gajah">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pagar-listrik-kolektif">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cash-reserve">
      <value value="2000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harga-grosir">
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-lahan-legal">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="solidaritas-kedekatan">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="innitial-adopter">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proportion-of-innitial-adopter">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-pagar-listrik">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bisa-disamper">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-sawit">
      <value value="34"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-karet">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-pengetahuan">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frekuensi-seminar">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="jumlah-titik-serangan-gajah">
      <value value="2"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Frontier adopter" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>count people with [decision-individual-buy-fence = True]</metric>
    <enumeratedValueSet variable="sawit-expenditure">
      <value value="10000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="jumlah-titik-serangan-gajah">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pengetahuan-seminar">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-pro/kontra">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kekayaan-agrikultural">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-kesadaran">
      <value value="79"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="holding-decision-tanam">
      <value value="350"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="padi-expenditure">
      <value value="13000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efek-bantuan-orang">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="karet-expenditure">
      <value value="10000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-padi">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frontier-scenario">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rate-serangan-gajah">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pagar-listrik-kolektif">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cash-reserve">
      <value value="2000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harga-grosir">
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-lahan-legal">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="solidaritas-kedekatan">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="innitial-adopter">
      <value value="false"/>
    </enumeratedValueSet>
    <steppedValueSet variable="proportion-of-innitial-adopter" first="0.1" step="0.1" last="0.5"/>
    <enumeratedValueSet variable="cost-pagar-listrik">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bisa-disamper">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-karet">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-sawit">
      <value value="34"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-pengetahuan">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frekuensi-seminar">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-people">
      <value value="18"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="sawit-expenditure">
      <value value="48500000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pengetahuan-seminar">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kekayaan-agrikultural">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-kesadaran">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="holding-decision-tanam">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="padi-expenditure">
      <value value="70000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="karet-expenditure">
      <value value="75000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-padi">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pagar-listrik-kolektif">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-strength-treshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-lahan-legal">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="solidaritas-kedekatan">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="innitial-adopter">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proportion-of-innitial-adopter">
      <value value="0.16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-pagar-listrik">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-pengetahuan">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frekuensi-seminar">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="jumlah-titik-serangan-gajah">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-pro/kontra">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efek-bantuan-orang">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radius-interaksi">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frontier-scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rate-serangan-gajah">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cash-reserve">
      <value value="2000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harga-grosir">
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-karet">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-sawit">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-people">
      <value value="18"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="kolektif" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>count people with [decision-individual-buy-fence = "Kolektif" ]</metric>
    <metric>count people with [decision-individual-buy-fence = True ]</metric>
    <enumeratedValueSet variable="sawit-expenditure">
      <value value="48500000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pengetahuan-seminar">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kekayaan-agrikultural">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-kesadaran">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="holding-decision-tanam">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="padi-expenditure">
      <value value="70000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="karet-expenditure">
      <value value="75000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-padi">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pagar-listrik-kolektif">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-strength-treshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-lahan-legal">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="solidaritas-kedekatan">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="innitial-adopter">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proportion-of-innitial-adopter">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-pagar-listrik">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-pengetahuan">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frekuensi-seminar">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="jumlah-titik-serangan-gajah">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-pro/kontra">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efek-bantuan-orang">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radius-interaksi">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frontier-scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rate-serangan-gajah">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cash-reserve">
      <value value="2000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harga-grosir">
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-karet">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-sawit">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-people">
      <value value="18"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="income" repetitions="6" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>sum [saldo] of people with [decision-individual-buy-fence = False or decision-individual-buy-fence = "ready to adopt"] / (count people with [decision-individual-buy-fence = False or decision-individual-buy-fence = "ready to adopt"] + 1)</metric>
    <metric>max [saldo] of people with [decision-individual-buy-fence = False or decision-individual-buy-fence = "ready to adopt"]</metric>
    <metric>min [saldo] of people with [decision-individual-buy-fence = False or decision-individual-buy-fence = "ready to adopt"]</metric>
    <metric>sum [saldo] of people with [decision-individual-buy-fence = True] / (count people with [decision-individual-buy-fence = True] + 1)</metric>
    <metric>max [saldo] of people with [decision-individual-buy-fence = True]</metric>
    <metric>min [saldo] of people with [decision-individual-buy-fence = True]</metric>
    <metric>count people with [decision-individual-buy-fence = True]</metric>
    <enumeratedValueSet variable="sawit-expenditure">
      <value value="48500000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pengetahuan-seminar">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kekayaan-agrikultural">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-kesadaran">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="holding-decision-tanam">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="padi-expenditure">
      <value value="90000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="karet-expenditure">
      <value value="75000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-padi">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pagar-listrik-kolektif">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-strength-treshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-lahan-legal">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="solidaritas-kedekatan">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="innitial-adopter">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proportion-of-innitial-adopter">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-pagar-listrik">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-pengetahuan">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frekuensi-seminar">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="jumlah-titik-serangan-gajah">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-pro/kontra">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efek-bantuan-orang">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radius-interaksi">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frontier-scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rate-serangan-gajah">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cash-reserve">
      <value value="2000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harga-grosir">
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-karet">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-sawit">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-people">
      <value value="18"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="orang-bangkrutts" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>count people with [decision-individual-buy-fence = "ready to adopt" and saldo + cash-reserve &lt; estimated-cost-pagar-listrik]</metric>
    <metric>num-people - count people</metric>
    <enumeratedValueSet variable="sawit-expenditure">
      <value value="48500000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pengetahuan-seminar">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kekayaan-agrikultural">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-kesadaran">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="holding-decision-tanam">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="padi-expenditure">
      <value value="90000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="karet-expenditure">
      <value value="75000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-padi">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pagar-listrik-kolektif">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="network-strength-treshold">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-lahan-legal">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="solidaritas-kedekatan">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="innitial-adopter">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proportion-of-innitial-adopter">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-pagar-listrik">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-pengetahuan">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frekuensi-seminar">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="jumlah-titik-serangan-gajah">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-pro/kontra">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efek-bantuan-orang">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radius-interaksi">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frontier-scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rate-serangan-gajah">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cash-reserve">
      <value value="2000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harga-grosir">
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-karet">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-sawit">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-people">
      <value value="18"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="jumlah pagar vs damage received v2" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>sum [ calculate-total-losses] of people with [decision-individual-buy-fence = False or decision-individual-buy-fence = "ready to adopt" ]</metric>
    <metric>count people with [decision-individual-buy-fence = True]</metric>
    <metric>num-people - count people</metric>
    <enumeratedValueSet variable="sawit-expenditure">
      <value value="48500000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pengetahuan-seminar">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kekayaan-agrikultural">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-kesadaran">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="holding-decision-tanam">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="padi-expenditure">
      <value value="90000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="karet-expenditure">
      <value value="75000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-padi">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pagar-listrik-kolektif">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-lahan-legal">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="solidaritas-kedekatan">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="innitial-adopter">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proportion-of-innitial-adopter">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-pagar-listrik">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frekuensi-seminar">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-pengetahuan">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="jumlah-titik-serangan-gajah">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-pro/kontra">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efek-bantuan-orang">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radius-interaksi">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frontier-scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rate-serangan-gajah">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cash-reserve">
      <value value="2000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harga-grosir">
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="collective-type">
      <value value="&quot;subgrouping&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-karet">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-sawit">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-people">
      <value value="18"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="mean losses vs pagar listrik" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>mean[ calculate-total-losses] of people with [decision-individual-buy-fence = False or decision-individual-buy-fence = "ready to adopt" ]</metric>
    <metric>count people with [decision-individual-buy-fence = True]</metric>
    <metric>num-people - count people</metric>
    <enumeratedValueSet variable="sawit-expenditure">
      <value value="48500000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pengetahuan-seminar">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kekayaan-agrikultural">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-kesadaran">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="holding-decision-tanam">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="padi-expenditure">
      <value value="90000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="karet-expenditure">
      <value value="75000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-padi">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pagar-listrik-kolektif">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-lahan-legal">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="solidaritas-kedekatan">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="innitial-adopter">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proportion-of-innitial-adopter">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-pagar-listrik">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frekuensi-seminar">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-pengetahuan">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="jumlah-titik-serangan-gajah">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-pro/kontra">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efek-bantuan-orang">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radius-interaksi">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frontier-scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rate-serangan-gajah">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cash-reserve">
      <value value="2000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harga-grosir">
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="collective-type">
      <value value="&quot;subgrouping&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-karet">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-sawit">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-people">
      <value value="18"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="dampak tanaman langsung" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>sum [ last last-reduced-damage-history] of patches with [kepemilikan != 0]</metric>
    <metric>count people with [decision-individual-buy-fence = True]</metric>
    <metric>num-people - count people</metric>
    <enumeratedValueSet variable="sawit-expenditure">
      <value value="48500000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pengetahuan-seminar">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kekayaan-agrikultural">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-kesadaran">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="holding-decision-tanam">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="padi-expenditure">
      <value value="90000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="karet-expenditure">
      <value value="75000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-padi">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pagar-listrik-kolektif">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-lahan-legal">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="solidaritas-kedekatan">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="innitial-adopter">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proportion-of-innitial-adopter">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-pagar-listrik">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frekuensi-seminar">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-pengetahuan">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="jumlah-titik-serangan-gajah">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-pro/kontra">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efek-bantuan-orang">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radius-interaksi">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frontier-scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rate-serangan-gajah">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cash-reserve">
      <value value="2000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harga-grosir">
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="collective-type">
      <value value="&quot;subgrouping&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-karet">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-sawit">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-people">
      <value value="18"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="collective consensus v2" repetitions="500" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>count people with [decision-individual-buy-fence = "Kolektif"]</metric>
    <metric>count people with [decision-individual-buy-fence = True]</metric>
    <metric>num-people - count people</metric>
    <metric>sum [ last last-reduced-damage-history] of patches with [kepemilikan != 0]</metric>
    <enumeratedValueSet variable="sawit-expenditure">
      <value value="48500000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pengetahuan-seminar">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kekayaan-agrikultural">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-kesadaran">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="holding-decision-tanam">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="padi-expenditure">
      <value value="90000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="karet-expenditure">
      <value value="75000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-padi">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pagar-listrik-kolektif">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-lahan-legal">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="solidaritas-kedekatan">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="innitial-adopter">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proportion-of-innitial-adopter">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-pagar-listrik">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frekuensi-seminar">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-pengetahuan">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="jumlah-titik-serangan-gajah">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-pro/kontra">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efek-bantuan-orang">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radius-interaksi">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frontier-scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rate-serangan-gajah">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cash-reserve">
      <value value="2000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harga-grosir">
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="collective-type">
      <value value="&quot;global consensus&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-sawit">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-karet">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-people">
      <value value="18"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="subgrouping" repetitions="200" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>count people with [decision-individual-buy-fence = "Kolektif"]</metric>
    <metric>count people with [decision-individual-buy-fence = True]</metric>
    <metric>num-people - count people</metric>
    <metric>sum [ last last-reduced-damage-history] of patches with [kepemilikan != 0]</metric>
    <enumeratedValueSet variable="sawit-expenditure">
      <value value="48500000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pengetahuan-seminar">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kekayaan-agrikultural">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-kesadaran">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="holding-decision-tanam">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="padi-expenditure">
      <value value="90000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="karet-expenditure">
      <value value="75000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-padi">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pagar-listrik-kolektif">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-lahan-legal">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="solidaritas-kedekatan">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="innitial-adopter">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proportion-of-innitial-adopter">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-pagar-listrik">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-pengetahuan">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frekuensi-seminar">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="jumlah-titik-serangan-gajah">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-pro/kontra">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radius-interaksi">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efek-bantuan-orang">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frontier-scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rate-serangan-gajah">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cash-reserve">
      <value value="2000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harga-grosir">
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="collective-type">
      <value value="&quot;subgrouping&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-karet">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-sawit">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-people">
      <value value="18"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="collective consensus v3" repetitions="1000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>count people with [decision-individual-buy-fence = "Kolektif"]</metric>
    <metric>count people with [decision-individual-buy-fence = True]</metric>
    <metric>num-people - count people</metric>
    <metric>sum [ last last-reduced-damage-history] of patches with [kepemilikan != 0]</metric>
    <enumeratedValueSet variable="sawit-expenditure">
      <value value="48500000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pengetahuan-seminar">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kekayaan-agrikultural">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-kesadaran">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="holding-decision-tanam">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="padi-expenditure">
      <value value="90000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="karet-expenditure">
      <value value="75000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-padi">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pagar-listrik-kolektif">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-lahan-legal">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="solidaritas-kedekatan">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="innitial-adopter">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proportion-of-innitial-adopter">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-pagar-listrik">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frekuensi-seminar">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-pengetahuan">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="jumlah-titik-serangan-gajah">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-pro/kontra">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efek-bantuan-orang">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radius-interaksi">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frontier-scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rate-serangan-gajah">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cash-reserve">
      <value value="2000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harga-grosir">
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="collective-type">
      <value value="&quot;global consensus&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-sawit">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-karet">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-people">
      <value value="18"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="subgrouping v2" repetitions="1000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>count people with [decision-individual-buy-fence = "Kolektif"]</metric>
    <metric>count people with [decision-individual-buy-fence = True]</metric>
    <metric>num-people - count people</metric>
    <metric>sum [ last last-reduced-damage-history] of patches with [kepemilikan != 0]</metric>
    <enumeratedValueSet variable="sawit-expenditure">
      <value value="48500000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pengetahuan-seminar">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kekayaan-agrikultural">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-kesadaran">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="holding-decision-tanam">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="padi-expenditure">
      <value value="90000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="karet-expenditure">
      <value value="75000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-padi">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pagar-listrik-kolektif">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-lahan-legal">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="solidaritas-kedekatan">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="innitial-adopter">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proportion-of-innitial-adopter">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-pagar-listrik">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-pengetahuan">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frekuensi-seminar">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="jumlah-titik-serangan-gajah">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-pro/kontra">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radius-interaksi">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efek-bantuan-orang">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frontier-scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rate-serangan-gajah">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cash-reserve">
      <value value="2000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harga-grosir">
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="collective-type">
      <value value="&quot;subgrouping&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-karet">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-sawit">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-people">
      <value value="18"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="baseline" repetitions="1000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1000"/>
    <metric>count people with [decision-individual-buy-fence = "Kolektif"]</metric>
    <metric>count people with [decision-individual-buy-fence = True]</metric>
    <metric>num-people - count people</metric>
    <metric>sum [ last last-reduced-damage-history] of patches with [kepemilikan != 0]</metric>
    <enumeratedValueSet variable="sawit-expenditure">
      <value value="48500000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pengetahuan-seminar">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="kekayaan-agrikultural">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-kesadaran">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="holding-decision-tanam">
      <value value="1000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="padi-expenditure">
      <value value="90000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="karet-expenditure">
      <value value="75000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-padi">
      <value value="90"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="pagar-listrik-kolektif">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-lahan-legal">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="solidaritas-kedekatan">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="innitial-adopter">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proportion-of-innitial-adopter">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cost-pagar-listrik">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="treshold-pengetahuan">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frekuensi-seminar">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="jumlah-titik-serangan-gajah">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="proporsi-pro/kontra">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radius-interaksi">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="efek-bantuan-orang">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="frontier-scenario">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="rate-serangan-gajah">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cash-reserve">
      <value value="2000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="harga-grosir">
      <value value="800000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="collective-type">
      <value value="&quot;subgrouping&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-karet">
      <value value="7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="siklus-panen-sawit">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="num-people">
      <value value="18"/>
    </enumeratedValueSet>
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
0
@#$#@#$#@
