TestAlpha[x_] := 0 <= x <= 25;
TextCode[messaggio_] := Select[ToCharacterCode[ToLowerCase[messaggio]] - 97, TestAlpha];
FromCode[messaggio_] := FromCharacterCode[messaggio + 97];


ShiftEncrypt[messaggio_, key_] := Mod[messaggio + key, 26];
ShiftDecrypt[messaggio_, key_] := ShiftEncrypt[messaggio, -key];

MessageEncode[ENC_, messaggio_, key_] := If[StringQ[messaggio],
        FromCode[ENC[TextCode[messaggio], key]],
        ENC[messaggio, key]
    ] ;

Frequencies[messaggio_]:=  Map[Count[messaggio, #] &, Range[0, 25]];

Distribution[messaggio_] := Module[ {freqs, n},
  freqs =Frequencies[messaggio];
  n = Length[messaggio];
  N[freqs/n]
  ];

CoincidenceIndex[messaggio_] := Module[{distribution},
  freqs = Map[Count[messaggio, #] &, Range[0, 25]];
  n = Length[messaggio];
  N[Plus @@ (freqs (freqs - 1)/(n ( n - 1)))]
  ];

MutualIncidence[messaggio_, language_] := 
 Plus @@ (Distribution[messaggio] Distribution[language])
