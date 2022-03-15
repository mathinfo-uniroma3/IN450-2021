AlphaTest[x_]:=0<=x<=25;
TextCode[text_]:=Select[
					ToCharacterCode[
						ToUpperCase[text]]-65,AlphaTest];
FromCode[textcode_]:=FromCharacterCode[textcode+65];

CoincidenceIndex[testo_] :=
 If[
  StringQ[testo],
  (*THEN*)
  CoincidenceIndex[TextCode[testo]],
  (*ELSE*)
  Module[{n, freqs},
   (
    n = Length[testo];
    freqs = Map[Count[testo, #] &, Range[0, 25]];
    N[Plus @@ (freqs (freqs - 1)/(n (n - 1)))]
    )]
  ];

testo = "001 FIOPPQNTPN BUUGGMOCYY JFMDVUUDEY ICLDYVITVL IJMMAAPYGF
            051 MOEHFKFMTV ZEIFMETVBV KDDNPFVKOI YHJZIVNRUI \
DCZMBKYAZX
            101 AMPEIZYEVW QOKDAPJUOK VTRKQGCZDH LFVIOIAVDM \
ZXHVLTJEJP
            151 EVSZUDMRUJ JUDRRCYNZJ NRJENKDTHG YJEVLRHHOK \
PTGLBZTJNS
            201 LINZJNVYUG ZBIBZUNFIO RNKVCHEAAU GZWEELTVMV \
NGPQGCVLRN
            251 WZCZCBUVZJ NIBUYMVGIT PENVYIILHN VYAYSQXROT \
BSYXRCAAUE
            301 YZMIGAEYZJ RTHDDQUAEZ YNVXOAKEDG MOCYYNKVTH \
AYDELUNUJJ";


ciphertext = TextCode[testo];
message = Import["http://www.repubblica.it"];
italian = Import["http://www.corriere.it"];
english = Import["http://www.nytimes.com"];
random = RandomInteger[{0, 25}, 3000];

TableForm@(tab = Table[
    Map[CoincidenceIndex[Take[#, len]] &, {TextCode@message, 
      TextCode@italian, TextCode@english, random}],
    {len, 10, 200, 10}])


TableForm@(tab = Table[
    Map[CoincidenceIndex[Take[#, len]] &, {TextCode@message, 
      TextCode@Italian, TextCode@English, random}],
    {len, 10, 200, 10}])

Export["advantage.pdf",    ListLinePlot[Transpose@tab, InterpolationOrder -> 2, 
 PlotLegends -> Automatic],"PDF"]

CoincidenceIndex[testo_, trunc_, n_] :=
 If[
  StringQ[testo],
  (*THEN*)
  CoincidenceIndex[TextCode[testo], trunc, n],
  (*ELSE*)
  Module[{ntesto, tfreqs},
   (
    ntesto = Take[testo, n];
    tfreqs = Take[Sort[Map[Count[ntesto, #] &, Range[0, 25]]], -trunc];
    N[Plus @@ (tfreqs (tfreqs - 1)/(n (n - 1)))]
    )]
  ]

TableForm@(tab = Table[
     Map[
      CoincidenceIndex[#, 15, len] &, {TextCode@message, 
       TextCode@Italian, TextCode@English, random}],
     {len, 10, 200, 10}]);
Export["advantage-truncated-stat.pdf",ListLinePlot[Transpose@tab, InterpolationOrder -> 2, 
 PlotLegends -> Automatic],"PDF"]