DEBUG[x___] := Print[x];
DEBUG[x___] := Null;


q = 26;
TestAlpha[x_] := 0 <= x <= q - 1
TextCode[messaggio_] := Select[ToCharacterCode[ToLowerCase[messaggio]] - 97, TestAlpha]
FromCode[messaggio_] := FromCharacterCode[messaggio + 97]

EncodingFunction[key_, message_] :=  FromCode@Map[ShiftEncrypt[key, #] &, TextCode[message]]
DecodingFunction[key_, message_] :=  FromCode@Map[ShiftDecrypt[key, #] &, TextCode[message]]

ShiftEncrypt[key_, plaintext_] := Mod[key + plaintext, q]
ShiftDecrypt[key_, plaintext_] := ShiftEncrypt[-key, plaintext]

(* SUBSTITUTION CIPHER *)
MyPick[stato_] := Module[{e, lista, perm},
  (
   lista = stato[[2]]; DEBUG[lista];
   perm = stato[[1]]; DEBUG[perm];
   e = RandomInteger[{1, Length[lista]}]; DEBUG[e];
   {Append[perm, lista[[e]]], Drop[lista, {e}]}
   )];

GenKey[] := Nest[MyPick, {{}, Range[0, q - 1]}, q][[1]]

SubstitutionEncrypt[key_, plaintext_] := key[[plaintext + 1]]
SubstitutionDecrypt[key_, ciphertext_] :=  Position[key, ciphertext][[1, 1]] - 1

EncodingFunction[key_, message_] :=  FromCode@Map[SubstitutionEncrypt[key, #] &, TextCode[message]]
DecodingFunction[key_, message_] :=  FromCode@Map[SubstitutionDecrypt[key, #] &, TextCode[message]]


repubblica = Import["http://www.repubblica.it"];
divina = TextCode[Import["https://www.gutenberg.org/files/1012/1012-0.txt"]];

ctx = EncodingFunction[key = GenKey[], repubblica];

Histogram[TextCode[ctx]]
Histogram[divina]


(* PERMUTATION CIPHER *)

blocksize = 6
PermKeyGen[] := Nest[MyPick, {{}, Range[1, blocksize]}, blocksize][[1]]

PermutationEncrypt[key_, plaintext_] := plaintext[[key]]
PermutationDecrypt[key_, ciphertext_] := Module[{invkey},
  invkey = Table[Position[key, i][[1, 1]], {i, 1, Length@key}];
  ciphertext[[invkey]]
  ]

EncodingFunction[key_, message_] := FromCode@Flatten@Map[PermutationEncrypt[key, #] &, 
    Partition[TextCode[message], Length[key], Length[key], 1, 0]]

DecodingFunction[key_, message_] := FromCode@Flatten@Map[PermutationDecrypt[key, #] &,
    Partition[TextCode[message], Length[key], Length[key], 1, 0]]

blocksize = 6
key = PermKeyGen[]
ctx = EncodingFunction[key, repubblica]
DecodingFunction[key, ctx]