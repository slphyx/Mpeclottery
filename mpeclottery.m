(* Author: Sompob Saralamba 9 April 2012 *)
(* There is a computer disease that anybody who works with computers knows about. 
It's a very serious disease and it interferes completely with the work.
The trouble with computers is that you 'play' with them! 
Richard P. Feynman *) 

BeginPackage["mpeclottery`"]

MpecExtract::usage="MpecExtract[PageNum,outfrom] extracts the lottery numbers from http://lotto.mthai.com/lottery/result-PageNum.html "
Mpeclottery::usage="Mpeclottery[outform] shows the latest lottery numbers which extracted from http://lotto.mthai.com/. "

Begin["`Private`"]

firstPrzd[dat_]:=Module[{num,address},
num=StringJoin[(Do[
If[StringCases[dat[[i]],__~~"รางวัลที่"]!={}&&StringTake[dat[[i+1]],1]=="1",Throw[{dat[[i]],dat[[i+1]],dat[[i+2]]}]
],{i,1,Length@dat}]//Catch)//Last//StringCases[#,DigitCharacter]&];
num
];

secondPrzd[dat_]:=Module[{tmp1,tmp2,address},
tmp1=Catch[Do[If[dat[[i]]=="รางวัลที่"&&StringTake[#,1]&@dat[[i+1]]=="2",Throw[dat[[i+#]]&/@Range[7]]],{i,1,Length@dat-1}]];
tmp2=StringCases[#,DigitCharacter]&/@tmp1;
StringJoin@tmp2[[2;;6]]
];

thirdPrzd[dat_]:=Module[{tmp1,tmp2,address},
tmp1=Catch[Do[If[dat[[i]]=="รางวัลที่"&&StringTake[#,1]&@dat[[i+1]]=="3",Throw[dat[[i+#]]&/@Range[11]]],{i,1,Length@dat-1}]];
tmp2=StringCases[#,DigitCharacter]&/@tmp1;
StringJoin@tmp2[[2;;11]]
];

forthPrzd[dat_]:=Module[{tmp1,tmp2,address},
tmp1=Catch[Do[If[dat[[i]]=="รางวัลที่"&&StringTake[#,1]&@dat[[i+1]]=="4",Throw[dat[[i+#]]&/@Range[52]]],{i,1,Length@dat-1}]];
tmp2=StringCases[#,DigitCharacter]&/@tmp1;
StringJoin@tmp2[[2;;51]]
];

fifthPrzd[dat_]:=Module[{tmp1,tmp2,address},
tmp1=Catch[Do[If[dat[[i]]=="รางวัลที่"&&StringTake[#,1]&@dat[[i+1]]=="5",Throw[dat[[i+#]]&/@Range[102]]],{i,1,Length@dat-1}]];
tmp2=StringCases[#,DigitCharacter]&/@tmp1;
StringJoin@tmp2[[2;;101]]
];

threeNumPrzd[dat_]:=Module[{tmp1,tmp2,address},
tmp1=StringCases[#,DigitCharacter]&/@(Catch[Do[If[dat[[i]]=="3"&&dat[[i+1]]=="ตัว</strong></p><p",Throw[dat[[i+#]]&/@Range[5]]],{i,1,Length@dat-1}]]);
tmp2=StringJoin@tmp1[[2;;5]];
tmp2
];

twoNumPrzd[dat_]:=Module[{tmp1,tmp2,address},
tmp1=StringCases[#,DigitCharacter]&/@(Catch[Do[If[dat[[i]]=="2"&&dat[[i+1]]=="ตัว</strong></p><p",Throw[dat[[i+#]]&/@Range[2]]],{i,1,Length@dat-1}]]);
tmp2=StringJoin@Last@tmp1;
tmp2
];

dateTHd[dat_]:=Module[{tmp1,tmp2,address},
tmp1=(Catch[Do[If[dat[[i]]=="<title>ตรวจสลากกินแบ่งรัฐบาล"||dat[[i]]=="<title>ตรวจผลสลากกินแบ่งรัฐบาล"||dat[[i]]=="สลากกินแบ่งรัฐบาล",Throw[dat[[i+#]]&/@Range[3]]],{i,1,Length@dat-1}]]);
StringJoin[{tmp1[[1]]," ",tmp1[[2]]," ",StringTake[tmp1[[3]],4]}]
];

convertTHmonth[dateth_]:=Module[{tmp},
tmp=StringReplace[dateth,{"มกราคม"->"January","กุมภาพันธ์"->"February","มีนาคม"->"March","เมษายน"->"April","พฤษภาคม"->"May","มิถุนายน"->"June","กรกฎาคม"->"July","สิงหาคม"->"August","กันยายน"->"September","ตุลาคม"->"October","พฤศจิกายน"->"November","ธันวาคม"->"December"}];
tmp
];

splitnum[text_,n_]:=StringJoin/@Partition[#,n]&@Characters@text

three=Image[CompressedData["
1:eJztmc1Nw0AQhS1RCKIRF0EJkThz4IrogBIogAsXOFADLSAOnCgjPPKUp9Hs
em3HgCPlfXLQ2ox3NvP2Z3Zzsbm+3Jx1XXeOzyM+P2VjjDHGGGOMMcYYY4wx
xhhjjDHmCOn7/uXtY7vj9flp7eaYf+L2/gGKf32+391cqbx2o8yfg/GetKb6
6AYrtspMpN9x8LtJaD5pT/vR3cGuzXIgE5XCgD1gpU7ajUqPf3F1QBl/vTqs
CBRH/Kn7wjHIfA+1NeqBI9jAF/16aVgRyIT4Lxd9u2dUTSaEuJZ4NCXVtXvK
k6HXddvuHhQUA7kxlqs1LEk2ThlMnoozAoiwYy6Ns642XHgOSy3rEojGsUxj
1c9bVj6qEc3gsbSk09Q8lrkE/GZcToAoPSdbRBhRZZxpwIWVBUYYZS3u6hIs
sypKo5yNXqZI3w3s79Rz2DxWqEamL2KmQOFYZjCpr5JnFHixA1BcBl+WUXRC
ITSN0EUpPfOE1KQh6dkAXrHZesvSzyJl5ox59aQljetoMyR9F2bp6oRfVZmH
umUnUSXsouk0wNn+XKgpFWQZkdcmK5mV0kvfhvRSpDrqUx+DcfUsVxOIko2Y
osCdJ/y5aFR2+/BqSY3x50iM/YEaKRVMYkkIpXm4hpI3/XBDL9WUIHWSlJ3G
VNNMJ+7K40YpnZeW+/fGgarq4Vu8GvtBzjxt+cr2DN0acyR8A203jBI=
"], "Byte", ColorSpace -> "RGB", Interleaving -> True];
two=Image[CompressedData["
1:eJztmMFNxDAQRS1RCKKRFEEJK3HmwBXRASVwoYC9wIEaaAFx4EQZMOzTfo1s
J5ugLMvhP3mR1zuxzfzMjJOLzfXl5qyUch6fx/j89I0xxhhjjDHGGGOMMcYY
Y4wxxqzIMAzPr+9fOz4/3u5urk69I7MyITH6hrjRkPvlaXvqfZk1QdbQemLE
/AekyFJpCOQqchmck7fzcr4rjsrt/UPIFKKEn6OqLr28VacrfXddFfHfLW3m
g4dDlGirBFTINydjc3fF0llucyRCDgJ5rdk4ac8xjnVD4mhO10sZdrSDB0cW
XT52Leftg/Zdg+7SpjQVLc630ciEueTReLDFmWTpsq+PuR82GGuEr9GfyKvz
JdaKeSH2zOatdUVWWS4iA+Nwyp8aKjNIP/+Vh5kqOkjM8WxC5UVRrHtGd071
X7hGVyANfTyGmrygIGzxZx4s+9hB7rJTMwukceRWv+t/1eI5MYiglGOack61
tBE4Tc5XOOdkCDljy1g2Yyort4/F8iKJS4plpq00ZYeO5QrcJc+gMopU+bNV
mWycf5VxVlkqdP2/9E2X9GXprDLbs8otOlypnuIl1WXMED38mQfzG6oxlXVS
4trK/0zbZSzrVo9aVSyTXqxyS34A4eilfmVTPRdPvGBs5+TartkYExvuLtR+
NeYP+AYJToEK
"], "Byte", ColorSpace -> "RGB", Interleaving -> True];
first=Image[CompressedData["
1:eJztmM1NxDAUhC3RCDSSIihhJc574IrogBKoAIkLHLYGWkAcOFFGGDTK6Cn/
yqJkozefslE2diw7M/Z7zs3heHu4KqVc4/eC3991VVXFJMYGSI4NkBwbIDk2
QHJsgOTYAMmxAZJjAyTHBkiODZAcGyA5NkBybIDk2ADJsQHOB+/w8f5u614s
xAY4E7zAn+9PHFt3ZCE2wAgPT8/jylL9uq5tgF0wc7BVw7gBTm+vlN4G2AVU
E6qVZub2jh033z++UA1nXvS2hqCPFhj6Uc0GuHxgAKgJpeiEobSNAR01NcEn
W3YOsBcgOtTHMT8WzEnvbYBNYJge+qubvdcjTy14ITbAysQwjTP/aulWNS74
nMJc9lmNQ2YawEZ0M+YJ8XocG2BlEJqpO4HEkglFGhHtwZq6z0yANRkRYrbP
+nwEZ2Z6k/2xAVaGIjJJaxVBdKimOa40XsOksqXJB9iOhNZiwptcJSb7YwNs
Aqdtq/+QL85oyU2iJUqzCHB9kILxy54NcLFoT9c1QAz6WvBVyimvFmiYuJGn
H+LngsnO2ADrQ327r10GYB1JqdIYFNgCVwxFk5gwzDSAvwRugmJ9pLuna+0E
474+lg5tGOe8n94d6F7Yb8/Nv4AZ4SPz8QtVI82P
"], "Byte", ColorSpace -> "RGB", Interleaving -> True];
second=Image[CompressedData["
1:eJztmM1NA0EMhUeikjSSIighEmcOXBEdUAIVIHGBAzXQAuLAiTLCY59imUkW
OYFNVuL7tFntzHqdn2d7PFmsLs9XZ621hV73en1dAwAAAAAAAAAAAAAA/E+W
A6f+FHAynl7ebq4uisayvL6900HMzBMJ9Pz4kIdjltZRBvUAkOU6UQ8bOBrS
KATVRQ6GzMf7qw7d1bkopaJFlpH4ejYPYSZIJukuWX9QX6rplg/LWgmAzqGc
UARmyxR9XefQAUAFmJRYpj10A9YlnYYxIzl2GnSTMss+D8ti15kDHoQiLrlu
6lyr9YO7wkcplpRezZ3ySknHjGfCiWa83Mek+4R4fN8YcDdI+k+HFNEvbO2s
TlR1q2azrGBOc+sbkSA/OufmMAx8HusWdoL6R2C5YVsdi+5bHm7v4yLBQ/3o
+uIRXevchoWmWAFywfmzrwrjeJO+MwDaIFxcZAW9QfNMNPldt++hPRcDwO+7
V62AX+JFfywA2iYGcgVwsudqbw/dri1v6otLgCu/m8Y4KAWTYn279LTE2abb
j7s/D3HDQ46TLHqlAkTxX3+H/wEAAAAAAAAAYIZ8Amax570=
"], "Byte", ColorSpace -> "RGB", Interleaving -> True];
third=Image[CompressedData["
1:eJztmMFtAjEQRS3RRi40skWkBCTOHHJF6SAlpIEccoEDNdBCxIETZWx++Nqv
iZddLMACof9k0Kx3bAx/xjtmOlu8ziYppRe8vvD6s40xxhhjjDHGGGOMMcaY
p6RpmuXH571XYe4D1D/sf0o8ESSb1TcahtRelbkA6LLe7t7f5ryEXkNKSUoY
Jepj2rZt4Q9nGPoI8zhIU9qQ6aT6vEVPGGfVz6ZiJNx88eZKIBDTE21ki4aa
bEhh+o9PC7eY7CNxZZ4b1gncXkw9mJhozDvmdZbR7FQPd/6Yp9kkGhLnLM/i
tsOJXxUWY/iRKR/ecckdOHsE60FPGw40NAlHxU4YuNSQwpo/HaOFhSXXVuWb
m6Mu3GCVm1HxWNVrH9axXYc4emoSdjKQGBWxvxwMcdlflaajn2XUVJVY/8gW
BZUzDDQFEhwURSVVX7YGHhmu/pZmkKE9lqpRQRm6G3eD1MmkqeTG9Jc9vhIe
8WJ14dyvjR7QJ9WXHXVM/ysBFYrwp2T04dYtn7Pq0z/+ReDErw0Ttv8785yu
S2kdR8khiwS5xadAiZTxnwRnvTHGGGMemV/kXOhC
"], "Byte", ColorSpace -> "RGB", Interleaving -> True];
forth=Image[CompressedData["
1:eJztmMFNxDAQRSPRBhcaSRGUsBJnDlwRHVACBXDhAgdqoAXEgRNlhI+e9DVK
drMJrMUi/lMS2bE9a+WPx+M921yeb066rjvVfa/7qxxCCCGEEEIIIYQQQggh
/Dmub+9uri5+exbhAPR9v6rn08vbWunlLR/vr6tnFlry/PggUdBUT1W3dpPW
auJaK73MDsMQ6Y8NCyo1pc5WTa0duqu8XHqN1UCug048/BRWOtdM5O8Lit7L
9wi5iiwn4P83pLhCRJe9vjFaiVpiuojbVIm0dZHykjcoUuM88ZkEwC/p5hxv
ZHBmPt4aIn07/J1V0JOsbPrZVXae5vfITTDHCCHd6qvAgQ6zGNk7JUK9fzfS
N8KrtX7w2uSqWu0kXrw4A/s4yuIAmEI4jNNnr/R4kbMIDaeaPwTaMdXFi9qH
stHprDa589aYb09YIj2u4j2IM0KkbwTfedeqdwyv0jtD8O7gbL8e4kjYap9V
E0vAbw26zwR8hK6a0jqVXmVCNN2cq3ffld7DQwt2HbpHYZZtvVarJ9hCTQbI
Hl1efq6fDg8hhBBCOAY+AZls32s=
"], "Byte", ColorSpace -> "RGB", Interleaving -> True];
fifth=Image[CompressedData["
1:eJztmMFNxDAQRSNRBUcaoQhKWIkzB66IDiiBBjjsBQ7UQAuIAyfKgA9PfA0h
8QZpk6zQf0oi2xlb3v3j8Tgnm4uzzVHXdce673R/lkMIIYQQQgghhBBCCOHf
c/rF2rMI63B1c/t4v23bXF+e65IlV7zlMJEuVR1JNmaJmjzb6mu09580hg0r
Ih11oY5kfXt9HjR7eHrRK1mqIDXb6jfGCQeFdEdZnoMhWo16i59I2Z3qYzzb
lMOe+Wsi1za2qxAusukvg/5n0q3Bam10FYMqkHu5kRaqve6Dc2CvxwEoxwHm
hmSMJ1u5/nZVaxwmhutCQV6xTSMQZRJ1N9KFQZwAtGdSPUf2SQNmhRWHlChb
g3ldfZZSLRYRn2EQ2jkLoBruZNdC0OlzI1XY568NvyAmD+bbktvR2ylcNfAh
Dullaa1tQGbICDtPfL3Bo/4CoOyY+naM3oGddqvP8dzBxCOrSve2+mhdc4NE
/mWQyoNrv7et99a+U/Tu2zHwlupI9opuwrc+vgmQOlJO1rcAdRVXUJNy1RE4
mjlb8AjVkepePyXyO9sf+4YQQgiHxgd3SO1I
"], "Byte", ColorSpace -> "RGB", Interleaving -> True];
logo=Image[CompressedData["
1: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
0cDAwMAj4D9R34XZ
"], "Byte", ColorSpace -> "RGB", Interleaving -> True];

makeit10[ls_List]:=Module[{qut},
If[Mod[Length@ls,10]!=0,Print["Please check the size of your input list."]];
Partition[ls,10]
];

ShowPrz[no_,dat_]:=Module[{tmp},
tmp=dat[[no+1,2]];
tmp
];



MpecExtract[html_Integer,outfrom_Integer]:=Module[{address,dat,raw,outls},
	address="http://lotto.mthai.com/lottery/result-"<>ToString@html<>".html";
	dat=StringSplit[Import[address,"Source"]];
	 outls=If[firstPrzd[dat]!= "",
	{{"date",convertTHmonth@dateTHd[dat]},{1,firstPrzd[dat]},{2,splitnum[#,6]&@secondPrzd[dat]},
	{3,splitnum[#,6]&@thirdPrzd[dat]},{4,splitnum[#,6]&@forthPrzd[dat]},{5,splitnum[#,6]&@fifthPrzd[dat]},{"three",splitnum[#,3]&@threeNumPrzd[dat]},
	{"two",twoNumPrzd[dat]}}
	,555];
Switch[outfrom,0,outls,1,
Grid[{{logo,SpanFromLeft,SpanFromLeft,SpanFromLeft,SpanFromLeft,SpanFromLeft,SpanFromLeft,SpanFromLeft,SpanFromLeft,SpanFromLeft},
{outls[[1,2]],SpanFromLeft},
{first,SpanFromLeft,SpanFromLeft,SpanFromLeft,SpanFromLeft,SpanFromLeft,SpanFromLeft,SpanFromLeft,SpanFromLeft,SpanFromLeft},
{ShowPrz[1,outls],SpanFromLeft},
{second,SpanFromLeft},
Insert[ShowPrz[2,outls],SpanFromLeft,{{2},{3},{4},{5},{6}}],
{third,SpanFromLeft},
ShowPrz[3,outls],
{forth,SpanFromLeft},
(makeit10@ShowPrz[4,outls])[[1]],
(makeit10@ShowPrz[4,outls])[[2]],
(makeit10@ShowPrz[4,outls])[[3]],
(makeit10@ShowPrz[4,outls])[[4]],
(makeit10@ShowPrz[4,outls])[[5]],
{fifth,SpanFromLeft},
(makeit10@ShowPrz[5,outls])[[1]],
(makeit10@ShowPrz[5,outls])[[2]],
(makeit10@ShowPrz[5,outls])[[3]],
(makeit10@ShowPrz[5,outls])[[4]],
(makeit10@ShowPrz[5,outls])[[5]],
(makeit10@ShowPrz[5,outls])[[6]],
(makeit10@ShowPrz[5,outls])[[7]],
(makeit10@ShowPrz[5,outls])[[8]],
(makeit10@ShowPrz[5,outls])[[9]],
(makeit10@ShowPrz[5,outls])[[10]],
{three,SpanFromLeft},
{"",SpanFromLeft,SpanFromLeft}~Join~ShowPrz[6,outls]~Join~{"",SpanFromLeft,SpanFromLeft},
{two,SpanFromLeft},
{ShowPrz[7,outls],SpanFromLeft}
},Frame->All]
]
];


Mpeclottery[outfrom_Integer]:=Module[{address1,address2,raw,dat,outls,html,targettxt},
	
	address1 = "http://lotto.mthai.com";
	raw = StringSplit[Import[address1, "Source"]];
	targettxt = "href=\"http://lotto.mthai.com/lottery/result-";
	
	html=StringDrop[StringDrop[
  Catch[If[StringCases[#, targettxt ~~ ___] != {}, Throw[#]] & /@ 
    raw], 44], -6];
	
	address2="http://lotto.mthai.com/lottery/result-"<>html<>".html";
	dat=StringSplit[Import[address2,"Source"]];
	 outls=If[firstPrzd[dat]!= "",
	{{"date",convertTHmonth@dateTHd[dat]},{1,firstPrzd[dat]},{2,splitnum[#,6]&@secondPrzd[dat]},
	{3,splitnum[#,6]&@thirdPrzd[dat]},{4,splitnum[#,6]&@forthPrzd[dat]},{5,splitnum[#,6]&@fifthPrzd[dat]},{"three",splitnum[#,3]&@threeNumPrzd[dat]},
	{"two",twoNumPrzd[dat]}}
	,555];
Switch[outfrom,0,outls,1,
Grid[{{logo,SpanFromLeft,SpanFromLeft,SpanFromLeft,SpanFromLeft,SpanFromLeft,SpanFromLeft,SpanFromLeft,SpanFromLeft,SpanFromLeft},
{outls[[1,2]],SpanFromLeft},
{first,SpanFromLeft,SpanFromLeft,SpanFromLeft,SpanFromLeft,SpanFromLeft,SpanFromLeft,SpanFromLeft,SpanFromLeft,SpanFromLeft},
{ShowPrz[1,outls],SpanFromLeft},
{second,SpanFromLeft},
Insert[ShowPrz[2,outls],SpanFromLeft,{{2},{3},{4},{5},{6}}],
{third,SpanFromLeft},
ShowPrz[3,outls],
{forth,SpanFromLeft},
(makeit10@ShowPrz[4,outls])[[1]],
(makeit10@ShowPrz[4,outls])[[2]],
(makeit10@ShowPrz[4,outls])[[3]],
(makeit10@ShowPrz[4,outls])[[4]],
(makeit10@ShowPrz[4,outls])[[5]],
{fifth,SpanFromLeft},
(makeit10@ShowPrz[5,outls])[[1]],
(makeit10@ShowPrz[5,outls])[[2]],
(makeit10@ShowPrz[5,outls])[[3]],
(makeit10@ShowPrz[5,outls])[[4]],
(makeit10@ShowPrz[5,outls])[[5]],
(makeit10@ShowPrz[5,outls])[[6]],
(makeit10@ShowPrz[5,outls])[[7]],
(makeit10@ShowPrz[5,outls])[[8]],
(makeit10@ShowPrz[5,outls])[[9]],
(makeit10@ShowPrz[5,outls])[[10]],
{three,SpanFromLeft},
{"",SpanFromLeft,SpanFromLeft}~Join~ShowPrz[6,outls]~Join~{"",SpanFromLeft,SpanFromLeft},
{two,SpanFromLeft},
{ShowPrz[7,outls],SpanFromLeft}
},Frame->All]
]
];


End[ ]
SetAttributes[Mpeclottery,{ReadProtected}];

EndPackage[]


