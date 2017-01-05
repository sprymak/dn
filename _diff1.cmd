/* */
FileName = 'ak' || Substr(DATE('S'), 4, 5) || '.dif'
"del " FileName
Parse Arg Rest
do while \(Rest='')
  Parse Var Rest Name Rest
  "diff.exe -uN I:\dn22s06\"Name Name"  >> "FileName
end
