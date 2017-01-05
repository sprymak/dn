/* */
FileName = 'ak' || Substr(DATE('S'), 4, 5) || '.dif'
"G:\DN2DIFF\diff.exe -urN -X G:\DN2DIFF\IGNORE.LST I:\dn22s06 .  > "FileName
