program tilgung
implicit none

real(kind = 8) :: ZS, KH, Z, R, RS, ZinsSumme
integer :: LZ

write (*,*) 'Bitte Kredithoehe in Euro eingeben'
read (*,*) KH
write (*,*) 'Bitte Zinssatz in Prozen eingeben'
read (*,*) ZS

Z = KH * ZS / 100
RS = KH + Z

write (*,*) 'Bitte Rate in Euro eingeben'
read (*,*) R

do while ( R < Z )
   write (*,*) 'Bitte eine hoehere Rate eingeben'
   read (*,*) R    
end do

LZ = 1
ZinsSumme = Z
RS = RS - R

do while ( RS > 0 )
   LZ = LZ + 1
   Z = RS * ZS / 100
   RS = RS + Z - R
   ZinsSumme = ZS + Z
   write (*,*) RS, R, ZS
end do

if ( RS < 0 ) then
   R = R + RS
   write (*,*) 'Letzte Rate ist ', R, ' Euro'
end if

write (*,*) 'Laufzeit: ', LZ, 'und Zinssumme: ',ZinsSumme

end program tilgung
