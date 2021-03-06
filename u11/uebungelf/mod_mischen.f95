module mod_mischen
  implicit none
  
  type filecomp            !typ fuer eingelesene dateien
    integer::unitn
    integer::inhalt
  end type filecomp 
 
  interface operator (<=)
    module procedure small
  end interface

contains
  
  logical function small(erstes,zweites)                ! vergleich zweier zahlen
    type(filecomp),intent(in)::erstes,zweites
    small=erstes%inhalt<=zweites%inhalt
  end function small
  
  subroutine sort(feld)
    type(filecomp),dimension(:)::feld
    integer::i,j,n
    type(filecomp)::hilf
   
  do i=2,size(feld)
    do j=1,i-1
      if (feld(i)<=feld(j))then     !ordnung kaputt
        hilf=feld(i)
        feld(j+1:i)=feld(j:i-1)
        feld(j)=hilf
        exit
      end if
    end do
  end do

  end subroutine sort

  subroutine insert(feld,wert)
    type(filecomp),dimension(:)::feld
    type(filecomp),intent(in)::wert
    integer::r,l,mitte

    l=1
    r=size(feld)+1          !im schlimmsten fall wird so auch der groesstmoegliche index erreicht   
      do
        mitte=(l+r)/2           !halbieren, abrunden
        if(mitte==1)then         !nur wert eintragen, keine verschiebung
          feld(mitte)=wert
          exit
        else if (mitte==size(feld,1))then    !vergleich mit feld(mitte+1)nicht moeglich, daher extra
          feld(1:mitte-1)=feld(2:mitte)
          feld(mitte)=wert
          exit
        else if((feld(mitte)<=wert).and.(wert<=feld(mitte+1)))then
          feld(1:mitte-1)=feld(2:mitte)
          feld(mitte)=wert
          exit
        end if
        if(wert<=feld(mitte))then        !zahl noch zu klein fuer mitte
          r=mitte
        else if(feld(mitte)<=wert)then          !zahl noch zu gross fuer mitte
          l=mitte
        end if
      end do

  end subroutine insert

end module mod_mischen
