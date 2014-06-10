module mod_mischenh
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
  
  subroutine tausche(x,y)
    type(filecomp)::x,y,hilf
    hilf=x
    x=y
    y=hilf
  end subroutine tausche
 
  recursive subroutine versickern(feld,groesse,i)              !an knoten i wird heap-eigenschaft hergestellt
    type(filecomp),dimension(:)::feld
    integer::i,l,r,gross,groesse
  
    l=i*2      !zugehoeriger linker kindknoten
    r=i*2+1    !zugehoeriger rechter kindknoten
  
    if((l<=groesse).and.(feld(l)%inhalt>feld(i)%inhalt))then   !linker kindnoten existiert und ist groesser
      gross=l
    else
      gross=i
    end if
    if((r<=groesse).and.(feld(r)%inhalt>feld(gross)%inhalt))then
      gross=r
    end if                                        !in gross ist nun das groesste der zu vergleichenden elemente
    if(gross/=i)then                            !nur was tun wenn i nicht schon groesser ist   
      call tausche(feld(i),feld(gross))            !groesstes element nun elternknoten
      call versickern(feld,groesse,gross)        !rek. aufruf um richtung blatt zu wandern
    end if 
  end subroutine versickern

  subroutine baueheap(feld)                      !baut feld um
    type(filecomp),dimension(:)::feld
    integer::i,test
    do i=size(feld)/2,1,-1
      call versickern(feld,size(feld),i)          !alle knoten bis auf die unterste ebene 
    end do
  end subroutine baueheap
  
  subroutine sortiereheap(feld)                   !liefert sortiertes feld
    type(filecomp),dimension(:)::feld
    integer::groesse,j,k
    
    call baueheap(feld)                            !erstmal heap eigenschaft erhalten
    groesse=size(feld)                               !groesse des zu bearbeitenden feldes aendert sich
    
    do j=size(feld),2,-1
      call tausche(feld(1),feld(groesse))            !erstes element ist maximal, kommt ganz runter
      groesse=groesse-1                              !und dort wird es nicht mehr angeruehrt
      call versickern(feld,groesse,1)               !neue wurzel muss einsickern
    end do 

  end subroutine sortiereheap

end module mod_mischenh
