       program test
        character(len=132) para(100),para_tmp
	  real epi_sort(100),epi_tmp

c    Sort the para_results.txt based on epicenter distance
	   open(1,file='para_result.txt',status='old')
         read(1,*) 
		do i=1,8
          read(1,'(a132)') para(i)
	    read(para(i)(5:14),*) epi_sort(i)
		enddo
	   close(1)

         do i=1,7
	    do j=i+1,8
	    if(epi_sort(i).gt.epi_sort(j)) then
	    para_tmp=para(j)
	    para(j)=para(i)
	    para(i)=para_tmp
	    epi_tmp=epi_sort(j)
	    epi_sort(j)=epi_sort(i)
	    epi_sort(i)=epi_tmp
	    endif
	    enddo
	   enddo

         do i=1,8
         write(*,'(a132)') para(i)
	   enddo

      end program test