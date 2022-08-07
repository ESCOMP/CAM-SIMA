module pio

   !! Fake PIO types and interfaces for testing

   implicit none
   private

   type, public :: file_desc_t
      character(len=32) :: name = "Fake PIO file descriptor"
   end type file_desc_t

end module pio
