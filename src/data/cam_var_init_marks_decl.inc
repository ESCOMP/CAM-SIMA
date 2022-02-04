!! public interfaces
   public :: mark_as_initialized
   public :: mark_as_read_from_file
   public :: is_initialized
   public :: is_read_from_file

!! Parameterized initialized_vars options - order matters
   integer, public, parameter ::  UNINITIALIZED = 0
   integer, public, parameter ::    INITIALIZED = 1
   integer, public, parameter ::          PARAM = 2
   integer, public, parameter :: READ_FROM_FILE = 3
