module phys_prop
  ! Mock stub for unit tests.
  ! Provides physprop_get_id (returns a deterministic index)
  ! and a no-op physprop_get so that aerosol_properties_mod compiles.
  use shr_kind_mod, only: r8 => shr_kind_r8

  implicit none
  private

  integer, parameter, public :: ot_length = 32

  public :: physprop_get_id
  public :: physprop_get

  integer, save :: next_id = 0

contains

  integer function physprop_get_id(filename)
    character(len=*), intent(in) :: filename
    next_id = next_id + 1
    physprop_get_id = next_id
  end function physprop_get_id

  subroutine physprop_get(id, sourcefile, opticstype, &
                          sw_hygro_ext, sw_hygro_ssa, sw_hygro_asm, lw_hygro_abs, &
                          sw_hygro_ext_wtp, sw_hygro_ssa_wtp, sw_hygro_asm_wtp, lw_hygro_abs_wtp, &
                          sw_nonhygro_ext, sw_nonhygro_ssa, sw_nonhygro_asm, &
                          sw_nonhygro_scat, sw_nonhygro_ascat, lw_abs, &
                          refindex_aer_sw, refindex_aer_lw, &
                          r_sw_ext, r_sw_scat, r_sw_ascat, r_lw_abs, mu, &
                          extpsw, abspsw, asmpsw, absplw, refrtabsw, &
                          refitabsw, refrtablw, refitablw, &
                          aername, density_aer, hygro_aer, dryrad_aer, dispersion_aer, &
                          num_to_mass_aer, ncoef, prefr, prefi, sigmag, &
                          dgnum, dgnumlo, dgnumhi, rhcrystal, rhdeliques, &
                          extpsw2, abspsw2, asmpsw2, absplw2, corefrac, nfrac, &
                          wgtpct, bcdust, kap, relh, &
                          nkap, nwtp, nbcdust, nrelh, &
                          sw_hygro_coreshell_ext, sw_hygro_coreshell_ssa, &
                          sw_hygro_coreshell_asm, lw_hygro_coreshell_abs)
    ! No-op stub: all optional arguments are simply ignored.
    ! Tests that need real optics data should mock at the radiative_aerosol level.
    integer, intent(in)  :: id
    character(len=256),       optional, intent(out) :: sourcefile
    character(len=ot_length), optional, intent(out) :: opticstype
    real(r8),          optional, pointer     :: sw_hygro_ext(:,:)
    real(r8),          optional, pointer     :: sw_hygro_ssa(:,:)
    real(r8),          optional, pointer     :: sw_hygro_asm(:,:)
    real(r8),          optional, pointer     :: lw_hygro_abs(:,:)
    real(r8),          optional, pointer     :: sw_hygro_ext_wtp(:,:)
    real(r8),          optional, pointer     :: sw_hygro_ssa_wtp(:,:)
    real(r8),          optional, pointer     :: sw_hygro_asm_wtp(:,:)
    real(r8),          optional, pointer     :: lw_hygro_abs_wtp(:,:)
    real(r8),          optional, pointer     :: sw_nonhygro_ext(:)
    real(r8),          optional, pointer     :: sw_nonhygro_ssa(:)
    real(r8),          optional, pointer     :: sw_nonhygro_asm(:)
    real(r8),          optional, pointer     :: sw_nonhygro_scat(:)
    real(r8),          optional, pointer     :: sw_nonhygro_ascat(:)
    real(r8),          optional, pointer     :: lw_abs(:)
    complex(r8),       optional, pointer     :: refindex_aer_sw(:)
    complex(r8),       optional, pointer     :: refindex_aer_lw(:)
    real(r8),          optional, pointer     :: r_sw_ext(:,:)
    real(r8),          optional, pointer     :: r_sw_scat(:,:)
    real(r8),          optional, pointer     :: r_sw_ascat(:,:)
    real(r8),          optional, pointer     :: r_lw_abs(:,:)
    real(r8),          optional, pointer     :: mu(:)
    real(r8),          optional, pointer     :: extpsw(:,:,:,:)
    real(r8),          optional, pointer     :: abspsw(:,:,:,:)
    real(r8),          optional, pointer     :: asmpsw(:,:,:,:)
    real(r8),          optional, pointer     :: absplw(:,:,:,:)
    real(r8),          optional, pointer     :: refrtabsw(:,:)
    real(r8),          optional, pointer     :: refitabsw(:,:)
    real(r8),          optional, pointer     :: refrtablw(:,:)
    real(r8),          optional, pointer     :: refitablw(:,:)
    character(len=20), optional, intent(out) :: aername
    real(r8),          optional, intent(out) :: density_aer
    real(r8),          optional, intent(out) :: hygro_aer
    real(r8),          optional, intent(out) :: dryrad_aer
    real(r8),          optional, intent(out) :: dispersion_aer
    real(r8),          optional, intent(out) :: num_to_mass_aer
    integer,           optional, intent(out) :: ncoef
    integer,           optional, intent(out) :: prefr
    integer,           optional, intent(out) :: prefi
    real(r8),          optional, intent(out) :: sigmag
    real(r8),          optional, intent(out) :: dgnum
    real(r8),          optional, intent(out) :: dgnumlo
    real(r8),          optional, intent(out) :: dgnumhi
    real(r8),          optional, intent(out) :: rhcrystal
    real(r8),          optional, intent(out) :: rhdeliques
    real(r8),          optional, pointer     :: extpsw2(:,:,:,:)
    real(r8),          optional, pointer     :: abspsw2(:,:,:,:)
    real(r8),          optional, pointer     :: asmpsw2(:,:,:,:)
    real(r8),          optional, pointer     :: absplw2(:,:,:,:)
    real(r8),          optional, pointer     :: corefrac(:)
    integer,           optional, intent(out) :: nfrac
    real(r8),          optional, pointer     :: wgtpct(:)
    real(r8),          optional, pointer     :: bcdust(:)
    real(r8),          optional, pointer     :: kap(:)
    real(r8),          optional, pointer     :: relh(:)
    integer,           optional, intent(out) :: nkap
    integer,           optional, intent(out) :: nwtp
    integer,           optional, intent(out) :: nbcdust
    integer,           optional, intent(out) :: nrelh
    real(r8),          optional, pointer     :: sw_hygro_coreshell_ext(:,:,:,:,:)
    real(r8),          optional, pointer     :: sw_hygro_coreshell_ssa(:,:,:,:,:)
    real(r8),          optional, pointer     :: sw_hygro_coreshell_asm(:,:,:,:,:)
    real(r8),          optional, pointer     :: lw_hygro_coreshell_abs(:,:,:,:,:)

    ! No-op: mock does not populate any fields
  end subroutine physprop_get

end module phys_prop
