"""
Location of CAM's "configure" python data structure, which
is used to pass any needed data between the relevant CAM 
"cime_config" scripts, and which stores all meta-data and
descriptions associated with CAM's configuration.
"""

#Import CIME libraries/functions:
#-------------------------------
from CIME.utils import expect
from standard_script_setup import *
#--------------------------------

class Config(object):

    ##############

    # Number of vertical levels:
    class Nlev(object):
        def __init__(self):
            self.value = None
            self.desc  = "Number of vertical levels."

    # Horizontal grid:
    class Hgrid(object):
        def __init__(self):
            self.value = None
            self.desc  = "Horizontal grid specifier.  The recognized values depend on \n \
                         the dynamics type and are contained in the horiz_grid.xml file." 

    # Number of latitudes in grid:
    class Nlat(object):
        def __init__(self):
            self.value = None
            self.desc  = "Number of unique latitude points in rectangular lat/lon grid."

    # Number of longitudes in grid:
    class Nlon(object):
        def __init__(self):
            self.value = None
            self.desc  = "Number of unique longitude points in rectangular lat/lon grid."

    # Max number of Fourier wavenumbers:
    class Trm(object):
        def __init__(self):
            self.value = 1
            self.desc  = "Maximum Fourier wavenumber."

    # Hightest Legendre polynomial degree:
    class Trn(object):
        def __init__(self):
            self.value = 1
            self.desc  = "Highest degree of the Legendre polynomials for m=0."

    # Hightest associated Legendre polynomial degree:
    class Trk(object):
        def __init__(self):
            self.value = 1
            self.desc  = "Highest degree of the associated Legendre polynomials." 

    # Number of Atmospheric (physics) columsn per chunk:
    class Pcols(object):
        def __init__(self):
            self.value = 16
            self.desc  = "Maximum number of columns in a chunk (physics data structure)."

    # Number of physics sub-columns:
    class Psubcols(object):
        def __init__(self):
            self.value = 1
            self.desc  = "Maximum number of sub-columns in a column (physics data structure)." 

    # CAM dynamical core:
    class Dyn(object):
        def __init__(self):
            self.value        = None
            self.valid_values = ["eul", "fv", "se", "none"]
            self.desc         = "Dynamics package: "+", ".join(self.valid_values)

    # CAM physics version:
    class Phys(object):
        def __init__(self):
            self.value        = None
            self.valid_values = ["cam3", "cam4", "cam5", "cam6", "held_suarez", "adiabatic", "kessler", \
                                 "tj2016", "spcam_sam1mom", "spcam_m2005"]
            self.desc         = "Physics package: "+", ".join(self.valid_values)

    # Planetary Boundary Layer (PBL) scheme:
    class Pbl(object):
        def __init__(self):
            self.value        = None
            self.valid_values = ["uw", "hb", "hbr", "clubb_sgs", "spcam_sam1mom", "spcam_m2005", "none"]
            self.desc         = "PBL package: "+", ".join(self.valid_values)

    # Macrophysics scheme:
    class Macrophys(object):
        def __init__(self):
            self.value        = None
            self.valid_values = ["rk", "park", "clubb_sgs", "spcam_sam1mom", "spcam_m2005", "none"]
            self.desc         = "Macrophysics package: "+", ".join(self.valid_values)

    # Microphysics scheme:
    class Microphys(object):
        def __init__(self):
            self.value        = None
            self.valid_values = ["rk", "mg1", "mg2", "spcam_m2005", "spcam_sam1mom", "none"]
            self.desc         = "Microphysics package: "+", ".join(self.valid_values)

    # Radiation scheme:
    class Rad(object):
        def __init__(self):
            self.value        = None
            self.valid_values = ["rrtmg", "camrt", "none"]
            self.desc         = "Radiative transfer calculation: "+", ".join(self.valid_values)

    # Max number of radiatively-active constituents:
    class Max_n_rad_cnst(object):
        def __init__(self):
            self.value = 30
            self.desc = "Maximum number of constituents that are radiatively active or in any one diagnostic list."

    # CAM Chemistry scheme:
    class Chem(object):
        def __init__(self):
            self.value        = None
            self.valid_values = ["trop_mam3", "trop_mam4", "trop_mam7", "trop_mozart", "trop_strat_mam4_vbs", "trop_strat_mam4_vbsext", \
                                 "waccm_ma", "waccm_mad", "waccm_mad_mam4", "waccm_ma_mam4", "waccm_ma_sulfur", "waccm_sc", "waccm_sc_mam4", \
                                 "waccm_tsmlt_mam4", "terminator", "none"]
            self.desc         = "Chemistry package: "+", ".join(self.valid_values)

    #Ocean component model:
    class Ocn(object):
        def __init__(self):
            self.value        = "docn"
            self.valid_values = ["docn", "dom", "som", "socn", "aquaplanet", "pop", "mom"]
            self.desc         = "Use data ocean model (docn or dom), stub ocean (socn), or aqua planet ocean \n \
                                (aquaplanet) in cam build.  When built from the CESM scripts the value of \n \
                                ocn may be set to pop or mom.  This doesn't impact how CAM is built, only how \n \
                                attributes are matched when searching for namelist defaults.  If ocn is set \n \
                                to som then the docn component is used."

    #Offline driver type:
    class Offline_drv(object):
        def __init__(self):
            self.value        = "stub"
            self.valid_values = ["aur", "rad", "stub"] 
            self.desc         = "Offline unit driver: \n \
                                aur  : aurora module unit test \n \
                                rad  : radiation offline unit drive \n \
                                stub : stub offline unit driver"


    def __init__(self): 
 
        """
        Initalize all configure variables.
        """

        #Number of vertical levels:
        self.nlev = self.Nlev()
  
        #Horizontal grid:
        self.hgrid = self.Hgrid()    

        # Set number of latitudes in grid:
        self.nlat = self.Nlat()

        # Set number of longitudes in grid:
        self.nlon = self.Nlon()

        # Set spherical harmonics (not sure if this is used outside Eulerian dycore?):
        self.trm = self.Trm()
 
        self.trn = self.Trn()

        self.trk = self.Trk()

        # Set number of atmospheric (physics) columns per chunk:
        self.pcols = self.Pcols()

        # Set number of physics sub-columns:
        self.psubcols = self.Psubcols()       

        # Set dynamical core:
        self.dyn = self.Dyn()

        # Set overall physics scheme:
        self.phys = self.Phys()

        # Set Planetary Boundary Layer (PBL) scheme:
        self.pbl = self.Pbl()

        # Set macrophysics scheme:
        self.macrophys = self.Macrophys()

        # Set microphysics scheme:
        self.microphys = self.Microphys()

        # Set radiation scheme:
        self.rad = self.Rad()

        # Set max number of radiatively-active constituents:
        self.max_n_rad_cnst = self.Max_n_rad_cnst()

        # Set chemistry scheme:
        self.chem = self.Chem()

        # Set ocean component model:
        self.ocn = self.Ocn()

        # Set offline driver type:
        self.offline_drv = self.Offline_drv()
         
        #Create dictionary for get/set functions:
        self.var_dict = {
                         "nlev"        : self.nlev,
                         "hgrid"       : self.hgrid,  
                         "nlat"        : self.nlat,
                         "nlon"        : self.nlon,
                         "trm"         : self.trm,
                         "trn"         : self.trn,
                         "trk"         : self.trk,
                         "pcols"       : self.pcols,
                         "psubcols"    : self.psubcols,
                         "dyn"         : self.dyn,
                         "phys"        : self.phys,
                         "pbl"         : self.pbl,
                         "macrophys"   : self.macrophys,
                         "microphys"   : self.microphys,
                         "rad"         : self.rad,
                         "max_n_rad_cnst" : self.max_n_rad_cnst,
                         "chem"        : self.chem,
                         "ocn"         : self.ocn,        
                         "offline_drv" : self.offline_drv
        } #End of dictonary

    ##############

    def print_var(self, varname):

        """
        Print the value and description of a specified
        variable to the CIME debug log.   
        """

        #Loop up specific config variable:
        var = self.var_dict[varname]

        #Extract CIME log:
        logger = logging.getLogger(__name__)

        #Print variable to logger: 
        logger.debug("CAM config variable name: %s", varname)
        logger.debug("CAM config variable description: %s", var.desc)
        logger.debug("CAM config variable value: %s" ,var.value)

    ##############

    def print_all(self):

        """
        Print the names, descriptions, and values of all CAM 
        configuration variables.
        """
   
        #Extract CIME log:
        logger = logging.getLogger(__name__)

        #Print separator:
        logger.debug("CAM configuration variables:")
        logger.debug("-----------------------------")

        #Loop over config dictionary values:
        for varname, var in self.var_dict.items():
            #Print variable to logger:
            logger.debug("CAM config variable name: %s", varname)
            logger.debug("CAM config variable description: %s", var.desc)
            logger.debug("CAM config variable value: %s" ,var.value)            
            logger.debug("-----------------------------")

   #############

    def set_value(self, varname, value):

        """
        Set configure variable to specified value.
        """

        #Look up specific config variable:
        var = self.var_dict[varname]

        #Check if variable object has "valid_values" list attribute:
        if hasattr(var, 'valid_values'):
            #If list exists, check that input value is in list:
            expect(value in var.valid_values, \
                   "{} not in list of valid settings for {}, cannot configure CAM".format(value, varname))

            #Set the value to the input value:
            var.value = value
        else:
            #If not, then just set the value to the input value:
            var.value = value  

    #############

    def get_value(self, varname):

        """
        return value for specified configure variable.
        """  

        #Look up specific config variable:
        var = self.var_dict[varname]
        
        #return variable value:
        return var.value  

############
#End of file
############

 
  
