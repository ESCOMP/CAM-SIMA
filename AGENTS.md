# CAM-SIMA and atmospheric_physics AI agent guidance

This file applies to work in the **CAM-SIMA** ("host model") and **atmospheric_physics** (CCPP-compliant physics schemes, submoduled at `src/physics/ncar_ccpp`). atmospheric_physics is submoduled into CAM-SIMA but must remain portable — it contains the science codebase for physics parameterizations and is also shared with **CAM**, the production atmospheric component of CESM.

## AI disclosure

CAM-SIMA and atmospheric_physics requires disclosure of AI-assisted contributions:

- In PR descriptions: state which model(s) were used and how.
- In commits made by agents or incorporating AI-assisted code: use `Assisted-by: model:version` (e.g., `Assisted-by: claude-opus:4.8`)
- The human contributor remains responsible for all committed code at all times.

## Boundary between CAM-SIMA host model and atmospheric_physics (portable CCPP physics)

The **CAM-SIMA host model**:
- Located at `ESCOMP/CAM-SIMA`. The default branch is the `development` branch. The `main` branch is empty and only contains a README to prevent users from easily inadvertently checking out unsupported development code.
- Provides infrastructure, coupling, dynamical cores, and I/O.
- Interfaces with CIME, PIO, MPI, CESM_share (`shr_*`) and provides CAM-specific utilities like `cam_abortutils`, `cam_history`, etc.
- Also provides CCPP metadata information via the Registry (`src/data/registry.xml`) and host-side `.meta` files.

The **atmospheric_physics** portable CCPP physics:
- Located at `ESCOMP/atmospheric_physics` and is a submodule that would be checked out under `CAM-SIMA/src/physics/ncar_ccpp`. The default branch is `main`. There are no feature branches under `ESCOMP` and new branches must be made under forks, never under `ESCOMP`.
- Contains CCPP-compliant, "portable" (i.e., free of host model code) physics parameterizations.
- May `use` `ccpp_kinds`, `ccpp_constituent_prop_mod`, and intra-package dependencies only.
- **Exception:** `schemes/sima_diagnostics/` is a non-portable directory centralizing CAM-SIMA-specific history output. Code outside `sima_diagnostics/` that calls `cam_history` routines is a bug.
- If a scheme under `atmospheric_physics` depends on a CAM-SIMA host side quantity, it receives it as a subroutine argument threaded through via standard names, not via a `use` statement to a CAM module.
- Is intended to be portable to any CCPP-enabled host model.

### Other key differences

| Convention | CAM-SIMA (host) | atmospheric_physics (physics) |
|-|-|-|
| Real kind | `r8` from `shr_kind_mod` | `kind_phys` from `ccpp_kinds` |
| Error handling | `call endrun(msg)` | Set `errmsg`/`errflg` (or `errcode`) and return |
| Abort utility | `cam_abortutils::endrun` | Never: return error to host |
| Constituent lookup | `cam_constituents::const_get_index` | `ccpp_const_get_idx(const_props, ...)` |
| Physical constants | `physconst` module | Passed as arguments via CCPP standard names |
| History output | `cam_history::history_add_field` / `history_out_field` | Only in dedicated `schemes/sima_diagnostics/*` schemes |
| Dimensions in dummy args | `(pcols, pver)` acceptable in host code | Assumed-shape `(:,:)` only |
| Module-level state | Allowed with care | Minimal; prefer arguments |

## Example code to scope for conventions

Learn the current conventions in the following files, which may be preferred over older schemes that predate recent convention updates:

* Host-side registry: `src/data/registry.xml`
* Host-side metadata: `src/data/physconst.meta`, `src/physics/utils/physics_grid.meta`
* Host-side input of gridded/decomposed files threaded to CCPP scheme: `src/physics/utils/gravity_wave_drag_ridge_read.{F90,meta}`
* Minimal CCPP scheme: `atmospheric_physics/schemes/cloud_fraction/convective_cloud_cover.F90`.
* Scheme with namelist XML: `atmospheric_physics/schemes/vertical_diffusion/vertical_diffusion_sponge_layer.{F90,meta}`, `atmospheric_physics/schemes/vertical_diffusion/vertical_diffusion_sponge_layer_namelist.xml`.
* Test (i.e., single physics scheme) SDFs are located in `atmospheric_physics/test/test_suites`; production (i.e., CAM4, CAM5, CAM7) SDFs are located in `atmospheric_physics/suites`.

## Key CCPP scheme conventions

- Subroutine names are `<scheme_name>_<phase>`; generally, the module name is the scheme name, although one file may contain multiple schemes (e.g., `physics_tendency_updaters.F90`).
- Valid phases: `register`, `init`, `timestep_init`, `run`, `timestep_final`, `final`. All phases are optional.
- Every phase subroutine has `errmsg` and `errflg` (or `errcode`) as `intent(out)`.
- Two required Doxygen lines precede each subroutine. Current form uses the `arg_table_` prefix on the html:
  ```
  !> \section arg_table_<scheme>_<phase> Argument Table
  !! \htmlinclude arg_table_<scheme>_<phase>.html
  ```
  Older schemes omit the `arg_table_` prefix on the html file but you should use the new convention.
- Explicit module-level `save` is not needed. Module-level variables have implicit `SAVE` per the Fortran standard. Recent refactors have been removing redundant explicit `save` statements.
- Do not initialize local variables on declaration lines (invokes implicit `SAVE`, which is not thread-safe). Initialize in the executable section.
- Place `use` statements at module level only for symbols needed by module-level declarations; all other `use` statements go inside the `subroutine` scope.
- Every CCPP scheme subroutine argument must have a corresponding entry in the companion `.meta` file with a valid standard name. A mismatch between the Fortran arguments and the `.meta` entries is the most common source of capgen errors.
- Constituent registration (declaring advected or non-advected constituents) happens in the `_register` phase, not `_init`. The `_register` phase runs before `_init` and before constituent indices are available.

## PR target branches

- **CAM-SIMA:** PRs target the `development` branch. The `main` branch is empty.
- **atmospheric_physics:** PRs target the `main` branch. The `development` branch is deprecated.
- New branches must be made under personal forks, never directly under `ESCOMP`.

## CIME build system, not CMake

CAM-SIMA uses **CIME** (not CMake, not plain Make). A full build requires an NCAR cluster (Derecho or Izumi) with configured environment modules, ESMF/NUOPC, PIO, and a CIME case directory. **You cannot produce a working executable on a laptop.** Design the verify loop accordingly: see [Testing](#testing).

If the user tells you they are running on Derecho or Izumi, a full `./case.build` is available; otherwise assume it is not.

CAM-SIMA compiles every `.F90` file in every directory listed in a generated `Filepath` text file. **Adding a new Fortran file in an existing source directory compiles it automatically** — no build-system file to update. New scheme directories under `schemes/` are discovered by capgen via `.meta` files.

## Auto-generated code by the CAM-SIMA host model or CCPP framework

These files live under `$CASE/bld/atm/obj/` and are regenerated every build and should not be edited directly, and may not be present in a development copy:

| Generated file | Generator | Modify instead |
|-|-|-|
| `physics_types.F90`, `.meta` | `src/data/generate_registry_data.py` | `src/data/registry.xml` |
| `cam_ccpp_cap.F90`, `ccpp_<suite>_cap.F90` | CCPP capgen | `.meta` files, SDFs |
| `physics_inputs.F90`, `phys_vars_init_check.F90` | `src/data/write_init_files.py` | `registry.xml`, `.meta` files |
| Scheme namelist readers | `cime_config/create_readnl_files.py` | `<scheme>_namelist.xml` |
| `ccpp_datatable.xml` | CCPP capgen | `.meta` files |

## Testing

### What you can run locally

|  | Command | Notes |
|-|-|-|
| CAM-SIMA Python unit tests | `pytest test/unit/` | Fast; covers build scripts |
| CAM-SIMA pylint | `pylint --rcfile=test/.pylintrc <files>` | Must score ≥ 9.5 |
| CAM-SIMA Fortran unit tests | `cmake -S test/unit/fortran -B build/unit-tests -DCAM_SIMA_ENABLE_TESTS=ON -DCMAKE_PREFIX_PATH=<pfunit>/build/installed && cd build/unit-tests && make && ctest` | Requires pFUnit and OpenMP C support (Linux/HPC; macOS default clang lacks OpenMP C) |
| atmospheric_physics pFUnit | `cmake -DCMAKE_PREFIX_PATH=<pfunit>/build/installed -DATMOSPHERIC_PHYSICS_ENABLE_TESTS=ON -S ./test/unit-test -B ./build && cd build && make && ctest -V` | |

pFUnit tests live at `test/unit-test/tests/<path>/test_<module>.pf` mirroring the scheme location.

### What you cannot run (CIME regression tests)

- Defined by `cime_config/testdefs/testlist_cam.xml`
- Run **only on Derecho or Izumi** (NCAR clusters, human-accessible)
- Full suite runs dozens of science configurations for a few timesteps / simulated hours and checks for bit-for-bit reproducibility
- Takes approximately 5 hours for CAM and an hour for SIMA, so it is not a lightweight tool and should be used sparingly, when work is substantially complete.

If you are running on Derecho/Izumi with the user's permission, regression tests become accessible; otherwise stop at unit tests and hand off.

### Snapshot testing and null dycore

Snapshot testing validates that a CCPP-ported physics parameterization in CAM-SIMA produces bit-for-bit identical results to the original CAM implementation. The workflow has two sides: CAM generates snapshot files, and CAM-SIMA consumes them via the null dycore.

#### How it works

1. **CAM generates snapshots.** A CAM run captures the physics state (state variables, constituents, physics buffer fields) into NetCDF files at specific points in the physics timestep. Each parameterization being tested gets a "before" and "after" snapshot - the state immediately before and immediately after the parameterization runs.
2. **CAM-SIMA reads the "before" snapshot.** A CAM-SIMA case is created with `CAM_CONFIG_OPTS="--dycore none --physics-suites test_sdf_name"`. The null dycore reads `ncdata` (the "before" snapshot) to initialize CAM-SIMA grid and state, matching the snapshot's resolution.
3. **CAM-SIMA runs the test suite.** A test SDF specifies only the parameterization(s) being validated. CAM-SIMA runs this suite starting from the "before" state.
4. **CAM-SIMA compares against the "after" snapshot.** The `physics_check_data` routine (in `src/physics/utils/physics_data.F90`) compares the resulting state against the "after" snapshot file specified by `ncdata_check`. Comparison uses absolute differences when values are small and relative differences otherwise, controlled by `min_difference` and `min_relative_value` thresholds.
5. **State is refreshed every timestep.** The null dycore re-reads state from `ncdata` at each timestep — there is no prognostic integration or continuity of state across timesteps. Each timestep is an independent before→run→compare cycle.

#### Key configuration

| Setting | Where | Purpose |
|-|-|-|
| `ncdata` | `user_nl_cam` | Path to the "before" snapshot file |
| `ncdata_check` | `user_nl_cam` | Path to the "after" snapshot file; **its presence triggers snapshot comparison mode** |
| `ic_file_input_names` | `src/data/registry.xml` | Maps registry variable standard names to the variable names used in the snapshot NetCDF file (e.g., mapping to `"T"` for temperature). Required for any state variable (including constituents) that needs to be read from the snapshot |
| `cam_take_snapshot_before` / `cam_take_snapshot_after` | CAM `user_nl_cam` | Name of the parameterization to snapshot (e.g., `"radiation_tend"`, `"convect_deep_tend"`, or `"user_set"` for custom placement) |
| `cam_snapshot_before_num` / `cam_snapshot_after_num` | CAM `user_nl_cam` | History tape number to write the before/after snapshot data to |

#### Null dycore (dyn/none) is not exclusively for snapshots

The null dycore is designed as a general-purpose physics testbed. Do not assume it is always running snapshot tests. The presence of a non-empty `ncdata_check` is what activates the snapshot comparison pathway (checked by `physics_data_init`: `if (len_trim(ncdata_check) > 0)`). Currently, only CAM generates snapshots. In the future, CAM-SIMA would generate and consume its own snapshot files.

#### Caveats

- **False passes from unapplied tendencies.** The "after" comparison only checks state variables that were *mutated* during the test suite. If tendencies are computed but never applied to state, the variable appears unchanged and is skipped — producing a false PASS. Set `debug_output` to a level `>= DEBUGOUT_INFO (1)` to enable verbose mode, which prints exactly which variables were compared and their average values.
- **Scalar variables.** Scalar (non-array) variables cannot be read from the snapshot file. They must be initialized directly in the scheme (`intent(out)`) or marked `access="protected"` in the registry.
- **First timestep is skipped.** CAM does not write snapshot data on the first model timestep (fields may be incomplete). Snapshot comparison in CAM-SIMA also accounts for this.

More details in the CAM-SIMA docs: `docs/conversion/create-snapshots.md` and `docs/conversion/run-cam-sima.md`.


