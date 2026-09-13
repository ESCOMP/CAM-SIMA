# CAM-SIMA and atmospheric_physics AI agent guidance

This file applies to work in the **CAM-SIMA** ("host model") and **atmospheric_physics** (CCPP-compliant physics schemes, submoduled at `src/physics/ncar_ccpp`). atmospheric_physics is submoduled into CAM-SIMA but must remain portable — it contains the science codebase for physics parameterizations and is also shared with **CAM**, the production atmospheric component of CESM.

Full conversion and usage documentation is hosted at https://escomp.github.io/CAM-SIMA-docs/ (separate `CAM-SIMA-docs` repo; not checked out alongside the code). Scheme-side conventions and portability rules live in atmospheric_physics' own `AGENTS.md` (at `src/physics/ncar_ccpp/AGENTS.md` when the submodule is checked out).

## If you remember nothing else

1. **Never hand-edit generated files**. Modify the generator inputs instead (see [the table below](#auto-generated-code-by-the-cam-sima-host-model-or-ccpp-framework)).
2. **Code under `src/physics/ncar_ccpp` stays portable**: no host-model `use` statements in schemes.
3. **Every PR states its answer impact.** The default for ports, refactors, and cleanups is bit-for-bit: do not reorder floating-point arithmetic, change parenthesization, or "simplify" expressions as a side effect of other work.
4. **Never invent CCPP standard names silently** — reuse blessed names and flag any new name in the PR (see [Standard names](#ccpp-standard-names)).
5. **Full builds and regression tests run only on NCAR clusters** (Derecho/Izumi). Unless the user says they are on one, verify with unit tests and hand off.

## AI disclosure

CAM-SIMA and atmospheric_physics **requires** disclosure of AI-assisted contributions:

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
- Located at `ESCOMP/atmospheric_physics` and is a submodule that would be checked out under `CAM-SIMA/src/physics/ncar_ccpp`. The default branch is `main`.
- Contains CCPP-compliant, "portable" (i.e., free of host model code) physics parameterizations.
- May `use` `ccpp_kinds`, `ccpp_constituent_prop_mod`, and intra-package dependencies only.
- **Exception:** `schemes/sima_diagnostics/` is a non-portable directory centralizing CAM-SIMA-specific history output. Code outside `sima_diagnostics/` that calls `cam_history` routines is a bug.
- If a scheme under `atmospheric_physics` depends on a CAM-SIMA host side quantity, it receives it as a subroutine argument threaded through via standard names, not via a `use` statement to a CAM module.
- Is intended to be portable to any CCPP-enabled host model.
- Scheme conventions, portability rules, and scheme-side exemplars: see its own `AGENTS.md` and `Code-style.md` (the latter applies to CAM-SIMA host Fortran as well).

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
* Scheme-side exemplars (minimal scheme, namelist XML, diagnostics, SDF locations): see atmospheric_physics `AGENTS.md`.

## CCPP standard names

The standard name is the interface: the CCPP framework connects host variables and scheme arguments by exact standard-name match, and every entry in a `.meta` file (host or scheme) requires one.

- **Reuse before coining.** Search `src/data/registry.xml` and recently converted schemes under `src/physics/ncar_ccpp` for current, blessed usage. The official dictionary ([ESCOMP/CCPPStandardNames](https://github.com/ESCOMP/CCPPStandardNames)) is authoritative but lags current usage.
- If a new name is unavoidable, follow the naming patterns of existing names and **flag it explicitly in the PR description** for CAM SE review. Units must be consistent with existing usage.
- The horizontal dimension standard name depends on phase: `horizontal_dimension` in non-`run` phases (and in host-side metadata), `horizontal_loop_extent` in the `run` phase.
- Vertical dimensions: `vertical_layer_dimension` (layers) and `vertical_interface_dimension` (interfaces).
- `errmsg`/`errflg` are `ccpp_error_message`/`ccpp_error_code`.

## Code style

Fortran style rules (which apply to host code too) are distilled in atmospheric_physics `Code-style.md`; the full standards are at https://escomp.github.io/CAM-SIMA-docs/development/cam-coding-standards/. Python must pass pylint (see [Testing](#testing)) and follow the black code style.

## Variable persistence: registry vs CCPP framework

- A quantity that must **persist across timesteps**, or be **read from an input file** (including snapshots), must be declared in `src/data/registry.xml`.
- A quantity needed only **within a single timestep** (e.g., passed from one scheme to a later one) needs no registry entry — the CCPP framework allocates and threads it automatically, as long as the producing and consuming schemes use matching standard names.

## PRs and commits

- **CAM-SIMA** PRs target `development`; **atmospheric_physics** PRs target `main` (its `development` branch is deprecated). Branch from a personal fork — never push branches under `ESCOMP`.
- Feature branches are squash-merged into the target branch. Do not add `simaX_YY_ZZZ` version tags to commit subjects (as seen in `git log`); those are applied only at release time, and only when regression tests change answers.
- **Every PR states whether it changes answers.** Bit-for-bit is the default expectation for ports, refactors, and cleanups — and it is what snapshot and regression testing verify. An intentional answer change needs scientific justification in the PR; an accidental one (reordered arithmetic, changed parenthesization, a "simplified" expression) is a bug.

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

atmospheric_physics scheme unit tests (pFUnit) run from that repo — see its `AGENTS.md`.

Some Python tests (e.g., `test/unit/python/test_write_init_files.py`) compare generated Fortran against reference files with `filecmp` and will fail whenever generator output legitimately changes. To update: run the test, inspect the newly generated files under `test/unit/python/tmp/` to verify they are correct, then copy them over the corresponding references in `test/unit/python/sample_files/`.

### What you cannot run (CIME regression tests)

- Defined by `cime_config/testdefs/testlist_cam.xml`
- Run **only on Derecho or Izumi** (NCAR clusters, human-accessible)
- Full suite runs dozens of science configurations for a few timesteps / simulated hours and checks for bit-for-bit reproducibility
- Takes approximately 5 hours for CAM and an hour for SIMA, so it is not a lightweight tool and should be used sparingly, when work is substantially complete.

If you are running on Derecho/Izumi with the user's permission, regression tests become accessible; otherwise stop at unit tests and hand off.

### Snapshot testing and null dycore

Snapshot testing validates that a CCPP-ported parameterization in CAM-SIMA produces bit-for-bit identical results to the original CAM implementation. CAM writes "before" and "after" snapshots (state, constituents, physics buffer) bracketing the parameterization; CAM-SIMA initializes from "before" via the null dycore, runs a test SDF containing only that parameterization, and compares the result against "after". Comparison uses absolute differences for small values and relative differences otherwise (`min_difference`, `min_relative_value` thresholds).

The null dycore (`dyn/none`) re-reads state from `ncdata` every timestep. There is no prognostic integration, so each timestep is an independent before→run→compare cycle.

Note: The null dycore is a general-purpose physics testbed and not snapshot-specific; the snapshot-comparison path activates only when `ncdata_check` is set. Currently only CAM generates snapshots; CAM-SIMA may generate its own in the future.

#### Key configuration

| Setting | Where | Purpose |
|-|-|-|
| `ncdata` | `user_nl_cam` | Path to the "before" snapshot file |
| `ncdata_check` | `user_nl_cam` | Path to the "after" snapshot file; **its presence triggers snapshot comparison mode** |
| `ic_file_input_names` | `src/data/registry.xml` | Maps registry standard names to the variable names in the snapshot NetCDF (e.g., `"T"` for temperature). Required for any state variable (including constituents) read from the snapshot |
| `cam_take_snapshot_before` / `cam_take_snapshot_after` | CAM `user_nl_cam` | Parameterization to snapshot (e.g., `"radiation_tend"`, or `"user_set"` for custom placement) |
| `cam_snapshot_before_num` / `cam_snapshot_after_num` | CAM `user_nl_cam` | History tape number for the before/after snapshot data |

#### Caveats

- **False passes from unapplied tendencies.** The "after" comparison only checks state variables that were *mutated* during the test suite. If tendencies are computed but never applied to state, the variable appears unchanged and is skipped, leading to a false positive. Raise `debug_output` to `>= DEBUGOUT_INFO (1)` to print exactly which variables were compared and their average values.
- **Scalar variables.** Scalar (non-array) variables cannot be read from the snapshot file. Initialize them directly in the scheme (`intent(out)`) or mark them `access="protected"` in the registry.
- **First timestep is skipped.** CAM does not write snapshot data on the first model timestep (fields may be incomplete); CAM-SIMA's comparison accounts for this.

Full mechanics and walkthrough: the conversion guide at https://escomp.github.io/CAM-SIMA-docs/ (see `create-snapshots` and `run-cam-sima`).


