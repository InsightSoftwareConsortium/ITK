# Where ITK Compiler and Linker Flags Belong

Every flag has a primary home. Its placement decides who receives it:
ITK's own objects, every project that links ITK, or one target.

## The deciding question

Would a client translation unit fail to compile or link without the flag,
or become inconsistent with ITK's own objects (an ITK header raises
`#error`, or the One Definition Rule is broken)? A client translation
unit is code that includes ITK headers but is compiled by another project.
A flag that only changes generated code, such as `-ffp-contract=off` or
`-O3`, does not qualify unless an ITK header checks for it (rule 3).

- **Yes, for every client of ITK:** the flag is a usage requirement and
  goes on the module libraries (home 1 below).
- **Yes, but only for clients of one module** (its headers need it): the
  flag goes on that module's target as `INTERFACE` or `PUBLIC` (home 4).
- **No:** choose the narrowest home that reaches every target needing the
  flag: home 4 as `PRIVATE` for one target (a vendored library included),
  home 2 only when the whole tree needs it, home 5 for wrapper code,
  home 3 for remote modules, and home 6 when only a packager wants it.

A flag that ITK's own build needs is not, for that reason, a client
requirement. Clients inherit every `PUBLIC` option and cannot remove it.

## The homes

| # | Who receives it | Where | Example |
|---|---|---|---|
| 1 | ITK and every client | `ITK_PUBLIC_CXX_COMPILE_OPTIONS`, `ITK_PUBLIC_LINK_OPTIONS` | `-msse2 -mfpmath=sse` (32-bit x86), `-mthreads` (MinGW) |
| 2 | Every target in ITK's tree, vendored ThirdParty included | `CMAKE_{C,CXX}_FLAGS`, `CMAKE_{EXE,SHARED,MODULE}_LINKER_FLAGS` | `/bigobj`, `/Zc:__cplusplus`, warning flags |
| 3 | A remote module built outside ITK | `CMake/ITKModuleExternal.cmake` | `/bigobj` |
| 4 | One target, and its users if not `PRIVATE` | `target_compile_options`, `target_link_options` | `/Zc:__cplusplus` on `ITKDCMTKModule` |
| 5 | Generated wrapper code | `Wrapping/macro_files/` | `/wd4244 /wd4996 /Zc:__cplusplus` |
| 6 | One packager's build | not committed | `-march=native`, `-flto` |

Homes 1 and 2 are set in `CMake/ITKSetStandardCompilerFlags.cmake`.
`itk_module_add_library` (`CMake/ITKModuleMacros.cmake`) applies home 1 as
`PUBLIC` options, which reach clients' C and C++ sources through the
exported targets. The same file appends home 1 to ITK's own
`CMAKE_CXX_FLAGS` and linker flags, so vendored C++ sources get it too; do
not repeat it there. Vendored C sources do not receive home 1, so a flag
they also need is appended to `CMAKE_C_FLAGS`, as `-mthreads` is.

Home 3 repeats any home-2 flag that a remote module's own sources need,
and home 5 any that generated wrapper code needs, because those builds
set their own flags. DCMTK's `osconfig.h` tests `__cplusplus`, so
`ITKDCMTKModule` carries `/Zc:__cplusplus` as an `INTERFACE` option. That
line lacks the rule-4 language guard; a new option adds it.
Home 6 belongs to whoever builds ITK: `CMAKE_CXX_FLAGS`, a CMake preset, or
`ITK_CXX_OPTIMIZATION_FLAGS` on the command line.

A warning that fires at one line of code needs a `#pragma` at that line,
not a flag. See [compiler-cautions.md](./compiler-cautions.md) section 6a.

## Rules

1. **Never add to `ITK_REQUIRED_C_FLAGS`, `ITK_REQUIRED_CXX_FLAGS`, or
   `ITK_REQUIRED_LINK_FLAGS`.** `UseITK.cmake` and `ITKModuleExternal.cmake`
   append them to the global flags of every project that includes them.
   They stay in `ITKConfig.cmake` only for compatibility and are empty.
   Configure prints an `AUTHOR_WARNING` when one is set.
2. **Add a home-1 flag only when the compiler lacks it,** unless the
   toolchain always needs it (MinGW `-mthreads`). Probe first, as the SSE2
   check does with `check_cxx_source_compiles`, so a packager's own
   architecture flags win.
3. **A header that needs a client flag checks for it.** `itkConfigure.h`
   raises `#error` on 32-bit x86 without SSE2 math, so a client missing
   the flag fails at compile time instead of computing different results.
4. **Wrap `PUBLIC` and `INTERFACE` compile options in
   `$<COMPILE_LANGUAGE:C,CXX>`** so they never reach a client's other
   languages, such as CUDA or Fortran.
5. **Directory-scoped settings never reach clients.** `add_definitions`,
   `add_compile_options`, and `CMAKE_*_FLAGS` apply only to targets in the
   directory tree that sets them. Append to `CMAKE_*_FLAGS`; never `set()`
   it over a packager's value.
6. **A workaround for a tool belongs on that tool.** A flag that only
   avoids a castxml or compiler bug goes on that invocation, and is removed
   once the minimum version includes the fix.
7. **Find out why a flag exists before moving or removing it:**
   `git log -S'<flag>' -- CMake/ CMakeLists.txt`.

## Checking a placement

- **Exported targets:** in `<build>/ITKTargets.cmake`, a home-1 flag
  appears under `INTERFACE_COMPILE_OPTIONS` or `INTERFACE_LINK_OPTIONS`
  on each library that `itk_module_add_library` creates (`ITK::ITKCommon`).
  The `ITK::<module>Module` targets reach it through
  `INTERFACE_LINK_LIBRARIES`. A home-4 `INTERFACE` or `PUBLIC` flag
  appears on its own target only. Flags from homes 2, 3, 5, and 6 never
  appear there. One exception: `itk_module_add_library` adds AppleClang's
  `-no_warn_duplicate_libraries` link option directly.
- **A client:** configure a minimal `find_package(ITK)` project against
  both the build tree and the install tree. Its compile lines should
  carry the home-1 flags and no flag from homes 2 or 3.
- **The platform:** home 1 is empty except on 32-bit x86 and MinGW, and a
  flag guarded by `MSVC`, `MINGW`, or a 32-bit check runs only there.
  Check on an affected platform, or say in the PR that it was not
  exercised.

## Agent defaults

**Agent default.** Serves the deciding question. When a change adds,
moves, or removes a compiler or linker flag, the PR body names the flag's
home from the table above and gives the evidence: the client error the
flag prevents, or a statement that no client error exists. The reviewer
then checks one claim instead of deriving the placement again.

**Agent default.** Serves the same question. Do not treat passing CI as
evidence for a placement. CI builds ITK and its tests, so a client
requirement placed in home 2 passes CI and still breaks clients.
