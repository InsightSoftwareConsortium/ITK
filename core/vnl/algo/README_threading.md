# vnl_algo — threading and reentrancy guarantees

This document inventories which `vnl_algo` solver paths are safe to
call concurrently from multiple threads and which require external
serialization. It accompanies the partial fixes for issue #22.

## Summary

- **Dense linear-algebra paths (`vnl_svd`, `vnl_qr`, `vnl_cholesky`,
  `vnl_lu_decomp`, `vnl_matrix_inverse`, `vnl_determinant`, ...) are
  effectively reentrant.** They call into f2c-translated LAPACK/BLAS
  routines (`dgetrf_`, `dgemm_`, etc.) whose only file-scope `static`
  storage is read-only constants emitted by f2c for FORTRAN's pass-by-
  reference convention. Two threads can call these paths concurrently
  on independent matrices without corruption.

- **Iterative solver paths (`vnl_sparse_symmetric_eigensystem`,
  `vnl_lbfgsb`) are internally serialized as of this version.** Each
  carries an internal `std::mutex` that holds for the duration of one
  `CalculateNPairs` / `minimize` invocation. Multi-threaded callers
  observe correct results, but at the cost that two threads cannot
  actually solve different systems in parallel — the second blocks
  until the first finishes.

- **Singletons in `vil_file_format` and `vsl_binary_loader_base` are
  internally synchronized.** Concurrent registration of formats /
  binary loaders from multiple threads is safe.

- **Dense LAPACK error reporting (`xerbla_`)** prints to stderr from
  whichever thread the error originated in. Concurrent errors may
  produce interleaved output, but no state is corrupted.

## Why iterative solvers need a band-aid mutex

vxl bundles netlib's f2c-translated LAPACK + ARPACK + L-BFGS-B in
`v3p/netlib/`. f2c translates FORTRAN's `SAVE` keyword (which makes
local variables persist across subroutine calls) by emitting C
file-scope `static` storage. For dense LAPACK routines this storage is
exclusively read-only constants (e.g., `static integer c__1 = 1;` to
provide an addressable `1` for FORTRAN's pass-by-reference
convention), so it is never written and is naturally thread-safe.

For *iterative* solvers, however, the SAVE'd state is genuinely
mutable — it carries the solver's internal history (Lanczos vectors,
limited-memory L-BFGS-B updates, reverse-communication state machine
positions) across the user's reverse-communication callback
boundaries. Two threads invoking the solver concurrently corrupt this
shared mutable state.

The proper fix is to regenerate `v3p/netlib/` from a thread-safe
LAPACK/ARPACK source — specifically a version that places the SAVE'd
locals in a per-call workspace passed by the caller, rather than in
file-scope state. This is tracked as **issue #23** ("upgrade netlib
to use LAPACK instead of LINPACK") and is a multi-month structural
project.

Until #23 lands, the band-aid is a per-solver-class `std::mutex`
inside the C++ wrapper. See:

- `core/vnl/algo/vnl_lbfgsb.cxx` — `lbfgsb_call_mutex()`
- `core/vnl/algo/vnl_sparse_symmetric_eigensystem.cxx` — `sse_call_mutex()`

## Solver-by-solver inventory

| Class | f2c routine | Reentrant on master? | Notes |
|---|---|---|---|
| `vnl_svd` | `dgesvd_` | yes | dense LAPACK; only read-only f2c constants |
| `vnl_qr` | `dgeqrf_`, `dorgqr_` | yes | dense LAPACK |
| `vnl_cholesky` | `dpotrf_` | yes | dense LAPACK |
| `vnl_lu_decomp` | `dgetrf_`, `dgetrs_` | yes | dense LAPACK |
| `vnl_matrix_inverse` | dispatches via `vnl_svd` | yes | inherits |
| `vnl_determinant` | `dgetrf_` | yes | dense LAPACK |
| `vnl_real_eigensystem` | `dgeev_` | yes | dense LAPACK |
| `vnl_symmetric_eigensystem` | `dsyev_` | yes | dense LAPACK |
| `vnl_generalized_eigensystem` | `dggev_` | yes | dense LAPACK |
| `vnl_levenberg_marquardt` | `lmder_`/`lmdif_` (MINPACK) | yes | passes workspace; no SAVE'd state |
| `vnl_powell` | local impl (no f2c) | yes | pure C++ |
| `vnl_amoeba` | local impl (no f2c) | yes | pure C++ |
| `vnl_conjugate_gradient` | local impl (no f2c) | yes | pure C++ |
| **`vnl_lbfgsb`** | `setulb_` (L-BFGS-B) | **internally serialized** | per-class mutex |
| **`vnl_sparse_symmetric_eigensystem`** | `dnlaso_`/`dsaupd_`/`dseupd_` (LASO/ARPACK) | **internally serialized** | per-class mutex; also the static `current_system` reverse-communication shim |
| `vnl_lbfgs` | `lbfgs_` (older LBFGS) | unknown — needs audit | likely also non-reentrant; call site is `vnl_lbfgs.cxx` |
| `vnl_brent_minimizer` | `dbrent_` | likely yes (no SAVE'd state in routine) | needs verification |

Items marked "likely yes" / "unknown" are candidates for a follow-up
audit but have not been observed to fail in practice in vxl's CI.

## Recommendations for callers

- **Dense linear algebra**: call freely from multiple threads on
  independent inputs. No external locking needed.

- **Iterative solvers**: vxl handles the locking internally as of this
  release. Callers that previously serialised solvers externally can
  remove that locking. Callers that did *not* serialise but were
  observing intermittent corruption should now be correct.

- **For maximum throughput on iterative solvers**: spawn one solver
  instance per thread is no longer sufficient (the mutex is per-class,
  not per-instance). Truly parallel iterative solving requires either
  fixing #23 (regenerate netlib) or using a different library
  (Eigen's iterative module, MKL, Spectra) outside vnl_algo.

- **Mixed workloads**: a thread doing `vnl_svd` and another thread
  doing `vnl_sparse_symmetric_eigensystem` do not contend; the
  iterative-solver mutex is local to its file.

## Static state outside `vnl_algo`

For completeness, other singletons in vxl that have been made thread-safe:

- `vil_file_format::all()` / `add_file_format` — see
  `core/vil/vil_file_format.cxx`.
- `vsl_binary_loader_base::register_this()` /
  `vsl_register_new_loader_clear_func()` / `vsl_delete_all_loaders()` —
  see `core/vsl/vsl_binary_loader_base.cxx`.
- `vsl_indent` per-stream indent map — see `core/vsl/vsl_indent.cxx`.
- `vbl_ref_count` reference counting uses atomic operations via
  `vcl_atomic_count.h`.

## Background

Issue #22 was filed in 2015 with three concerns:

1. f2c-generated `static` state in `v3p/netlib`.
2. The `vil` image-loader singleton.
3. The `vsl` binary-loader singleton.

The 2015 description grouped all three as broadly "non-thread-safe".
Subsequent investigation (2026) refined this:

- **(1)** is overwhelmingly read-only constants; only iterative solver
  paths carry mutable SAVE'd state. Those paths are now serialised
  internally (band-aid) pending #23.
- **(2)** and **(3)** were genuine TOCTOU and unsynchronised-mutation
  races, fixed by adding internal mutexes to the registration paths.
- A fourth concern not in the original issue — `vsl_indent`'s per-
  stream indent map — was also fixed in the same effort.
