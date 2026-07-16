JointHistogramMutualInformation Metric Correction — Downstream Impact
====================================================================

This report documents a behavior change in
`itk::JointHistogramMutualInformationImageToImageMetricv4` introduced by
[PR #6569](https://github.com/InsightSoftwareConsortium/ITK/pull/6569), and
the downstream consumers whose numerical output — and stored regression
baselines — change as a result.

It is a companion to the [ITK v6 Migration Guide](./itk_6_migration_guide.md):
these are *correctness* fixes, but registration results computed with the
affected metric change (they move toward the true optimum), and any test that
pins the old numbers must be re-baselined. One protected API is removed:
`JointHistogramMutualInformationGetValueAndDerivativeThreader::ComputeFixedImageMarginalPDFDerivative`
(and its per-thread `FixedImageMarginalPDFInterpolator`) had no remaining
callers after the marginal-pairing fix; subclasses that called it should use
`ComputeMovingImageMarginalPDFDerivative` against the correct marginal.


Summary
-------

Three correctness defects are corrected (the related
`ScaleLogarithmicTransform` Jacobian fix was split out to
[PR #6572](https://github.com/InsightSoftwareConsortium/ITK/pull/6572)):

1. **`JointHistogramMutualInformation` marginal PDFs were swapped.**
   The joint PDF is indexed `(fixed bin, moving bin)`. Summing over the fixed
   axis yields the *moving* marginal, but that result was stored into
   `m_FixedImageMarginalPDF` (and vice-versa). `GetValue()` then divided
   `p(a,b)` by `p_moving(a) * p_fixed(b)`. The value is therefore wrong for any
   image pair whose two intensity marginals differ; it is (nearly) invisible
   when the two marginals are the same, e.g. registering an image to a shifted
   or rotated copy of itself.

2. **The metric derivative was not an MI gradient.**
   The per-sample weight is now the standard Parzen-window (Viola & Wells)
   form `(dJPDF/J - dMmPDF/Pm) / log(2)`, replacing a weight that mixed a
   log-ratio-weighted joint term with a wrong-signed marginal term. Because the
   derivative reads the *moving* marginal, fix (2) depends on fix (1): the two
   must land together. On the ITK registration test the corrected derivative
   converges to within &lt;1% of the true shift versus ~35% for the old formula,
   and its direction agrees with the independently-correct Mattes metric
   (cosine 0.95–0.99).

3. **The derivative was not unit-consistent and could spike on sparse bins.**
   `ComputeJointPDFPoint()` maps intensities to a `[0,1]` bin axis, so the
   per-sample gradient needs the chain-rule factor
   `d(bin)/d(intensity) = 1/(TrueMax - TrueMin)`. With it, `GetDerivative()`
   matches a central finite difference of `GetValue()` and no longer scales
   with the moving image's intensity range (previously an 8-bit vs 16-bit
   encoding of the same data produced a 256x different derivative). Each
   log-slope term is additionally clamped to `|ln(eps)|/binwidth` — the
   steepest slope the `eps`-guarded PDF can represent across one bin — which
   never engages on well-posed data and only bounds the worst-case per-sample
   weight.

Fixes (1) and (2) are coupled and change the JointHistogram MI metric's value
**and** gradient; fix (3) further rescales the gradient.


Derivative magnitude and learning rates
---------------------------------------

The corrected, unit-consistent derivative differs in magnitude from the old
formula — typically by orders of magnitude, depending on the moving image's
intensity range.

- Pipelines using a **scales estimator** (e.g.
  `RegistrationParameterScalesFromPhysicalShift`, the
  `ImageRegistrationMethodv4` default, or SimpleITK's
  `SetOptimizerScalesFromPhysicalShift()` +
  `EstimateLearningRate`) absorb the rescale automatically — ITK's own
  registration tests converge to results within existing tolerances on all
  platforms except one float configuration.
- Pipelines with a **hand-tuned fixed learning rate** and no scales estimator
  must re-tune it: the old rate was calibrated against a derivative that was
  neither a gradient nor unit-consistent. Prefer switching to a scales
  estimator over re-tuning a constant.
- The derivative remains the gradient of the per-sample average estimator; it
  can differ from a finite difference of the plug-in histogram sum reported by
  `GetValue()` by the Parzen-smoothing mismatch (measured 0.68x–1.03x across
  histogram configurations). Direction is exact.

Who is affected
---------------

Only workflows that **explicitly select the JointHistogram MI metric** change.
The Mattes MI metric (`itk::MattesMutualInformationImageToImageMetricv4`),
correlation, mean-squares, and demons metrics are untouched. No public
signature changes, so all downstream code compiles unchanged.

### ITK

| Test | Nature | Status |
|------|--------|--------|
| `itkSimpleImageRegistrationTestFloat` / `Double` | baseline-image compare | **Baselines regenerated in PR #6569** (primary double, `.1` float, `.2` x86_64 float). The pre-fix `.3` variant is intentionally dropped: it encoded a platform output of the incorrect metric that no corrected build reproduces; the three regenerated variants cover the CI matrix. |
| `itkJointHistogramMutualInformationImageToImageMetricv4Test` | value/derivative consistency + finite-difference gradient check (hardened in PR #6569; failures now fail the test) | Passes. |
| `itkImageToImageMetricv4RegistrationTest`, `…Test2`, `itkSimpleImageRegistrationTest2/3`, `…WithMaskAndSampling`, `itkQuasiNewtonOptimizerv4RegistrationTest`, `itkMultiStartImageToImageMetricv4RegistrationTest` | convergence / no-exception | Pass (convergence improves). |

### SimpleITK

`SetMetricAsJointHistogramMutualInformation()` maps directly to the affected
class.

- `Testing/Unit/sitkImageRegistrationMethodTests.cxx` pins a metric value:
  `EXPECT_NEAR(-0.52624100016564002, MetricEvaluate, 2e-6)` for
  `SetMetricAsJointHistogramMutualInformation(20, 1.5)`. The pair is asymmetric
  (its JHMI value differs from its Mattes value), so the marginal fix moves the
  value past the `2e-6` tolerance. **The expected constant must be updated.**
- The `ImageRegistrationMethod2`, `ImageRegistrationMethodBSpline3`, and
  `ImageRegistrationMethodDisplacement1` examples (C++/Python/Java/C#/R) are the
  canonical JHMI examples; their final metric/transform shifts.
- Requires a follow-up SimpleITK change after this lands in ITK.

### BRAINSTools (BRAINSFit `--costMetric MIH`)

Three baseline-comparing tests in `BRAINSFit/TestSuite/CMakeLists.txt`:

- `BRAINSFitTest_MIHAffineRotationMasks`
- `BRAINSFitTest_MIHScaleSkewVersorRotationMasks`
- `BRAINSFitTest_MIHMetricBrainToItself`

These are mono-modal (brain to a rotated brain / to itself), so the two
marginals are nearly identical and fix (2) barely moves them; fix (3) may still
shift the converged transform. Run them against a before/after ITK to see
whether they breach tolerance. The BRAINSFit default `MMI` (Mattes) and the
`NMIH` metric (which uses the v3
`itk::NormalizedMutualInformationHistogramImageToImageMetric`, a different
class) are **not** affected.

### ANTs (`antsRegistration --metric MI[...]`)

In `itkantsRegistrationHelper.hxx` the `MI` metric enum builds the affected
JointHistogram class (the separate `Mattes` metric does not). However:

- ANTs' own CTest regression suite exercises only the **legacy `ANTS`
  binary** (`ANTS_MI_1/2_test.cmake`, `COMMAND ANTS … -m MI[…] -r Gauss`),
  which uses ANTs' *internal* MI implementation, not the ITK class — **not
  affected**. No `antsRegistration`/`antsAI`/`MeasureImageSimilarity`
  exact-baseline test was found.
- The user-facing impact is in the pipeline scripts that call
  `antsRegistration MI` — `antsRegistrationSyNQuick.sh`,
  `antsBrainExtraction.sh`, `antsCorticalThickness.sh`,
  `antsMultivariateTemplateConstruction2.sh`, `antsJointLabelFusion2.sh` — whose
  outputs change (converging closer to the true alignment). These are not
  pinned baselines, so no CI turns red, but results drift for users.

### Not affected

- **elastix / ITKElastix** — no use of the ITK JointHistogram metric (their MI
  is `AdvancedMattesMutualInformation`).
- Non-registration toolkits (teem, VTK, DCMTK, …) — no metric consumers.
- `ScaleLogarithmicTransform` — its Jacobian fix moved to
  [PR #6572](https://github.com/InsightSoftwareConsortium/ITK/pull/6572); no
  forest consumer optimizes it (BRAINSTools references it only for
  transform-file I/O).


Re-baselining checklist
-----------------------

| Project | Item | Action |
|---------|------|--------|
| ITK | `itkSimpleImageRegistrationTest{Float,Double}` baselines | Done in PR #6569 (`.nii` content links regenerated, blobs in ITKTestingData). |
| SimpleITK | `sitkImageRegistrationMethodTests.cxx` JHMI constant | Recompute `MetricEvaluate` and update the expected value. |
| SimpleITK | `ImageRegistrationMethod2` / `BSpline3` / `Displacement1` example outputs | Refresh any pinned example results. |
| BRAINSTools | `BRAINSFitTest_MIH{AffineRotationMasks,ScaleSkewVersorRotationMasks,MetricBrainToItself}` | Run before/after; regenerate `*.result.nii.gz` if tolerance is breached. |
| ANTs | pipeline-script outputs (no pinned baselines) | Optional smoke comparison to quantify the (beneficial) drift. |


Verification notes
------------------

The correction moves results **toward** the true optimum, not away — the
corrected derivative agrees in direction with ITK's independently-correct
Mattes metric, and on the tested pairs the metric value's argmin is essentially
unchanged (the marginal swap offsets the value by a term that varies only
weakly with alignment). Downstream
baselines that shift are encoding the *previous, incorrect* behavior and should
be regenerated rather than treated as regressions.
