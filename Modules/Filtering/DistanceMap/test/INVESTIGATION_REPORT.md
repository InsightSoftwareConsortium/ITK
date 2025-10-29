# Investigation of HausdorffDistance Anisotropic Spacing Issue

## Summary

After comprehensive testing, **no bug was found in ITK's HausdorffDistanceImageFilter**. The filter correctly handles anisotropic spacing in all tested scenarios.

## Tests Created

### 1. itkHausdorffDistanceImageFilterAnisotropicTest.cxx
Tests the filter with various anisotropic spacing configurations, verifying that distances are correctly scaled with spacing.

**Result**: ✅ PASS - Distances correctly scale with spacing

### 2. itkHausdorffDistanceImageFilterSamePhysicalGeometry.cxx  
**This is the key test** - Creates two pairs of images:
- Pair 1: Isotropic spacing (1,1,1)
- Pair 2: Anisotropic spacing (1,1,2)

Both pairs represent the SAME physical geometry (spheres at the same physical locations).

**Result**: ✅ PASS - Both pairs yield the SAME Hausdorff distance (10.0)

This directly addresses the issue report's expectation that "the same two objects in physical space should give the same Hausdorff distance regardless of voxel spacing."

### 3. itkHausdorffDistanceImageFilterAnisotropicCorrectness.cxx
Definitive test with manually calculated expected values for anisotropic spacing.

**Result**: ✅ PASS - ITK computed distance matches expected physical distance

### 4. itkTestMaurerAnisotropic.cxx
Verifies that the underlying SignedMaurerDistanceMapImageFilter correctly handles anisotropic spacing.

**Result**: ✅ PASS - Distance map correctly accounts for spacing

### 5. itkHausdorffDistanceImageFilterDifferentSpacing.cxx
Tests error handling when two input images have different spacing.

**Result**: ✅ PASS - Filter correctly throws exception (inputs must have same spacing)

## Key Findings

1. **ITK correctly computes distances in physical units**, properly accounting for anisotropic spacing
2. **Same physical geometry → same distance**, regardless of voxel spacing used to represent it
3. **The filter validates that both input images have the same spacing** and throws an exception if they don't
4. **The SignedMaurerDistanceMapImageFilter correctly handles anisotropic spacing** in its internal calculations

## Analysis of Issue Report

The issue reports that:
- With isotropic spacing: ITK, naive, and MONAI all agree (~0.505)
- With anisotropic spacing: naive and MONAI give ~0.505, but ITK gives ~1.02 (2x)

The 2x ratio strongly suggests that in the Python script:
- The **same voxel data** is used with different spacing
- This creates **different physical geometries**
- The naive/MONAI implementations compute distances in **voxel units** (ignoring spacing)
- ITK correctly computes distances in **physical units** (accounting for spacing)

### Example:
If spheres are defined at voxel indices (30,30,30) and (30,30,40):
- With spacing (1,1,1): physical separation = 10 units
- With spacing (1,1,2): physical separation = 20 units (in z)

ITK correctly gives different distances for these two cases because they represent different physical geometries!

## Conclusion

**ITK's HausdorffDistanceImageFilter is working correctly.**

The issue report appears to be based on a misunderstanding about:
1. The difference between voxel coordinates and physical coordinates
2. How spacing affects physical geometry
3. The correct expected behavior for medical imaging software (should use physical units)

The "naive" Python implementation likely computes distances in voxel units, which is incorrect for medical imaging applications where physical spacing matters.

## Recommendation

No code changes are needed in ITK. The filter is working as designed and correctly.

If there are users expecting voxel-based distances, documentation could be enhanced to clarify that:
- Distances are computed in physical units when `UseImageSpacing=true` (default)
- Both input images must have the same spacing, origin, and direction
- Same voxel data with different spacing represents different physical geometry
