/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include <iostream>
#include "itkImageRegion.h"
#include "itkFloatingPointExceptions.h"

int
itkImageRegionTest(int, char *[])
{

  constexpr unsigned int dimension = 3;

  using CoordinateType = double;
  using RegionType = itk::ImageRegion<dimension>;
  using IndexType = RegionType::IndexType;
  using SizeType = RegionType::SizeType;
  using SliceRegionType = RegionType::SliceRegion;
  using ContinuousIndexType = itk::ContinuousIndex<CoordinateType, dimension>;

  using IndexNumericTraits = itk::NumericTraits<IndexType::IndexValueType>;
  using ContinuousIndexNumericTraits = itk::NumericTraits<ContinuousIndexType::ValueType>;

  bool passed = true;

  SizeType       sizeA = { { 10, 20, 30 } };
  const SizeType sizeB = { { 5, 10, 15 } };

  IndexType       startA = { { 12, 12, 12 } };
  const IndexType startB = { { 14, 14, 14 } };
  const IndexType endA = { { 21, 31, 41 } };

  RegionType regionA;
  RegionType regionB;

  regionA.SetSize(sizeA);
  regionA.SetIndex(startA);

  if (regionA.GetUpperIndex() != endA)
  {
    std::cout << "Upper index is " << regionA.GetUpperIndex() << " instead of " << endA << '\n';
    return EXIT_FAILURE;
  }
  RegionType regionC;
  regionC.SetIndex(startA);
  regionC.SetUpperIndex(endA);
  if (regionC.GetSize() != sizeA)
  {
    std::cout << "Size is " << regionC.GetSize() << " instead of " << sizeA << '\n';
    return EXIT_FAILURE;
  }

  // Take slices of a region
  try
  {
    SliceRegionType sliceA;
    sliceA = regionA.Slice(0);
    std::cout << "regionA.Slice(0): " << sliceA;
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << "Caught unexpected exception" << err;
    return EXIT_FAILURE;
  }

  try
  {
    SliceRegionType sliceA;
    sliceA = regionA.Slice(1);
    std::cout << "regionA.Slice(1): " << sliceA;
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << "Caught unexpected exception" << err;
    return EXIT_FAILURE;
  }

  try
  {
    SliceRegionType sliceA;
    sliceA = regionA.Slice(2);
    std::cout << "regionA.Slice(2): " << sliceA;
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << "Caught unexpected exception" << err;
    return EXIT_FAILURE;
  }

  try
  {
    SliceRegionType sliceA;
    sliceA = regionA.Slice(20);
    std::cout << "regionA.Slice(20): " << sliceA;
    std::cout << "Failed to catch expected exception" << '\n';
    return EXIT_FAILURE;
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cout << "Caught expected exception" << err;
  }

  regionB.SetSize(sizeB);
  regionB.SetIndex(startB);

  // Test IsInside( integerIndex )
  IndexType index = startA;
  if (!regionA.IsInside(index))
  {
    std::cout << "Error with IsInside 1." << '\n';
    passed = false;
  }
  index[0] = startA[0] + sizeA[0] - 1;
  if (!regionA.IsInside(index))
  {
    std::cout << "Error with IsInside 2." << '\n';
    passed = false;
  }
  index[0] = startA[0] - 1;
  if (regionA.IsInside(index))
  {
    std::cout << "Error with IsInside 3. Expected false." << '\n';
    passed = false;
  }
  index[0] = IndexNumericTraits::max();
  if (regionA.IsInside(index))
  {
    std::cout << "Error with IsInside 4. Expected false." << '\n';
    passed = false;
  }
  if (IndexNumericTraits::is_signed)
  {
    index[0] = IndexNumericTraits::min();
    if (regionA.IsInside(index))
    {
      std::cout << "Error with IsInside 5. Expected false." << '\n';
      passed = false;
    }
  }

  // Test IsInside( ContinuousIndex )
  ContinuousIndexType indexC;
  indexC[0] = startA[0];
  indexC[1] = startA[1];
  indexC[2] = startA[2];
  if (!regionA.IsInside(indexC))
  {
    std::cout << "Error with IsInside 1C." << '\n';
    passed = false;
  }
  indexC[0] = startA[0] + sizeA[0] - 0.5;
  indexC[1] = startA[1] + sizeA[1] - 0.5;
  indexC[2] = startA[2] + sizeA[2] - 0.5;
  if (!regionA.IsInside(indexC))
  {
    std::cout << "Error with IsInside 2C." << '\n';
    passed = false;
  }
  indexC[0] = startA[0] - 1;
  if (regionA.IsInside(indexC))
  {
    std::cout << "Error with IsInside 3C. Expected false." << '\n'
              << "  indexC: " << indexC << '\n'
              << "  start & size: " << startA << ' ' << sizeA << '\n';
    passed = false;
  }
  std::cout << "Testing ContinuousIndexNumericTraits::min()." << '\n';
  indexC[0] = ContinuousIndexNumericTraits::min();
  if (regionA.IsInside(indexC))
  {
    std::cout << "Error with IsInside 5C. Expected false." << '\n';
    passed = false;
  }
  /* Some tests cause floating point exceptions, so
   * only run them when Floating Point Exceptions are not enabled. */
  if (!itk::FloatingPointExceptions::GetEnabled())
  {
    std::cout << "Floating Point Exceptions's are disabled. " << '\n';
    std::cout << "...Proceeding with tests that can generate Floating Point Exceptions's." << '\n';

    /* Generates overflow exception */
    std::cout << "Testing ContinuousIndexNumericTraits::max()." << '\n';
    indexC[0] = ContinuousIndexNumericTraits::max();
    if (regionA.IsInside(indexC))
    {
      std::cout << "Error with IsInside 4C. Expected false." << '\n';
      passed = false;
    }
    /* Note for NaN. IsInside doesn't properly catch NaN. It gets cast to integer
     * which means it becomes a large negative number so it falls outside of
     * region bounds. In this way the test returns false appropriately, but for
     * the wrong reasons. If this test fails, then the compiler is handling a
     * cast of NaN to integer differently. */
    if (ContinuousIndexNumericTraits::has_quiet_NaN)
    {
      std::cout << "Testing quiet NaN behavior." << '\n';
      indexC[0] = ContinuousIndexNumericTraits::quiet_NaN();
      if (regionA.IsInside(indexC))
      {
        std::cout << "Error with IsInside 6C. Expected false." << '\n';
        passed = false;
      }
    }
    /* Note that signaling_NaN seems to simply wrap quiet_NaN */
    if (ContinuousIndexNumericTraits::has_signaling_NaN)
    {
      std::cout << "Testing signaling NaN behavior." << '\n';
      indexC[0] = ContinuousIndexNumericTraits::signaling_NaN();
      if (regionA.IsInside(indexC))
      {
        std::cout << "Error with IsInside 7C. Expected false." << '\n';
        passed = false;
      }
    }
    std::cout << "Testing infinity behavior." << '\n';
    indexC[0] = ContinuousIndexNumericTraits::infinity();
    if (regionA.IsInside(indexC))
    {
      std::cout << "Error with IsInside 8C. Expected false." << '\n';
      passed = false;
    }
  } // ! FloatingPointExceptions::GetEnabled()
  else
  {
    std::cout << "Not testing behavior that triggers Floating Point Exceptions." << '\n';
  }

  if (!itk::FloatingPointExceptions::GetEnabled() && ContinuousIndexNumericTraits::has_quiet_NaN)
  {
    std::cout << "Floating Point Exceptions's are disabled. Test some more NaN-related behavior..." << '\n';
    /* NaN behavior
     * Experimenting. Can be removed before final merge.
     * Issue with ImageRegion::IsInside is that it's using RoundHalfIntegerUp
     * and thereby casting to integer and
     * rounding, and thus NaN's get converted to a large negative int and
     * are only caught indirectly. */

    indexC.Fill(13);
    if (regionA.IsInside(indexC))
    {
      std::cout << "13,13,13 IsInside" << '\n';
    }
    else
    {
      std::cout << "13,13,13 is not inside !" << '\n';
    }

    indexC[0] = ContinuousIndexNumericTraits::quiet_NaN();
    if (regionA.IsInside(indexC))
    {
      std::cout << "** NaN,13,13 *is* inside. **" << '\n';
    }
    else
    {
      std::cout << "NaN,13,13 is not inside" << '\n';
    }

    std::cout << "NaN < -1 = " << (indexC[0] < -1.0) << '\n';
    std::cout << "NaN > -1 = " << (indexC[0] > -1.0) << '\n';

    const CoordinateType NaN = ContinuousIndexNumericTraits::quiet_NaN();
    std::cout << "RoundHalfIntegerUp(NaN): " << itk::Math::RoundHalfIntegerUp<CoordinateType>(NaN) << '\n';
    std::cout << "RoundHalfIntegerUp< CoordinateType >(NaN) < static_cast<CoordinateType> (0): "
              << (itk::Math::RoundHalfIntegerUp<CoordinateType>(NaN) < static_cast<CoordinateType>(0)) << '\n';
    std::cout << "RoundHalfIntegerUp< CoordinateType >(NaN) > static_cast<CoordinateType> (0): "
              << (itk::Math::RoundHalfIntegerUp<CoordinateType>(NaN) > static_cast<CoordinateType>(0)) << '\n';
    auto rf = itk::Math::RoundHalfIntegerUp<CoordinateType>(NaN);
    std::cout << "CoordinateType = RoundHalfIntegerUp(NaN): " << rf << '\n';
    auto rl = itk::Math::RoundHalfIntegerUp<RegionType::IndexValueType, CoordinateType>(NaN);
    std::cout << "RegionType::IndexValueType type = RoundHalfIntegerUp(NaN): " << rl << '\n';
    std::cout << "static_cast<RegionType::IndexValueType>( NaN ): " << static_cast<RegionType::IndexValueType>(NaN)
              << '\n';
    std::cout << "NumericTraits<RegionType::IndexValueType>::min(): "
              << itk::NumericTraits<RegionType::IndexValueType>::min() << '\n';
    std::cout << "CoordinateType min(): " << ContinuousIndexNumericTraits::min() << '\n';
    std::cout << "...end NaN tests." << '\n' << '\n';
  }

  // Test IsInside( region )
  if (!regionA.IsInside(regionB))
  {
    passed = false;
  }

  if (regionB.IsInside(regionA))
  {
    passed = false;
  }

  // Test ShrinkByRadius
  IndexType shrinkIndex;
  shrinkIndex[0] = 22;
  shrinkIndex[1] = 343;
  shrinkIndex[2] = 5;
  SizeType shrinkSize;
  shrinkSize[0] = 33;
  shrinkSize[1] = 21;
  shrinkSize[2] = 3;
  RegionType shrinkRegion;
  shrinkRegion.SetIndex(shrinkIndex);
  shrinkRegion.SetSize(shrinkSize);
  RegionType padAndShrinkRegion = shrinkRegion;

  const itk::OffsetValueType offsetValueRadius = 4;
  padAndShrinkRegion.PadByRadius(offsetValueRadius);
  padAndShrinkRegion.ShrinkByRadius(offsetValueRadius);
  if (shrinkRegion != padAndShrinkRegion)
  {
    passed = false;
    std::cerr << "Pad and shrink by an OffSetValueType radius failed." << '\n';
  }

  SizeType sizeRadius;
  sizeRadius[0] = 0;
  sizeRadius[1] = 1;
  sizeRadius[2] = 2;
  padAndShrinkRegion.PadByRadius(sizeRadius);
  padAndShrinkRegion.ShrinkByRadius(sizeRadius);
  if (shrinkRegion != padAndShrinkRegion)
  {
    passed = false;
    std::cerr << "Pad and shrink by an SizeType radius failed." << '\n';
  }

  RegionType::IndexValueArrayType indexValueArrayRadius;
  indexValueArrayRadius[0] = 0;
  indexValueArrayRadius[1] = 1;
  indexValueArrayRadius[2] = 2;
  padAndShrinkRegion.PadByRadius(indexValueArrayRadius);
  padAndShrinkRegion.ShrinkByRadius(indexValueArrayRadius);
  if (shrinkRegion != padAndShrinkRegion)
  {
    passed = false;
    std::cerr << "Pad and shrink by an IndexValueArrayType radius failed." << '\n';
  }

  if (passed)
  {
    std::cout << "ImageRegion test passed." << '\n';
    return EXIT_SUCCESS;
  }
  else
  {
    std::cout << "ImageRegion test failed." << '\n';
    return EXIT_FAILURE;
  }
}
