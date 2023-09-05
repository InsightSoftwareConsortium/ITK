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

#include <set>
#include "itkHistogramToRunLengthFeaturesFilter.h"
#include "itkMath.h"
#include "itkHistogram.h"
#include "itkSample.h"
#include "itkTestingMacros.h"

int
itkHistogramTest(int, char *[])
{
  int pass = EXIT_SUCCESS;

  using MeasurementType = float;
  constexpr unsigned int numberOfComponents = 3;

  // create a histogram with 3 components measurement vectors
  using HistogramType = itk::Statistics::Histogram<MeasurementType, itk::Statistics::DenseFrequencyContainer2>;
  auto histogram = HistogramType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(histogram, Histogram, Sample);

  using MeasurementVectorType = HistogramType::MeasurementVectorType;
  using InstanceIdentifier = HistogramType::InstanceIdentifier;
  using IndexType = HistogramType::IndexType;

  // initializes a 64 x 64 x 64 histogram with equal size interval
  HistogramType::SizeType size(numberOfComponents);
  size.Fill(64);
  unsigned long totalSize = size[0] * size[1] * size[2];

  MeasurementVectorType lowerBound(numberOfComponents);
  MeasurementVectorType upperBound(numberOfComponents);

  lowerBound.Fill(0);
  upperBound.Fill(1024);

  // Test exceptions

  // Call Initialize() before calling SetMeasurementVectorSize()
  ITK_TRY_EXPECT_EXCEPTION(histogram->Initialize(size));

  histogram->SetMeasurementVectorSize(numberOfComponents);
  ITK_TEST_EXPECT_EQUAL(histogram->GetMeasurementVectorSize(), numberOfComponents);

  histogram->Initialize(size, lowerBound, upperBound);

  histogram->SetToZero();

  double interval = (upperBound[0] - lowerBound[0]) / static_cast<HistogramType::MeasurementType>(size[0]);

  MeasurementVectorType measurements(numberOfComponents);
  measurements.Fill(512);

  IndexType index(numberOfComponents);
  IndexType ind(numberOfComponents);
  index.Fill(32);

  histogram->GetIndex(measurements, ind);
  ITK_TEST_EXPECT_EQUAL(ind, index);

  InstanceIdentifier id = histogram->GetInstanceIdentifier(index);
  ITK_TEST_EXPECT_EQUAL(histogram->GetIndex(id), index);

  // Test for outside
  index.Fill(-5);
  ITK_TEST_EXPECT_TRUE(histogram->IsIndexOutOfBounds(index));

  // Test for inside
  index.Fill(32);
  ITK_TEST_EXPECT_TRUE(!histogram->IsIndexOutOfBounds(index));

  // Test for outside
  index.Fill(100);
  ITK_TEST_EXPECT_TRUE(histogram->IsIndexOutOfBounds(index));

  ITK_TEST_EXPECT_EQUAL(histogram->Size(), totalSize);
  ITK_TEST_EXPECT_EQUAL(histogram->GetSize(), size);

  // Query the bounds of the bin using the index of the bin.

  const MeasurementType & expectedValMeasType1 = lowerBound[0] + interval * 31;
  const MeasurementType & obtainedValMeasType1 = histogram->GetBinMin(0, 31);
  if (itk::Math::NotAlmostEquals(obtainedValMeasType1, expectedValMeasType1))
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in itk::Statistics::Histogram::GetBinMin" << std::endl;
    std::cerr << "Expected: " << expectedValMeasType1 << std::endl;
    std::cerr << ", but got: " << obtainedValMeasType1 << std::endl;
    pass = EXIT_FAILURE;
  }

  const MeasurementType & expectedValMeasType2 = lowerBound[0] + interval * 32;
  const MeasurementType & obtainedValMeasType2 = histogram->GetBinMax(0, 31);
  if (itk::Math::NotAlmostEquals(obtainedValMeasType2, expectedValMeasType2))
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in itk::Statistics::Histogram::GetBinMax" << std::endl;
    std::cerr << "Expected: " << expectedValMeasType2 << std::endl;
    std::cerr << ", but got: " << obtainedValMeasType2 << std::endl;
    pass = EXIT_FAILURE;
  }

  // Query the histogram bin extremes using a value within the bin

  const MeasurementType & expectedValMeasType3 = lowerBound[0] + interval * 31;
  const MeasurementType & obtainedValMeasType3 = histogram->GetBinMinFromValue(0, lowerBound[0] + interval * 31.5);
  if (itk::Math::NotAlmostEquals(obtainedValMeasType3, expectedValMeasType3))
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in itk::Statistics::Histogram::GetBinMinFromValue" << std::endl;
    std::cerr << "Expected: " << expectedValMeasType2 << std::endl;
    std::cerr << ", but got: " << obtainedValMeasType2 << std::endl;
    pass = EXIT_FAILURE;
  }

  const MeasurementType & expectedValMeasType4 = lowerBound[0];
  const MeasurementType & obtainedValMeasType4 = histogram->GetBinMinFromValue(0, itk::NumericTraits<float>::min());
  if (itk::Math::NotAlmostEquals(obtainedValMeasType4, expectedValMeasType4))
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in itk::Statistics::Histogram::GetBinMinFromValue" << std::endl;
    std::cerr << "Expected: " << expectedValMeasType4 << std::endl;
    std::cerr << ", but got: " << obtainedValMeasType4 << std::endl;
    pass = EXIT_FAILURE;
  }

  const MeasurementType & expectedValMeasType5 = lowerBound[0] + interval * (size[0] - 1);
  const MeasurementType & obtainedValMeasType5 = histogram->GetBinMinFromValue(0, itk::NumericTraits<float>::max());
  if (itk::Math::NotAlmostEquals(obtainedValMeasType5, expectedValMeasType5))
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in itk::Statistics::Histogram::GetBinMinFromValue" << std::endl;
    std::cerr << "Expected: " << expectedValMeasType5 << std::endl;
    std::cerr << ", but got: " << obtainedValMeasType5 << std::endl;
    pass = EXIT_FAILURE;
  }

  const MeasurementType & expectedValMeasType6 = lowerBound[0] + interval * 32;
  const MeasurementType & obtainedValMeasType6 = histogram->GetBinMaxFromValue(0, lowerBound[0] + interval * 31.5);
  if (itk::Math::NotAlmostEquals(obtainedValMeasType6, expectedValMeasType6))
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in itk::Statistics::Histogram::GetBinMaxFromValue" << std::endl;
    std::cerr << "Expected: " << expectedValMeasType6 << std::endl;
    std::cerr << ", but got: " << obtainedValMeasType6 << std::endl;
    pass = EXIT_FAILURE;
  }

  const MeasurementType & expectedValMeasType7 = lowerBound[0] + interval;
  const MeasurementType & obtainedValMeasType7 = histogram->GetBinMaxFromValue(0, itk::NumericTraits<float>::min());
  if (itk::Math::NotAlmostEquals(obtainedValMeasType7, expectedValMeasType7))
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in itk::Statistics::Histogram::GetBinMaxFromValue" << std::endl;
    std::cerr << "Expected: " << expectedValMeasType7 << std::endl;
    std::cerr << ", but got: " << obtainedValMeasType7 << std::endl;
    pass = EXIT_FAILURE;
  }

  const MeasurementType & expectedValMeasType8 = upperBound[0];
  const MeasurementType & obtainedValMeasType8 = histogram->GetBinMaxFromValue(0, itk::NumericTraits<float>::max());
  if (itk::Math::NotAlmostEquals(obtainedValMeasType8, expectedValMeasType8))
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in itk::Statistics::Histogram::GetBinMaxFromValue" << std::endl;
    std::cerr << "Expected: " << expectedValMeasType8 << std::endl;
    std::cerr << ", but got: " << obtainedValMeasType8 << std::endl;
    pass = EXIT_FAILURE;
  }

  index.Fill(31);
  const MeasurementType & expectedValMeasType9 = lowerBound[0] + interval * 31;
  const MeasurementType & obtainedValMeasType9 = histogram->GetHistogramMinFromIndex(index)[0];
  if (itk::Math::NotAlmostEquals(obtainedValMeasType9, expectedValMeasType9))
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in itk::Statistics::Histogram::GetHistogramMinFromIndex" << std::endl;
    std::cerr << "Expected: " << expectedValMeasType9 << std::endl;
    std::cerr << ", but got: " << obtainedValMeasType9 << std::endl;
    pass = EXIT_FAILURE;
  }

  const MeasurementType & expectedValMeasType10 = lowerBound[0] + interval * 32;
  const MeasurementType & obtainedValMeasType10 = histogram->GetHistogramMaxFromIndex(index)[0];
  if (itk::Math::NotAlmostEquals(obtainedValMeasType10, expectedValMeasType10))
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in itk::Statistics::Histogram::GetHistogramMaxFromIndex" << std::endl;
    std::cerr << "Expected: " << expectedValMeasType10 << std::endl;
    std::cerr << ", but got: " << obtainedValMeasType10 << std::endl;
    pass = EXIT_FAILURE;
  }

  for (id = 0; id < static_cast<InstanceIdentifier>(totalSize); ++id)
  {
    histogram->SetFrequency(id, 1);
    histogram->IncreaseFrequency(id, 1);

    ITK_TEST_EXPECT_EQUAL(histogram->GetFrequency(id), 2);
  }

  // Quantile 1
  double expectedValD = 307.2;
  double obtainedValD = histogram->Quantile(0, 0.3);
  if (itk::Math::NotAlmostEquals(obtainedValD, expectedValD))
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in itk::Statistics::Histogram::Quantile" << std::endl;
    std::cerr << "Expected: " << expectedValD << std::endl;
    std::cerr << ", but got: " << obtainedValD << std::endl;
    pass = EXIT_FAILURE;
  }

  // Quantile 2
  expectedValD = 512.0;
  obtainedValD = histogram->Quantile(0, 0.5);
  if (itk::Math::NotAlmostEquals(obtainedValD, expectedValD))
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in itk::Statistics::Histogram::Quantile" << std::endl;
    std::cerr << "Expected: " << expectedValD << std::endl;
    std::cerr << ", but got: " << obtainedValD << std::endl;
    pass = EXIT_FAILURE;
  }

  // Quantile 3
  expectedValD = 716.8;
  obtainedValD = histogram->Quantile(0, 0.7);
  if (itk::Math::NotAlmostEquals(obtainedValD, expectedValD))
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in itk::Statistics::Histogram::Quantile" << std::endl;
    std::cerr << "Expected: " << expectedValD << std::endl;
    std::cerr << ", but got: " << obtainedValD << std::endl;
    pass = EXIT_FAILURE;
  }


  // Histogram with SparseFrequencyContainer2
  using SparseHistogramType = itk::Statistics::Histogram<MeasurementType, itk::Statistics::SparseFrequencyContainer2>;
  auto sparseHistogram = SparseHistogramType::New();

  sparseHistogram->SetMeasurementVectorSize(numberOfComponents);

  // initializes a 64 x 64 x 64 histogram with equal size interval
  sparseHistogram->Initialize(size, lowerBound, upperBound);
  sparseHistogram->SetToZero();
  interval = (upperBound[0] - lowerBound[0]) / static_cast<SparseHistogramType::MeasurementType>(size[0]);

  measurements.Fill(512);
  index.Fill(32);
  sparseHistogram->GetIndex(measurements, ind);
  ITK_TEST_EXPECT_EQUAL(ind, index);

  id = sparseHistogram->GetInstanceIdentifier(index);
  ITK_TEST_EXPECT_EQUAL(sparseHistogram->GetIndex(id), index);

  index.Fill(100);
  ITK_TEST_EXPECT_TRUE(sparseHistogram->IsIndexOutOfBounds(index));
  ITK_TEST_EXPECT_EQUAL(sparseHistogram->Size(), totalSize);
  ITK_TEST_EXPECT_EQUAL(sparseHistogram->GetSize(), size);

  const MeasurementType & expectedValMeasType11 = lowerBound[0] + interval * 31;
  const MeasurementType & obtainedValMeasType11 = sparseHistogram->GetBinMin(0, 31);
  if (itk::Math::NotAlmostEquals(obtainedValMeasType11, expectedValMeasType11))
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in itk::Statistics::Histogram::GetBinMin" << std::endl;
    std::cerr << "Expected: " << obtainedValMeasType11 << std::endl;
    std::cerr << ", but got: " << expectedValMeasType11 << std::endl;
    pass = EXIT_FAILURE;
  }

  const MeasurementType & expectedValMeasType12 = lowerBound[0] + interval * 32;
  const MeasurementType & obtainedValMeasType12 = sparseHistogram->GetBinMax(0, 31);
  if (itk::Math::NotAlmostEquals(obtainedValMeasType12, expectedValMeasType12))
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in itk::Statistics::Histogram::GetBinMax" << std::endl;
    std::cerr << "Expected: " << obtainedValMeasType12 << std::endl;
    std::cerr << ", but got: " << expectedValMeasType12 << std::endl;
    pass = EXIT_FAILURE;
  }


  for (id = 0; id < static_cast<SparseHistogramType::InstanceIdentifier>(totalSize); ++id)
  {
    ITK_TEST_EXPECT_TRUE(sparseHistogram->SetFrequency(id, 1));
    ITK_TEST_EXPECT_TRUE(sparseHistogram->IncreaseFrequency(id, 1));
    ITK_TEST_EXPECT_EQUAL(sparseHistogram->GetFrequency(id), 2);
  }

  expectedValD = 512.0;
  obtainedValD = sparseHistogram->Quantile(0, 0.5);
  if (itk::Math::NotAlmostEquals(obtainedValD, expectedValD))
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in itk::Statistics::Histogram::Quantile" << std::endl;
    std::cerr << "Expected: " << expectedValD << std::endl;
    std::cerr << ", but got: " << obtainedValD << std::endl;
    pass = EXIT_FAILURE;
  }

  auto clipBinsAtEnds = true;
  ITK_TEST_SET_GET_BOOLEAN(histogram, ClipBinsAtEnds, clipBinsAtEnds);

  clipBinsAtEnds = false;
  ITK_TEST_SET_GET_BOOLEAN(histogram, ClipBinsAtEnds, clipBinsAtEnds);

  ITK_TEST_EXPECT_EQUAL(histogram->GetMeasurementVectorSize(), numberOfComponents);

  constexpr unsigned int measurementVectorSize = 17;
  ITK_TRY_EXPECT_EXCEPTION(histogram->SetMeasurementVectorSize(measurementVectorSize));

  index.Fill(0);
  MeasurementVectorType measurement = histogram->GetMeasurementVector(index);
  for (unsigned int kid0 = 0; kid0 < numberOfComponents; ++kid0)
  {
    float expectedValF = 8.0;
    float obtainedValF = measurement[kid0];
    if (itk::Math::NotAlmostEquals(obtainedValF, expectedValF))
    {
      std::cerr << "Test failed!" << std::endl;
      std::cerr << "Error in itk::Statistics::Histogram::GetMeasurementVector at index [" << index << "]" << std::endl;
      std::cerr << "Expected value " << expectedValF << std::endl;
      std::cerr << " differs from " << obtainedValF << std::endl;
      pass = EXIT_FAILURE;
      break;
    }
  }

  histogram->SetClipBinsAtEnds(true);

  measurement = histogram->GetMeasurementVector(index);
  for (unsigned int kid1 = 0; kid1 < numberOfComponents; ++kid1)
  {
    float expectedValF = 8.0;
    float obtainedValF = measurement[kid1];
    if (itk::Math::NotAlmostEquals(obtainedValF, expectedValF))
    {
      std::cerr << "Test failed!" << std::endl;
      std::cerr << "Error in itk::Statistics::Histogram::GetMeasurementVector at index [" << index << "]" << std::endl;
      std::cerr << "Expected value " << expectedValF << std::endl;
      std::cerr << " differs from " << obtainedValF << std::endl;
      pass = EXIT_FAILURE;
      break;
    }
  }

  constexpr InstanceIdentifier instanceId = 0;
  measurement = histogram->GetMeasurementVector(instanceId);
  for (unsigned int kid2 = 0; kid2 < numberOfComponents; ++kid2)
  {
    float expectedValF = 8.0;
    float obtainedValF = measurement[kid2];
    if (itk::Math::NotAlmostEquals(obtainedValF, expectedValF))
    {
      std::cerr << "Test failed!" << std::endl;
      std::cerr << "Error in itk::Statistics::Histogram::GetMeasurementVector at index [" << index << "]" << std::endl;
      std::cerr << "Expected value " << expectedValF << std::endl;
      std::cerr << " differs from " << obtainedValF << std::endl;
      pass = EXIT_FAILURE;
      break;
    }
  }

  // Test GetIndex with different settings of SetClipBinsAtEnds
  MeasurementVectorType outOfLowerRange(numberOfComponents);
  MeasurementVectorType outOfUpperRange(numberOfComponents);

  for (unsigned int k = 0; k < numberOfComponents; ++k)
  {
    outOfLowerRange[k] = lowerBound[k] - 13;
    outOfUpperRange[k] = upperBound[k] + 23;
  }

  histogram->SetClipBinsAtEnds(false);

  IndexType index1(numberOfComponents);
  bool      getindex1 = histogram->GetIndex(outOfLowerRange, index1);

  std::cout << "GetIndex() with SetClipBinsAtEnds() = false " << std::endl;
  std::cout << "Boolean " << getindex1 << " Index " << index1 << std::endl;

  ITK_TEST_EXPECT_TRUE(getindex1);

  for (unsigned int k1 = 0; k1 < numberOfComponents; ++k1)
  {
    ITK_TEST_EXPECT_EQUAL(index1[k1], 0);
  }


  histogram->SetClipBinsAtEnds(true);

  getindex1 = histogram->GetIndex(outOfLowerRange, index1);

  std::cout << "GetIndex() with SetClipBinsAtEnds() = true " << std::endl;
  std::cout << "Boolean " << getindex1 << " Index " << index1 << std::endl;

  ITK_TEST_EXPECT_TRUE(!getindex1);

  histogram->SetClipBinsAtEnds(false);

  IndexType index2(numberOfComponents);
  bool      getindex2 = histogram->GetIndex(outOfUpperRange, index2);

  std::cout << "GetIndex() with SetClipBinsAtEnds() = false " << std::endl;
  std::cout << "Boolean " << getindex2 << " Index " << index2 << std::endl;

  ITK_TEST_EXPECT_TRUE(getindex2);

  for (unsigned int k2 = 0; k2 < numberOfComponents; ++k2)
  {
    ITK_TEST_EXPECT_EQUAL(index2[k2], static_cast<long>(size[k2]) - 1);
  }


  histogram->SetClipBinsAtEnds(true);

  getindex2 = histogram->GetIndex(outOfUpperRange, index2);

  std::cout << "GetIndex() with SetClipBinsAtEnds() = true " << std::endl;
  std::cout << "Boolean " << getindex2 << " Index " << index2 << std::endl;

  ITK_TEST_EXPECT_TRUE(!getindex2);

  // Test GetIndex() for values that are above the median value of the bin.
  IndexType pindex(numberOfComponents);
  pindex.Fill(32);
  MeasurementVectorType measurementVector = histogram->GetMeasurementVector(pindex);

  for (unsigned int gik1 = 0; gik1 < numberOfComponents; ++gik1)
  {
    measurementVector[gik1] += 0.3;
  }

  IndexType gindex;
  histogram->GetIndex(measurementVector, gindex);

  for (unsigned int gik2 = 0; gik2 < numberOfComponents; ++gik2)
  {
    ITK_TEST_EXPECT_EQUAL(gindex[gik2], 32);
  }

  // Test GetIndex() for values that are below the median value of the bin.
  for (unsigned int gik3 = 0; gik3 < numberOfComponents; ++gik3)
  {
    measurementVector[gik3] -= 0.6;
  }

  histogram->GetIndex(measurementVector, gindex);

  for (unsigned int gik4 = 0; gik4 < numberOfComponents; ++gik4)
  {
    ITK_TEST_EXPECT_EQUAL(gindex[gik4], 32);
  }

  // Test GetIndex on the upper and lower bounds
  IndexType upperIndex(numberOfComponents);
  ITK_TEST_EXPECT_TRUE(histogram->GetIndex(upperBound, upperIndex));

  for (unsigned int k1 = 0; k1 < numberOfComponents; ++k1)
  {
    ITK_TEST_EXPECT_EQUAL(upperIndex[k1], 63);
  }

  IndexType lowerIndex(numberOfComponents);
  ITK_TEST_EXPECT_TRUE(histogram->GetIndex(lowerBound, lowerIndex));

  for (unsigned int k1 = 0; k1 < numberOfComponents; ++k1)
  {
    ITK_TEST_EXPECT_EQUAL(lowerIndex[k1], 0);
  }

  // Test GetIndex above the upper bound of a bin
  histogram->SetClipBinsAtEnds(false);
  MeasurementVectorType measurementVectorAbove(numberOfComponents);
  for (unsigned int gupk1 = 0; gupk1 < numberOfComponents; ++gupk1)
  {
    measurementVectorAbove[gupk1] = 129.9;
  }

  IndexType aboveUpperIndex(numberOfComponents);
  ITK_TEST_EXPECT_TRUE(histogram->GetIndex(measurementVectorAbove, aboveUpperIndex));

  // Get the mean value for a dimension
  unsigned int dimension = 0;
  double       mean = histogram->Mean(dimension);
  std::cout << "Mean value along dimension " << dimension << " : " << mean << std::endl;

  HistogramType::Iterator itr = histogram->Begin();
  HistogramType::Iterator end = histogram->End();

  HistogramType::TotalAbsoluteFrequencyType totalFrequency = histogram->GetTotalFrequency();

  InstanceIdentifier histogramSize = histogram->Size();

  while (itr != end)
  {
    itr.SetFrequency(itr.GetFrequency() + 1);
    ++itr;
  }

  HistogramType::TotalAbsoluteFrequencyType newTotalFrequency = histogram->GetTotalFrequency();
  ITK_TEST_EXPECT_EQUAL(newTotalFrequency, histogramSize + totalFrequency);

  // Exercise GetIndex() method in the iterator.
  std::cout << "TEST GetIndex() and GetFrequency() in the iterator" << std::endl;
  itr = histogram->Begin();
  end = histogram->End();

  // Print out only some of them, the first 10...
  for (unsigned int kk = 0; kk < 10 && itr != end; ++itr, ++kk)
  {
    std::cout << itr.GetIndex() << " : " << itr.GetFrequency() << std::endl;
  }


  // Exercise GetMin / GetMax methods
  {
    const double            epsilon = 1e-6;
    HistogramType::SizeType size2 = histogram->GetSize();

    HistogramType::BinMinContainerType binMinimums = histogram->GetMins();

    for (unsigned int dim = 0; dim < numberOfComponents; ++dim)
    {
      HistogramType::BinMinVectorType binDimensionMinimums = histogram->GetDimensionMins(dim);
      for (unsigned int k = 0; k < size2[dim]; ++k)
      {
        HistogramType::MeasurementType minA = binMinimums[dim][k];
        HistogramType::MeasurementType minB = binDimensionMinimums[k];
        if (itk::Math::abs(minA - minB) > epsilon)
        {
          std::cerr.precision(static_cast<int>(itk::Math::abs(std::log10(epsilon))));
          std::cerr << "Test failed!" << std::endl;
          std::cerr << "Error in itk:Statistics::Histogram::GetMins/GetDimensionMins at index [" << k << "]"
                    << std::endl;
          std::cerr << "Expected value " << minA << std::endl;
          std::cerr << " differs from: " << minB;
          std::cerr << " by more than " << epsilon << std::endl;
          return EXIT_FAILURE;
        }

        HistogramType::MeasurementType minC = histogram->GetBinMin(dim, k);
        if (itk::Math::abs(minA - minC) > epsilon)
        {
          std::cerr.precision(static_cast<int>(itk::Math::abs(std::log10(epsilon))));
          std::cerr << "Test failed!" << std::endl;
          std::cerr << "Error in itk:Statistics::Histogram::GetMins/GetBinMin at index [" << k << "]" << std::endl;
          std::cerr << "Expected value " << minA << std::endl;
          std::cerr << " differs from: " << minC;
          std::cerr << " by more than " << epsilon << std::endl;
          return EXIT_FAILURE;
        }
      }
    }

    HistogramType::BinMaxContainerType binMaximums = histogram->GetMaxs();

    for (unsigned int dim = 0; dim < numberOfComponents; ++dim)
    {
      HistogramType::BinMaxVectorType binDimensionMaximums = histogram->GetDimensionMaxs(dim);
      for (unsigned int k = 0; k < size2[dim]; ++k)
      {
        HistogramType::MeasurementType maxA = binMaximums[dim][k];
        HistogramType::MeasurementType maxB = binDimensionMaximums[k];
        if (itk::Math::abs(maxA - maxB) > epsilon)
        {
          std::cerr.precision(static_cast<int>(itk::Math::abs(std::log10(epsilon))));
          std::cerr << "Test failed!" << std::endl;
          std::cerr << "Error in itk:Statistics::Histogram::GetMaxs/GetDimensionMaxs at index [" << k << "]"
                    << std::endl;
          std::cerr << "Expected value " << maxA << std::endl;
          std::cerr << " differs from: " << maxB;
          std::cerr << " by more than " << epsilon << std::endl;
          return EXIT_FAILURE;
        }

        HistogramType::MeasurementType maxC = histogram->GetBinMax(dim, k);
        if (itk::Math::abs(maxA - maxC) > epsilon)
        {
          std::cerr.precision(static_cast<int>(itk::Math::abs(std::log10(epsilon))));
          std::cerr << "Test failed!" << std::endl;
          std::cerr << "Error in itk:Statistics::Histogram::GetMaxs/GetBinMax at index [" << k << "]" << std::endl;
          std::cerr << "Expected value " << maxA << std::endl;
          std::cerr << " differs from: " << maxC;
          std::cerr << " by more than " << epsilon << std::endl;
          return EXIT_FAILURE;
        }
      }
    }
  }


  // Test methods specific to Iterators
  {
    using IteratorType = HistogramType::Iterator;
    IteratorType iter = histogram->Begin();
    IteratorType iter2 = histogram->End();

    iter2 = iter;
    ITK_TEST_EXPECT_TRUE(iter == iter2);

    IteratorType iter3(histogram);
    ITK_TEST_EXPECT_TRUE(histogram->Begin() == iter3);

    unsigned int counter = 0;
    while (iter3 != histogram->End())
    {
      ++iter3;
      counter++;
    }

    ITK_TEST_EXPECT_EQUAL(counter, histogram->Size());

    IteratorType iter4(iter2);
    ITK_TEST_EXPECT_TRUE(iter4 == iter2);

    IteratorType iter5 = iter2;
    ITK_TEST_EXPECT_TRUE(iter5 == iter2);

    itk::Statistics::Sample<HistogramType::MeasurementVectorType>::InstanceIdentifier id2 = 7;
    IteratorType                                                                      iter6(id2, histogram);
    ITK_TEST_EXPECT_EQUAL(iter6.GetInstanceIdentifier(), id2);
  }

  // Test methods specific to ConstIterators
  {
    using ConstIteratorType = HistogramType::ConstIterator;
    ConstIteratorType iter = histogram->Begin();
    ConstIteratorType iter2 = histogram->End();

    iter2 = iter;
    ITK_TEST_EXPECT_TRUE(!(iter != iter2));
    ITK_TEST_EXPECT_TRUE(iter == iter2);

    ConstIteratorType iter3(iter2);
    ITK_TEST_EXPECT_TRUE(iter3 == iter2);

    const HistogramType * constHistogram = histogram.GetPointer();

    ConstIteratorType iter4(constHistogram->Begin());
    ConstIteratorType iter5(histogram->Begin());
    ITK_TEST_EXPECT_TRUE(iter5 == iter4);

    ConstIteratorType iter6(constHistogram);
    ConstIteratorType iter7(histogram);
    ITK_TEST_EXPECT_TRUE(iter6 == iter7);

    ConstIteratorType                                                                 iter8(histogram);
    itk::Statistics::Sample<HistogramType::MeasurementVectorType>::InstanceIdentifier id3 = 0;
    ITK_TEST_EXPECT_EQUAL(iter8.GetInstanceIdentifier(), id3);

    unsigned int      counter = 0;
    ConstIteratorType iter10(constHistogram);
    ITK_TEST_EXPECT_TRUE(constHistogram->Begin() == iter10);

    while (iter10 != constHistogram->End())
    {
      ++iter10;
      counter++;
    }

    ITK_TEST_EXPECT_EQUAL(counter, constHistogram->Size());
  }

  // Test streaming enumeration for HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature elements
  const std::set<itk::Statistics::HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature> allRunLengthFeature{
    itk::Statistics::HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature::ShortRunEmphasis,
    itk::Statistics::HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature::LongRunEmphasis,
    itk::Statistics::HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature::GreyLevelNonuniformity,
    itk::Statistics::HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature::RunLengthNonuniformity,
    itk::Statistics::HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature::LowGreyLevelRunEmphasis,
    itk::Statistics::HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature::HighGreyLevelRunEmphasis,
    itk::Statistics::HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature::ShortRunLowGreyLevelEmphasis,
    itk::Statistics::HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature::ShortRunHighGreyLevelEmphasis,
    itk::Statistics::HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature::LongRunLowGreyLevelEmphasis,
    itk::Statistics::HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature::LongRunHighGreyLevelEmphasis
  };
  for (const auto & ee : allRunLengthFeature)
  {
    std::cout << "STREAMED ENUM VALUE HistogramToRunLengthFeaturesFilterEnums::RunLengthFeature: " << ee << std::endl;
  }


  std::cout << "Test finished." << std::endl;
  return pass;
}
