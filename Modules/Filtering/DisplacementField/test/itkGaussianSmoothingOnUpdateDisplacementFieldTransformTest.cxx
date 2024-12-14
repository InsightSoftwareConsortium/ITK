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

#include "itkGaussianSmoothingOnUpdateDisplacementFieldTransform.h"
#include "itkNumericTraits.h"
#include "itkMath.h"

/**
 * Test the UpdateTransformParameters and related methods,
 * introduced by this derivation.
 *
 * TODO: Create a more complete numerical test for the smoothing.
 */

int
itkGaussianSmoothingOnUpdateDisplacementFieldTransformTest(int, char *[])
{

  constexpr unsigned int dimensions = 2;
  using DisplacementTransformType = itk::GaussianSmoothingOnUpdateDisplacementFieldTransform<double, dimensions>;

  /* Create a displacement field transform */
  auto displacementTransform = DisplacementTransformType::New();
  using FieldType = DisplacementTransformType::DisplacementFieldType;
  auto field = FieldType::New(); // This is based on itk::Image

  FieldType::SizeType   size;
  FieldType::IndexType  start;
  FieldType::RegionType region;
  const int             dimLength = 20;
  size.Fill(dimLength);
  start.Fill(0);
  region.SetSize(size);
  region.SetIndex(start);
  field->SetRegions(region);
  field->Allocate();

  const DisplacementTransformType::OutputVectorType zeroVector{};
  field->FillBuffer(zeroVector);

  displacementTransform->SetDisplacementField(field);

  /* Test SmoothDisplacementFieldGauss */
  std::cout << "Test SmoothDisplacementFieldGauss" << '\n';
  using ParametersValueType = DisplacementTransformType::ParametersValueType;
  const ParametersValueType                            paramsZero{};
  DisplacementTransformType::ParametersType            params;
  DisplacementTransformType::ParametersType            paramsFill(displacementTransform->GetNumberOfParameters());
  const DisplacementTransformType::ParametersValueType paramsFillValue = 0.0;
  paramsFill.Fill(paramsFillValue);
  // Add an outlier to visually see that some smoothing is taking place.
  const unsigned int outlier = dimLength * dimensions * 4 + dimLength * dimensions / 2;
  paramsFill(outlier) = 99.0;
  paramsFill(outlier + 1) = 99.0;
  displacementTransform->SetGaussianSmoothingVarianceForTheTotalField(0.25);
  displacementTransform->SetGaussianSmoothingVarianceForTheUpdateField(3);
  displacementTransform->SetParameters(paramsFill);
  // params = displacementTransform->GetParameters();
  // std::cout << "params *before* SmoothDisplacementFieldGauss: " << '\n'
  //          << params << '\n';
  params = displacementTransform->GetParameters();
  // std::cout << "field->GetPixelContainer *after* Smooth: "
  //          << field->GetPixelContainer() << '\n';
  /* We should see 0's on all boundaries from the smoothing routine */
  unsigned int linelength = dimLength * dimensions;
  for (unsigned int i = 0; i < displacementTransform->GetNumberOfParameters(); ++i)
  {
    bool ok = true;
    if (i < linelength && itk::Math::NotAlmostEquals(params[i], paramsZero))
    {
      ok = false;
    }
    if (i % linelength == 0 && itk::Math::NotAlmostEquals(params[i], paramsZero))
    {
      ok = false;
    }
    if (i % linelength == (linelength - 1) && itk::Math::NotAlmostEquals(params[i], paramsZero))
    {
      ok = false;
    }
    if (!ok)
    {
      std::cout << "0-valued boundaries not found when expected "
                << "after smoothing." << '\n';
      std::cout << "params: " << '\n' << params << '\n';
      return EXIT_FAILURE;
    }
  }
  /* Check that we have some smoothing around the outlier we set above. */
  std::cout << "Parameters *after* SmoothDisplacementFieldGauss, around "
            << "outlier: " << '\n';
  for (int i = -2; i < 3; ++i)
  {
    for (int j = -2; j < 3; ++j)
    {
      const unsigned int index = outlier + static_cast<unsigned int>(i * (int)(dimLength * dimensions) + j);
      std::cout << params(index) << ' ';
    }
    std::cout << '\n';
  }

  /* Test UpdateTransformParameters */
  std::cout << "Testing UpdateTransformParameters..." << '\n';
  /* fill with 0 */
  field->FillBuffer(zeroVector);
  DisplacementTransformType::DerivativeType update(displacementTransform->GetNumberOfParameters());
  update.Fill(1.2);
  displacementTransform->UpdateTransformParameters(update);
  params = displacementTransform->GetParameters();
  // std::cout  << "params: " << '\n' << params << '\n';
  //<< "derivativeTruth: " << '\n' << derivative << '\n'
  /* We should see 0's on all boundaries from the smoothing routine */
  {
    linelength = dimLength * dimensions;
    for (unsigned int i = 0; i < displacementTransform->GetNumberOfParameters(); ++i)
    {
      bool ok = true;
      if (i < linelength && itk::Math::NotAlmostEquals(params[i], paramsZero))
      {
        ok = false;
      }
      if (i % linelength == 0 && itk::Math::NotAlmostEquals(params[i], paramsZero))
      {
        ok = false;
      }
      if (i % linelength == (linelength - 1) && itk::Math::NotAlmostEquals(params[i], paramsZero))
      {
        ok = false;
      }
      if (!ok)
      {
        std::cout << "0-valued boundaries not found when expected "
                  << "after UpdateTransformParameters:" << '\n';
        std::cout << "params: " << '\n' << params << '\n';
        return EXIT_FAILURE;
      }
    }
  }

  /* Update with an uneven field to verify some smoothing is happening. */
  field->FillBuffer(zeroVector);
  update.Fill(1.0);
  update(outlier) = 99.0;
  update(outlier + 1) = 99.0;
  displacementTransform->UpdateTransformParameters(update);
  params = displacementTransform->GetParameters();
  std::cout << "UpdateTransformParameters with uneven update: " << '\n' << "params: " << '\n' << params << '\n';
  /* Check that we have some smoothing around the outlier we set above. */
  std::cout << "Parameters *after* UpdateTransformParameters with "
            << "uneven field, around outlier: " << '\n';
  for (int i = -2; i < 3; ++i)
  {
    for (int j = -2; j < 3; ++j)
    {
      const unsigned int index = outlier + static_cast<unsigned int>(i * (int)(dimLength * dimensions) + j);
      std::cout << params(index) << ' ';
      if (itk::Math::AlmostEquals(params(index), paramsFillValue))
      {
        std::cout << "Expected to read a smoothed value at this index."
                  << " Instead, read " << params(index) << '\n';
        return EXIT_FAILURE;
      }
    }
    std::cout << '\n';
  }

  /* Exercise Get/Set sigma */
  displacementTransform->SetGaussianSmoothingVarianceForTheUpdateField(2);
  std::cout << "sigma: " << displacementTransform->GetGaussianSmoothingVarianceForTheUpdateField() << '\n';

  displacementTransform->SetGaussianSmoothingVarianceForTheTotalField(2);
  std::cout << "sigma: " << displacementTransform->GetGaussianSmoothingVarianceForTheTotalField() << '\n';

  return EXIT_SUCCESS;
}
