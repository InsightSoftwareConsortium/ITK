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

#include "itkAffineTransform.h"
#include "itkSimilarity3DTransform.h"
#include "itkTestingMacros.h"

// Verifies the polar-decomposition path that DCMTKTransformIO uses for
// DICOM RIGID_SCALE: a rotation*scale matrix built from a known versor,
// scale, and translation must round-trip through
// Similarity3DTransform::SetMatrix(m, tol) + SetOffset(t) back to the
// same parameters, and produce identical point mappings to the affine
// the IO assembles from the DICOM Decimal-String entries.
int
itkDCMTKTransformIORigidScaleRoundTripTest(int, char *[])
{
  int testStatus = EXIT_SUCCESS;

  constexpr unsigned int Dimension = 3;
  using ScalarType = double;
  using SimilarityTransformType = itk::Similarity3DTransform<ScalarType>;
  using AffineTransformType = itk::AffineTransform<ScalarType, Dimension>;

  const ScalarType                  expectedScale = 1.25;
  SimilarityTransformType::AxisType axis;
  axis[0] = 0.0;
  axis[1] = 0.0;
  axis[2] = 1.0;
  constexpr ScalarType                      angle = 0.3; // radians
  SimilarityTransformType::OutputVectorType expectedTranslation;
  expectedTranslation[0] = 7.0;
  expectedTranslation[1] = -3.0;
  expectedTranslation[2] = 2.5;

  auto expected = SimilarityTransformType::New();
  expected->SetRotation(axis, angle);
  expected->SetScale(expectedScale);
  expected->SetTranslation(expectedTranslation);

  // The IO assembles an AffineTransform from the 12 DICOM matrix entries
  // before the matrix-type branch fires.
  auto affine = AffineTransformType::New();
  affine->SetMatrix(expected->GetMatrix());
  affine->SetOffset(expected->GetOffset());

  // Exercise the exact code path the IO uses for RIGID_SCALE.
  auto similarity = SimilarityTransformType::New();
  similarity->SetMatrix(affine->GetMatrix(), 1e-5);
  similarity->SetOffset(affine->GetOffset());

  ITK_TEST_EXPECT_TRUE_STATUS_VALUE(itk::Math::Absolute(similarity->GetScale() - expectedScale) < 1e-10, testStatus);

  const auto recoveredVersor = similarity->GetVersor();
  const auto expectedVersor = expected->GetVersor();
  ITK_TEST_EXPECT_TRUE_STATUS_VALUE(itk::Math::Absolute(recoveredVersor.GetX() - expectedVersor.GetX()) < 1e-10,
                                    testStatus);
  ITK_TEST_EXPECT_TRUE_STATUS_VALUE(itk::Math::Absolute(recoveredVersor.GetY() - expectedVersor.GetY()) < 1e-10,
                                    testStatus);
  ITK_TEST_EXPECT_TRUE_STATUS_VALUE(itk::Math::Absolute(recoveredVersor.GetZ() - expectedVersor.GetZ()) < 1e-10,
                                    testStatus);
  ITK_TEST_EXPECT_TRUE_STATUS_VALUE(itk::Math::Absolute(recoveredVersor.GetW() - expectedVersor.GetW()) < 1e-10,
                                    testStatus);

  const auto recoveredTranslation = similarity->GetTranslation();
  for (unsigned int d = 0; d < Dimension; ++d)
  {
    ITK_TEST_EXPECT_TRUE_STATUS_VALUE(itk::Math::Absolute(recoveredTranslation[d] - expectedTranslation[d]) < 1e-10,
                                      testStatus);
  }

  AffineTransformType::InputPointType probes[3];
  probes[0][0] = 1.0;
  probes[0][1] = 0.0;
  probes[0][2] = 0.0;
  probes[1][0] = 0.0;
  probes[1][1] = 1.0;
  probes[1][2] = 0.0;
  probes[2][0] = 5.0;
  probes[2][1] = -2.0;
  probes[2][2] = 3.0;
  for (const auto & p : probes)
  {
    const auto aOut = affine->TransformPoint(p);
    const auto sOut = similarity->TransformPoint(p);
    for (unsigned int d = 0; d < Dimension; ++d)
    {
      ITK_TEST_EXPECT_TRUE_STATUS_VALUE(itk::Math::Absolute(aOut[d] - sOut[d]) < 1e-10, testStatus);
    }
  }

  return testStatus;
}
