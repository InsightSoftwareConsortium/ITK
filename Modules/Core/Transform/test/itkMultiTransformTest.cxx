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

#include "itkAffineTransform.h"
#include "itkMultiTransform.h"
#include "itkTranslationTransform.h"
#include "itkTestingMacros.h"
#include "itkDisplacementFieldTransform.h"

namespace
{

const double epsilon = 1e-10;

template <typename TMatrix>
bool
testMatrix(const TMatrix & m1, const TMatrix & m2)
{
  bool pass = true;

  for (unsigned int i = 0; i < TMatrix::RowDimensions; ++i)
  {
    for (unsigned int j = 0; j < TMatrix::ColumnDimensions; ++j)
    {
      if (itk::Math::abs(m1[i][j] - m2[i][j]) > epsilon)
      {
        pass = false;
      }
    }
  }
  return pass;
}

template <typename TVector>
bool
testVectorArray(const TVector & v1, const TVector & v2)
{
  bool pass = true;

  for (unsigned int i = 0; i < v1.Size(); ++i)
  {
    if (itk::Math::abs(v1[i] - v2[i]) > epsilon)
    {
      pass = false;
    }
  }
  return pass;
}

} // namespace

/******/

constexpr unsigned int itkMultiTransformTestNDimensions = 2;

template <class TScalar = double, unsigned int VDimension = itkMultiTransformTestNDimensions>
class MultiTransformTestTransform : public itk::MultiTransform<TScalar, VDimension>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(MultiTransformTestTransform);

  /** Standard class type aliases. */
  using Self = MultiTransformTestTransform;
  using Superclass = itk::MultiTransform<TScalar, VDimension, VDimension>;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(MultiTransformTestTransform);

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro(Self);

  /** Sub transform type **/
  using TransformType = typename Superclass::TransformType;
  using typename Superclass::TransformTypePointer;
  /** InverseTransform type. */
  using typename Superclass::InverseTransformBasePointer;
  using typename Superclass::InputPointType;
  using typename Superclass::JacobianType;

  typename Superclass::OutputPointType
  TransformPoint(const InputPointType & point) const override
  {
    return point;
  }

  void
  ComputeJacobianWithRespectToParameters(const InputPointType & itkNotUsed(p),
                                         JacobianType &         itkNotUsed(jacobian)) const override
  {
    itkExceptionMacro("ComputeJacobianWithRespectToParamters( InputPointType, JacobianType"
                      " is unimplemented for "
                      << this->GetNameOfClass());
  }

protected:
  MultiTransformTestTransform() = default;
  ~MultiTransformTestTransform() override = default;
};

/******/

int
itkMultiTransformTest(int, char *[])
{
  const unsigned int VDimension = itkMultiTransformTestNDimensions;

  /* Create multi-transform */
  using MultiTransformType = MultiTransformTestTransform<double, VDimension>;
  using Superclass = MultiTransformType::Superclass;
  using ScalarType = Superclass::ScalarType;

  auto multiTransform = MultiTransformType::New();

  /* Test obects */
  using Matrix2Type = itk::Matrix<ScalarType, VDimension, VDimension>;
  using Vector2Type = itk::Vector<ScalarType, VDimension>;

  /* Test that we have an empty the queue */
  if (multiTransform->GetNumberOfTransforms() != 0)
  {
    std::cout << "Failed. Expected GetNumberOfTransforms to return 0." << '\n';
    return EXIT_FAILURE;
  }

  if (!multiTransform->IsTransformQueueEmpty())
  {
    std::cout << "ERROR: expected IsTransformQueueEmpty to return true." << '\n';
    return EXIT_FAILURE;
  }

  /* Add an affine transform */
  using AffineType = itk::AffineTransform<ScalarType, VDimension>;
  auto        affine = AffineType::New();
  Matrix2Type matrix2;
  matrix2[0][0] = 0;
  matrix2[0][1] = -1;
  matrix2[1][0] = 1;
  matrix2[1][1] = 0;
  Vector2Type vector2;
  vector2[0] = 10;
  vector2[1] = 100;
  affine->SetMatrix(matrix2);
  affine->SetOffset(vector2);

  multiTransform->AddTransform(affine);

  /* Test that we have one transform in the queue */
  if (multiTransform->IsTransformQueueEmpty())
  {
    std::cout << "ERROR: expected IsTransformQueueEmpty to return false." << '\n';
    return EXIT_FAILURE;
  }
  if (multiTransform->GetNumberOfTransforms() != 1)
  {
    std::cout << "Failed adding transform to queue." << '\n';
    return EXIT_FAILURE;
  }

  /* Retrieve the transform and check that it's the same */
  std::cout << "Retrieve 1st transform." << '\n';
  const AffineType::ConstPointer affineGet =
    dynamic_cast<const AffineType *>(multiTransform->GetNthTransformConstPointer(0));
  if (affineGet.IsNull())
  {
    std::cout << "Failed retrieving transform from queue." << '\n';
    return EXIT_FAILURE;
  }

  std::cout << "Retrieve matrix and offset. " << '\n';
  const Matrix2Type matrix2Get = affineGet->GetMatrix();
  const Vector2Type vector2Get = affineGet->GetOffset();
  if (!testMatrix(matrix2, matrix2Get) || !testVectorArray(vector2, vector2Get))
  {
    std::cout << "Failed retrieving correct transform data." << '\n';
    return EXIT_FAILURE;
  }

  /* Get parameters with single transform.
   * Should be same as GetParameters from affine transform. */
  std::cout << "Get Parameters: " << '\n';
  Superclass::ParametersType parametersTest = multiTransform->GetParameters();
  Superclass::ParametersType parametersTruth = affine->GetParameters();
  std::cout << "affine parametersTruth: " << '\n'
            << parametersTruth << '\n'
            << "parametersTest from Multi: " << '\n'
            << parametersTest << '\n';

  if (!testVectorArray(parametersTest, parametersTruth))
  {
    std::cout << "Failed GetParameters() for single transform." << '\n';
    return EXIT_FAILURE;
  }

  /* Set parameters with single transform. */
  Superclass::ParametersType parametersNew(6);
  parametersNew[0] = 0;
  parametersNew[1] = 10;
  parametersNew[2] = 20;
  parametersNew[3] = 30;
  parametersNew[4] = 40;
  parametersNew[5] = 50;
  std::cout << "Set Parameters: " << '\n';
  multiTransform->SetParameters(parametersNew);
  std::cout << "retrieving... " << '\n';
  Superclass::ParametersType parametersReturned = multiTransform->GetParameters();
  std::cout << "parametersNew: " << '\n'
            << parametersNew << '\n'
            << "parametersReturned: " << '\n'
            << parametersReturned << '\n';
  if (!testVectorArray(parametersNew, parametersReturned))
  {
    std::cout << "Failed SetParameters() for single transform." << '\n';
    return EXIT_FAILURE;
  }

  /* Test fixed parameters set/get */
  parametersTest = multiTransform->GetFixedParameters();
  parametersTruth = affine->GetFixedParameters();
  std::cout << "Get Fixed Parameters: " << '\n'
            << "affine parametersTruth: " << '\n'
            << parametersTruth << '\n'
            << "parametersTest from Multi: " << '\n'
            << parametersTest << '\n';

  if (!testVectorArray(parametersTest, parametersTruth))
  {
    std::cout << "Failed GetFixedParameters() for single transform." << '\n';
    return EXIT_FAILURE;
  }

  parametersNew.SetSize(parametersTruth.Size());
  parametersNew.Fill(1);
  parametersNew[0] = 42;

  std::cout << "Set Fixed Parameters: " << '\n';
  multiTransform->SetFixedParameters(parametersNew);
  std::cout << "retrieving... " << '\n';
  parametersReturned = multiTransform->GetFixedParameters();
  std::cout << "parametersNew: " << '\n'
            << parametersNew << '\n'
            << "parametersReturned: " << '\n'
            << parametersReturned << '\n';
  if (!testVectorArray(parametersNew, parametersReturned))
  {
    std::cout << "Failed SetFixedParameters() for single transform." << '\n';
    return EXIT_FAILURE;
  }

  /* Reset affine transform to original values */
  multiTransform->ClearTransformQueue();

  if (multiTransform->GetNumberOfTransforms() != 0)
  {
    std::cout << "Failed. Expected GetNumberOfTransforms to return 0 after ClearTransformQueue." << '\n';
    return EXIT_FAILURE;
  }

  affine = AffineType::New();
  affine->SetMatrix(matrix2);
  affine->SetOffset(vector2);
  multiTransform->AddTransform(affine);

  /* Test inverse */
  auto inverseMultiTransform = MultiTransformType::New();
  if (!multiTransform->GetInverse(inverseMultiTransform))
  {
    std::cout << "ERROR: GetInverse() failed." << '\n';
    return EXIT_FAILURE;
  }
  auto inverseAffine = AffineType::New();
  if (!affine->GetInverse(inverseAffine))
  {
    std::cout << "FAILED getting inverse of affine." << '\n';
    return EXIT_FAILURE;
  }
  if (!testVectorArray(inverseAffine->GetParameters(), inverseMultiTransform->GetParameters()))
  {
    std::cout << "ERROR: Wrong parameters for single-transform inverse." << '\n';
    return EXIT_FAILURE;
  }

  /* Test IsLinear() */
  std::cout << "Test IsLinear" << '\n';
  if (!multiTransform->IsLinear())
  {
    std::cout << "ERROR: expected true for IsLinear but got false." << '\n';
    return EXIT_FAILURE;
  }

  /* GetTransformCategory */
  if (multiTransform->GetTransformCategory() != affine->GetTransformCategory())
  {
    std::cout << "ERROR: GetTransformCategory returned " << multiTransform->GetTransformCategory() << " instead of "
              << affine->GetTransformCategory() << '\n';
    return EXIT_FAILURE;
  }

  /*
   * Create and add 2nd transform, displacement field
   */

  /* Create a displacement field transform */
  using DisplacementTransformType = itk::DisplacementFieldTransform<double, VDimension>;
  auto displacementTransform = DisplacementTransformType::New();
  using FieldType = DisplacementTransformType::DisplacementFieldType;
  auto field = FieldType::New(); // This is based on itk::Image


  const int                  dimLength = 4;
  auto                       size = itk::MakeFilled<FieldType::SizeType>(dimLength);
  const FieldType::IndexType start{};
  FieldType::RegionType      region;
  region.SetSize(size);
  region.SetIndex(start);
  field->SetRegions(region);
  field->Allocate();

  DisplacementTransformType::OutputVectorType vector;
  vector[0] = 0.5;
  vector[1] = 1.0;
  field->FillBuffer(vector);

  displacementTransform->SetDisplacementField(field);

  /* set inverse field */
  auto inverseField = FieldType::New(); // This is based on itk::Image
  inverseField->SetRegions(region);
  inverseField->Allocate();
  DisplacementTransformType::OutputVectorType inverseVector;
  inverseVector[0] = 0.5;
  inverseVector[1] = 1.0;
  inverseField->FillBuffer(inverseVector);
  displacementTransform->SetInverseDisplacementField(inverseField);

  std::cout << '\n' << " ** Adding 2nd transform ** " << '\n' << '\n';

  multiTransform->ClearTransformQueue();
  multiTransform->AppendTransform(displacementTransform);
  multiTransform->PrependTransform(affine);

  std::cout << '\n' << "Two-component Multi Transform:" << '\n' << multiTransform;

  /* Test that we have two transforms in the queue */
  if (multiTransform->GetNumberOfTransforms() != 2)
  {
    std::cout << "Failed adding 2nd transform to queue." << '\n';
    return EXIT_FAILURE;
  }

  if (multiTransform->GetNthTransformConstPointer(0) != affine)
  {
    std::cout << "ERROR: 1st transform is not affine as expected." << '\n';
    return EXIT_FAILURE;
  }

  if (multiTransform->GetNthTransformConstPointer(1) != displacementTransform)
  {
    std::cout << "ERROR: 2nd transform is not displacementTransform as expected." << '\n';
    return EXIT_FAILURE;
  }

  /* Test inverse with two transforms. */
  if (!multiTransform->GetInverse(inverseMultiTransform))
  {
    std::cout << "Expected GetInverse() to succeed." << '\n';
    return EXIT_FAILURE;
  }
  std::cout << '\n' << "Inverse two-component transform: " << inverseMultiTransform;

  /* Test inverse parameters using settings from above. */

  if (!testVectorArray(inverseAffine->GetParameters(),
                       inverseMultiTransform->GetNthTransformConstPointer(0)->GetParameters()))
  {
    std::cout << "ERROR: Wrong parameters for affine in two-transform inverse." << '\n';
    return EXIT_FAILURE;
  }

  auto inverseDisplacement = DisplacementTransformType::New();
  if (!displacementTransform->GetInverse(inverseDisplacement))
  {
    std::cout << "FAILED getting inverse of displacementTransform." << '\n';
    return EXIT_FAILURE;
  }

  if (!testVectorArray(inverseDisplacement->GetParameters(),
                       inverseMultiTransform->GetNthTransformConstPointer(1)->GetParameters()))
  {
    std::cout << "ERROR: Wrong parameters for displacementTransform in two-transform inverse." << '\n';
    return EXIT_FAILURE;
  }

  /* Test IsLinear() */
  std::cout << "Test IsLinear" << '\n';
  if (multiTransform->IsLinear())
  {
    std::cout << "ERROR: expected false for IsLinear but got true." << '\n';
    return EXIT_FAILURE;
  }

  /* Test GetNumberOfParameters */
  std::cout << "GetNumberOfParameters: " << '\n';
  unsigned int       affineParamsN = affine->GetNumberOfParameters();
  unsigned int       displacementParamsN = displacementTransform->GetNumberOfParameters();
  const unsigned int nParameters = multiTransform->GetNumberOfParameters();
  std::cout << "Number of parameters: " << nParameters << '\n';
  if (nParameters != affineParamsN + displacementParamsN)
  {
    std::cout << "GetNumberOfParameters failed for two-transform." << '\n'
              << "Expected " << affineParamsN + displacementParamsN << '\n';
  }

  /* Test GetNumberOfLocalParameters */
  std::cout << "GetNumberOfLocalParameters: " << '\n';
  const unsigned int affineLocalParamsN = affine->GetNumberOfLocalParameters();
  const unsigned int displacementLocalParamsN = displacementTransform->GetNumberOfLocalParameters();
  const unsigned int nLocalParameters = multiTransform->GetNumberOfLocalParameters();
  std::cout << "Number of local parameters: " << nParameters << '\n';
  if (nLocalParameters != affineLocalParamsN + displacementLocalParamsN)
  {
    std::cout << "GetNumberOfLocalParameters failed for two-transform." << '\n'
              << "Expected " << affineLocalParamsN + displacementLocalParamsN << '\n';
  }

  /* Get parameters with multi-transform. They're filled from transforms in same order as queue. */
  parametersTest = multiTransform->GetParameters();
  parametersTruth.SetSize(displacementParamsN + affineParamsN);
  /* Fill using different method than is used in the class. */
  for (unsigned int n = 0; n < affineParamsN; ++n)
  {
    parametersTruth.SetElement(n, affine->GetParameters().GetElement(n));
  }
  for (unsigned int n = 0; n < displacementParamsN; ++n)
  {
    parametersTruth.SetElement(n + affineParamsN, displacementTransform->GetParameters().GetElement(n));
  }
  std::cout << "Get Multi-transform Parameters: " << '\n'
            << "parametersTruth: " << '\n'
            << parametersTruth << '\n'
            << "parametersTest from Multi: " << '\n'
            << parametersTest << '\n';

  if (!testVectorArray(parametersTest, parametersTruth))
  {
    std::cout << "Failed GetParameters() for two-transform." << '\n';
    return EXIT_FAILURE;
  }

  /* Set parameters with multi transform. */
  parametersNew.SetSize(parametersTruth.Size());
  parametersNew.Fill(3.14);
  parametersNew[0] = 19;
  parametersNew[parametersTruth.Size() - 1] = 71;
  std::cout << "Set Multi-transform Parameters: " << '\n';
  multiTransform->SetParameters(parametersNew);
  std::cout << "retrieving... " << '\n';
  parametersReturned = multiTransform->GetParameters();
  // std::cout << "parametersNew: " << '\n' << parametersNew << '\n' << "parametersReturned: " << '\n' <<
  // parametersReturned << '\n';
  if (!testVectorArray(parametersNew, parametersReturned))
  {
    std::cout << "Failed SetParameters() for two-transform." << '\n';
    return EXIT_FAILURE;
  }

  /* reset the original parameters */
  multiTransform->SetParameters(parametersTruth);

  /* Test get fixed parameters with multi-transform */
  std::cout << "Get Multi-transform Fixed Parameters: " << '\n';
  parametersTest = multiTransform->GetFixedParameters();
  affineParamsN = affine->GetFixedParameters().Size();
  displacementParamsN = displacementTransform->GetFixedParameters().Size();
  parametersTruth.SetSize(displacementParamsN + affineParamsN);
  parametersTruth.Fill(0); // Try this to quiet valgrind
  for (unsigned int n = 0; n < affineParamsN; ++n)
  {
    parametersTruth.SetElement(n, affine->GetFixedParameters().GetElement(n));
  }
  for (unsigned int n = 0; n < displacementParamsN; ++n)
  {
    parametersTruth.SetElement(n + affineParamsN, displacementTransform->GetFixedParameters().GetElement(n));
  }
  std::cout << '\n'
            << "Fixed: parametersTruth: " << '\n'
            << parametersTruth << '\n'
            << "parametersTest: " << '\n'
            << parametersTest << '\n';

  if (!testVectorArray(parametersTest, parametersTruth))
  {
    std::cout << "Failed GetFixedParameters() for multi transform." << '\n';
    return EXIT_FAILURE;
  }

  /* Test set fixed parameters with multi-transform */
  std::cout << "Set Multi-transform Fixed Parameters: " << '\n';
  multiTransform->SetFixedParameters(parametersTruth);
  std::cout << "retrieving... " << '\n';
  parametersReturned = multiTransform->GetFixedParameters();
  std::cout << "parametersTruth: " << '\n'
            << parametersTruth << '\n'
            << "parametersReturned: " << '\n'
            << parametersReturned << '\n';

  if (!testVectorArray(parametersTruth, parametersReturned))
  {
    std::cout << "Failed SetFixedParameters() for multi transform." << '\n';
    return EXIT_FAILURE;
  }

  /* reset the original inverse field */
  displacementTransform->SetInverseDisplacementField(inverseField);

  /* Test GetNumberOfFixedParameters */
  std::cout << "GetNumberOfFixedParameters: " << '\n';
  const unsigned int affineFixedParamsN = affine->GetFixedParameters().size();
  const unsigned int displacementFixedParamsN = displacementTransform->GetFixedParameters().size();
  const unsigned int nFixedParameters = multiTransform->GetNumberOfFixedParameters();
  std::cout << "Number of Fixed parameters: " << nParameters << '\n';
  if (nFixedParameters != affineFixedParamsN + displacementFixedParamsN)
  {
    std::cout << "GetNumberOfFixedParameters failed for two-transform." << '\n'
              << "Expected " << affineFixedParamsN + displacementFixedParamsN << '\n';
  }

  /*
   * Add a third transform
   */
  auto affine3 = AffineType::New();
  matrix2[0][0] = 0;
  matrix2[0][1] = -1;
  matrix2[1][0] = 1;
  matrix2[1][1] = 0;
  vector2[0] = 10;
  vector2[1] = 100;
  affine3->SetMatrix(matrix2);
  affine3->SetOffset(vector2);

  multiTransform->AddTransform(affine3);

  /* Reset first affine to non-singular values */
  affine->SetMatrix(matrix2);
  affine->SetOffset(vector2);

  /* Test accessors */
  const Superclass::TransformQueueType transformQueue = multiTransform->GetTransformQueue();
  if (transformQueue.size() != 3)
  {
    std::cout << "Failed getting transform queue." << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "Got TransformQueue." << '\n';

  std::cout << "MultiTransform with 3 transforms: " << '\n' << multiTransform << '\n';

  /* Get inverse */
  auto inverseTransform3 = MultiTransformType::New();
  if (!multiTransform->GetInverse(inverseTransform3))
  {
    std::cout << "Failed calling GetInverse() (3)." << '\n';
    return EXIT_FAILURE;
  }

  /* Test UpdateTransformParameters.
   * NOTE Once there are transforms that do something other than simple
   * addition in TransformUpdateParameters, this should be updated here.
   */
  {
    std::cout << "Testing UpdateTransformParameters 1. " << '\n';
    Superclass::ParametersType truth = multiTransform->GetParameters();
    Superclass::DerivativeType update(multiTransform->GetNumberOfParameters());
    update.Fill(10.0);
    const AffineType::ScalarType factor = 0.5;
    truth += update * factor;
    multiTransform->UpdateTransformParameters(update, factor);
    const Superclass::ParametersType updateResult = multiTransform->GetParameters();
    if (!testVectorArray(truth, updateResult))
    {
      std::cout << "UpdateTransformParameters 1 failed. " << '\n'
                << " truth:  " << truth << '\n'
                << " result: " << updateResult << '\n';
      return EXIT_FAILURE;
    }

    /* Test with wrong size for update */
    update.SetSize(1);
    ITK_TRY_EXPECT_EXCEPTION(multiTransform->UpdateTransformParameters(update, factor));
  }

  /* Test SetParameters with wrong size array */
  std::cout << '\n' << "Test SetParameters with wrong size array. EXPECT EXCEPTION:" << '\n';
  parametersTruth.SetSize(1);
  ITK_TRY_EXPECT_EXCEPTION(multiTransform->SetParameters(parametersTruth));

  /* Test SetFixedParameters with wrong size array */
  std::cout << '\n' << "Test SetFixedParameters with wrong size array. EXPECT EXCEPTION:" << '\n';
  ITK_TRY_EXPECT_EXCEPTION(multiTransform->SetFixedParameters(parametersTruth));

  /* GetTransformCategory */
  if (multiTransform->GetTransformCategory() != MultiTransformType::TransformCategoryEnum::UnknownTransformCategory)
  {
    std::cout << "ERROR: GetTransformCategory returned " << multiTransform->GetTransformCategory()
              << " instead of Unknown." << '\n';
    return EXIT_FAILURE;
  }

  /* RemoveTransform. From end of queue */
  multiTransform->RemoveTransform();
  if (multiTransform->GetNumberOfTransforms() != 2)
  {
    std::cout << "ERROR: Wrong number of transforms after RemoveTransform: " << multiTransform->GetNumberOfTransforms()
              << '\n';
    return EXIT_FAILURE;
  }

  /* Get front and back transforms */
  if (multiTransform->GetFrontTransform() != affine)
  {
    std::cout << "ERROR: GetFrontTransform failed." << '\n';
  }
  if (multiTransform->GetBackTransform() != displacementTransform)
  {
    std::cout << "ERROR: GetBackTransform failed." << '\n';
  }

  /* Test printing */
  multiTransform->Print(std::cout);

  std::cout << "Passed test!" << '\n';
  return EXIT_SUCCESS;
}
