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
#ifndef itkLandmarkBasedTransformInitializer_hxx
#define itkLandmarkBasedTransformInitializer_hxx

#include "itkMatrix.h"
#include "itkSymmetricEigenAnalysis.h"
#include "itkPrintHelper.h"

#include <cmath>
#include "vnl/algo/vnl_qr.h"

namespace itk
{
/** Default transform initializer, if transform type isn't
 * specifically handled.
 */
template <typename TTransform, typename TFixedImage, typename TMovingImage>
template <typename TTransform2>
void
LandmarkBasedTransformInitializer<TTransform, TFixedImage, TMovingImage>::InternalInitializeTransform(TTransform2 *)
{

  if (dynamic_cast<BSplineTransformType *>(this->m_Transform.GetPointer()) != nullptr)
  {
    this->InternalInitializeTransform(static_cast<BSplineTransformType *>(nullptr));
    return;
  }

  if (dynamic_cast<AffineTransformType *>(this->m_Transform.GetPointer()) != nullptr)
  {
    this->InternalInitializeTransform(static_cast<AffineTransformType *>(nullptr));
    return;
  }

  if (dynamic_cast<Similarity3DTransformType *>(this->m_Transform.GetPointer()) != nullptr)
  {
    this->InternalInitializeTransform(static_cast<Similarity3DTransformType *>(nullptr));
    return;
  }

  if (dynamic_cast<VersorRigid3DTransformType *>(this->m_Transform.GetPointer()) != nullptr)
  {
    this->InternalInitializeTransform(static_cast<VersorRigid3DTransformType *>(nullptr));
    return;
  }

  if (dynamic_cast<Rigid2DTransformType *>(this->m_Transform.GetPointer()) != nullptr)
  {
    this->InternalInitializeTransform(static_cast<Rigid2DTransformType *>(nullptr));
    return;
  }

  itkExceptionMacro("Unsupported Transform Type " << this->m_Transform->GetNameOfClass());
}

template <typename TTransform, typename TFixedImage, typename TMovingImage>
void
LandmarkBasedTransformInitializer<TTransform, TFixedImage, TMovingImage>::InternalInitializeTransform(
  BSplineTransformType *)
{
  auto * transform = dynamic_cast<BSplineTransformType *>(this->m_Transform.GetPointer());
  if (transform == nullptr)
  {
    itkExceptionMacro("BSplineTransform Expected but transform is " << this->m_Transform->GetNameOfClass());
  }
  if (m_ReferenceImage.IsNull())
  {
    itkExceptionMacro(
      << "Reference image required for BSplineTransform initialization is NULL (not set or set to NULL).");
  }

  const size_t numberOfLandMarks = this->m_MovingLandmarks.size();

  // Instantiating B-spline filter and creating B-spline domain.

  using VectorType = Vector<double, ImageDimension>;
  using VectorImageType = Image<VectorType, ImageDimension>;
  using PointSetType = PointSet<typename VectorImageType::PixelType, ImageDimension>;
  using FilterType = BSplineScatteredDataPointSetToImageFilter<PointSetType, VectorImageType>;

  using WeightsContainerType = typename FilterType::WeightsContainerType;
  auto weights = WeightsContainerType::New();
  weights->Reserve(numberOfLandMarks);

  if (!this->m_LandmarkWeight.empty())
  {
    if (this->m_LandmarkWeight.size() != numberOfLandMarks)
    {
      itkExceptionMacro("Size mismatch between number of landmarks pairs and weights");
    }
    auto weightIt = this->m_LandmarkWeight.begin();
    for (unsigned int i = 0; weightIt != this->m_LandmarkWeight.end(); ++i, ++weightIt)
    {
      weights->InsertElement(i, (*weightIt));
    }
  }
  else
  {
    // Set weights to identity.
    for (unsigned int i = 0; i < numberOfLandMarks; ++i)
    {
      weights->InsertElement(i, 1);
    }
  }

  // Set a pointSet from the input landmarks.
  auto pointSet = PointSetType::New();

  auto fixedIt = this->m_FixedLandmarks.begin();
  auto movingIt = this->m_MovingLandmarks.begin();
  for (size_t i = 0; fixedIt != this->m_FixedLandmarks.end(); ++i, ++fixedIt, ++movingIt)
  {
    pointSet->SetPoint(static_cast<typename PointSetType::PointIdentifier>(i), (*fixedIt));
    VectorType vectorTmp;
    for (unsigned int d = 0; d < ImageDimension; ++d)
    {
      vectorTmp[d] = (*movingIt)[d] - (*fixedIt)[d];
    }
    pointSet->SetPointData(static_cast<typename PointSetType::PointIdentifier>(i), vectorTmp);
  }

  const typename VectorImageType::SizeType      size = this->m_ReferenceImage->GetLargestPossibleRegion().GetSize();
  const typename VectorImageType::PointType     origin = this->m_ReferenceImage->GetOrigin();
  const typename VectorImageType::SpacingType   spacing = this->m_ReferenceImage->GetSpacing();
  const typename VectorImageType::DirectionType direction = this->m_ReferenceImage->GetDirection();

  auto filter = FilterType::New();
  // Define the parametric domain.
  filter->SetOrigin(origin);
  filter->SetSpacing(spacing);
  filter->SetSize(size);
  filter->SetDirection(direction);
  filter->SetInput(pointSet);
  filter->SetPointWeights(weights);
  filter->SetGenerateOutputImage(false);
  filter->SetSplineOrder(SplineOrder);

  auto ncps = MakeFilled<typename FilterType::ArrayType>(
    this->m_BSplineNumberOfControlPoints); // Should be greater than SplineOrder
  filter->SetNumberOfControlPoints(ncps);

  filter->SetNumberOfLevels(3);

  const typename FilterType::ArrayType close{};
  filter->SetCloseDimension(close);

  filter->Update();

  // Set the BSpline transform.

  using CoefficientImageType = typename BSplineTransformType::ImageType;

  typename BSplineTransformType::CoefficientImageArray coefficientImages;
  for (unsigned int j = 0; j < ImageDimension; ++j)
  {
    using SelectorType = VectorIndexSelectionCastImageFilter<VectorImageType, CoefficientImageType>;
    auto selector = SelectorType::New();
    selector->SetInput(filter->GetPhiLattice());
    selector->SetIndex(j);

    coefficientImages[j] = selector->GetOutput();
    coefficientImages[j]->Update();
    coefficientImages[j]->DisconnectPipeline();
  }

  transform->SetCoefficientImages(coefficientImages);
}

/** Compute an affine transform from landmark set.
 *  This was implemented by Eun Young Kim based on method
 *  described in the paper "Closed-form solution of absolute
 *  orientation using orthonormal matrices" by Horn, Hilden, and
 *  Negahdaripour. See http://www.opticsinfobase.org/abstract.cfm?&id=3143
 */
template <typename TTransform, typename TFixedImage, typename TMovingImage>
void
LandmarkBasedTransformInitializer<TTransform, TFixedImage, TMovingImage>::InternalInitializeTransform(
  AffineTransformType *)
{
  auto * transform = dynamic_cast<AffineTransformType *>(this->m_Transform.GetPointer());
  if (transform == nullptr)
  {
    itkExceptionMacro("AffineTransform Expected but transform is " << this->m_Transform->GetNameOfClass());
  }

  const auto numberOfLandmarks = static_cast<const unsigned int>(this->m_MovingLandmarks.size());

  if (numberOfLandmarks < LandmarkPointContainer::value_type::GetPointDimension() + 1)
  {
    itkExceptionMacro(" insufficient number of landmarks, expected "
                      << LandmarkPointContainer::value_type::GetPointDimension() + 1 << " got " << numberOfLandmarks);
  }


  // Set Landmark Weight.
  // If no landmark weights are given, weight matrix is identity matrix.
  vnl_matrix<ParametersValueType> vnlWeight(numberOfLandmarks, numberOfLandmarks, 0);
  vnlWeight.set_identity();

  if (!this->m_LandmarkWeight.empty())
  {
    if (this->m_LandmarkWeight.size() != numberOfLandmarks)
    {
      itkExceptionMacro(" size mismatch between number of landmars pairs and weights");
    }
    auto weightIt = this->m_LandmarkWeight.begin();
    for (unsigned int i = 0; weightIt != this->m_LandmarkWeight.end(); ++i, ++weightIt)
    {
      vnlWeight(i, i) = (*weightIt);
    }
  }
  // Normalize weights.
  vnlWeight = vnlWeight / vnlWeight.fro_norm();

  // Convert Point to Matrix for Matrix Multiplication

  // q
  // dim+1=4 * numberOfLandmarks matrix
  vnl_matrix<ParametersValueType> q(ImageDimension + 1, numberOfLandmarks, 0.0F);
  auto                            fixedIt = this->m_FixedLandmarks.begin();
  for (unsigned int i = 0; fixedIt != this->m_FixedLandmarks.end(); ++i, ++fixedIt)
  {
    for (unsigned int dim = 0; dim < ImageDimension; ++dim)
    {
      q(dim, i) = (*fixedIt)[dim];
    }
    q(ImageDimension, i) = 1.0F;
  }
  q *= vnlWeight;

  // p
  // dim=3 * numberOfLandmarks matrix
  vnl_matrix<ParametersValueType> p(ImageDimension, numberOfLandmarks, 0.0F);
  auto                            movingIt = this->m_MovingLandmarks.begin();
  for (unsigned int i = 0; movingIt != this->m_MovingLandmarks.end(); ++i, ++movingIt)
  {
    for (unsigned int dim = 0; dim < ImageDimension; ++dim)
    {
      p(dim, i) = (*movingIt)[dim];
    }
  }

  p *= vnlWeight;
  // EX) 3 dimensional case
  //          | sum( q(1,i)*q(1,i) sum( q(1,i)*q(2,i) sum( q(1,i)*q(3,i) sum( q(1,i) ) |
  // Q[4x4] = | sum( q(2,i)*q(1,i) sum( q(2,i)*q(2,i) sum( q(2,i)*q(3,i) sum( q(2,i) ) |
  //          | sum( q(3,i)*q(1,i) sum( q(3,i)*q(2,i) sum( q(3,i)*q(3,i) sum( q(3,i) ) |
  //          | sum( q(1,i) )      sum( q(2,i) )      sum( q(3,i) )      sum( q(4,i) ) |
  //
  //          | a(1,1) a(2,1) a(3,1) |
  // a[4x3] = | a(1,2) a(2,2) a(3,2) |
  //          | a(1,3) a(2,3) a(3,3) |
  //          | a(1,4) a(2,4) a(3,4) |
  //
  //          | sum( q(1,i)*p(1,i) )  sum( q(1,i)*p(2,i) ) sum( q(1,i)*p(3,i) )  |
  // C[4x3] = | sum( q(2,i)*p(1,i) )  sum( q(2,i)*p(2,i) ) sum( q(2,i)*p(3,i) )  |
  //          | sum( q(3,i)*p(1,i) )  sum( q(3,i)*p(2,i) ) sum( q(3,i)*p(3,i) )  |
  //          | sum( p(1,i) )         sum( p(2,i) )        sum( p(3,i) )         |
  //

  // [Q]
  //
  vnl_matrix<ParametersValueType> Q(ImageDimension + 1, ImageDimension + 1, 0.0F);
  for (unsigned int i = 0; i < numberOfLandmarks; ++i)
  { // Iterate for the number of landmarks.
    vnl_matrix<ParametersValueType> qTemp(ImageDimension + 1, 1);

    // Convert vector to column matrix.
    for (unsigned int k = 0; k < ImageDimension + 1; ++k)
    {
      qTemp(k, 0) = q.get(k, i);
    }
    vnl_matrix<ParametersValueType> qTempT(1, ImageDimension + 1);
    qTempT = qTemp.transpose();
    Q = Q + qTemp * qTempT;
  }

  // [C]
  vnl_matrix<ParametersValueType> C(ImageDimension + 1, ImageDimension, 0);
  for (unsigned int i = 0; i < numberOfLandmarks; ++i)
  {
    vnl_matrix<ParametersValueType> qTemp(ImageDimension + 1, 1);
    vnl_matrix<ParametersValueType> pTemp(1, ImageDimension);

    // Convert vector to column matrix.
    for (unsigned int k = 0; k < ImageDimension + 1; ++k)
    {
      qTemp(k, 0) = q.get(k, i);
    }
    for (unsigned int k = 0; k < ImageDimension; ++k)
    {
      pTemp(0, k) = p.get(k, i);
    }

    C = C + qTemp * pTemp;
  }

  // [Solve Qa=C]
  // :: Solving code is from
  // https://www.itk.org/pipermail/insight-users/2008-December/028207.html
  const vnl_matrix<ParametersValueType> transposeAffine = vnl_qr<ParametersValueType>(Q).solve(C);
  vnl_matrix<ParametersValueType>       Affine = transposeAffine.transpose();

  const vnl_matrix<ParametersValueType> AffineRotation(Affine.get_n_columns(0, ImageDimension));

  // [Convert ITK Affine Transformation from vnl]
  //
  // Define Matrix Type.
  const itk::Matrix<ParametersValueType, ImageDimension, ImageDimension> mA(AffineRotation);
  itk::Vector<ParametersValueType, ImageDimension>                       mT;
  for (unsigned int t = 0; t < ImageDimension; ++t)
  {
    mT[t] = Affine(t, ImageDimension);
  }

  transform->SetMatrix(mA);
  transform->SetOffset(mT);
}

template <typename TTransform, typename TFixedImage, typename TMovingImage>
void
LandmarkBasedTransformInitializer<TTransform, TFixedImage, TMovingImage>::InternalInitializeTransform(
  VersorRigid3DTransformType *)
{
  itkDebugMacro("Internal Initialize VersorRigid3DTransformType");
  auto * transform = dynamic_cast<VersorRigid3DTransformType *>(this->m_Transform.GetPointer());
  if (transform == nullptr)
  {
    itkExceptionMacro("VersorRigid3DTransformType Expected but transform is " << this->m_Transform->GetNameOfClass());
  }

  // Sanity check
  if constexpr (ImageDimension != 3)
  {
    itkExceptionMacro("Transform is VersorRigid3DTransform and Fixed image dimension is not 3");
  }
  if (MovingImageType::ImageDimension != 3)
  {
    itkExceptionMacro("Transform is VersorRigid3DTransform and Moving image dimension is not 3");
  }

  // ----------------------------------------------------------------------------
  //    Compute the necessary transform to match the two sets of landmarks
  //
  //    The solution is based on
  //    Berthold K. P. Horn (1987),
  //    "Closed-form solution of absolute orientation using unit quaternions,"
  //    Journal of the Optical Society of America A, 4:629-642
  //
  //    Original python implementation by David G. Gobbi
  //    Adapted from the code in VTK: Hybrid/vtkLandmarkTransform
  // ----------------------------------------------------------------------------

  using VectorType = typename VersorRigid3DTransformType::OutputVectorType;

  // Compute the centroids.
  PointType3D fixedCentroid = ComputeCentroid(this->m_FixedLandmarks);

  PointType3D movingCentroid = ComputeCentroid(this->m_MovingLandmarks);

  itkDebugMacro("fixed centroid  = " << fixedCentroid);
  itkDebugMacro("moving centroid  = " << movingCentroid);

  using VersorType = typename VersorRigid3DTransformType::VersorType;

  VersorType versor;

  // If we have at least 3 landmarks, we can compute a rotation.
  // Otherwise the versor will be an identity versor.
  if (this->m_FixedLandmarks.size() >= ImageDimension)
  {
    itk::Matrix<ParametersValueType, ImageDimension, ImageDimension> M;

    auto fixedItr = this->m_FixedLandmarks.begin();
    auto movingItr = this->m_MovingLandmarks.begin();

    VectorType fixedCentered;
    VectorType movingCentered;

    fixedCentered.Fill(0.0);
    movingCentered.Fill(0.0);

    itkDebugStatement(int ii = 0);

    // Computations are relative to the Center of Rotation.
    while (movingItr != this->m_MovingLandmarks.end())
    {
      for (unsigned int i = 0; i < ImageDimension; ++i)
      {
        fixedCentered[i] = (*fixedItr)[i] - fixedCentroid[i];
        movingCentered[i] = (*movingItr)[i] - movingCentroid[i];
      }

      for (unsigned int i = 0; i < ImageDimension; ++i)
      {
        for (unsigned int j = 0; j < ImageDimension; ++j)
        {
          M[i][j] += fixedCentered[i] * movingCentered[j];
        }
      }

      itkDebugStatement(++ii);
      itkDebugMacro("f_" << ii << " = " << fixedCentered);
      itkDebugMacro("m_" << ii << " = " << movingCentered);

      ++movingItr;
      ++fixedItr;
    }

    // Build the 4x4 matrix N.

    itk::Matrix<ParametersValueType, 4, 4> N;

    CreateMatrix(N, M);

    itkDebugMacro("For Closed form solution: ");
    itkDebugMacro("M matrix " << M);
    itkDebugMacro("N matrix " << N);

    vnl_matrix<ParametersValueType> eigenVectors(4, 4);
    vnl_vector<ParametersValueType> eigenValues(4);

    using SymmetricEigenAnalysisType = itk::SymmetricEigenAnalysisFixedDimension<4,
                                                                                 itk::Matrix<ParametersValueType, 4, 4>,
                                                                                 vnl_vector<ParametersValueType>,
                                                                                 vnl_matrix<ParametersValueType>>;
    const SymmetricEigenAnalysisType symmetricEigenSystem;

    symmetricEigenSystem.ComputeEigenValuesAndVectors(N, eigenValues, eigenVectors);

    itkDebugMacro("EigenVectors " << eigenVectors);
    itkDebugMacro("EigenValues " << eigenValues);

    // By default eigen values are sorted in ascending order therefore the
    // maximum eigen value is the one in the fourth place = index 3. We need the
    // eigen vector associated with the maximum eigenvalue, so we take the
    // eigenvector from the last row, index=3.

    versor.Set(eigenVectors[3][1], eigenVectors[3][2], eigenVectors[3][3], eigenVectors[3][0]);
    itkDebugMacro("Resulting versor" << versor);
  }


  transform->SetCenter(fixedCentroid);
  transform->SetRotation(versor);

  VectorType translation = transform->GetTranslation();
  translation = movingCentroid - fixedCentroid;
  transform->SetTranslation(translation);
}

template <typename TTransform, typename TFixedImage, typename TMovingImage>
void
LandmarkBasedTransformInitializer<TTransform, TFixedImage, TMovingImage>::InternalInitializeTransform(
  Similarity3DTransformType *)
{
  itkDebugMacro("Internal Initialize Similarity3DTransformType");
  auto * transform = dynamic_cast<Similarity3DTransformType *>(this->m_Transform.GetPointer());
  if (transform == nullptr)
  {
    itkExceptionMacro("Similarity3DTransformType Expected but transform is " << this->m_Transform->GetNameOfClass());
  }

  // Sanity check for dimension.
  if constexpr (ImageDimension != 3)
  {
    itkExceptionMacro("Transform is Similiarity3DTransform and Fixed image dimension is not 3");
  }
  if (MovingImageType::ImageDimension != 3)
  {
    itkExceptionMacro("Transform is Similiarity3DTransform and Moving image dimension is not 3");
  }

  // ----------------------------------------------------------------------------
  //    Compute the necessary transform to match the two sets of landmarks
  //
  //    The solution is based on
  //    Berthold K. P. Horn (1987),
  //    "Closed-form solution of absolute orientation using unit quaternions,"
  //    Journal of the Optical Society of America A, 4:629-642
  //
  //    Original python implementation by David G. Gobbi
  //    Adapted from the code in VTK: Hybrid/vtkLandmarkTransform
  // ----------------------------------------------------------------------------

  using VectorType = typename Similarity3DTransformType::OutputVectorType;

  // Compute the centroids.
  PointType3D fixedCentroid = ComputeCentroid(this->m_FixedLandmarks);

  PointType3D movingCentroid = ComputeCentroid(this->m_MovingLandmarks);

  itkDebugMacro("fixed centroid  = " << fixedCentroid);
  itkDebugMacro("moving centroid  = " << movingCentroid);

  using VersorType = typename VersorRigid3DTransformType::VersorType;

  VersorType versor;
  // For computing the scaling factor.
  double sa = 0.0;
  double sb = 0.0;

  // If we have at least 3 landmarks, we can compute a rotation.
  // Otherwise the versor will be an identity versor.
  if (this->m_FixedLandmarks.size() >= ImageDimension)
  {
    itk::Matrix<ParametersValueType, ImageDimension, ImageDimension> M;

    auto fixedItr = this->m_FixedLandmarks.begin();
    auto movingItr = this->m_MovingLandmarks.begin();

    VectorType fixedCentered;
    VectorType movingCentered;

    fixedCentered.Fill(0.0);
    movingCentered.Fill(0.0);

    itkDebugStatement(int ii = 0);

    // Computations are relative to the Center of Rotation.
    while (movingItr != this->m_MovingLandmarks.end())
    {
      for (unsigned int i = 0; i < ImageDimension; ++i)
      {
        fixedCentered[i] = (*fixedItr)[i] - fixedCentroid[i];
        movingCentered[i] = (*movingItr)[i] - movingCentroid[i];
      }

      // Accumulate scale factors (if desired).
      sa +=
        fixedCentered[0] * fixedCentered[0] + fixedCentered[1] * fixedCentered[1] + fixedCentered[2] * fixedCentered[2];
      sb += movingCentered[0] * movingCentered[0] + movingCentered[1] * movingCentered[1] +
            movingCentered[2] * movingCentered[2];

      for (unsigned int i = 0; i < ImageDimension; ++i)
      {
        for (unsigned int j = 0; j < ImageDimension; ++j)
        {
          M[i][j] += fixedCentered[i] * movingCentered[j];
        }
      }

      itkDebugStatement(++ii);
      itkDebugMacro("f_" << ii << " = " << fixedCentered);
      itkDebugMacro("m_" << ii << " = " << movingCentered);

      ++movingItr;
      ++fixedItr;
    }

    // Build the 4x4 matrix N.

    itk::Matrix<ParametersValueType, 4, 4> N;

    CreateMatrix(N, M);
    itkDebugMacro("For Closed form solution: ");
    itkDebugMacro("M matrix " << M);
    itkDebugMacro("N matrix " << N);

    vnl_matrix<ParametersValueType> eigenVectors(4, 4);
    vnl_vector<ParametersValueType> eigenValues(4);

    using SymmetricEigenAnalysisType = itk::SymmetricEigenAnalysisFixedDimension<4,
                                                                                 itk::Matrix<ParametersValueType, 4, 4>,
                                                                                 vnl_vector<ParametersValueType>,
                                                                                 vnl_matrix<ParametersValueType>>;
    const SymmetricEigenAnalysisType symmetricEigenSystem;

    symmetricEigenSystem.ComputeEigenValuesAndVectors(N, eigenValues, eigenVectors);

    itkDebugMacro("EigenVectors " << eigenVectors);
    itkDebugMacro("EigenValues " << eigenValues);

    // By default eigen values are sorted in ascending order therefore the
    // maximum eigen value is the one  in the fourth place = index 3. We need the
    // eigen vector associated with the maximum eigenvalue, so we take the
    // eigenvector from the last row, index=3.

    versor.Set(eigenVectors[3][1], eigenVectors[3][2], eigenVectors[3][3], eigenVectors[3][0]);
    itkDebugMacro("Resulting versor" << versor);
  }

  transform->SetCenter(fixedCentroid);
  transform->SetRotation(versor);

  // Compute required scaling factor.
  double scale = 1;

  if (sb != 0 && sa != 0)
  {
    scale = std::sqrt(sb / sa);
  }
  transform->SetScale(scale);

  VectorType translation = transform->GetTranslation();
  translation = movingCentroid - fixedCentroid;
  transform->SetTranslation(translation);
}

template <typename TTransform, typename TFixedImage, typename TMovingImage>
void
LandmarkBasedTransformInitializer<TTransform, TFixedImage, TMovingImage>::InternalInitializeTransform(
  Rigid2DTransformType *)
{
  itkDebugMacro("Internal Initialize Rigid2DTransformType");
  auto * transform = dynamic_cast<Rigid2DTransformType *>(this->m_Transform.GetPointer());

  if (transform == nullptr)
  {
    itkExceptionMacro("Rigid2DTransformType Expected but transform is " << this->m_Transform->GetNameOfClass());
  }
  // Sanity check.
  if constexpr (ImageDimension != 2)
  {
    itkExceptionMacro("Transform is Rigid2DTransform and Fixed image dimension is not 2");
  }
  if (MovingImageType::ImageDimension != 2)
  {
    itkExceptionMacro("Transform is Rigid2DTransform and Moving image dimension is not 2");
  }

  const double PI = 4.0 * std::atan(1.0);
  using VectorType = typename Rigid2DTransformType::OutputVectorType;
  using PointType = typename Rigid2DTransformType::OutputPointType;

  // Initialize the transform to identity.
  transform->SetIdentity();

  // Compute the centroids.
  PointType fixedCentroid{};
  auto      fixedItr = this->m_FixedLandmarks.begin();
  while (fixedItr != this->m_FixedLandmarks.end())
  {
    fixedCentroid[0] += (*fixedItr)[0];
    fixedCentroid[1] += (*fixedItr)[1];
    ++fixedItr;
  }

  fixedCentroid[0] /= this->m_FixedLandmarks.size();
  fixedCentroid[1] /= this->m_FixedLandmarks.size();

  auto      movingItr = this->m_MovingLandmarks.begin();
  PointType movingCentroid{};
  while (movingItr != this->m_MovingLandmarks.end())
  {
    movingCentroid[0] += (*movingItr)[0];
    movingCentroid[1] += (*movingItr)[1];
    ++movingItr;
  }

  movingCentroid[0] /= this->m_MovingLandmarks.size();
  movingCentroid[1] /= this->m_MovingLandmarks.size();

  itkDebugMacro("fixed centroid  = " << fixedCentroid);
  itkDebugMacro("moving centroid  = " << movingCentroid);

  double rotationAngle = 0.0;

  // If we have at least 2 landmarks, we can compute a rotation.
  // Otherwise the rotation matrix will be identity.
  //
  // For the Rigid2DTransform, the least squares error will be minimized
  // by choosing the offset as the distance between the two centroids,
  // fixed centroid (after having undergone the rotation transform, that
  // we must compute) and the moving centroid.
  // The rotation angle will be given by the cross and dot products of the
  // fixed and moving landmark vectors, the vectors being computed relative
  // to the fixed and moving centroids.
  if (this->m_FixedLandmarks.size() >= 2)
  {
    fixedItr = this->m_FixedLandmarks.begin();
    movingItr = this->m_MovingLandmarks.begin();

    VectorType fixedCentered;
    VectorType movingCentered;

    fixedCentered.Fill(0.0);
    movingCentered.Fill(0.0);

    itkDebugStatement(int ii = 0);
    double s_dot = 0;
    double s_cross = 0;

    // Computations are relative to the Center of Rotation.
    while (movingItr != this->m_MovingLandmarks.end())
    {
      fixedCentered[0] = (*fixedItr)[0] - fixedCentroid[0];
      movingCentered[0] = (*movingItr)[0] - movingCentroid[0];
      fixedCentered[1] = (*fixedItr)[1] - fixedCentroid[1];
      movingCentered[1] = (*movingItr)[1] - movingCentroid[1];

      s_dot += (movingCentered[0] * fixedCentered[0]) + (movingCentered[1] * fixedCentered[1]);
      s_cross += (movingCentered[1] * fixedCentered[0]) - (movingCentered[0] * fixedCentered[1]);

      itkDebugStatement(++ii);
      itkDebugMacro("f_" << ii << " = " << fixedCentered);
      itkDebugMacro("m_" << ii << " = " << movingCentered);

      ++movingItr;
      ++fixedItr;
    }

    itkDebugMacro("Dot Product of landmarks: " << s_dot << " Cross Product: " << s_cross);
    if (itk::Math::abs(s_dot) > 0.00005)
    {
      rotationAngle = std::atan2(s_cross, s_dot);
    }
    else
    {
      rotationAngle = -0.5 * PI;
    }
  }
  else
  {
    itkWarningMacro("Less than 2 landmarks available. Rotation is not computed");
  }

  auto t = Rigid2DTransformType::New();
  t->SetIdentity();
  t->SetAngle(rotationAngle);

  transform->SetCenter(fixedCentroid);
  transform->SetAngle(rotationAngle);

  VectorType translation = transform->GetTranslation();
  itkDebugMacro("Initial transform translation: " << translation);
  translation = movingCentroid - fixedCentroid;
  itkDebugMacro("translation computed as difference of centroids: " << translation);
  transform->SetTranslation(translation);
}

template <typename TTransform, typename TFixedImage, typename TMovingImage>
void
LandmarkBasedTransformInitializer<TTransform, TFixedImage, TMovingImage>::InitializeTransform()
{
  // Sanity check.
  if (!m_Transform)
  {
    itkExceptionMacro("Transform has not been set");
  }
  if (this->m_FixedLandmarks.size() != this->m_MovingLandmarks.size())
  {
    itkExceptionMacro("Different number of fixed and moving landmarks");
  }
  this->InternalInitializeTransform(static_cast<TTransform *>(nullptr));
}

template <typename TTransform, typename TFixedImage, typename TMovingImage>
auto
LandmarkBasedTransformInitializer<TTransform, TFixedImage, TMovingImage>::ComputeCentroid(
  const LandmarkPointContainer inputLandmarks) -> PointType3D
{
  // Compute the centroids.
  PointType3D centroid{};
  auto        fixedItr = inputLandmarks.begin();
  while (fixedItr != inputLandmarks.end())
  {
    centroid[0] += (*fixedItr)[0];
    centroid[1] += (*fixedItr)[1];
    centroid[2] += (*fixedItr)[2];
    ++fixedItr;
  }

  centroid[0] /= inputLandmarks.size();
  centroid[1] /= inputLandmarks.size();
  centroid[2] /= inputLandmarks.size();

  return centroid;
}

template <typename TTransform, typename TFixedImage, typename TMovingImage>
void
LandmarkBasedTransformInitializer<TTransform, TFixedImage, TMovingImage>::CreateMatrix(
  itk::Matrix<ParametersValueType, 4, 4> &                               N,
  const itk::Matrix<ParametersValueType, ImageDimension, ImageDimension> M)
{
  // On-diagonal elements.
  N[0][0] = M[0][0] + M[1][1] + M[2][2];
  N[1][1] = M[0][0] - M[1][1] - M[2][2];
  N[2][2] = -M[0][0] + M[1][1] - M[2][2];
  N[3][3] = -M[0][0] - M[1][1] + M[2][2];

  // Off-diagonal elements.
  N[0][1] = N[1][0] = M[1][2] - M[2][1];
  N[0][2] = N[2][0] = M[2][0] - M[0][2];
  N[0][3] = N[3][0] = M[0][1] - M[1][0];

  N[1][2] = N[2][1] = M[0][1] + M[1][0];
  N[1][3] = N[3][1] = M[2][0] + M[0][2];
  N[2][3] = N[3][2] = M[1][2] + M[2][1];

  return;
}

template <typename TTransform, typename TFixedImage, typename TMovingImage>
void
LandmarkBasedTransformInitializer<TTransform, TFixedImage, TMovingImage>::PrintSelf(std::ostream & os,
                                                                                    Indent         indent) const
{
  using namespace print_helper;

  Superclass::PrintSelf(os, indent);

  itkPrintSelfObjectMacro(ReferenceImage);
  itkPrintSelfObjectMacro(Transform);

  os << indent << "FixedLandmarks: " << m_FixedLandmarks << std::endl;
  os << indent << "MovingLandmarks: " << m_MovingLandmarks << std::endl;
  os << indent << "LandmarkWeight: " << m_LandmarkWeight << std::endl;

  os << indent << "BSplineNumberOfControlPoints: " << m_BSplineNumberOfControlPoints << std::endl;
}
} // namespace itk

#endif /* itkLandmarkBasedTransformInitializer_hxx */
