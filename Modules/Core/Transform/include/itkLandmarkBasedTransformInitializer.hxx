/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef __itkLandmarkBasedTransformInitializer_hxx
#define __itkLandmarkBasedTransformInitializer_hxx

#include "itkLandmarkBasedTransformInitializer.h"
#include "itkMatrix.h"
#include "itkSymmetricEigenAnalysis.h"

#include <cmath>
#include "vnl/algo/vnl_qr.h"

namespace itk
{
template< typename TTransform, typename TFixedImage, typename TMovingImage >
LandmarkBasedTransformInitializer< TTransform, TFixedImage, TMovingImage >
::LandmarkBasedTransformInitializer()
{}

/** default transform initializer, if transform type isn't
 * specifically handled.
 */
template< typename TTransform, typename TFixedImage, typename TMovingImage >
template <typename TTransform2>
void
LandmarkBasedTransformInitializer< TTransform, TFixedImage, TMovingImage >
::InternalInitializeTransform(TTransform *)
{
  itkWarningMacro( << "Unsupported Transform Type "
                   << this->m_Transform->GetNameOfClass() );
  m_Transform->SetIdentity();
}

/** Compute an affine transform from landmark set.
 *  This was implemented by Eun Young Kim based on method
 *  described in the paper "Closed-form solution of absolute
 *  orientation using orthonormal matrices" by Horn, Hilden, and
 *  Negahdaripour. See http://www.opticsinfobase.org/abstract.cfm?&id=3143
 */
template< typename TTransform, typename TFixedImage, typename TMovingImage >
void
LandmarkBasedTransformInitializer< TTransform, TFixedImage, TMovingImage >
::InternalInitializeTransform(AffineTransformType *)
{
  AffineTransformType *transform =
    dynamic_cast<AffineTransformType *>(this->m_Transform.GetPointer());
  if ( transform == 0 )
    {
    itkExceptionMacro( << "AffineTransform Expected but transform is "
                       << this->m_Transform->GetNameOfClass() );
    }

  if( m_MovingLandmarks.size() != m_FixedLandmarks.size() )
    {
    itkExceptionMacro( << " size mismatch between Fixed and Moving Landmarks" );
    }
  const unsigned int NumberOfLandMarks = m_MovingLandmarks.size();


  //[Set Landmark Weight]
  //:: If no landmark weights are given, weight matrix is identity matrix
  vnl_matrix<double> vnlWeight( NumberOfLandMarks,NumberOfLandMarks,0);
  vnlWeight.set_identity();

  if( !m_LandmarkWeight.empty() )
  {
    if( m_LandmarkWeight.size() != NumberOfLandMarks)
    {
      itkExceptionMacro( << " size mismatch between number of landmars pairs and weights" );
    }
    LandmarkWeightConstIterator weightIt = m_LandmarkWeight.begin();
    for(int i = 0; weightIt != m_LandmarkWeight.end(); ++i, ++weightIt )
    {
      vnlWeight(i,i)=(*weightIt);
    }
  }
  // Normalize weights
  //
  vnlWeight=vnlWeight/vnlWeight.fro_norm();

  //[Convert Point to Matrix for Matrix Muptiplication]
  //

  // q
  // dim+1=4 * NumberOfLandMarks matrix
  vnl_matrix< double > q( ImageDimension+1, NumberOfLandMarks, 0.0F);
  PointsContainerConstIterator fixedIt = m_FixedLandmarks.begin();
  for(int i = 0; fixedIt != m_FixedLandmarks.end(); ++i, ++fixedIt )
    {
    for( int dim =0; dim< ImageDimension; dim++ )
      {
      q( dim,i ) = (*fixedIt)[dim];
      }
    q(ImageDimension,i)=1.0F;
    }
  q *= vnlWeight;

  // p
  // dim=3 * NumberOfLandMarks matrix
  vnl_matrix< double > p( ImageDimension, NumberOfLandMarks,0.0F);
  PointsContainerConstIterator movingIt = m_MovingLandmarks.begin();
  for(int i = 0; movingIt != m_MovingLandmarks.end(); ++i, ++movingIt )
    {
    for( int dim =0; dim< ImageDimension; dim++ )
      {
      p( dim,i ) = (*movingIt)[dim];
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
  vnl_matrix<double> Q( ImageDimension+1,ImageDimension+1, 0.0F );
  for( unsigned int i =0; i<NumberOfLandMarks; i++)
    { // Iterate for the number of landmakrs
    vnl_matrix< double > qTemp( ImageDimension+1, 1 );
    // convert vector to colume matrix
    for( unsigned int k=0; k<ImageDimension+1;k++)
      {
      qTemp(k,0)=q.get(k,i);
      }
    vnl_matrix< double > qTempT( 1, ImageDimension+1 );
    qTempT= qTemp.transpose();
    Q = Q +  qTemp*qTempT;
    }

  // [C]
  //
  vnl_matrix<double> C( ImageDimension+1,ImageDimension, 0 );
  for( unsigned int i =0; i<NumberOfLandMarks; i++)
    {
    vnl_matrix< double > qTemp( ImageDimension+1, 1 );
    vnl_matrix< double > pTemp( 1, ImageDimension );
    // convert vector to colume matrix
    for( unsigned int k=0; k<ImageDimension+1;k++)
      {
      qTemp(k,0)=q.get(k,i);
      }
    for( unsigned int k=0; k<ImageDimension;k++)
      {
      pTemp(0,k)=p.get(k,i);
      }

    C = C +  qTemp*pTemp;
    }

  // [Solve Qa=C]
  // :: Solving code is from
  // http://www.itk.org/pipermail/insight-users/2008-December/028207.html
  vnl_matrix<double> transposeAffine =
    vnl_qr<double> ( Q ).solve( C );
  vnl_matrix<double> Affine= transposeAffine.transpose();

  vnl_matrix<double> AffineRotation =
    vnl_matrix<double>(Affine.get_n_columns(0,ImageDimension));

  // [Convert ITK Affine Transformation from vnl]
  //
  // Define Matrix Type
  //
  itk::Matrix<double,ImageDimension,ImageDimension> mA =
    itk::Matrix<double,ImageDimension,ImageDimension>(AffineRotation);
  itk::Vector<double,ImageDimension> mT;
  for(unsigned int t=0;t<ImageDimension;t++)
    {
    mT[t] = Affine(t,ImageDimension);
    }

  transform->SetMatrix( mA );
  transform->SetOffset( mT );

}

template< typename TTransform, typename TFixedImage, typename TMovingImage >
void
LandmarkBasedTransformInitializer< TTransform, TFixedImage, TMovingImage >
::InternalInitializeTransform(VersorRigid3DTransformType *)
{
  itkDebugMacro("Internal Initialize VersorRigid3DTransformType");
  VersorRigid3DTransformType *transform = dynamic_cast< VersorRigid3DTransformType * >(
    this->m_Transform.GetPointer() );
  if ( transform == 0 )
    {
    itkExceptionMacro( << "VersorRigid3DTransformType Expected but transform is "
                       << this->m_Transform->GetNameOfClass() );
    }

  // Sanity check
  if ( ImageDimension != 3 )
    {
    itkExceptionMacro(
      "Transform is VersorRigid3DTransform and Fixed image dimension is not 3");
    return;
    }
  if ( MovingImageType::ImageDimension != 3 )
    {
    itkExceptionMacro(
      "Transform is VersorRigid3DTransform and Moving image dimension is not 3");
    return;
    }

  // --- compute the necessary transform to match the two sets of landmarks
  // ---
  //
  //
  //    The solution is based on
  //    Berthold K. P. Horn (1987),
  //    "Closed-form solution of absolute orientation using unit
  // quaternions,"
  //    Journal of the Optical Society of America A, 4:629-642
  //
  //
  //    Original python implementation by David G. Gobbi
  //    Readpted from the code in VTK: Hybrid/vtkLandmarkTransform
  //
  //----------------------------------------------------------------------------

  typedef typename VersorRigid3DTransformType::OutputVectorType VectorType;
  typedef typename VersorRigid3DTransformType::OutputPointType  PointType;
  typedef typename VersorRigid3DTransformType::CenterType       RotationCenterType;

  // Compute the centroids
  PointType fixedCentroid;
  fixedCentroid.Fill(0.0);
  PointsContainerConstIterator fixedItr = m_FixedLandmarks.begin();
  while ( fixedItr != m_FixedLandmarks.end() )
    {
    fixedCentroid[0] += ( *fixedItr )[0];
    fixedCentroid[1] += ( *fixedItr )[1];
    fixedCentroid[2] += ( *fixedItr )[2];
    ++fixedItr;
    }

  fixedCentroid[0] /= m_FixedLandmarks.size();
  fixedCentroid[1] /= m_FixedLandmarks.size();
  fixedCentroid[2] /= m_FixedLandmarks.size();

  PointsContainerConstIterator movingItr = m_MovingLandmarks.begin();
  PointType                    movingCentroid;
  movingCentroid.Fill(0.0);
  while ( movingItr != m_MovingLandmarks.end() )
    {
    movingCentroid[0] += ( *movingItr )[0];
    movingCentroid[1] += ( *movingItr )[1];
    movingCentroid[2] += ( *movingItr )[2];
    ++movingItr;
    }

  movingCentroid[0] /= m_MovingLandmarks.size();
  movingCentroid[1] /= m_MovingLandmarks.size();
  movingCentroid[2] /= m_MovingLandmarks.size();

  itkDebugMacro(<< "fixed centroid  = " <<  fixedCentroid);
  itkDebugMacro(<< "moving centroid  = " << movingCentroid);

  typedef typename VersorRigid3DTransformType::VersorType VersorType;

  VersorType versor;

  // If we have at least 3 landmarks, we can compute a rotation.
  // Otherwise the versor will be an identity versor.
  if ( m_FixedLandmarks.size() >= ImageDimension )
    {
    itk::Matrix< double, ImageDimension, ImageDimension > M;

    fixedItr  = m_FixedLandmarks.begin();
    movingItr = m_MovingLandmarks.begin();

    VectorType fixedCentered;
    VectorType movingCentered;

    fixedCentered.Fill(0.0);
    movingCentered.Fill(0.0);

    itkDebugStatement(int ii = 0; )

      // Computations are relative to the Center of Rotation.
      while ( movingItr != m_MovingLandmarks.end() )
        {
        for ( unsigned int i = 0; i < ImageDimension; i++ )
          {
          fixedCentered[i]  = ( *fixedItr )[i]  - fixedCentroid[i];
          movingCentered[i] = ( *movingItr )[i] - movingCentroid[i];
          }

        for ( unsigned int i = 0; i < ImageDimension; i++ )
          {
          for ( unsigned int j = 0; j < ImageDimension; j++ )
            {
            // mmm this indices i,j may have to be reverted...
            M[i][j] += fixedCentered[i] * movingCentered[j];
            }
          }

        itkDebugStatement(++ii; )
          itkDebugMacro(<< "f_" << ii << " = " << fixedCentered);
        itkDebugMacro(<< "m_" << ii << " = " << movingCentered);

        ++movingItr;
        ++fixedItr;
        }

    // -- build the 4x4 matrix N --

    itk::Matrix< double, 4, 4 > N;

    // on-diagonal elements
    N[0][0] =  M[0][0] + M[1][1] + M[2][2];
    N[1][1] =  M[0][0] - M[1][1] - M[2][2];
    N[2][2] = -M[0][0] + M[1][1] - M[2][2];
    N[3][3] = -M[0][0] - M[1][1] + M[2][2];
    // off-diagonal elements
    N[0][1] = N[1][0] = M[1][2] - M[2][1];
    N[0][2] = N[2][0] = M[2][0] - M[0][2];
    N[0][3] = N[3][0] = M[0][1] - M[1][0];

    N[1][2] = N[2][1] = M[0][1] + M[1][0];
    N[1][3] = N[3][1] = M[2][0] + M[0][2];
    N[2][3] = N[3][2] = M[1][2] + M[2][1];

    itkDebugMacro(<< "For Closed form solution: ");
    itkDebugMacro(<< "M matrix " << M);
    itkDebugMacro(<< "N matrix " << N);

    vnl_matrix< double > eigenVectors(4, 4);
    vnl_vector< double > eigenValues(4);

    typedef itk::SymmetricEigenAnalysis<
      itk::Matrix< double, 4, 4 >,
      vnl_vector< double >,
      vnl_matrix< double > > SymmetricEigenAnalysisType;
    SymmetricEigenAnalysisType symmetricEigenSystem(4);

    symmetricEigenSystem.ComputeEigenValuesAndVectors(N, eigenValues, eigenVectors);

    itkDebugMacro(<< "EigenVectors " << eigenVectors);
    itkDebugMacro(<< "EigenValues " << eigenValues);

    // By default eigen values are sorted in ascending order.  therefore the
    // maximum
    // eigen value is the one  in the fourth place = index 3. We need the
    // eigen
    // vector associated with the maximum eigenvalue, so we take the
    // eigenvector
    // from the last row, index=3.

    versor.Set(eigenVectors[3][1],
               eigenVectors[3][2],
               eigenVectors[3][3],
               eigenVectors[3][0]);
    itkDebugMacro(<< "Resulting versor" << versor);
    }
  else
    {
    // Remember..
    // Less than 3 landmarks available. Rotation is not computed
    }

  transform->SetCenter(fixedCentroid);
  transform->SetRotation(versor);

  VectorType translation = transform->GetTranslation();
  translation = movingCentroid - fixedCentroid;
  transform->SetTranslation(translation);

}

template< typename TTransform, typename TFixedImage, typename TMovingImage >
void
LandmarkBasedTransformInitializer< TTransform, TFixedImage, TMovingImage >
::InternalInitializeTransform(Rigid2DTransformType *)
{
  itkDebugMacro("Internal Initialize VersorRigid3DTransformType");
  Rigid2DTransformType *transform =
    dynamic_cast< Rigid2DTransformType * >(this->m_Transform.GetPointer() );

  if ( transform == 0 )
    {
    itkExceptionMacro( << "VersorRigid3DTransformType Expected but transform is "
                       << this->m_Transform->GetNameOfClass() );
    }
  // Sanity check
  if ( ImageDimension != 2 )
    {
    itkExceptionMacro(
      "Transform is Rigid2DTransfrom and Fixed image dimension is not 2");
    return;
    }
  if ( MovingImageType::ImageDimension != 2 )
    {
    itkExceptionMacro(
      "Transform is Rigid2DTransform and Moving image dimension is not 2");
    return;
    }

  const double PI = 4.0 * vcl_atan(1.0);
  typedef typename Rigid2DTransformType::OutputVectorType VectorType;
  typedef typename Rigid2DTransformType::OutputPointType  PointType;

  //Initialize the transform to identity
  transform->SetIdentity();

  // Compute the centroids
  PointType fixedCentroid;
  fixedCentroid.Fill(0.0);
  PointsContainerConstIterator fixedItr = m_FixedLandmarks.begin();
  while ( fixedItr != m_FixedLandmarks.end() )
    {
    fixedCentroid[0] += ( *fixedItr )[0];
    fixedCentroid[1] += ( *fixedItr )[1];
    ++fixedItr;
    }

  fixedCentroid[0] /= m_FixedLandmarks.size();
  fixedCentroid[1] /= m_FixedLandmarks.size();

  PointsContainerConstIterator movingItr = m_MovingLandmarks.begin();
  PointType                    movingCentroid;
  movingCentroid.Fill(0.0);
  while ( movingItr != m_MovingLandmarks.end() )
    {
    movingCentroid[0] += ( *movingItr )[0];
    movingCentroid[1] += ( *movingItr )[1];
    ++movingItr;
    }

  movingCentroid[0] /= m_MovingLandmarks.size();
  movingCentroid[1] /= m_MovingLandmarks.size();

  itkDebugMacro(<< "fixed centroid  = " <<  fixedCentroid);
  itkDebugMacro(<< "moving centroid  = " << movingCentroid);

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
  if ( m_FixedLandmarks.size() >= 2 )
    {
    fixedItr  = m_FixedLandmarks.begin();
    movingItr = m_MovingLandmarks.begin();

    VectorType fixedCentered;
    VectorType movingCentered;

    fixedCentered.Fill(0.0);
    movingCentered.Fill(0.0);

    itkDebugStatement(int ii = 0; )
      double s_dot   = 0;
    double s_cross = 0;
    // Computations are relative to the Center of Rotation.
    while ( movingItr != m_MovingLandmarks.end() )
      {
      fixedCentered[0]  = ( *fixedItr )[0]  - fixedCentroid[0];
      movingCentered[0] = ( *movingItr )[0] - movingCentroid[0];
      fixedCentered[1]  = ( *fixedItr )[1]  - fixedCentroid[1];
      movingCentered[1] = ( *movingItr )[1] - movingCentroid[1];

      s_dot += ( movingCentered[0] * fixedCentered[0] )
        + ( movingCentered[1] * fixedCentered[1] );
      s_cross += ( movingCentered[1] * fixedCentered[0] )
        - ( movingCentered[0] * fixedCentered[1] );

      itkDebugStatement(++ii; )
        itkDebugMacro(<< "f_" << ii << " = " << fixedCentered);
      itkDebugMacro(<< "m_" << ii << " = " << movingCentered);

      ++movingItr;
      ++fixedItr;
      }

    itkDebugMacro(<< "Dot Product of landmarks: " << s_dot << " Cross Product: " << s_cross);
    if ( vcl_fabs(s_dot) > 0.00005 )
      {
      rotationAngle = vcl_atan2(s_cross, s_dot);
      }
    else
      {
      rotationAngle = -0.5 * PI;
      }
    }
  else
    {
    itkWarningMacro(<< "Less than 2 landmarks available. Rotation is not computed");
    }

  typename Rigid2DTransformType::Pointer t = Rigid2DTransformType::New();
  t->SetIdentity();
  t->SetAngle(rotationAngle);

  transform->SetCenter(fixedCentroid);
  transform->SetAngle(rotationAngle);

  VectorType translation = transform->GetTranslation();
  itkDebugMacro(<< "Initial transform translation: " << translation);
  translation = movingCentroid - fixedCentroid;
  itkDebugMacro(<< "translation computed as difference of centroids: " << translation);
  transform->SetTranslation(translation);
}

template< typename TTransform, typename TFixedImage, typename TMovingImage >
void
LandmarkBasedTransformInitializer< TTransform, TFixedImage, TMovingImage >
::InitializeTransform()
{
  // Sanity check
  if ( !m_Transform )
    {
    itkExceptionMacro("Transform has not been set");
    return;
    }
  if ( m_FixedLandmarks.size() != m_MovingLandmarks.size() )
    {
    itkExceptionMacro("Different number of fixed and moving landmarks");
    return;
    }
  this->InternalInitializeTransform(static_cast<TTransform *>(0));
}

template< typename TTransform, typename TFixedImage, typename TMovingImage >
void
LandmarkBasedTransformInitializer< TTransform, TFixedImage, TMovingImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Transform   = " << std::endl;
  if ( m_Transform )
    {
    os << indent << m_Transform  << std::endl;
    }
  else
    {
    os << indent << "None" << std::endl;
    }

  os << indent << "FixedImage   = " << std::endl;
  if ( m_FixedImage )
    {
    os << indent << m_FixedImage  << std::endl;
    }
  else
    {
    os << indent << "None" << std::endl;
    }

  os << indent << "MovingImage   = " << std::endl;
  if ( m_MovingImage )
    {
    os << indent << m_MovingImage  << std::endl;
    }
  else
    {
    os << indent << "None" << std::endl;
    }

  os << indent << "Fixed Landmarks: " << std::endl;
  PointsContainerConstIterator fitr = m_FixedLandmarks.begin();
  while ( fitr != m_FixedLandmarks.end() )
    {
    os << indent << *fitr << std::endl;
    ++fitr;
    }
  os << indent << "Moving Landmarks: " << std::endl;
  PointsContainerConstIterator mitr = m_MovingLandmarks.begin();
  while ( mitr != m_MovingLandmarks.end() )
    {
    os << indent << *mitr << std::endl;
    ++mitr;
    }
}
}  // namespace itk

#endif /* __itkLandmarkBasedTransformInitializer_hxx */
