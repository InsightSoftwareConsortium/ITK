/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLandmarkBasedTransformInitializer.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkLandmarkBasedTransformInitializer_txx
#define __itkLandmarkBasedTransformInitializer_txx

#include "itkLandmarkBasedTransformInitializer.h"
#include "itkMatrix.h"
#include "itkSymmetricEigenAnalysis.h"

namespace itk
{


template < class TTransform, class TFixedImage, class TMovingImage >
LandmarkBasedTransformInitializer< TTransform, TFixedImage, TMovingImage >
::LandmarkBasedTransformInitializer() 
{
}


  
template < class TTransform, class TFixedImage, class TMovingImage >
void 
LandmarkBasedTransformInitializer<TTransform, TFixedImage, TMovingImage >
::InitializeTransform() 
{
  // Sanity check
  if( !m_FixedImage )
    {
    itkExceptionMacro( "Fixed Image has not been set" );
    return;
    }
  if( !m_MovingImage )
    {
    itkExceptionMacro( "Moving Image has not been set" );
    return;
    }
  if( !m_Transform )
    {
    itkExceptionMacro( "Transform has not been set" );
    return;
    }
  if( m_FixedLandmarks.size() != m_MovingLandmarks.size() )
    {
    itkExceptionMacro("Different number of fixed and moving landmarks");
    return;
    }


  
  // We will do an explicit typeid check here (via dynamic_cast) to check
  // the transform type. The initialization scheme will generally be different
  // based on the transform type and the dimension. As more transforms are
  // supported in future, an explicit typeid check is expected to be done here.
  // Note that the typeid is done via dynamic_cast. This means that as more transforms
  // are added in future, you will have to order your checks from the bottom
  // of the transform hierarchy, upwards.
  //
  InputTransformType  transformType = Else; 
  VersorRigid3DTransformType *testPtr = dynamic_cast< VersorRigid3DTransformType *>(
      this->m_Transform.GetPointer() );
  if( testPtr ) 
    {  
    transformType = VersorRigid3Dtransform;
    }


  
  unsigned int numberOfLandmarks = m_FixedLandmarks.size();
  

  // If images come from filters, then update those filters.
  if( m_FixedImage->GetSource() )
    { 
    m_FixedImage->GetSource()->Update();
    }
  if( m_MovingImage->GetSource() )
    { 
    m_MovingImage->GetSource()->Update();
    }


  InputPointType    rotationCenter;
  OutputVectorType  translationVector;


  switch( transformType )
    {
      
    case VersorRigid3Dtransform:
      {
      // Sanity check
      if( FixedImageType::ImageDimension != 3 )
        {
        itkExceptionMacro(
            "Transform is VersorRigid3DTransform and Fixed image dimension is not 3");
        return;
        }
      if( MovingImageType::ImageDimension != 3 )
        {
        itkExceptionMacro(
         "Transform is VersorRigid3DTransform and Moving image dimension is not 3");
        return;
        }
        
      // --- compute the necessary transform to match the two sets of landmarks ---
      //
      // 
      //    The solution is based on
      //    Berthold K. P. Horn (1987),
      //    "Closed-form solution of absolute orientation using unit quaternions,"
      //    Journal of the Optical Society of America A, 4:629-642
      //
      //
      //    Original python implementation by David G. Gobbi
      //    Readpted from the code in VTK: Hybrid/vtkLandmarkTransform
      //
      //----------------------------------------------------------------------------

      RotationCenterType rotationCenter = m_Transform->GetCenter();
      
      // Compute the centroids
      typedef typename LandmarkPointType::VectorType VectorType;
      
      VectorType fixedVector;
      fixedVector.Fill( 0.0 );

      PointsContainerConstIterator fixedItr = m_FixedLandmarks.begin();
      while( fixedItr != m_FixedLandmarks.end() )
        {
        fixedVector += *fixedItr - rotationCenter;
        ++fixedItr;
        }

      VectorType movingVector;
      movingVector.Fill( 0.0 );

      PointsContainerConstIterator movingItr = m_MovingLandmarks.begin();
      while( movingItr != m_MovingLandmarks.end() )
        {
        movingVector += *movingItr - rotationCenter;
        ++movingItr;
        }

      VectorType fixedCentroidFromRotationCenter;
      VectorType movingCentroidFromRotationCenter;

      for(unsigned int ic=0; ic<3; ic++)
        {
        fixedCentroidFromRotationCenter[ic]  = fixedVector[ic]  / m_FixedLandmarks.size();
        movingCentroidFromRotationCenter[ic] = movingVector[ic] / m_MovingLandmarks.size();
        }

      LandmarkPointType fixedCentroid  = rotationCenter + fixedCentroidFromRotationCenter;
      LandmarkPointType movingCentroid = rotationCenter + movingCentroidFromRotationCenter;
      
      itkDebugMacro(<< "fixed centroid  = " <<  fixedCentroid);
      itkDebugMacro(<< "moving centroid  = " << movingCentroid);
      
      typedef typename TransformType::VersorType VersorType;

      VersorType versor;

      // If we have at least 3 landmarks, we can compute a rotation.
      // Otherwise the versor will be an identity versor.
      if( numberOfLandmarks >= 3 )
        {
        itk::Matrix<double,3,3> M;

        fixedItr  = m_FixedLandmarks.begin();
        movingItr = m_MovingLandmarks.begin();

        VectorType fixedCentered;
        VectorType movingCentered;

        fixedCentered.Fill( 0.0 );
        movingCentered.Fill( 0.0 );

        int ii=0;
        // Computations are relative to the Center of Rotation.
        while( movingItr != m_MovingLandmarks.end() )
          {
          fixedCentered  = *fixedItr  - fixedCentroid;
          movingCentered = *movingItr - movingCentroid;
          
          ++ii;
          itkDebugMacro(<< "f_" << ii << " = " << fixedCentered );
          itkDebugMacro(<< "m_" << ii << " = " << movingCentered );

          for(unsigned int i=0; i<3; i++)
            {
            for(unsigned int j=0; j<3; j++)
              {
              // mmm this indices i,j may have to be reverted...
              M[i][j] += fixedCentered[i] * movingCentered[j];
              }
            }
          ++movingItr;
          ++fixedItr;
          }

       // -- build the 4x4 matrix N --
        
        itk::Matrix<double,4,4> N;
        
        // on-diagonal elements
        N[0][0] =  M[0][0] +M[1][1] +M[2][2];
        N[1][1] =  M[0][0] -M[1][1] -M[2][2];
        N[2][2] = -M[0][0] +M[1][1] -M[2][2];
        N[3][3] = -M[0][0] -M[1][1] +M[2][2];
        // off-diagonal elements
        N[0][1] = N[1][0] = M[1][2] -M[2][1];
        N[0][2] = N[2][0] = M[2][0] -M[0][2];
        N[0][3] = N[3][0] = M[0][1] -M[1][0];

        N[1][2] = N[2][1] = M[0][1] +M[1][0];
        N[1][3] = N[3][1] = M[2][0] +M[0][2];
        N[2][3] = N[3][2] = M[1][2] +M[2][1];

       
        itkDebugMacro( << "For Closed form solution: ");
        itkDebugMacro(<< "M matrix " << M );
        itkDebugMacro(<< "N matrix " << N );

        vnl_matrix<double> eigenVectors(4,4);
        vnl_vector<double> eigenValues(4);

        typedef itk::SymmetricEigenAnalysis< 
              itk::Matrix< double,4,4 >,  
              vnl_vector< double >,
              vnl_matrix< double > > SymmetricEigenAnalysisType;
        SymmetricEigenAnalysisType symmetricEigenSystem(4);
        
        symmetricEigenSystem.ComputeEigenValuesAndVectors( N, eigenValues, eigenVectors );

        itkDebugMacro( << "EigenVectors " << eigenVectors);
        itkDebugMacro( << "EigenValues " << eigenValues);

        // By default eigen values are sorted in ascending order.  therefore the maximum
        // eigen value is the one  in the fourth place = index 3. We need the eigen
        // vector associated with the maximum eigenvalue, so we take the eigenvector
        // from the last row, index=3. 

        versor.Set( eigenVectors[3][1],
                    eigenVectors[3][2],
                    eigenVectors[3][3],
                    eigenVectors[3][0]  );
        itkDebugMacro(<< "Resulting versor" << versor);
        }
      else
        {
        itkWarningMacro(<< "Less than 3 landmarks available. Rotation is not computed");
        }
      
      m_Transform->SetCenter(fixedCentroid);
      m_Transform->SetRotation( versor );

      LandmarkPointType fixedCentroidRotated = 
        rotationCenter + versor.Transform(  fixedCentroid - rotationCenter );

      VectorType translation = m_Transform->GetTranslation(); 

      translation += movingCentroid - fixedCentroid;
     
      m_Transform->SetTranslation( translation );
 
      break;
      }  
 
      
    case Else:
      itkWarningMacro(<< "Landmark initialization using the specified input transform not implemented");
      m_Transform->SetIdentity();
  
      
    default:
      itkWarningMacro(<< "Landmark initialization using the specified input transform not implemented");
      m_Transform->SetIdentity();
  
    }
  
      
}
  

      

template < class TTransform, class TFixedImage, class TMovingImage >
void 
LandmarkBasedTransformInitializer<TTransform, TFixedImage, TMovingImage >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
     
  os << indent << "Transform   = " << std::endl;
  if (m_Transform)
    { 
    os << indent << m_Transform  << std::endl;
    }
  else
    {
    os << indent << "None" << std::endl;
    }      

  os << indent << "FixedImage   = " << std::endl;
  if (m_FixedImage)
    { 
    os << indent << m_FixedImage  << std::endl;
    }
  else
    {
    os << indent << "None" << std::endl;
    }      

  os << indent << "MovingImage   = " << std::endl;
  if (m_MovingImage)
    { 
    os << indent << m_MovingImage  << std::endl;
    }
  else
    {
    os << indent << "None" << std::endl;
    }      

  os << indent << "Fixed Landmarks: " << std::endl;
  PointsContainerConstIterator fitr = m_FixedLandmarks.begin();
  while( fitr != m_FixedLandmarks.end() )
    {
    os << indent << *fitr << std::endl;
    ++fitr;
    }
  os << indent << "Moving Landmarks: " << std::endl;
  PointsContainerConstIterator mitr = m_MovingLandmarks.begin();
  while( mitr != m_MovingLandmarks.end() )
    {
    os << indent << *mitr << std::endl;
    ++mitr;
    }
    
}
 
}  // namespace itk

#endif /* __itkLandmarkBasedTransformInitializer_txx */
