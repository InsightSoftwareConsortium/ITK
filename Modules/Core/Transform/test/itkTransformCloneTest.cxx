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
#include "itkAffineTransform.h"
#include "itkCompositeTransform.h"
const double epsilon = 1e-10;

template <typename TMatrix>
bool testMatrix( const TMatrix & m1, const TMatrix & m2 )
{
  bool pass = true;

  for( unsigned int  i = 0; i < TMatrix::RowDimensions; i++ )
    {
    for( unsigned int j = 0; j < TMatrix::ColumnDimensions; j++ )
      {
      if( std::fabs( m1[i][j] - m2[i][j] ) > epsilon )
        {
        pass = false;
        }
      }
    }
  return pass;
}

template <typename TVector>
bool testVector( const TVector & v1, const TVector & v2 )
{
  bool pass = true;

  for( unsigned int i = 0; i < TVector::Dimension; i++ )
    {
    if( std::fabs( v1[i] - v2[i] ) > epsilon )
      {
      pass = false;
      }
    }
  return pass;
}

int itkTransformCloneTest(int, char *[])
{
  typedef itk::AffineTransform<double, 3> AffineTransformType;
  typedef itk::Transform<double,3,3>      Transform3DType;
  AffineTransformType::Pointer affineXfrm =
    AffineTransformType::New();
  AffineTransformType::OutputVectorType axis,offset;
  axis[0] = -1.0;
  axis[1] = 1.0;
  axis[2] = 0.0;
  affineXfrm->Rotate3D(axis,1.3);
  axis[0] = 0.0;
  axis[1] = 0.0;
  axis[2] = -1.0;
  affineXfrm->Rotate3D(axis,0.5);
  offset[0] = 999.0;
  offset[1] = -31415926.0;
  offset[2] = 32.768;
  affineXfrm->Translate(offset);

  Transform3DType::Pointer clonePtr =
    affineXfrm->Clone().GetPointer();
  AffineTransformType::Pointer cloneAffineXfrm =
    dynamic_cast<AffineTransformType *>(clonePtr.GetPointer());

  if(cloneAffineXfrm.IsNull())
    {
    std::cerr << "Failed to downcast return value from Clone to "
              << "AffineTransform, reported type is "
              << clonePtr->GetTransformTypeAsString()
              << std::endl;
    return EXIT_FAILURE;
    }
  const AffineTransformType::MatrixType &cloneMatrix =
    cloneAffineXfrm->GetMatrix();
  const AffineTransformType::OffsetType &cloneOffset =
    cloneAffineXfrm->GetOffset();
  if(!testMatrix(cloneMatrix,affineXfrm->GetMatrix()))
    {
    std::cerr << "Matrix mismatch between original and clone"
              << std::endl;
    return EXIT_FAILURE;
    }
  if(!testVector(cloneOffset,affineXfrm->GetOffset()))
    {
    std::cerr << "Offset mismatch between original and clone"
              << std::endl;
    return EXIT_FAILURE;
    }
  typedef itk::CompositeTransform<double,3> CompositeTransformType;
  CompositeTransformType::Pointer compositeXfrm =
    CompositeTransformType::New();
  compositeXfrm->AddTransform(clonePtr.GetPointer());
  compositeXfrm->SetOnlyMostRecentTransformToOptimizeOn();

  CompositeTransformType::Pointer cloneCompositeXfrm =
    dynamic_cast<CompositeTransformType *>(compositeXfrm->Clone().GetPointer());

  if((compositeXfrm->GetNumberOfTransforms() !=
      cloneCompositeXfrm->GetNumberOfTransforms()))
    {
    std::cerr << "Number of transforms doesn't match" << std::endl;
    return EXIT_FAILURE;
    }
  for(unsigned i = 0; i < compositeXfrm->GetNumberOfTransforms(); ++i)
    {
    AffineTransformType::ConstPointer originalXfrm =
      dynamic_cast<const AffineTransformType *>(compositeXfrm->GetNthTransformConstPointer(i));
    AffineTransformType::ConstPointer  cloneXfrm =
      dynamic_cast<const AffineTransformType *>(cloneCompositeXfrm->GetNthTransformConstPointer(i));

    if(originalXfrm.IsNull() || cloneXfrm.IsNull())
      {
      std::cerr << "Failed downcast to Affine Transform" << std::endl;
      return EXIT_FAILURE;
      }
    if(!testMatrix(originalXfrm->GetMatrix(),cloneXfrm->GetMatrix()))
      {
      std::cerr << "ConstituentTransformMismatch  at "
                << i << std::endl;
      return EXIT_FAILURE;
      }

    if(compositeXfrm->GetNthTransformToOptimize(i) !=
       cloneCompositeXfrm->GetNthTransformToOptimize(i))
      {
      std::cerr << "Transform optimize flag mismatch at "
                << i << std::endl;
      }
    }
  return EXIT_SUCCESS;
}
