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

#include <iostream>

#include "itkMath.h"
#include "itkSymmetricSecondRankTensor.h"
#include "itkImageRegionIterator.h"

int itkSymmetricSecondRankTensorTest(int, char* [] )
{
  // Test it all

  float val[6] = {1.8f, 0.2f, 0.5f, 3.4f, 2.0f, 1.2f};

  typedef itk::SymmetricSecondRankTensor<float,3>         Float3DTensorType;
  typedef itk::SymmetricSecondRankTensor<unsigned char,3> Uchar3DTensorType;

  Float3DTensorType pixel(val);

  unsigned char pixelInit0[6] = {255, 255, 255,128,34,17};
  unsigned char pixelInit1[6] = {255, 255, 244,19,23,29};

  Uchar3DTensorType pixelArray[2];
  pixelArray[0] = pixelInit0;
  pixelArray[1] = pixelInit1;

  std::cout << "sizeof(pixel) = " << sizeof (pixel) << std::endl;
  if (sizeof(pixel) != 6 * sizeof(Float3DTensorType::ComponentType))
    {
    std::cerr << "ERROR: sizeof(pixel) == " << sizeof(pixel) << " but is should be " << 6 * sizeof(Float3DTensorType::ComponentType) << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "pixel.GetNumberOfComponents = " << pixel.GetNumberOfComponents() << std::endl;
  std::cout << "pixel.GetNthComponent()" << std::endl;
  for (unsigned int i = 0; i < pixel.GetNumberOfComponents(); i++)
    {
    std::cout << "\tpixel[" << i << "] = " << pixel.GetNthComponent(i) << std::endl;
    }

  pixel(0,0) = 11.0;
  pixel(0,1) = 21.0;
  pixel(0,2) = 15.0;
  pixel(1,0) = 11.0;
  pixel(1,1) = 31.0;
  pixel(1,2) = 10.0;
  pixel(2,0) = 11.0; // these three last element will overwrite its symmetric counterparts
  pixel(2,1) = 41.0;
  pixel(2,2) = 14.0;

  std::cout << "testing the pixel(i,j) APID" << std::endl;
  for (unsigned int i = 0; i < pixel.GetNumberOfComponents(); i++)
    {
    std::cout << "\tpixel[" << i << "] = " << pixel.GetNthComponent(i) << std::endl;
    }

  std::cout << "pixel[0] = 111; pixel[1] = 222; pixel[2] = 333;" << std::endl;
  std::cout << "pixel[3] = 444; pixel[4] = 555; pixel[5] = 666;" << std::endl;

  pixel[0] = 111; pixel[1] = 222; pixel[2] = 333;
  pixel[3] = 444; pixel[4] = 555; pixel[5] = 666;

  for (unsigned int i = 0; i < pixel.GetNumberOfComponents(); i++)
    {
    std::cout << "\tpixel[" << i << "] = " << pixel.GetNthComponent(i) << std::endl;
    }

  std::cout << "std::cout << pixel << std::endl;" << std::endl;
  std::cout << "\t" << pixel << std::endl;

  for (unsigned int j = 0; j < 2; j++)
    {
    std::cout << "pixelArray["<< j << "].GetNumberOfComponents = " << pixelArray[j].GetNumberOfComponents() << std::endl;
    std::cout << "pixelArray[" << j << "].GetNthComponent()" << std::endl;
    for (unsigned int i = 0; i < pixelArray[j].GetNumberOfComponents(); i++)
      {
      std::cout << "\tpixelArray[" << j << "].GetNthComponent(" << i << ") = " << static_cast<int>(pixelArray[j].GetNthComponent(i)) << std::endl;
      }
    }

  std::cout << "Testing arithmetic methods" << std::endl;
  Float3DTensorType pa;
  Float3DTensorType pb;

  pa[0] = 1.25;
  pa[1] = 3.25;
  pa[2] = 5.25;
  pa[3] = 1.25;
  pa[4] = 3.25;
  pa[5] = 5.25;

  pb[0] = 1.55;
  pb[1] = 3.55;
  pb[2] = 5.55;
  pb[3] = 1.55;
  pb[4] = 3.55;
  pb[5] = 5.55;

  Float3DTensorType pc;

  pc = pa + pb;
  std::cout << "addition = " << pc << std::endl;

  pc = pa - pb;
  std::cout << "subtraction = " << pc << std::endl;

  pc += pb;
  std::cout << "in-place addition = " << pc << std::endl;

  pc -= pb;
  std::cout << "in-place subtraction = " << pc << std::endl;

  pc = pa * 3.2;
  std::cout << "product by scalar = " << pc << std::endl;

  /** Create an Image of tensors  */
  typedef Float3DTensorType           PixelType;
  typedef itk::Image< PixelType, 3 >  ImageType;

  ImageType::Pointer dti = ImageType::New();

  ImageType::SizeType  size;
  ImageType::IndexType start;
  ImageType::RegionType region;

  size[0] = 128;
  size[1] = 128;
  size[2] = 128;

  start[0] = 0;
  start[1] = 0;
  start[2] = 0;

  region.SetIndex( start );
  region.SetSize( size );

  dti->SetRegions( region );
  dti->Allocate();

  ImageType::SpacingType spacing;
  spacing[0] = 0.5;
  spacing[1] = 0.5;
  spacing[2] = 1.5;

  ImageType::PointType origin;
  origin[0] = 25.5;
  origin[1] = 25.5;
  origin[2] = 27.5;

  dti->SetOrigin( origin );
  dti->SetSpacing( spacing );

  PixelType tensor;

  tensor[0] = 1.2;
  tensor[1] = 2.2;
  tensor[2] = 3.2;
  tensor[3] = 4.2;
  tensor[4] = 5.2;
  tensor[5] = 6.2;

  dti->FillBuffer( tensor );

  typedef itk::ImageRegionIterator< ImageType > IteratorType;

  IteratorType it( dti, region );
  it.GoToBegin();

  while( !it.IsAtEnd() )
    {
    it.Set( tensor );
    ++it;
    }

  // Test Eigen values computation
  {
    typedef itk::SymmetricSecondRankTensor<double,3>         Double3DTensorType;

    Double3DTensorType tensor3D;

    double v[3];
    v[0] = 19.0;
    v[1] = 23.0;
    v[2] = 29.0;

    tensor3D(0,0) = v[0];
    tensor3D(0,1) =  0.0;
    tensor3D(0,2) =  0.0;
    tensor3D(1,0) =  0.0; // overrides (0,1)
    tensor3D(1,1) = v[1];
    tensor3D(1,2) =  0.0;
    tensor3D(2,0) =  0.0; // overrides (0,2)
    tensor3D(2,1) =  0.0; // overrides (1,2)
    tensor3D(2,2) = v[2];

    std::cout << "SymmetricTensor = " << std::endl;
    std::cout << tensor3D << std::endl;

    Double3DTensorType::EigenValuesArrayType     eigenValues;
    Double3DTensorType::EigenValuesArrayType     eigenValues2;
    Double3DTensorType::EigenVectorsMatrixType   eigenVectors;

    tensor3D.ComputeEigenAnalysis( eigenValues, eigenVectors );
    tensor3D.ComputeEigenValues( eigenValues2 );

    std::cout << "EigenValues = " << std::endl;
    std::cout << eigenValues << std::endl;

    std::cout << "EigenVectors = " << std::endl;
    std::cout << eigenVectors << std::endl;

    const double tolerance = 1e-4;

    {
      Double3DTensorType::EigenValuesArrayType     expectedValues;
      expectedValues[0] = v[0];
      expectedValues[1] = v[1];
      expectedValues[2] = v[2];

      for(unsigned int i=0; i<3; i++)
        {
        if( std::fabs( expectedValues[i] - eigenValues[i] ) > tolerance )
          {
          std::cerr << "EigenAnalysis computation failed" << std::endl;
          std::cerr << "expectedValues = " << expectedValues << std::endl;
          std::cerr << "eigenValues    = " << eigenValues << std::endl;
          return EXIT_FAILURE;
          }
        }

      for(unsigned int j=0; j<3; j++)
        {
        if( std::fabs( expectedValues[j] - eigenValues2[j] ) > tolerance )
          {
          std::cerr << "EigenValues computation failed" << std::endl;
          std::cerr << "expectedValues = " << expectedValues << std::endl;
          std::cerr << "eigenValues    = " << eigenValues2 << std::endl;
          return EXIT_FAILURE;
          }
        }

    }

    // Now let's do something more involved...
    tensor3D(0,0) =  7.0;
    tensor3D(0,1) =  0.0;
    tensor3D(0,2) =  3.0;
    tensor3D(1,0) =  0.0; // overrides (0,1)
    tensor3D(1,1) =  0.0;
    tensor3D(1,2) =  0.0;
    tensor3D(2,0) =  3.0; // overrides (0,2)
    tensor3D(2,1) =  0.0; // overrides (1,2)
    tensor3D(2,2) =  7.0;

    std::cout << "SymmetricTensor = " << std::endl;
    std::cout << tensor3D << std::endl;

    tensor3D.ComputeEigenAnalysis( eigenValues, eigenVectors );
    tensor3D.ComputeEigenValues( eigenValues2 );

    std::cout << "EigenValues = " << std::endl;
    std::cout << eigenValues << std::endl;

    std::cout << "EigenVectors = " << std::endl;
    std::cout << eigenVectors << std::endl;

    {
      Double3DTensorType::EigenValuesArrayType     expectedValues;
      expectedValues[0] =  0.0;
      expectedValues[1] =  4.0;
      expectedValues[2] = 10.0;

      for(unsigned int i=0; i<3; i++)
        {
        if( std::fabs( expectedValues[i] - eigenValues[i] ) > tolerance )
          {
          std::cerr << "EigenAnalysis computation failed" << std::endl;
          std::cerr << "expectedValues = " << expectedValues << std::endl;
          std::cerr << "eigenValues    = " << eigenValues << std::endl;
          return EXIT_FAILURE;
          }
        }

      for(unsigned int j=0; j<3; j++)
        {
        if( std::fabs( expectedValues[j] - eigenValues2[j] ) > tolerance )
          {
          std::cerr << "EigenValues computation failed" << std::endl;
          std::cerr << "expectedValues = " << expectedValues << std::endl;
          std::cerr << "eigenValues    = " << eigenValues2 << std::endl;
          return EXIT_FAILURE;
          }
        }


    }

    // Now let's do one where we know the rotation...
    tensor3D(0,0) =  9.0;
    tensor3D(0,1) =  0.0;
    tensor3D(0,2) =  7.0;
    tensor3D(1,0) =  0.0; // overrides (0,1)
    tensor3D(1,1) =  0.0;
    tensor3D(1,2) =  0.0;
    tensor3D(2,0) =  7.0; // overrides (0,2)
    tensor3D(2,1) =  0.0; // overrides (1,2)
    tensor3D(2,2) =  3.0;

    std::cout << "SymmetricTensor = " << std::endl;
    std::cout << tensor3D << std::endl;

    tensor3D.ComputeEigenAnalysis( eigenValues, eigenVectors );
    tensor3D.ComputeEigenValues( eigenValues2 );

    std::cout << "EigenValues = " << std::endl;
    std::cout << eigenValues << std::endl;

    std::cout << "EigenVectors = " << std::endl;
    std::cout << eigenVectors << std::endl;

    {
      Double3DTensorType::EigenValuesArrayType     expectedValues;
      expectedValues[0] = -1.61577;
      expectedValues[1] =  0.00000;
      expectedValues[2] = 13.61580;

      for(unsigned int i=0; i<3; i++)
        {
        if( std::fabs( expectedValues[i] - eigenValues[i] ) > tolerance )
          {
          std::cerr << "Eigenvalue computation failed" << std::endl;
          std::cerr << "expectedValues = " << expectedValues << std::endl;
          std::cerr << "eigenValues    = " << eigenValues << std::endl;
          return EXIT_FAILURE;
          }
        }

      for(unsigned int j=0; j<3; j++)
        {
        if( std::fabs( expectedValues[j] - eigenValues2[j] ) > tolerance )
          {
          std::cerr << "EigenValues computation failed" << std::endl;
          std::cerr << "expectedValues = " << expectedValues << std::endl;
          std::cerr << "eigenValues    = " << eigenValues2 << std::endl;
          return EXIT_FAILURE;
          }
        }

    }

  } // end of Test Eigen values computation


  // Test GetTrace() method
  {

    typedef itk::SymmetricSecondRankTensor<double,3>         Double3DTensorType;
    typedef Double3DTensorType::AccumulateValueType          AccumulateValueType;

    Double3DTensorType tensor3D;

    tensor3D(0,0) =  19.0;
    tensor3D(0,1) =   0.0;
    tensor3D(0,2) =   0.0;
    tensor3D(1,0) =   0.0; // overrides (0,1)
    tensor3D(1,1) =  23.0;
    tensor3D(1,2) =   0.0;
    tensor3D(2,0) =   7.0; // overrides (0,2)
    tensor3D(2,1) =   0.0; // overrides (1,2)
    tensor3D(2,2) =  29.0;

    AccumulateValueType expectedTrace =
              itk::NumericTraits< AccumulateValueType >::ZeroValue();

    expectedTrace += tensor3D(0,0);
    expectedTrace += tensor3D(1,1);
    expectedTrace += tensor3D(2,2);

    const double tolerance = 1e-4;

    AccumulateValueType computedTrace = tensor3D.GetTrace();
    if( std::fabs( computedTrace - expectedTrace ) > tolerance )
      {
      std::cerr << "Error computing the Trace" << std::endl;
      std::cerr << "Expected trace = " << expectedTrace << std::endl;
      std::cerr << "Computed trace = " << computedTrace << std::endl;
      return EXIT_FAILURE;
      }

  } // end of Test GetTrace() method


  // Test Matrix * SymmetricSecondRankTensor function
  {

    typedef itk::SymmetricSecondRankTensor<double,3>   Double3DTensorType;
    typedef itk::Matrix<double, 3, 3>                  Double3DMatrixType;

    Double3DTensorType tensor3D;

    tensor3D(0,0) =  19.0;
    tensor3D(0,1) =   0.0;
    tensor3D(0,2) =   0.0;
    tensor3D(1,0) =   0.0; // overrides (0,1)
    tensor3D(1,1) =  23.0;
    tensor3D(1,2) =   0.0;
    tensor3D(2,0) =   7.0; // overrides (0,2)
    tensor3D(2,1) =   0.0; // overrides (1,2)
    tensor3D(2,2) =  29.0;

    Double3DMatrixType matrix3D;
    matrix3D.Fill(1.0);
    std::vector<double> ans;
    ans.push_back(26);
    ans.push_back(23);
    ans.push_back(36);

    Double3DMatrixType result1 = tensor3D.PreMultiply( matrix3D );
    std::cout << result1 << std::endl;
    for (unsigned int ii = 0; ii < 3; ++ii)
    {
      if (itk::Math::NotAlmostEquals(result1[0][ii], ans[ii]))
      {
        std::cout << "PreMultiply FAILED" << std::endl;
        return EXIT_FAILURE;
      }
    }

    Double3DMatrixType result2 = tensor3D.PostMultiply( matrix3D );
    std::cout << result2 << std::endl;
    for (unsigned int ii = 0; ii < 3; ++ii)
    {
      if (itk::Math::NotAlmostEquals(result2[ii][0], ans[ii]))
      {
        std::cout << "PostMultiply FAILED" << std::endl;
        return EXIT_FAILURE;
      }
    }

    Double3DTensorType result3 = tensor3D.Rotate( matrix3D );
    std::cout << result3 << std::endl;
    for (unsigned int ii = 0; ii < 6; ++ii)
    {
      if (itk::Math::NotAlmostEquals(result3[ii], 85))
      {
        std::cout << "Rotate FAILED" << std::endl;
        return EXIT_FAILURE;
      }
    }

  } // end of Matrix * SymmetricSecondRankTensor test

  //Test casting constructors
  {
    typedef itk::SymmetricSecondRankTensor<int,3>     Int3DTensorType;

    Int3DTensorType intTensor;
    intTensor[0] =   1;
    intTensor[1] =  -2;
    intTensor[2] =   3;
    intTensor[3] =   4;
    intTensor[4] =   5;
    intTensor[5] =   6;

    //Test constructors
    Float3DTensorType floatTensor(intTensor);

    //test Assignment
    Float3DTensorType floatTensor2 = intTensor;

    //Test casting
    Float3DTensorType floatTensor3 = static_cast<Float3DTensorType>(intTensor);

    //Check that all floatTensors have are the same
    float precision = 1e-6;
    for (unsigned int i=0;i<Float3DTensorType::InternalDimension;++i)
    {
      float intVal = static_cast<float>(intTensor[i]);
      if ( (floatTensor[i] - intVal) > precision ||
          (floatTensor2[i] - intVal) > precision ||
          (floatTensor3[i] - intVal) > precision )
      {
        std::cerr << "Error failed casting/templated Constructor Test" << std::endl;
        return EXIT_FAILURE;
      }
    }
  }

  return EXIT_SUCCESS;
}
