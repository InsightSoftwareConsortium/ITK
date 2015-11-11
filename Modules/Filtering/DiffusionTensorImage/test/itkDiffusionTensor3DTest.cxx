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

#include "itkDiffusionTensor3D.h"
#include "itkImageRegionIterator.h"
#include "itkMath.h"

int itkDiffusionTensor3DTest(int, char* [] )
{
  // Test it all
  float val[6] = {1.8f, 0.2f, 0.5f, 3.4f, 2.0f, 1.2f};

  typedef itk::DiffusionTensor3D<float>         Float3DTensorType;
  typedef itk::DiffusionTensor3D<unsigned char> Uchar3DTensorType;

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
  bool passed = true;
  for (unsigned int i = 0; i < pixel.GetNumberOfComponents(); i++)
    {
    if (itk::Math::NotExactlyEquals(pixel.GetNthComponent(i), val[i]))
      {
      std::cout << "Float3DTensorType pixel(val) failed." << std::endl
                << "\tExpected val["
                << i
                << "] = "
                << val[i]
                << " but got pixel.GetNthComponent("
                << i
                << ") = "
                << pixel.GetNthComponent(i)
                << std::endl;
      passed = false;
      }
    else
      {
      std::cout << "\tpixel[" << i << "] = " << pixel.GetNthComponent(i) << std::endl;
      }
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

  std::cout << "testing the pixel(i,j) API" << std::endl;
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
    typedef itk::DiffusionTensor3D<double>         Double3DTensorType;

    Double3DTensorType tensor2;

    double v[3];
    v[0] = 19.0;
    v[1] = 23.0;
    v[2] = 29.0;

    tensor2(0,0) = v[0];
    tensor2(0,1) =  0.0;
    tensor2(0,2) =  0.0;
    tensor2(1,0) =  0.0; // overrides (0,1)
    tensor2(1,1) = v[1];
    tensor2(1,2) =  0.0;
    tensor2(2,0) =  0.0; // overrides (0,2)
    tensor2(2,1) =  0.0; // overrides (1,2)
    tensor2(2,2) = v[2];

    std::cout << "DiffusionTensor3D = " << std::endl;
    std::cout << tensor2 << std::endl;

    Double3DTensorType::EigenValuesArrayType     eigenValues;
    Double3DTensorType::EigenVectorsMatrixType   eigenVectors;

    tensor2.ComputeEigenAnalysis( eigenValues, eigenVectors );

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
          std::cerr << "Eigenvalue computation failed" << std::endl;
          std::cerr << "expectedValues = " << expectedValues << std::endl;
          std::cerr << "eigenValues    = " << eigenValues << std::endl;
          return EXIT_FAILURE;
          }
        }
    }

    // Now let's do something more involved...
    tensor2(0,0) =  7.0;
    tensor2(0,1) =  0.0;
    tensor2(0,2) =  3.0;
    tensor2(1,0) =  0.0; // overrides (0,1)
    tensor2(1,1) =  0.0;
    tensor2(1,2) =  0.0;
    tensor2(2,0) =  3.0; // overrides (0,2)
    tensor2(2,1) =  0.0; // overrides (1,2)
    tensor2(2,2) =  7.0;

    std::cout << "DiffusionTensor3D = " << std::endl;
    std::cout << tensor2 << std::endl;

    tensor2.ComputeEigenAnalysis( eigenValues, eigenVectors );

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
          std::cerr << "Eigenvalue computation failed" << std::endl;
          std::cerr << "expectedValues = " << expectedValues << std::endl;
          std::cerr << "eigenValues    = " << eigenValues << std::endl;
          return EXIT_FAILURE;
          }
        }
    }

    // Now let's do one where we know the rotation...
    tensor2(0,0) =  9.0;
    tensor2(0,1) =  0.0;
    tensor2(0,2) =  7.0;
    tensor2(1,0) =  0.0; // overrides (0,1)
    tensor2(1,1) =  0.0;
    tensor2(1,2) =  0.0;
    tensor2(2,0) =  7.0; // overrides (0,2)
    tensor2(2,1) =  0.0; // overrides (1,2)
    tensor2(2,2) =  3.0;

    std::cout << "DiffusionTensor3D = " << std::endl;
    std::cout << tensor2 << std::endl;

    tensor2.ComputeEigenAnalysis( eigenValues, eigenVectors );

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
    }


  }

  // Test GetTrace() and GetFractionalAnisotropy methods
  {

    typedef itk::DiffusionTensor3D<double>          Double3DTensorType;
    typedef Double3DTensorType::AccumulateValueType AccumulateValueType;
    typedef Double3DTensorType::RealValueType       RealValueType;

    Double3DTensorType tensor3;

    tensor3(0,0) =  19.0;
    tensor3(0,1) =   0.0;
    tensor3(0,2) =   0.0;
    tensor3(1,0) =   0.0; // overrides (0,1)
    tensor3(1,1) =  23.0;
    tensor3(1,2) =   0.0;
    tensor3(2,0) =   7.0; // overrides (0,2)
    tensor3(2,1) =   0.0; // overrides (1,2)
    tensor3(2,2) =  29.0;

    AccumulateValueType expectedTrace =
              itk::NumericTraits< AccumulateValueType >::ZeroValue();

    expectedTrace += tensor3(0,0);
    expectedTrace += tensor3(1,1);
    expectedTrace += tensor3(2,2);

    const double tolerance = 1e-4;

    AccumulateValueType computedTrace = tensor3.GetTrace();
    if( std::fabs( computedTrace - expectedTrace ) > tolerance )
      {
      std::cerr << "Error computing the Trace" << std::endl;
      std::cerr << "Expected trace = " << expectedTrace << std::endl;
      std::cerr << "Computed trace = " << computedTrace << std::endl;
      return EXIT_FAILURE;
      }

    // Test the value of internal scalar product
    const RealValueType expectedInternalScalarProduct = 1829;

    RealValueType computedInternalScalarProduct = tensor3.GetInnerScalarProduct();
    if( std::fabs( computedInternalScalarProduct - expectedInternalScalarProduct ) > tolerance )
      {
      std::cerr << "Error computing Internal Scalar Product" << std::endl;
      std::cerr << "Expected = " << expectedInternalScalarProduct << std::endl;
      std::cerr << "Computed = " << computedInternalScalarProduct << std::endl;
      return EXIT_FAILURE;
      }


    // Test the value of Fractional Anisotropy
    const RealValueType expectedFractionalAnisotropy = 0.349177;

    RealValueType computedFractionalAnisotropy = tensor3.GetFractionalAnisotropy();
    if( std::fabs( computedFractionalAnisotropy - expectedFractionalAnisotropy ) > tolerance )
      {
      std::cerr << "Error computing Fractional Anisotropy" << std::endl;
      std::cerr << "Expected = " << expectedFractionalAnisotropy << std::endl;
      std::cerr << "Computed = " << computedFractionalAnisotropy << std::endl;
      return EXIT_FAILURE;
      }

    // Test the value of Relative Anisotropy
    const RealValueType expectedRelativeAnisotropy = 1.9044;

    RealValueType computedRelativeAnisotropy = tensor3.GetRelativeAnisotropy();
    if( std::fabs( computedRelativeAnisotropy - expectedRelativeAnisotropy ) > tolerance )
      {
      std::cerr << "Error computing Relative Anisotropy" << std::endl;
      std::cerr << "Expected = " << expectedRelativeAnisotropy << std::endl;
      std::cerr << "Computed = " << computedRelativeAnisotropy << std::endl;
      return EXIT_FAILURE;
      }


  } // end of Test GetTrace() method

  //Test Numeric Traits
  {
    typedef itk::DiffusionTensor3D<int>             TensorType;

    TensorType maxTensor = itk::NumericTraits<TensorType>::max();
    std::cout << maxTensor <<std::endl;

    TensorType minTensor = itk::NumericTraits<TensorType>::min();
    std::cout << minTensor <<std::endl;

    TensorType nonpositiveMinTensor
                          = itk::NumericTraits<TensorType>::NonpositiveMin();
    std::cout << nonpositiveMinTensor <<std::endl;

    TensorType zeroValue
                          = itk::NumericTraits<TensorType>::ZeroValue();
    std::cout << zeroValue <<std::endl;

    TensorType oneValue
                          = itk::NumericTraits<TensorType>::OneValue();
    std::cout << oneValue <<std::endl;

    TensorType zero = itk::NumericTraits<TensorType>::ZeroValue();
    std::cout << zero <<std::endl;

    TensorType one = itk::NumericTraits<TensorType>::OneValue();
    std::cout << one <<std::endl;
  }

  //Test casting constructors
  {
    typedef itk::DiffusionTensor3D<int>     Int3DTensorType;

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

  return (passed ? EXIT_SUCCESS : EXIT_FAILURE);
}
