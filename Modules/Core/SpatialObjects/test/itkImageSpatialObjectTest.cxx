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
// Disable warning for long symbol names in this file only

/*
* This is a test file for the itkImageSpatialObject class.
* The suported pixel types does not include itkRGBPixel, itkRGBAPixel, etc...
* So far it only allows to manage images of simple types like unsigned short,
* unsigned int, or itk::Vector<...>.
*/


#include "itkImageRegionIterator.h"

#include "itkImageSpatialObject.h"
#include "itkLinearInterpolateImageFunction.h"


int itkImageSpatialObjectTest(int, char* [])
{
  #define NDimensions 3

  typedef double                                     ScalarType;
  typedef unsigned short                             Pixel;
  typedef itk::Image<Pixel,NDimensions>              ImageType;
  typedef itk::ImageSpatialObject<NDimensions,Pixel> ImageSpatialObject;
  typedef itk::ImageRegionIterator<ImageType>        Iterator;
  typedef itk::Point<ScalarType,NDimensions>         Point;

  ImageType::Pointer image = ImageType::New();
  ImageType::SizeType size = {{ 10, 10, 10 }};
  ImageType::IndexType index = {{ 0, 0, 0 }};
  ImageType::RegionType region;
  ImageType::PointType origin;
  origin.Fill(5);

  region.SetSize(size);
  region.SetIndex(index);
  image->SetOrigin(origin);
  image->SetLargestPossibleRegion(region);
  image->SetBufferedRegion(region);
  image->SetRequestedRegion(region);
  image->Allocate();

  Iterator it(image,region);
  Pixel p =0;

  for(; !it.IsAtEnd(); ++it, ++p)
    {
    it.Set(p);
    }
  it.GoToBegin();

  ImageSpatialObject::Pointer imageSO = ImageSpatialObject::New();
  imageSO->Print(std::cout);

  imageSO->SetImage(image);
  ImageSpatialObject::TransformType::OffsetType offset;
  offset.Fill(5);

  imageSO->GetObjectToParentTransform()->SetOffset(offset);
  imageSO->ComputeObjectToWorldTransform();

  Point q,r;
  double returnedValue,expectedValue;

  r.Fill(9);
  q.Fill(15);

  imageSO->ComputeBoundingBox();
  std::cout << "Bounding Box = " << imageSO->GetBoundingBox()->GetBounds() << std::endl;
  std::cout<<"IsInside()...";
  if( imageSO->IsInside(r) || !imageSO->IsInside(q) )
    {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout<<"[PASSED]"<<std::endl;
    }

  q.Fill(15.1);
  expectedValue = 555;

  try
    {
    imageSO->ValueAt(q,returnedValue);
    }
  catch( itk::ExceptionObject & )
    {
    throw;
    }

  std::cout<<"ValueAt()...";
  if( itk::Math::NotAlmostEquals( returnedValue, expectedValue ) )
    {
    std::cout << "Expected: " << expectedValue << " returned: " << returnedValue << std::endl;
    std::cout <<"[FAILED]: " << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout<<"[PASSED]"<<std::endl;
    }

  ImageSpatialObject::OutputVectorType derivative,expectedDerivative;
  Pixel expectedPixel;

  imageSO->DerivativeAt(q,1,derivative);
  expectedPixel = 1;
  expectedDerivative[0]=expectedPixel;
  expectedPixel = 10;
  expectedDerivative[1]=expectedPixel;
  expectedPixel = 100;
  expectedDerivative[2]=expectedPixel;
  std::cout<<"DerivativeAt()...";
  if( derivative != expectedDerivative )
    {
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout<<"[PASSED]"<<std::endl;
    }

  // Now testing the ValueAt() with an interpolator
  typedef itk::LinearInterpolateImageFunction<ImageType> InterpolatorType;
  InterpolatorType::Pointer interpolator = InterpolatorType::New();
  imageSO->SetInterpolator(interpolator);
  expectedValue = 566.1;

  try
    {
    imageSO->ValueAt(q,returnedValue);
    }
  catch( itk::ExceptionObject & )
    {
    throw;
    }

  std::cout<<"ValueAt() with interpolator...";
  if( std::fabs(returnedValue-expectedValue)>0.001 )
    {
    std::cout << "Expected: " << expectedValue << " returned: " << returnedValue << std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout<<"[PASSED]"<<std::endl;
    }


  imageSO->DerivativeAt(q,1,derivative);
  expectedDerivative[0]=1;
  expectedDerivative[1]=10;
  expectedDerivative[2]=100;
  std::cout<<"DerivativeAt() with interpolator ...";
  if(  std::fabs(derivative[0]-expectedDerivative[0])>0.00001
    || std::fabs(derivative[1]-expectedDerivative[1])>0.00001
    || std::fabs(derivative[2]-expectedDerivative[2])>0.00001
    )
    {
    std::cout << "Expected: " << derivative << " returned: " << expectedDerivative << std::endl;
    std::cout<<"[FAILED]"<<std::endl;
    return EXIT_FAILURE;
    }
  else
    {
    std::cout<<"[PASSED]"<<std::endl;
    }

  imageSO->Print(std::cout);

  return EXIT_SUCCESS;
}
