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
/**
 *
 *  This program illustrates the use of Adaptors and
 *  Accessors
 *
 *  The example shows how an Adaptor can be used to
 *  get acces only to thered component of an RGBPixel image
 *  giving the appearance of being just a 'float' image
 *
 *  That will allow to pass the red component of this
 *  image as input or output to any filter that expects
 *  a float image
 *
 */

#include "itkImageAdaptor.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkRedPixelAccessor.h"


//-------------------------------------
//     Typedefs for convenience
//-------------------------------------
typedef itk::Image< itk::RGBPixel<float>,   2 > myImageType;


typedef itk::RedPixelAccessor<float> myRedAccessorType;

typedef itk::ImageAdaptor< myImageType, myRedAccessorType > myRedAdaptorType;

typedef itk::ImageRegionIteratorWithIndex< myImageType >       myIteratorType;

typedef itk::ImageRegionIteratorWithIndex< myRedAdaptorType >  myRedIteratorType;

//-------------------------
//
//   Main code
//
//-------------------------
int itkOrientedImageAdaptorTest(int, char* []) {


  myImageType::SizeType size;
  size[0] = 2;
  size[1] = 2;

  myImageType::IndexType index;
  index[0] = 0;
  index[1] = 0;

  myImageType::RegionType region;
  region.SetIndex( index );
  region.SetSize(  size  );

  myImageType::Pointer myImage = myImageType::New();


  myImage->SetLargestPossibleRegion( region );
  myImage->SetBufferedRegion( region );
  myImage->SetRequestedRegion( region );
  myImage->Allocate();

  myIteratorType  it1( myImage, myImage->GetRequestedRegion() );

  // Value to initialize the pixels
  myImageType::PixelType::ComponentType colorInit[3] = {1.0f, 0.5f, 0.5f};
  myImageType::PixelType color = colorInit;

  // Initializing all the pixel in the image
  it1.GoToBegin();
  while( !it1.IsAtEnd() )
    {
    it1.Set(color);
    ++it1;
    }

  // Reading the values to verify the image content
  std::cout << "--- Before --- " << std::endl;
  it1.GoToBegin();
  while( !it1.IsAtEnd() )
    {
    const myImageType::PixelType c( it1.Get() );
    std::cout << c.GetRed()   << "  ";
    std::cout << c.GetGreen() << "  ";
    std::cout << c.GetBlue()  << std::endl;
    ++it1;
    }

  myRedAdaptorType::Pointer myAdaptor = myRedAdaptorType::New();
  myAdaptor->SetImage( myImage );


  myRedIteratorType  it2( myAdaptor, myAdaptor->GetRequestedRegion() );

  // Set the values of the Red component of myImage, using myAdaptor
  it2.GoToBegin();
  while( !it2.IsAtEnd() )
    {
    it2.Set( 0.4 );
    ++it2;
    }


  std::cout << "--- After --- " << std::endl;

  it1.GoToBegin();
  while( !it1.IsAtEnd() )
    {
    const myImageType::PixelType c( it1.Get() );
    std::cout << c.GetRed()   << "  ";
    std::cout << c.GetGreen() << "  ";
    std::cout << c.GetBlue()  << std::endl;
    ++it1;
    }

  // Test the Set/Get Methods of the adaptor
  // First test the get methods of the adaptor

  if (myImage->GetPixelContainer() != myAdaptor->GetPixelContainer())
    {
    std::cout << "image pixel container != adaptor pixel container: "
              << myImage->GetPixelContainer() << " != "
              << myAdaptor->GetPixelContainer()
              << std::endl;
    return EXIT_FAILURE;
    }

  float forigin[2] = {2.0, 3.0};
  myImage->SetOrigin(forigin);
  if (myImage->GetOrigin() != myAdaptor->GetOrigin())
    {
    std::cout << "image origin != adaptor origin: "
              << myImage->GetOrigin() << " != "
              << myAdaptor->GetOrigin()
              << std::endl;
    return EXIT_FAILURE;
    }

  double dorigin[2] = {2.0, 3.0};
  myImage->SetOrigin(dorigin);
  if (myImage->GetOrigin() != myAdaptor->GetOrigin())
    {
    std::cout << "image origin != adaptor origin: "
              << myImage->GetOrigin() << " != "
              << myAdaptor->GetOrigin()
              << std::endl;
    return EXIT_FAILURE;
    }


  myImageType::PointType imageOrigin;
  imageOrigin.Fill(10.0);
  myImage->SetOrigin(imageOrigin);
  if (myImage->GetOrigin() != myAdaptor->GetOrigin())
    {
    std::cout << "image origin != adaptor origin: "
              << myImage->GetOrigin() << " != "
              << myAdaptor->GetOrigin()
              << std::endl;
    return EXIT_FAILURE;
    }

  float fspacing[2] = {2.0, 3.0};
  myImage->SetSpacing(fspacing);
  if (myImage->GetSpacing() != myAdaptor->GetSpacing())
    {
    std::cout << "image spacing != adaptor spacing: "
              << myImage->GetSpacing() << " != "
              << myAdaptor->GetSpacing()
              << std::endl;
    return EXIT_FAILURE;
    }

  double dspacing[2] = {2.0, 3.0};
  myImage->SetSpacing(dspacing);
  if (myImage->GetSpacing() != myAdaptor->GetSpacing())
    {
    std::cout << "image spacing != adaptor spacing: "
              << myImage->GetSpacing() << " != "
              << myAdaptor->GetSpacing()
              << std::endl;
    return EXIT_FAILURE;
    }


  myImageType::SpacingType imageSpacing;
  imageSpacing.Fill(10.0);

  myImage->SetSpacing(imageSpacing);
  if (myImage->GetSpacing() != myAdaptor->GetSpacing())
    {
    std::cout << "image spacing != adaptor spacing: "
              << myImage->GetSpacing() << " != "
              << myAdaptor->GetSpacing()
              << std::endl;
    return EXIT_FAILURE;
    }

  myImageType::DirectionType imageDirection;
  imageDirection.SetIdentity();
  imageDirection[1][1]=10.0;

  myImage->SetDirection(imageDirection);
  if (myImage->GetDirection() != myAdaptor->GetDirection())
    {
    std::cout << "image direction != adaptor direction: "
              << myImage->GetDirection() << " != "
              << myAdaptor->GetDirection()
              << std::endl;
    return EXIT_FAILURE;
    }

  // Now test the set methods of the adaptor
  forigin[0] = 20.0;
  myAdaptor->SetOrigin(forigin);
  if (myImage->GetOrigin() != myAdaptor->GetOrigin())
    {
    std::cout << "image origin != adaptor origin: "
              << myImage->GetOrigin() << " != "
              << myAdaptor->GetOrigin()
              << std::endl;
    return EXIT_FAILURE;
    }

  dorigin[0] = 20.0;
  myAdaptor->SetOrigin(dorigin);
  if (myImage->GetOrigin() != myAdaptor->GetOrigin())
    {
    std::cout << "image origin != adaptor origin: "
              << myImage->GetOrigin() << " != "
              << myAdaptor->GetOrigin()
              << std::endl;
    return EXIT_FAILURE;
    }

  imageOrigin.Fill(100.0);

  myAdaptor->SetOrigin(imageOrigin);
  if (myImage->GetOrigin() != myAdaptor->GetOrigin())
    {
    std::cout << "image origin != adaptor origin: "
              << myImage->GetOrigin() << " != "
              << myAdaptor->GetOrigin()
              << std::endl;
    return EXIT_FAILURE;
    }

  fspacing[0] = 20.0;
  myAdaptor->SetSpacing(fspacing);
  if (myImage->GetSpacing() != myAdaptor->GetSpacing())
    {
    std::cout << "image spacing != adaptor spacing: "
              << myImage->GetSpacing() << " != "
              << myAdaptor->GetSpacing()
              << std::endl;
    return EXIT_FAILURE;
    }

  dspacing[0] = 20.0;
  myAdaptor->SetSpacing(dspacing);
  if (myImage->GetSpacing() != myAdaptor->GetSpacing())
    {
    std::cout << "image spacing != adaptor spacing: "
              << myImage->GetSpacing() << " != "
              << myAdaptor->GetSpacing()
              << std::endl;
    return EXIT_FAILURE;
    }

  imageSpacing.Fill(100.0);

  myAdaptor->SetSpacing(imageSpacing);
  if (myImage->GetSpacing() != myAdaptor->GetSpacing())
    {
    std::cout << "image spacing != adaptor spacing: "
              << myImage->GetSpacing() << " != "
              << myAdaptor->GetSpacing()
              << std::endl;
    return EXIT_FAILURE;
    }

  imageDirection[1][1]=100.0;

  myAdaptor->SetDirection(imageDirection);
  if (myImage->GetDirection() != myAdaptor->GetDirection())
    {
    std::cout << "image direction != adaptor direction: "
              << myImage->GetDirection() << " != "
              << myAdaptor->GetDirection()
              << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
