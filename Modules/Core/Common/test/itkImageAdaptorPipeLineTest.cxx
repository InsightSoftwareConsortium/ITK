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

//-------------------------------------------
//
//  Example of the use of Adaptors
//  to get access to the red component
//  of an RGBPixel image
//
//-------------------------------------------


#include "itkImageAdaptor.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkRedPixelAccessor.h"
#include "itkAddImageFilter.h"
#include "itkStdStreamStateSave.h"

int itkImageAdaptorPipeLineTest(int, char* [] )
{
// Save the format stream variables for std::cout
// They will be restored when coutState goes out of scope
// scope.
  itk::StdStreamStateSave coutState(std::cout);

  //-------------------------------------------------------------
  //                        Typedefs
  //-------------------------------------------------------------

  // Float Image typedefs
  typedef   float                                    myFloatPixelType;
  typedef   itk::Image<myFloatPixelType, 3>          myFloatImageType;

  typedef   myFloatImageType::SizeType               mySizeType;
  typedef   myFloatImageType::IndexType              myIndexType;
  typedef   myFloatImageType::RegionType             myRegionType;


  // RGBPixel Image typedefs
  typedef   itk::RGBPixel<myFloatPixelType>           myRGBPixelPixelType;
  typedef   itk::Image<myRGBPixelPixelType, 3>        myRGBPixelImageType;
  typedef   itk::RedPixelAccessor<myFloatPixelType>   myAccessorType;
  typedef   itk::ImageAdaptor<myRGBPixelImageType,
                              myAccessorType>         myAdaptorType;
  typedef itk::ImageRegionIteratorWithIndex<
                                   myFloatImageType > myFloatIteratorType;


  typedef itk::ImageRegionIteratorWithIndex<
                                     myRGBPixelImageType >   myRGBPixelIteratorType;


  typedef itk::AddImageFilter< myAdaptorType,
                               myFloatImageType,
                               myFloatImageType >       myFilterType;


  //-------------------------------------------------------------
  //                 Create and Allocate the image
  //-------------------------------------------------------------

  // Define their size, and start index
  mySizeType size;
  size[0] = 2;
  size[1] = 2;
  size[2] = 2;    // Small size, because we are printing it

  myIndexType start;
  start[0]=  0;
  start[1]=  0;
  start[2]=  0;

  myRegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  const float spacing[3] = { 1.0, 1.0, 1.0 };

  //-------------------------------------------------------------
  //                 Create and Initialize the RGBPixel image
  //-------------------------------------------------------------

  myRGBPixelImageType::Pointer   myRGBPixelImage = myRGBPixelImageType::New();

  myRGBPixelImage->SetLargestPossibleRegion( region );
  myRGBPixelImage->SetBufferedRegion( region );
  myRGBPixelImage->SetRequestedRegion( region );
  myRGBPixelImage->Allocate();
  myRGBPixelImage->SetSpacing( spacing );

  myRGBPixelIteratorType it(  myRGBPixelImage, myRGBPixelImage->GetRequestedRegion() );

  myRGBPixelPixelType initialRGBPixelValue;
  initialRGBPixelValue.SetRed( 10 );
  initialRGBPixelValue.SetBlue( 30 );
  initialRGBPixelValue.SetGreen( 20 );

  while( !it.IsAtEnd() )
  {
    it.Set( initialRGBPixelValue );
    ++it;
  }

  std::cout << "Initial RGBPixel Image Values : " << std::endl;
  it.GoToBegin();
  while( !it.IsAtEnd() )
  {
    myIndexType index = it.GetIndex();
    std::cout <<  "[";
    std::cout.width(3);
    std::cout << index[0] << ",";
    std::cout.width(3);
    std::cout << index[1] << ",";
    std::cout.width(3);
    std::cout << index[2] << "] =  ";
    std::cout.width(4);
    std::cout <<  it.Get().GetRed() << ",";
    std::cout.width(4);
    std::cout <<  it.Get().GetGreen() << ",";
    std::cout.width(4);
    std::cout <<  it.Get().GetBlue() << std::endl;
    ++it;
  }


  std::cout << "RGBPixel Image Initializaed" << std::endl;

  //-------------------------------------------------------------
  //                 Create and Initialize the Float image
  //-------------------------------------------------------------

  myFloatImageType::Pointer   myFloatImage = myFloatImageType::New();

  myFloatImage->SetLargestPossibleRegion( region );
  myFloatImage->SetBufferedRegion( region );
  myFloatImage->SetRequestedRegion( region );
  myFloatImage->Allocate();
  myFloatImage->SetSpacing( spacing );

  myFloatIteratorType itf(  myFloatImage, myFloatImage->GetRequestedRegion() );

  myFloatPixelType initialFloatValue = 5.0;

  while( !itf.IsAtEnd() )
  {
    itf.Set( initialFloatValue );
    ++itf;
  }

  std::cout << "Initial Float Image Values : " << std::endl;
  itf.GoToBegin();
  while( !itf.IsAtEnd() )
  {
    myIndexType index = itf.GetIndex();
    std::cout <<  "[";
    std::cout.width(3);
    std::cout << index[0] << ",";
    std::cout.width(3);
    std::cout << index[1] << ",";
    std::cout.width(3);
    std::cout << index[2] << "] =  ";
    std::cout.width(8);
    std::cout <<  itf.Get() << std::endl;
    ++itf;
  }

  std::cout << "Float Image Initializaed" << std::endl;


  //-------------------------------------------------------------
  //         Create the adaptor and connect the image
  //-------------------------------------------------------------

  myAdaptorType::Pointer myAdaptor = myAdaptorType::New();

  myAdaptor->SetImage( myRGBPixelImage );

  //-------------------------------------------------------------
  //         Create the filter and connect the inputs
  //-------------------------------------------------------------

  myFilterType::Pointer    filter   = myFilterType::New();

  filter->SetInput1( myAdaptor );
  filter->SetInput2( myFloatImage );


  //-------------------------------------------------------------
  //      Set the requested region of  the Output image
  //-------------------------------------------------------------

  myFloatImageType::Pointer myFloatOutputImage = filter->GetOutput();
  myFloatOutputImage->SetSpacing( spacing );

  std::cout << "Float Output Image Initializaed" << std::endl;

  //-------------------------------------------------------------
  //         Force the execution of the filter
  //-------------------------------------------------------------

  std::cout << "Calling filter Update" << std::endl;

  filter->Update();

  std::cout << "Filter Updated" << std::endl;

  //-------------------------------------------------------------
  //         Force the execution of the filter
  //-------------------------------------------------------------

  myFloatOutputImage = filter->GetOutput();

  myFloatIteratorType ito(  myFloatOutputImage, myFloatOutputImage->GetRequestedRegion() );


  std::cout << std::endl;
  std::cout << "Filter Output :" << std::endl;
  while( !ito.IsAtEnd() )
  {
    myIndexType index = ito.GetIndex();
    std::cout <<  "[";
    std::cout.width(3);
    std::cout << index[0] << ",";
    std::cout.width(3);
    std::cout << index[1] << ",";
    std::cout.width(3);
    std::cout << index[2] << "] =  ";
    std::cout.width(8);
    std::cout <<  ito.Get() << std::endl;
    ++ito;
  }

  //-------------------------------------------------------------
  //         Test Graft() method
  //-------------------------------------------------------------
  myAdaptorType::Pointer myGraftedAdaptor = myAdaptorType::New();
  myGraftedAdaptor->Graft(myAdaptor);
  if(myGraftedAdaptor->GetPixelContainer() != myAdaptor->GetPixelContainer() )
  {
    std::cerr << "Graft test failed." << std::endl;
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;

}
