//-------------------------------------------
//
//  Example of the use of Adaptors
//  to get access to the red component
//  of an RGB image
//
//-------------------------------------------


#include <itkImage.h>
#include <itkIndex.h>
#include <itkImageRegionSimpleIterator.h>
#include <itkImageAdaptor.h>
#include <itkDataAccessorRGBtoRed.h>
#include <itkFilterImageAdd.h>


int main()
{

  //-------------------------------------------------------------
  //                        Typedefs
  //-------------------------------------------------------------

  // Float Image typedefs
  typedef   float                                    myFloatPixelType;
  typedef   itk::Image<myFloatPixelType, 3>          myFloatImageType;

  typedef   myFloatImageType::SizeType               mySizeType;
  typedef   myFloatImageType::IndexType              myIndexType;
  typedef   myFloatImageType::RegionType             myRegionType;


  // RGB Image typedefs
  typedef   itk::RGB<myFloatPixelType>               myRGBPixelType;
  typedef   itk::Image<myRGBPixelType, 3>            myRGBImageType;



  typedef   itk::DataAccessorRGBtoRed<myFloatPixelType> myAccessorType;

  typedef   itk::ImageAdaptor<myRGBImageType,
                              myAccessorType>        myAdaptorType;

  typedef itk::ImageRegionSimpleIterator< 
                                   myFloatImageType > myFloatIteratorType;


  typedef itk::ImageRegionSimpleIterator< 
                                     myRGBImageType >   myRGBIteratorType;


  typedef itk::FilterImageAdd< myAdaptorType, 
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

  float spacing[3];
  spacing[0] = 1.0;
  spacing[1] = 1.0;
  spacing[2] = 1.0;
  
  //-------------------------------------------------------------
  //                 Create and Initialize the RGB image
  //-------------------------------------------------------------

  myRGBImageType::Pointer   myRGBImage = myRGBImageType::New();

  myRGBImage->SetLargestPossibleRegion( region );
  myRGBImage->SetBufferedRegion( region );
  myRGBImage->SetRequestedRegion( region );
  myRGBImage->Allocate();
  myRGBImage->SetSpacing( spacing );

  myRGBIteratorType it(  myRGBImage, myRGBImage->GetRequestedRegion() );

  myRGBPixelType initialRGBValue;
  initialRGBValue.SetRed( 10 );
  initialRGBValue.SetBlue( 30 );
  initialRGBValue.SetGreen( 20 );


  it.Begin();
  while( !it.IsAtEnd() ) 
  {
    it.Set( initialRGBValue );  
    ++it;
  }

  std::cout << "Initial RGB Image Values : " << std::endl;
  it.Begin();
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


  std::cout << "RGB Image Initializaed" << std::endl;
 
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

  itf.Begin();
  while( !itf.IsAtEnd() ) 
  {
    itf.Set( initialFloatValue );  
    ++itf;
  }

  std::cout << "Initial Float Image Values : " << std::endl;
  itf.Begin();
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

  myAdaptor->SetImage( myRGBImage );




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
  ito.Begin();
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

  return 0;

}



