//-----------------------------------------------
//
//  This is a Test of itk::FilterImageAdd class
//
//-----------------------------------------------


#include <itkImage.h>
#include <itkFilterImageAdd.h>
#include <itkImageRegionSimpleIterator.h>


int main() 
{

  // Define the dimension of the images
  const unsigned int myDimension = 3;

  // Declare the types of the images
  typedef itk::Image<float, myDimension>  myImageType1;
  typedef itk::Image<float, myDimension>  myImageType2;
  typedef itk::Image<float, myDimension>  myImageType3;

  // Declare the type of the index to access images
  typedef itk::Index<myDimension>         myIndexType;

  // Declare the type of the size 
  typedef itk::Size<myDimension>          mySizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion<myDimension>        myRegionType;

  // Create two images
  myImageType1::Pointer inputImageA  = myImageType1::New();
  myImageType2::Pointer inputImageB  = myImageType2::New();
  
  // Define their size, and start index
  mySizeType size;
  size[0] = 2;
  size[1] = 2;
  size[2] = 2;

  myIndexType start;
  start[0] = 0;
  start[1] = 0;
  start[2] = 0;

  myRegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  // Initialize Image A
  inputImageA->SetLargestPossibleRegion( region );
  inputImageA->SetBufferedRegion( region );
  inputImageA->Allocate();

  // Initialize Image B
  inputImageB->SetLargestPossibleRegion( region );
  inputImageB->SetBufferedRegion( region );
  inputImageB->Allocate();


  // Declare Iterator types apropriated for each image 
  typedef itk::ImageRegionSimpleIterator<myImageType1>  myIteratorType1;
  typedef itk::ImageRegionSimpleIterator<myImageType2>  myIteratorType2;
  typedef itk::ImageRegionSimpleIterator<myImageType3>  myIteratorType3;

  // Create one iterator for Image A (this is a light object)
  myIteratorType1 it1( inputImageA, inputImageA->GetBufferedRegion() );

  // Initialize the content of Image A
  it1.Begin();
  while( !it1.IsAtEnd() ) 
  {
    *it1 = 2.0;
    ++it1;
  }

  // Create one iterator for Image B (this is a light object)
  myIteratorType2 it2( inputImageB, inputImageB->GetBufferedRegion() );

  // Initialize the content of Image B
  it2.Begin();
  while( !it2.IsAtEnd() ) 
  {
    *it2 = 3.0;
    ++it2;
  }

  // Declare the type for the ADD filter
  typedef itk::FilterImageAdd<
                                myImageType1,
                                myImageType2,
                                myImageType3  >       myFilterType;
            

  // Create an ADD Filter                                
  myFilterType::Pointer filter = myFilterType::New();


  // Connect the input images
  filter->SetInput1( inputImageA ); 
  filter->SetInput2( inputImageB );

  // Get the Smart Pointer to the Filter Output 
  myImageType3::Pointer outputImage = filter->GetOutput();
  
  outputImage->SetLargestPossibleRegion(
      inputImageA->GetLargestPossibleRegion() );
  

  outputImage->SetBufferedRegion(  inputImageA->GetBufferedRegion() );
  outputImage->SetRequestedRegion( outputImage->GetLargestPossibleRegion() );
  outputImage->Allocate();
  

  // Execute the filter
  filter->Update();

  // Create an iterator for going through the image output
  myIteratorType3 it3(outputImage, outputImage->GetBufferedRegion());
  
  //  Print the content of the result image
  it3.Begin();
  while( !it3.IsAtEnd() ) 
  {
    std::cout << *it3 << std::endl;
    ++it3;
  }


  // All objects should be automatically destroyed at this point
  return 0;

}




