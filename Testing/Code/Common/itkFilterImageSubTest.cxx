// need to check type of the pixel.
// subtraction should be performed differently up to the type
// eg. If result output image is unsigned type, negative value should be zero 

//---------------------------------
//
//  Test of itk::Image class
//
//---------------------------------

#include <itkImage.h>
#include <itkFilterImageSub.h>
#include <itkImageRegionSimpleIterator.h>
#include <itkImageRegionIterator.h>
#include <itkReadMetaImage.h>
#include <itkWriteMetaImage.h>

int main() 
{
  typedef itk::Image<unsigned short, 2> myMetaImageType1;
  typedef itk::Image<unsigned short, 2> myMetaImageType2;
  typedef itk::Image<short, 2> myMetaImageType3;

  // read meta image as an first input image
  typedef itk::ReadMetaImage<myMetaImageType1> myReadMetaImageType1; 
  myReadMetaImageType1::Pointer metaImage1 = myReadMetaImageType1::New();  
  metaImage1->SetFileName("input_filename1");
  metaImage1->Execute();
  
  // read meta image as an second input image
  typedef itk::ReadMetaImage<myMetaImageType2> myReadMetaImageType2; 
  myReadMetaImageType2::Pointer metaImage2 = myReadMetaImageType2::New();  
  metaImage2->SetFileName("input_filename2");
  metaImage2->Execute();
  
  myMetaImageType1::Pointer inputMetaImageA  = metaImage1->GetOutput(); 
  myMetaImageType2::Pointer inputMetaImageB  = metaImage2->GetOutput();
  
  typedef itk::FilterImageSub<myMetaImageType1,myMetaImageType2,myMetaImageType3> myFilterType;
  myFilterType::Pointer filter = myFilterType::New();

  filter->SetInput1( inputMetaImageA ); 
  filter->SetInput2( inputMetaImageB );
  filter->Execute();
  
  myMetaImageType3::Pointer outputImage = filter->GetOutput();
  
  // write meta image
  typedef itk::WriteMetaImage<myMetaImageType3> myWriteMetaImageType; 
  myWriteMetaImageType::Pointer metaImage3 = myWriteMetaImageType::New(); 
  metaImage3->SetInput(outputImage);   
  metaImage3->SetFileName("output_filename.mha");
  metaImage3->Execute();
  
  return 0;
}


