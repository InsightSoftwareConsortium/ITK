#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkMedianImageFilter.h"


int main( int argc, char ** argv )
{

  typedef float                              InputPixelType;
  typedef float                              OutputPixelType;
  typedef itk::Image< InputPixelType,  3 >   InputImageType;
  typedef itk::Image< OutputPixelType, 3 >   OutputImageType;

  typedef itk::ImageFileReader< InputImageType >  ReaderType;

  typedef itk::MedianImageFilter<
               InputImageType, OutputImageType >  FilterType;

  ReaderType::Pointer reader = ReaderType::New();
  FilterType::Pointer filter = FilterType::New();

  reader->SetFileName( argv[1] );
  filter->SetInput( reader->GetOutput() );

  InputImageType::SizeType indexRadius;
  
  indexRadius[0] = 2; // radius along x
  indexRadius[1] = 2; // radius along y
  indexRadius[2] = 2; // radius along z

  filter->SetRadius( indexRadius );

  filter->Update();

  return 0;

}

