#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkBinaryThresholdImageFilter.h"


int main( int argc, char ** argv )
{

  typedef unsigned char                      InputPixelType;
  typedef unsigned char                      OutputPixelType;
  typedef itk::Image< InputPixelType,  3 >   InputImageType;
  typedef itk::Image< OutputPixelType, 3 >   OutputImageType;

  typedef itk::ImageFileReader< InputImageType >  ReaderType;

  typedef itk::BinaryThresholdImageFilter<
               InputImageType, OutputImageType >  FilterType;

  ReaderType::Pointer reader = ReaderType::New();
  FilterType::Pointer filter = FilterType::New();

  reader->SetFileName( argv[1] );
  filter->SetInput( reader->GetOutput() );

  filter->SetOutsideValue( 0 );
  filter->SetInsideValue( 255 );

  filter->SetLowerThreshold(  85 );
  filter->SetUpperThreshold( 194 );

  filter->Update();

  return 0;

}

