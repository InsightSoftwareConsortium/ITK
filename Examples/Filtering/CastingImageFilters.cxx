#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkCastImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"


int main( int argc, char ** argv )
{

  typedef unsigned char                      InputPixelType;
  typedef float                              OutputPixelType;
  typedef itk::Image< InputPixelType,  3 >   InputImageType;
  typedef itk::Image< OutputPixelType, 3 >   OutputImageType;

  typedef itk::ImageFileReader< InputImageType >  ReaderType;

  typedef itk::CastImageFilter<
               InputImageType, OutputImageType >  CastFilterType;

  typedef itk::RescaleIntensityImageFilter<
               InputImageType, OutputImageType >  RescaleFilterType;

  ReaderType::Pointer reader = ReaderType::New();

  CastFilterType::Pointer castFilter = CastFilterType::New();
  RescaleFilterType::Pointer rescaleFilter = RescaleFilterType::New();

  reader->SetFileName( argv[1] );
  
  castFilter->SetInput( reader->GetOutput() );
  rescaleFilter->SetInput( reader->GetOutput() );

  castFilter->Update();
  rescaleFilter->Update();

  return 0;

}

