#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkCastImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkShiftScaleImageFilter.h"
#include "itkNormalizeImageFilter.h"


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

  typedef itk::ShiftScaleImageFilter<
               InputImageType, OutputImageType >  ShiftScaleFilterType;

  typedef itk::NormalizeImageFilter<
               InputImageType, OutputImageType >  NormalizeFilterType;

  ReaderType::Pointer reader = ReaderType::New();

  CastFilterType::Pointer castFilter = CastFilterType::New();
  RescaleFilterType::Pointer rescaleFilter = RescaleFilterType::New();
  ShiftScaleFilterType::Pointer shiftFilter = ShiftScaleFilterType::New();
  NormalizeFilterType::Pointer normzalizeFilter = NormalizeFilterType::New();

  reader->SetFileName( argv[1] );
  
  castFilter->SetInput( reader->GetOutput() );
  rescaleFilter->SetInput( reader->GetOutput() );
  shiftFilter->SetInput( reader->GetOutput() );
  normzalizeFilter->SetInput( reader->GetOutput() );

  rescaleFilter->SetOutputMinimum(  10 );
  rescaleFilter->SetOutputMaximum( 250 );

  shiftFilter->SetShift( 25 );
  shiftFilter->SetScale( 1.2 );

  castFilter->Update();
  rescaleFilter->Update();
  shiftFilter->Update();
  normzalizeFilter->Update();

  return 0;

}

