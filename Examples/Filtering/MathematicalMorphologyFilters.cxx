#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkBinaryErodeImageFilter.h"
#include "itkBinaryDilateImageFilter.h"
#include "itkBinaryBallStructuringElement.h" 

int main( int argc, char ** argv )
{

  const unsigned int Dimension = 3;
  
  typedef unsigned char   InputPixelType;
  typedef unsigned char   OutputPixelType;

  typedef itk::Image< InputPixelType,  Dimension >   InputImageType;
  typedef itk::Image< OutputPixelType, Dimension >   OutputImageType;

  typedef itk::ImageFileReader< InputImageType >  ReaderType;

  typedef itk::BinaryBallStructuringElement< 
                      InputImageType::PixelType,
                      Dimension>                  StructuringElementType;

  typedef itk::BinaryErodeImageFilter<
                            InputImageType, 
                            OutputImageType,
                            StructuringElementType >  ErodeFilterType;

  typedef itk::BinaryDilateImageFilter<
                            InputImageType, 
                            OutputImageType, 
                            StructuringElementType >  DilateFilterType;

  ReaderType::Pointer reader = ReaderType::New();

  ErodeFilterType::Pointer  binaryErode  = ErodeFilterType::New();
  DilateFilterType::Pointer binaryDilate = DilateFilterType::New();

  StructuringElementType structuringElement;

  structuringElement.SetRadius( 2 );  // 5x5x5 structuring element

  binaryErode->SetKernel( structuringElement );
  binaryDilate->SetKernel( structuringElement );

  reader->SetFileName( argv[1] );
  
  // Equivalent of the Openning operator
  binaryErode->SetInput( reader->GetOutput() );
  binaryDilate->SetInput( binaryErode->GetOutput() );

  binaryErode->SetErodeValue( 128 );
  binaryDilate->SetDilateValue( 128 );

  binaryDilate->Update();

  return 0;

}

