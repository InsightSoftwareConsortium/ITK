#include "itkImage.h"
#include "itkImageFileReader.h"


int main( int argc, char ** argv )
{

  typedef unsigned char                      PixelType;
  typedef itk::Image< PixelType, 3 >         ImageType;
  typedef itk::ImageFileReader< ImageType >  ReaderType;

  ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName( argv[1] );

  reader->Update();

  ImageType::Pointer image = reader->GetOutput();

  return 0;

}

