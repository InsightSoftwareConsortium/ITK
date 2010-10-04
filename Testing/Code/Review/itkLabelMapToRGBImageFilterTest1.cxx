#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"

#include "itkLabelImageToLabelMapFilter.h"
#include "itkLabelMapToRGBImageFilter.h"


int itkLabelMapToRGBImageFilterTest1(int argc, char * argv[])
{
  if( argc != 3 )
    {
    std::cerr << "usage: " << argv[0] << " input output" << std::endl;
    // std::cerr << "  : " << std::endl;
    exit(1);
    }

  const int dim = 2;

  typedef itk::Image< unsigned char, dim > IType;

  typedef itk::ImageFileReader< IType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::LabelImageToLabelMapFilter< IType > ConverterType;
  ConverterType::Pointer converter = ConverterType::New();
  converter->SetInput( reader->GetOutput() );

//  typedef itk::RGBPixel< unsigned char > RGBPixelType;
//  typedef itk::Image< RGBPixelType, dim > RGBImageType;

  typedef itk::LabelMapToRGBImageFilter< ConverterType::OutputImageType > ColorizerType;
  ColorizerType::Pointer colorizer = ColorizerType::New();
  colorizer->SetInput( converter->GetOutput() );

  itk::SimpleFilterWatcher watcher(colorizer, "filter");

  typedef itk::ImageFileWriter< ColorizerType::OutputImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( colorizer->GetOutput() );
  writer->SetFileName( argv[2] );
  writer->Update();
  return 0;
}
