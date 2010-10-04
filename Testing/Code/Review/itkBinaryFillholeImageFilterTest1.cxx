#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkCommand.h"
#include "itkSimpleFilterWatcher.h"

#include "itkLabelObject.h"
#include "itkLabelMap.h"
#include "itkBinaryFillholeImageFilter.h"


int itkBinaryFillholeImageFilterTest1(int argc, char * argv[])
{

  if( argc != 5 )
    {
    std::cerr << "usage: " << argv[0] << " input output conn fg" << std::endl;
    // std::cerr << "  : " << std::endl;
    exit(1);
    }

  const int dim = 2;

  typedef itk::Image< unsigned char, dim > IType;

  typedef itk::ImageFileReader< IType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  reader->Update();

 typedef itk::BinaryFillholeImageFilter< IType > I2LType;
  I2LType::Pointer reconstruction = I2LType::New();
  reconstruction->SetInput( reader->GetOutput() );
  reconstruction->SetFullyConnected( atoi(argv[3]) );
  reconstruction->SetForegroundValue( atoi(argv[4]) );
//   reconstruction->SetBackgroundValue( atoi(argv[5]) );
  itk::SimpleFilterWatcher watcher(reconstruction, "filter");

  typedef itk::ImageFileWriter< IType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( reconstruction->GetOutput() );
  writer->SetFileName( argv[2] );
  writer->Update();
  return 0;
}
