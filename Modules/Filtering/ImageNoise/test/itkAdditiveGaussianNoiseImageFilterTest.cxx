#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"

#include "itkAdditiveGaussianNoiseImageFilter.h"
#include "itkTestingMacros.h"

int main(int argc, char * argv[])
{

  if( argc < 3 )
    {
    std::cerr << "usage: " << argv[0] << " intput output std_dev mean" << std::endl;
    std::cerr << " input: the input image" << std::endl;
    std::cerr << " output: the output image" << std::endl;
    // std::cerr << "  : " << std::endl;
    exit(1);
    }

  const int dim = 2;
  
  typedef unsigned char PType;
  typedef itk::Image< PType, dim > IType;

  typedef itk::ImageFileReader< IType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::AdditiveGaussianNoiseImageFilter< IType, IType > FilterType;
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput( reader->GetOutput() );
  TEST_SET_GET_VALUE( 1.0, filter->GetStandardDeviation() );
  if( argc >= 4 )
    {
    filter->SetStandardDeviation( atof(argv[3]) );
    TEST_SET_GET_VALUE( atof(argv[3]), filter->GetStandardDeviation() );
    }
  TEST_SET_GET_VALUE( 0.0, filter->GetMean() );
  if( argc >= 5 )
    {
    filter->SetMean( atof(argv[4]) );
    TEST_SET_GET_VALUE( atof(argv[4]), filter->GetMean() );
    }

  itk::SimpleFilterWatcher watcher(filter, "filter");

  typedef itk::ImageFileWriter< IType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( argv[2] );
  writer->Update();

  return 0;
}

