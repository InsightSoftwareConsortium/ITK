#include <iomanip>
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkCommand.h"
#include "itkSimpleFilterWatcher.h"

#include "itkTimeProbe.h"
#include "itkMultiThreader.h"
#include <itkBinaryThresholdImageFilter.h>
#include "itkBinaryErodeParaImageFilter.h"

int
itkBinaryErodeParaTest(int argc, char * argv[])
{
  if (argc != 5)
  {
    std::cerr << "Usage: " << argv[0] << " inputimage threshold size outim " << std::endl;
    return (EXIT_FAILURE);
  }

  itk::MultiThreader::SetGlobalMaximumNumberOfThreads(1);
  const int dim = 2;

  typedef unsigned char          PType;
  typedef itk::Image<PType, dim> IType;

  typedef itk::ImageFileReader<IType> ReaderType;
  ReaderType::Pointer                 reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  try
  {
    reader->Update();
  }
  catch (itk::ExceptionObject & excp)
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }
  // threshold the input to create a mask
  typedef itk::BinaryThresholdImageFilter<IType, IType> ThreshType;
  ThreshType::Pointer                                   thresh = ThreshType::New();
  thresh->SetInput(reader->GetOutput());

  thresh->SetUpperThreshold(atoi(argv[2]));
  thresh->SetInsideValue(0);
  thresh->SetOutsideValue(1);
  // now to apply the erosion
  typedef itk::BinaryErodeParaImageFilter<IType, IType> FilterType;

  FilterType::Pointer filter = FilterType::New();

  filter->SetInput(thresh->GetOutput());
  filter->SetUseImageSpacing(true);
  filter->SetRadius(atof(argv[3]));

  typedef itk::ImageFileWriter<IType> WriterType;
  WriterType::Pointer                 writer = WriterType::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[4]);
  try
  {
    writer->Update();
  }
  catch (itk::ExceptionObject & excp)
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
