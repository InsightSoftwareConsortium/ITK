#include <iomanip>
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkCommand.h"
#include "itkSimpleFilterWatcher.h"

#include "itkBinaryThresholdImageFilter.h"
#include "itkMorphologicalDistanceTransformImageFilter.h"
#include "itkTimeProbe.h"
#include "itkMultiThreader.h"


int
itkParaDTTest(int argc, char * argv[])
{

  // int iterations = 1;

  if (argc != 4)
  {
    std::cerr << "Usage: " << argv[0] << " inputimage threshold outsideval outim1" << std::endl;
    return (EXIT_FAILURE);
  }

  itk::MultiThreader::SetGlobalMaximumNumberOfThreads(1);
  const int dim = 2;

  typedef unsigned char          PType;
  typedef itk::Image<PType, dim> IType;
  typedef itk::Image<float, dim> FType;

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

  IType::Pointer input = reader->GetOutput();

  // threshold the input to create a mask
  typedef itk::BinaryThresholdImageFilter<IType, IType> ThreshType;
  ThreshType::Pointer                                   thresh = ThreshType::New();
  thresh->SetInput(input);

  thresh->SetUpperThreshold(atoi(argv[2]));
  thresh->SetInsideValue(0);
  thresh->SetOutsideValue(255);

  // now to apply the distance transform
  typedef itk::MorphologicalDistanceTransformImageFilter<IType, FType> FilterType;

  FilterType::Pointer filter = FilterType::New();

  filter->SetInput(thresh->GetOutput());
  filter->SetOutsideValue(atoi(argv[3]));
  try
  {
    filter->Update();
  }
  catch (itk::ExceptionObject & excp)
  {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  typedef itk::ImageFileWriter<FType> WriterType;
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
