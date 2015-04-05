#include <iomanip>
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkCommand.h"
#include "itkSimpleFilterWatcher.h"

#include "plotutils.h"
#include "ioutils.h"

#include "itkBinaryThresholdImageFilter.h"
#include "itkMorphologicalDistanceTransformImageFilter.h"
#include "itkTimeProbe.h"
#include "itkMultiThreader.h"


int
main(int argc, char * argv[])
{

  int iterations = 1;

  if (argc != 6)
  {
    std::cerr << "Usage: " << argv[0] << " inputimage threshold outsideval outim1 outim2" << std::endl;
    return (EXIT_FAILURE);
  }

  iterations = atoi(argv[1]);

  itk::MultiThreader::SetGlobalMaximumNumberOfThreads(1);
  const int dim = 2;

  typedef unsigned char          PType;
  typedef itk::Image<PType, dim> IType;
  typedef itk::Image<float, dim> FType;

  IType::Pointer input = readIm<IType>(argv[1]);

  // threshold the input to create a mask
  typedef itk::BinaryThresholdImageFilter<IType, IType> ThreshType;
  ThreshType::Pointer                                   thresh = ThreshType::New();
  thresh->SetInput(input);

  thresh->SetUpperThreshold(atoi(argv[2]));
  thresh->SetInsideValue(0);
  thresh->SetOutsideValue(255);
  writeIm<IType>(thresh->GetOutput(), argv[4]);
  // now to apply the distance transform
  typedef itk::MorphologicalDistanceTransformImageFilter<IType, FType> FilterType;

  FilterType::Pointer filter = FilterType::New();

  filter->SetInput(thresh->GetOutput());
  filter->SetOutsideValue(atoi(argv[3]));
  filter->Update();

  writeIm<FType>(filter->GetOutput(), argv[5]);
  return EXIT_SUCCESS;
}
