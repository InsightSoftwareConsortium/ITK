#include <iomanip>
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkCommand.h"
#include "itkSimpleFilterWatcher.h"
#include "itkChangeInformationImageFilter.h"
#include "itkSignedMaurerDistanceMapImageFilter.h"

// #include "plotutils.h"
#include "ioutils.h"

#include "itkBinaryThresholdImageFilter.h"
#include "itkMorphologicalSignedDistanceTransformImageFilter.h"

#include "itkSignedDanielssonDistanceMapImageFilter.h"

#include "itkTimeProbe.h"
#include "itkMultiThreader.h"


int
main(int argc, char * argv[])
{

  int iterations = 1;

  if (argc != 8)
  {
    std::cerr << "Usage: " << argv[0] << " inputimage threshold outsideval outim1 outim2 outim3" << std::endl;
    return (EXIT_FAILURE);
  }

  iterations = atoi(argv[1]);

  itk::MultiThreader::SetGlobalMaximumNumberOfThreads(1);
  const int dim = 2;

  typedef unsigned char          PType;
  typedef itk::Image<PType, dim> IType;
  typedef itk::Image<float, dim> FType;

  IType::Pointer inputOrig = readIm<IType>(argv[1]);

  typedef itk::ChangeInformationImageFilter<IType> ChangeType;
  ChangeType::Pointer                              changer = ChangeType::New();
  changer->SetInput(inputOrig);
  ChangeType::SpacingType newspacing;

  newspacing[0] = 0.5;
  newspacing[1] = 0.25;


  changer->SetOutputSpacing(newspacing);
  changer->ChangeSpacingOn();

  IType::Pointer input = changer->GetOutput();

  // threshold the input to create a mask
  typedef itk::BinaryThresholdImageFilter<IType, IType> ThreshType;
  ThreshType::Pointer                                   thresh = ThreshType::New();
  thresh->SetInput(input);

  thresh->SetUpperThreshold(atoi(argv[2]));
  thresh->SetInsideValue(0);
  thresh->SetOutsideValue(255);
  writeIm<IType>(thresh->GetOutput(), argv[4]);
  // now to apply the signed distance transform
  typedef itk::MorphologicalSignedDistanceTransformImageFilter<IType, FType> FilterType;

  FilterType::Pointer filter = FilterType::New();

  filter->SetInput(thresh->GetOutput());
  //  filter->SetOutsideValue(atoi(argv[3]));
  filter->SetUseImageSpacing(true);

  itk::TimeProbe ParabolicT, MaurerT, DanielssonT;

  std::cout << "Parabolic  Maurer  Danielsson" << std::endl;
  const unsigned TESTS = 10;
#if 1
  for (unsigned repeats = 0; repeats < TESTS; repeats++)
  {
    ParabolicT.Start();
    filter->Modified();
    filter->Update();
    ParabolicT.Stop();
  }

  writeIm<FType>(filter->GetOutput(), argv[5]);
#endif
  typedef itk::SignedMaurerDistanceMapImageFilter<IType, FType> MaurerType;
  MaurerType::Pointer                                           maurer = MaurerType::New();
  maurer->SetInput(thresh->GetOutput());
  maurer->SetUseImageSpacing(true);
  maurer->SetSquaredDistance(false);

  for (unsigned repeats = 0; repeats < TESTS; repeats++)
  {
    MaurerT.Start();
    maurer->Modified();
    maurer->Update();
    MaurerT.Stop();
  }
  writeIm<FType>(maurer->GetOutput(), argv[6]);

  typedef itk::SignedDanielssonDistanceMapImageFilter<IType, FType> DanielssonType;
  DanielssonType::Pointer                                           daniel = DanielssonType::New();
  daniel->SetInput(thresh->GetOutput());
  daniel->SetUseImageSpacing(true);

  for (unsigned repeats = 0; repeats < TESTS; repeats++)
  {
    DanielssonT.Start();
    daniel->Modified();
    daniel->Update();
    DanielssonT.Stop();
  }
  writeIm<FType>(daniel->GetDistanceMap(), argv[7]);


  std::cout << std::setprecision(3) << ParabolicT.GetMean() << "\t" << MaurerT.GetMean() << "\t"
            << DanielssonT.GetMean() << std::endl;


  return EXIT_SUCCESS;
}
