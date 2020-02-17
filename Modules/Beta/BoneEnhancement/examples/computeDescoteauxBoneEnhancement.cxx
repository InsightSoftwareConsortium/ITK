#include <iostream>
#include "itkArray.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkMultiScaleHessianEnhancementImageFilter.h"
#include "itkDescoteauxEigenToMeasureImageFilter.h"
#include "itkDescoteauxEigenToMeasureParameterEstimationFilter.h"
#include "itkCommand.h"

class MyCommand : public itk::Command
{
public:
  itkNewMacro(MyCommand);

public:
  void
  Execute(itk::Object * caller, const itk::EventObject & event) override
  {
    Execute((const itk::Object *)caller, event);
  }

  void
  Execute(const itk::Object * caller, const itk::EventObject & event) override
  {
    if (!itk::ProgressEvent().CheckEvent(&event))
    {
      return;
    }
    const auto * processObject = dynamic_cast<const itk::ProcessObject *>(caller);
    if (!processObject)
    {
      return;
    }
    float progress = processObject->GetProgress() * 100;
    if (int(progress) > int(m_PastProgress))
    {
      m_PastProgress = progress;
      // \r is a cheap trick to reset the line
      // The spaces are a dirty trick since the output buffer is not reset everytime
      std::cout << "\rProgress: " << m_PastProgress << "%"
                << "                                " << std::flush;
      if (m_PastProgress >= 99)
      {
        std::cout << std::endl;
      }
    }
  }

private:
  float m_PastProgress = -1;
};

int
main(int argc, char * argv[])
{
  if (argc < 6)
  {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0];
    std::cerr << " <InputFileName> <OutputMeasure> ";
    std::cerr << " <SetEnhanceBrightObjects[0,1]> ";
    std::cerr << " <NumberOfSigma> <Sigma1> [<Sigma2> <Sigma3>] ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  /* Read input Parameters */
  std::string inputFileName = argv[1];
  std::string outputMeasureFileName = argv[2];

  int                enhanceBrightObjects = std::stoi(argv[3]);
  int                numberOfSigma = std::stoi(argv[4]);
  double             thisSigma;
  itk::Array<double> sigmaArray;
  sigmaArray.SetSize(numberOfSigma);
  for (int i = 0; i < numberOfSigma; ++i)
  {
    thisSigma = std::stod(argv[5 + i]);
    sigmaArray.SetElement(i, thisSigma);
  }

  std::cout << "Read in the following parameters:" << std::endl;
  std::cout << "  InputFilePath:               " << inputFileName << std::endl;
  std::cout << "  OutputMeasure:               " << outputMeasureFileName << std::endl;
  if (enhanceBrightObjects == 1)
  {
    std::cout << "  SetEnhanceBrightObjects:     "
              << "Enhancing bright objects" << std::endl;
  }
  else
  {
    std::cout << "  SetEnhanceBrightObjects:     "
              << "Enhancing dark objects" << std::endl;
  }
  std::cout << "  NumberOfSigma:               " << numberOfSigma << std::endl;
  std::cout << "  Sigmas:                      " << sigmaArray << std::endl;
  std::cout << std::endl;

  /* Setup Types */
  constexpr unsigned int ImageDimension = 3;
  using InputPixelType = short;
  using InputImageType = itk::Image<InputPixelType, ImageDimension>;
  using OutputPixelType = float;
  using OutputImageType = itk::Image<OutputPixelType, ImageDimension>;

  using ReaderType = itk::ImageFileReader<InputImageType>;
  using MeasureWriterType = itk::ImageFileWriter<OutputImageType>;
  using MultiScaleHessianFilterType = itk::MultiScaleHessianEnhancementImageFilter<InputImageType, OutputImageType>;
  using DescoteauxEigenToMeasureImageFilterType =
    itk::DescoteauxEigenToMeasureImageFilter<MultiScaleHessianFilterType::EigenValueImageType, OutputImageType>;
  using DescoteauxEigenToMeasureParameterEstimationFilterType =
    itk::DescoteauxEigenToMeasureParameterEstimationFilter<MultiScaleHessianFilterType::EigenValueImageType>;

  /* Do preprocessing */
  std::cout << "Reading in " << inputFileName << std::endl;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(inputFileName);
  reader->Update();

  /* Multiscale measure */
  MultiScaleHessianFilterType::Pointer                           multiScaleFilter = MultiScaleHessianFilterType::New();
  DescoteauxEigenToMeasureParameterEstimationFilterType::Pointer estimationFilter =
    DescoteauxEigenToMeasureParameterEstimationFilterType::New();
  DescoteauxEigenToMeasureImageFilterType::Pointer descoFilter = DescoteauxEigenToMeasureImageFilterType::New();
  multiScaleFilter->SetInput(reader->GetOutput());
  multiScaleFilter->SetEigenToMeasureImageFilter(descoFilter);
  multiScaleFilter->SetEigenToMeasureParameterEstimationFilter(estimationFilter);
  multiScaleFilter->SetSigmaArray(sigmaArray);

  std::cout << "Running multiScaleFilter..." << std::endl;
  MyCommand::Pointer myCommand = MyCommand::New();
  multiScaleFilter->AddObserver(itk::ProgressEvent(), myCommand);
  multiScaleFilter->Update();

  MeasureWriterType::Pointer measureWriter = MeasureWriterType::New();
  measureWriter->SetInput(multiScaleFilter->GetOutput());
  measureWriter->SetFileName(outputMeasureFileName);

  std::cout << "Writing results to " << outputMeasureFileName << std::endl;
  measureWriter->Write();

  return EXIT_SUCCESS;
}
