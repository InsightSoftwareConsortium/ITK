#include <iostream>
#include "itkArray.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkKrcahEigenToScalarPreprocessingImageToImageFilter.h"
#include "itkMultiScaleHessianEnhancementImageFilter.h"
#include "itkKrcahEigenToScalarImageFilter.h"
#include "itkCommand.h"

class MyCommand : public itk::Command
{
  public:
    itkNewMacro( MyCommand );
  public:
    void Execute(itk::Object *caller, const itk::EventObject & event) override {
      Execute( (const itk::Object *)caller, event);
    }
 
    void Execute(const itk::Object * caller, const itk::EventObject & event) override {
      if( ! itk::ProgressEvent().CheckEvent( &event ) ) {
        return;
      }
      const itk::ProcessObject * processObject = dynamic_cast< const itk::ProcessObject * >( caller );
      if( ! processObject ) {
        return;
      }
      float progress = processObject->GetProgress() * 100;
      if (int(progress) > int(m_PastProgress)) {
        m_PastProgress = progress;
        // \r is a cheap trick to reset the line
        // The spaces are a dirty trick since the output buffer is not reset everytime
        std::cout << "\rProgress: " << m_PastProgress << "%" << "                                " << std::flush;
        if (m_PastProgress >= 99) {
          std::cout << std::endl;
        }
      }
    }
  private:
    float m_PastProgress = -1;
};

int main(int argc, char * argv[])
{
  if( argc < 8 )
  {
    std::cerr << "Usage: "<< std::endl;
    std::cerr << argv[0];
    std::cerr << " <InputFileName> <OutputPreprocessed> <OutputMeasure> ";
    std::cerr << " <SetEnhanceBrightObjects[0,1]> ";
    std::cerr << " <UseImplementationParameters[0,1]> ";
    std::cerr << " <NumberOfSigma> <Sigma1> [<Sigma2> <Sigma3>] ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  /* Read input Parameters */
  std::string inputFileName = argv[1];
  std::string outputPreprocessedFileName = argv[2];
  std::string outputMeasureFileName = argv[3];

  int enhanceBrightObjects = atoi(argv[4]);
  int parameterSetToImplement = atoi(argv[5]);
  int numberOfSigma = atoi(argv[6]);
  double thisSigma;
  itk::Array< double > sigmaArray;
  sigmaArray.SetSize(numberOfSigma);
  for (unsigned int i = 0; i < numberOfSigma; ++i) {
    thisSigma = atof(argv[7+i]);
    sigmaArray.SetElement(i, thisSigma);
  }

  std::cout << "Read in the following parameters:" << std::endl;
  std::cout << "  InputFilePath:               " << inputFileName << std::endl;
  std::cout << "  OutputPreprocessed:          " << outputPreprocessedFileName << std::endl;
  std::cout << "  OutputMeasure:               " << outputMeasureFileName << std::endl;
  if (enhanceBrightObjects == 1) {
    std::cout << "  SetEnhanceBrightObjects:     " << "Enhancing bright objects" << std::endl;
  } else {
    std::cout << "  SetEnhanceBrightObjects:     " << "Enhancing dark objects" << std::endl;
  }
  if (parameterSetToImplement == 1) {
    std::cout << "  UseImplementationParameters: " << "Using implementation parameters" << std::endl;
  } else {
    std::cout << "  UseImplementationParameters: " << "Using journal article parameters" << std::endl;
  }
  std::cout << "  NumberOfSigma:               " << numberOfSigma << std::endl;
  std::cout << "  Sigmas:                      " << sigmaArray << std::endl;
  std::cout << std::endl;

  /* Setup Types */
  constexpr unsigned int ImageDimension = 3;
  typedef short                                       InputPixelType;
  typedef itk::Image<InputPixelType, ImageDimension>  InputImageType;
  typedef float                                       OutputPixelType;
  typedef itk::Image<OutputPixelType, ImageDimension> OutputImageType;

  typedef itk::ImageFileReader< InputImageType >      ReaderType;
  typedef itk::ImageFileWriter< InputImageType >      PreprocessedWriterType;
  typedef itk::ImageFileWriter< OutputImageType >     MeasureWriterType;
  typedef itk::KrcahEigenToScalarPreprocessingImageToImageFilter< InputImageType >
                                                      PreprocessFilterType;
  typedef itk::MultiScaleHessianEnhancementImageFilter< InputImageType, OutputImageType >
                                                      MultiScaleHessianFilterType;
  typedef itk::KrcahEigenToScalarImageFilter< MultiScaleHessianFilterType::EigenValueImageType, OutputImageType >
                                                      KrcahEigenToScalarFilterType;

  /* Do preprocessing */
  ReaderType::Pointer  reader = ReaderType::New();
  reader->SetFileName(inputFileName);

  PreprocessFilterType::Pointer preprocessingFilter = PreprocessFilterType::New();
  preprocessingFilter->SetInput(reader->GetOutput());

  std::cout << "Running preprocessing..." << std::endl;
  MyCommand::Pointer myCommand = MyCommand::New();
  preprocessingFilter->AddObserver(itk::ProgressEvent(), myCommand);
  preprocessingFilter->Update();

  PreprocessedWriterType::Pointer preprocessingWriter = PreprocessedWriterType::New();
  preprocessingWriter->SetInput(preprocessingFilter->GetOutput());
  preprocessingWriter->SetFileName(outputPreprocessedFileName);

  std::cout << "Writing out " << outputPreprocessedFileName << std::endl;
  preprocessingWriter->Write();

  /* Multiscale measure */
  MultiScaleHessianFilterType::Pointer multiScaleFilter = MultiScaleHessianFilterType::New();
  KrcahEigenToScalarFilterType::Pointer krcahFilter = KrcahEigenToScalarFilterType::New();
  multiScaleFilter->SetInput(preprocessingFilter->GetOutput());
  multiScaleFilter->SetEigenToScalarImageFilter(krcahFilter);
  multiScaleFilter->SetSigmaArray(sigmaArray);

  std::cout << "Running multiScaleFilter..." << std::endl;
  MyCommand::Pointer command2 = MyCommand::New();
  multiScaleFilter->AddObserver(itk::ProgressEvent(), command2);
  multiScaleFilter->Update();

  MeasureWriterType::Pointer measureWriter = MeasureWriterType::New();
  measureWriter->SetInput(multiScaleFilter->GetOutput());
  measureWriter->SetFileName(outputMeasureFileName);

  std::cout << "Writing results to " << outputMeasureFileName << std::endl;
  measureWriter->Write();

  return EXIT_SUCCESS;
}