#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkOtsuMultipleThresholdsCalculator.h"
#include "itkScalarImageToHistogramGenerator.h"
#include "itkBinaryThresholdImageFilter.h"

int main( int argc, char * argv[] )
{
  if( argc < 1 )
    {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImageFile outputImageFile ";  
    std::cerr << " insideValue    outsideValue  numberOfThresholdsToCalculate "  << std::endl;  
    return 1;
    }
  
  //Convenience typedefs
  typedef  unsigned char  InputPixelType;
  typedef  unsigned char  OutputPixelType;
  typedef itk::Image< InputPixelType,  2 >   InputImageType;
  typedef itk::Image< OutputPixelType, 2 >   OutputImageType;
  typedef itk::Statistics::ScalarImageToHistogramGenerator< InputImageType > ScalarImageToHistogramGeneratorType;
  typedef itk::ImageFileReader< InputImageType >  ReaderType;
  typedef itk::ImageFileWriter< InputImageType >  WriterType;
  typedef  itk::OtsuMultipleThresholdsCalculator< ScalarImageToHistogramGeneratorType::HistogramType >   CalculatorType;
  typedef itk::BinaryThresholdImageFilter< InputImageType, OutputImageType >  filter;
    
  //Create using static New() method
  ScalarImageToHistogramGeneratorType::Pointer scalarImageToHistogramGenerator = ScalarImageToHistogramGeneratorType::New();
  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();
  CalculatorType::Pointer calculator = CalculatorType::New();
  
  //Connect pipeline
  calculator->SetNumberOfThresholds(4);
  reader->SetFileName( argv[1] );
  scalarImageToHistogramGenerator->SetInput(reader->GetOutput());
  scalarImageToHistogramGenerator->SetNumberOfBins(128);
  calculator->SetInputHistogram(scalarImageToHistogramGenerator->GetOutput());
  
  
  //Invoke pipeline
  //reader->Update();
  scalarImageToHistogramGenerator->Compute();
  calculator->Update();
  const CalculatorType::OutputType &thresholdVector = calculator->GetOutput(); 

  CalculatorType::OutputType::const_iterator itNum = thresholdVector.begin();

  for(; itNum < thresholdVector.end(); itNum++) {
    std::cout << "OtsuThreshold["
      << (int)(itNum - thresholdVector.begin())
      << "] = " << *itNum << std::endl;  }

  

  return 0;
}

