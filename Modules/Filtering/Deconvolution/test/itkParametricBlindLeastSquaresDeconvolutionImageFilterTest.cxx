/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkFFTConvolutionImageFilter.h"
#include "itkGaussianImageSource.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkParametricBlindLeastSquaresDeconvolutionImageFilter.h"
#include "itkTestingMacros.h"

// Define a version of the GaussianImageSource that has only the sigma
// parameters
namespace itk
{
template< typename TOutputImage >
class ExampleImageSource : public GaussianImageSource< TOutputImage >
{
public:
  /** Standard typedefs. */
  typedef ExampleImageSource                   Self;
  typedef GaussianImageSource< TOutputImage >  Superclass;
  typedef SmartPointer< Self >                 Pointer;
  typedef SmartPointer< const Self >           ConstPointer;

  /** Output image typedefs */
  typedef TOutputImage                            OutputImageType;
  typedef typename OutputImageType::Pointer       OutputImagePointer;
  typedef typename OutputImageType::PixelType     OutputImagePixelType;
  typedef typename OutputImageType::PixelType     PixelType;
  typedef typename OutputImageType::RegionType    RegionType;
  typedef typename OutputImageType::SpacingType   SpacingType;
  typedef typename OutputImageType::PointType     PointType;
  typedef typename OutputImageType::DirectionType DirectionType;
  typedef typename OutputImageType::SizeType      SizeType;
  typedef typename OutputImageType::SizeValueType SizeValueType;

  typedef typename Superclass::ParametersValueType  ParametersValueType;
  typedef typename Superclass::ParametersType       ParametersType;
  typedef std::vector< bool >                       EnabledArrayType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

   /** ImageDimension constant */
  itkStaticConstMacro(OutputImageDimension,
                      unsigned int,
                      TOutputImage::ImageDimension);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ExampleImageSource, GaussianImageSource);

  /** Set the parameters for this source. Setting the parameters does
   * not mark the image source as modified; subclasses should override
   * this method to forward parameters through setters that call
   * Modified(). */
  virtual void SetParameters( const ParametersType & parameters ) ITK_OVERRIDE
  {
    ParametersType gaussianParameters = this->Superclass::GetParameters();
    for ( unsigned int i = 0; i < OutputImageDimension; ++i )
      {
      gaussianParameters[i] = parameters[i];
      }

    this->Superclass::SetParameters( gaussianParameters );
  }

  /** Get the parameters for this source. */
  virtual ParametersType GetParameters() const ITK_OVERRIDE
  {
    ParametersType gaussianParameters = this->Superclass::GetParameters();
    ParametersType parameters(OutputImageDimension);
    for ( unsigned int i = 0; i < OutputImageDimension; ++i )
      {
      parameters[i] = gaussianParameters[i];
      }

    return parameters;
  }

  /** Get the number of parameters. */
  virtual unsigned int GetNumberOfParameters() const ITK_OVERRIDE
  {
    return OutputImageDimension;
  }

protected:
  ExampleImageSource() {};
  virtual ~ExampleImageSource() ITK_OVERRIDE {};

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ExampleImageSource);
};
}


int itkParametricBlindLeastSquaresDeconvolutionImageFilterTest(int argc, char* argv[])
{
  if ( argc < 6 )
    {
    std::cerr << "Usage: " << argv[0]
              << " <input image> <output image> <iterations> <alpha> <beta> [convolution image]"
              << std::endl;
    return EXIT_FAILURE;
    }

  typedef float                              PixelType;
  const unsigned int                         Dimension = 2;
  typedef itk::Image< PixelType, Dimension > ImageType;
  typedef itk::ImageFileReader< ImageType >  ReaderType;
  typedef itk::ImageFileWriter< ImageType >  WriterType;

  ReaderType::Pointer inputReader = ReaderType::New();
  inputReader->SetFileName( argv[1] );
  inputReader->Update();

  // Create a masked parametric image source so that we can optimize
  // for the sigma parameters only.
  typedef itk::ExampleImageSource< ImageType > KernelSourceType;
  KernelSourceType::Pointer kernelSource = KernelSourceType::New();
  kernelSource->SetScale( 1.0 );

  KernelSourceType::SizeType size = {{32, 32}};
  kernelSource->SetSize( size );

  KernelSourceType::PointType origin;
  origin[0] = 0.0;
  origin[1] = 0.0;
  kernelSource->SetOrigin( origin );

  KernelSourceType::SpacingType spacing;
  spacing[0] = 1.0;
  spacing[1] = 1.0;
  kernelSource->SetSpacing( spacing );

  KernelSourceType::ArrayType sigma;
  sigma[0] = 2.0;
  sigma[1] = 4.0;
  kernelSource->SetSigma( sigma );

  KernelSourceType::ArrayType mean;
  mean[0] = 0.5 * (size[0] - 1) * spacing[0];
  mean[1] = 0.5 * (size[1] - 1) * spacing[1];
  kernelSource->SetMean( mean );

  // Generate a convolution of the input image with a kernel computed
  // from a parametric image source. We'll try to recover those
  // parameters later.
  typedef itk::FFTConvolutionImageFilter< ImageType > ConvolutionFilterType;
  ConvolutionFilterType::Pointer convolutionFilter = ConvolutionFilterType::New();
  convolutionFilter->SetInput( inputReader->GetOutput() );
  convolutionFilter->NormalizeOn();
  convolutionFilter->SetKernelImage( kernelSource->GetOutput() );

  // Use the same SizeGreatestPrimeFactor across FFT backends to get
  // consistent results.
  convolutionFilter->SetSizeGreatestPrimeFactor( 5 );

  // Create an instance of the deconvolution filter
  typedef itk::ParametricBlindLeastSquaresDeconvolutionImageFilter< ImageType, KernelSourceType >
    DeconvolutionFilterType;
  DeconvolutionFilterType::Pointer deconvolutionFilter = DeconvolutionFilterType::New();
  deconvolutionFilter->SetKernelSource( kernelSource );
  deconvolutionFilter->SetSizeGreatestPrimeFactor( 5 );

  // Change the sigma settings here to something different

  KernelSourceType::ParametersType parameters( kernelSource->GetParameters() );
  parameters[0] = 3.0;
  parameters[1] = 3.0;
  kernelSource->SetParameters( parameters );

  deconvolutionFilter->NormalizeOn();
  double alpha = atof( argv[4] );
  double beta  = atof( argv[5] );
  deconvolutionFilter->SetAlpha( alpha );
  deconvolutionFilter->SetBeta( beta );
  deconvolutionFilter->SetInput( convolutionFilter->GetOutput() );
  deconvolutionFilter->SetNumberOfIterations( atoi( argv[3] ) );
  deconvolutionFilter->UpdateLargestPossibleRegion();

  std::cout << "Kernel parameters: " << kernelSource->GetParameters()
            << std::endl;
  try
    {
    WriterType::Pointer writer = WriterType::New();
    writer->SetFileName( argv[2] );
    writer->SetInput( deconvolutionFilter->GetOutput() );
    writer->Update();
    }
  catch ( itk::ExceptionObject & e )
    {
    std::cerr << "Unexpected exception caught when writing deconvolution image: "
              << e << std::endl;
    return EXIT_FAILURE;
    }

  KernelSourceType::ParametersValueType expectedSigmaX = 2.90243;
  if ( std::abs( kernelSource->GetParameters()[0] - expectedSigmaX ) > 1e-5 )
    {
    std::cerr << "Kernel parameter[0] should have been " << expectedSigmaX
              << ", was " << kernelSource->GetParameters()[0] << "."
              << std::endl;
    return EXIT_FAILURE;
    }

  KernelSourceType::ParametersValueType expectedSigmaY = 2.90597;
  if ( std::abs( kernelSource->GetParameters()[1] - expectedSigmaY ) > 1e-5 )
    {
    std::cerr << "Kernel parameter[1] should have been " << expectedSigmaY
              << ", was " << kernelSource->GetParameters()[0] << "."
              << std::endl;
    return EXIT_FAILURE;
    }

  // Optionally write the convolution image on which we're testing the
  // deconvolution filter
  if ( argc >= 7 )
    {
    try
      {
      WriterType::Pointer writer = WriterType::New();
      writer->SetFileName( argv[6] );
      writer->SetInput( convolutionFilter->GetOutput() );
      writer->Update();
      }
    catch ( itk::ExceptionObject & e )
      {
      std::cerr << "Unexpected exception caught when writing convolution image: "
                << e << std::endl;
      return EXIT_FAILURE;
      }
    }

  // Exercise the setters/getters
  TEST_SET_GET_VALUE( alpha, deconvolutionFilter->GetAlpha() );
  TEST_SET_GET_VALUE( beta, deconvolutionFilter->GetBeta() );

  return EXIT_SUCCESS;
}
