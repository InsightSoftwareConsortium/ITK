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
#include "itkArray.h"
#include "itkResampleImageFilter.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkPhaseCorrelationImageRegistrationMethod.h"
#include "itkMaxPhaseCorrelationOptimizer.h"
#include "itkImageFileWriter.h"
#include "itkCastImageFilter.h"

namespace itk
{

template< typename TFixedPixel,
          typename TMovingPixel,
          unsigned int NDimension >
class PhaseCorrelationImageRegistrationMethodImageSource: public itk::Object
{
public:
  typedef PhaseCorrelationImageRegistrationMethodImageSource Self;
  typedef Object                                             Superclass;
  typedef SmartPointer<Self>                                 Pointer;
  typedef SmartPointer<const Self>                           ConstPointer;
  typedef Array<double>                                      ParametersType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Image, Object);

  typedef itk::Image<TMovingPixel,NDimension> MovingImageType;
  typedef itk::Image<TFixedPixel,NDimension > FixedImageType;


  const MovingImageType * GetMovingImage(void) const
    {
    return m_MovingImage.GetPointer();
    }

  const FixedImageType * GetFixedImage(void) const
    {
    return m_FixedImage.GetPointer();
    }

  const ParametersType & GetActualParameters(void) const
  {
    return m_Parameters;
  }

  void GenerateImages( const typename MovingImageType::SizeType & size )
  {
    typename MovingImageType::IndexType index;
    index.Fill(0);
    typename MovingImageType::RegionType region;
    region.SetSize( size );
    region.SetIndex( index );

    m_MovingImage->SetLargestPossibleRegion( region );
    m_MovingImage->SetBufferedRegion( region );
    m_MovingImage->SetRequestedRegion( region );
    m_MovingImage->Allocate();

    m_FixedImage->SetLargestPossibleRegion( region );
    m_FixedImage->SetBufferedRegion( region );
    m_FixedImage->SetRequestedRegion( region );
    m_FixedImage->Allocate();

    /* Fill images with a gaussian*/
    typedef  itk::ImageRegionIteratorWithIndex<MovingImageType>
        MovingImageIteratorType;

    typedef  itk::ImageRegionIteratorWithIndex<FixedImageType>
        FixedImageIteratorType;


    itk::Point<double,2> center;
    center[0] = (double)region.GetSize()[0]/2.0;
    center[1] = (double)region.GetSize()[1]/2.0;

    const double s = (double)region.GetSize()[0]/2.0;

    itk::Point<double,2>  p;
    itk::Vector<double,2> d;

    /* Set the displacement */
    itk::Vector<double,2> displacement;
    displacement[0] = m_Parameters[0];
    displacement[1] = m_Parameters[1];


    MovingImageIteratorType ri(m_MovingImage,region);
    FixedImageIteratorType ti(m_FixedImage,region);
    while(!ri.IsAtEnd())
    {
      p[0] = ri.GetIndex()[0];
      p[1] = ri.GetIndex()[1];
      d = p-center;
      d += displacement;
      const double x = d[0];
      const double y = d[1];
      const unsigned char value = sqrt(x*x+y*y)>s ? 0 : 1;
      ri.Set( static_cast<typename MovingImageType::PixelType>(value) );
      ++ri;
    }


    while(!ti.IsAtEnd())
    {
      p[0] = ti.GetIndex()[0];
      p[1] = ti.GetIndex()[1];
      d = p-center;
      const double x = d[0];
      const double y = d[1];
      const double value = sqrt(x*x+y*y)>s ? 0 : 1;
      ti.Set( static_cast<typename FixedImageType::PixelType>(value) );
      ++ti;
    }
  }

protected:
  PhaseCorrelationImageRegistrationMethodImageSource()
  {
    m_MovingImage = MovingImageType::New();
    m_FixedImage  = FixedImageType::New();
    m_Parameters  = ParametersType(2);
    m_Parameters[0] = 7.0;
    m_Parameters[1] = 3.0;
  }

private:
  typename FixedImageType::Pointer     m_FixedImage;
  typename MovingImageType::Pointer    m_MovingImage;

  ParametersType                       m_Parameters;
};

}


template < unsigned int NDimension,
           typename TFixedImagePixel,
     typename TMovingImagePixel >
int PhaseCorrelationRegistration( int argc, char* argv[] )
{
  if (argc < 3)
    {
    std::cerr << "Usage: " << argv[0] << " <<dimension><fixedTypeChar><movingTypeChar>> <phaseCorrelationFile>" << std::endl;
    return EXIT_FAILURE;
    }
  const char * phaseCorrelationFile = argv[2];

  bool pass = true;

  // Fixed Image Type
  typedef itk::Image<TFixedImagePixel, NDimension>   FixedImageType;

  // Moving Image Type
  typedef itk::Image<TMovingImagePixel, NDimension>  MovingImageType;

  // Size Type
  typedef typename MovingImageType::SizeType           SizeType;

  // test image source
  typedef itk::PhaseCorrelationImageRegistrationMethodImageSource<
      typename FixedImageType::PixelType,
      typename MovingImageType::PixelType,
      NDimension >                                      ImageSourceType;

  // Resample filter
  typedef itk::ResampleImageFilter< MovingImageType, MovingImageType >
                                                       ResamplerType;

  // Interpolator for resampler
  typedef itk::LinearInterpolateImageFunction< MovingImageType, double >
                                                       InterpolatorType;


  // Registration method
  typedef itk::PhaseCorrelationImageRegistrationMethod< FixedImageType,
                                                        MovingImageType >
                                                       PCMType;

  // Operator type
  typedef itk::PhaseCorrelationOperator< typename itk::NumericTraits< TFixedImagePixel >::RealType, NDimension >     OperatorType;

  // Optimizer type
  typedef itk::MaxPhaseCorrelationOptimizer<PCMType>   OptimizerType;

  // Transform type
  typedef typename PCMType::TransformType              TransformType;
  typedef typename TransformType::ParametersType       ParametersType;


  typename OperatorType::Pointer       pcmOperator   = OperatorType::New();
  typename OptimizerType::Pointer      pcmOptimizer  = OptimizerType::New();
  typename PCMType::Pointer            pcm           = PCMType::New();

  typename ImageSourceType::Pointer    imageSource   = ImageSourceType::New();
  typename ResamplerType::Pointer      resampler     = ResamplerType::New();
  typename InterpolatorType::Pointer   interpolator  = InterpolatorType::New();

  SizeType size;
  double spacing[ NDimension ];
  double origin[ NDimension ];
  typename MovingImageType::SizeType   newMovingSize;
  typename MovingImageType::PointType  newMovingOrigin;
  for (unsigned int i = 0; i < NDimension; i++)
    {
    size[i] = 100;
    spacing[i] = 1.0;
    origin[i] = 0.0;
    newMovingSize[i] = 100;
    newMovingOrigin[i] = 0.0;
    }

  // increase the resolution and size and crop moving image in 1st dimension
  // this tests the ability of PCM to padd the images to the same real size
  // and to resample the images to the same pixel size and spacing
  spacing[0] = 0.8;
  newMovingSize[0] = (unsigned long)( 100.0 / spacing[0]  - 10 );

  imageSource->GenerateImages( size );

  resampler->SetInterpolator( interpolator );
  resampler->SetDefaultPixelValue( 0 );
  resampler->SetOutputSpacing( spacing );
  resampler->SetOutputOrigin( origin );
  resampler->SetSize( newMovingSize );

  resampler->SetInput( imageSource->GetMovingImage() );

  resampler->Update();

  typename FixedImageType::ConstPointer  fixedImage
                                            = imageSource->GetFixedImage();
  typename MovingImageType::Pointer      movingImage
                                            = resampler->GetOutput();

  // shift the origin of the moving image in 2nd dimension
  // this tests the ability of PCM to introduce between-image origin offset
  // into the transformation (so the final parameters can be directly used to
  // resample the two images into the same coordinate system)
  // ! supposing that the input images have all origin components == 0.0 !
  newMovingOrigin[1] = 2.0;
  movingImage->SetOrigin(newMovingOrigin);


  //
  // Connect all the components required for Registratio
  //
  pcm->SetOperator(    pcmOperator  );
  pcm->SetOptimizer(   pcmOptimizer );
  pcm->SetFixedImage(  fixedImage   );
  pcm->SetMovingImage( movingImage  );


  //
  // Execute the registration.
  // This can potentially throw an exception
  //
  try
    {
    pcm->DebugOn();
    pcm->Update();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cerr << e << std::endl;
    pass = false;
    }

  //
  // Get registration result and validate it.
  //
  ParametersType actualParameters    = imageSource->GetActualParameters();
  ParametersType finalParameters     = pcm->GetTransformParameters();
  ParametersType transformParameters = pcm->GetOutput()->Get()->GetParameters();

  const unsigned int numberOfParameters = actualParameters.Size();

  const double tolerance = 1.0;  // equivalent to 1 pixel.

  // Validate first two parameters (introduced by image source)
  for(unsigned int i=0; i<numberOfParameters; i++)
    {
    // the parameters are negated in order to get the inverse transformation.
    // this only works for comparing translation parameters....
    std::cout << finalParameters[i] << " == "
              << -(actualParameters[i]+newMovingOrigin[i]) << " == "
              << transformParameters[i] << std::endl;

    if(  ( itk::Math::abs( finalParameters[i] -
                          (-(actualParameters[i]+newMovingOrigin[i]))
                        ) > tolerance ) ||
         ( itk::Math::abs( transformParameters[i] -
                          (-(actualParameters[i]+newMovingOrigin[i]))
                        ) > tolerance ) )
      {
      std::cout << "Tolerance exceeded at component " << i << std::endl;
      pass = false;
      }
    }

  // All other parameters must be 0
  for (unsigned int i=numberOfParameters; i<NDimension; i++)
    {
    if (  ( vnl_math_abs ( finalParameters[i] ) > tolerance )
        ||
          ( vnl_math_abs ( finalParameters[i] ) > tolerance ) )
      {
      std::cout << "Tolerance exceeded at component " << i << std::endl;
      pass = false;
      }
    }

  using WriterType = itk::ImageFileWriter< typename PCMType::RealImageType >;
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( phaseCorrelationFile );
  writer->SetInput( pcm->GetPhaseCorrelationImage() );
  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cerr << e << std::endl;
    pass = false;
    }

  if( !pass )
    {
    std::cout << "Test FAILED." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test PASSED." << std::endl;
  return EXIT_SUCCESS;
}


int itkPhaseCorrelationImageRegistrationMethodTest( int argc, char* argv[] )
{
  if( argc < 2 )
    {
    std::cerr << "Usage: " << argv[0] << " <<Dimension><FixedPixelTypeCharacter><MovingImageTypeCharacter>>" << std::endl;
    return EXIT_FAILURE;
    }

  if( !strcmp( argv[1], "2cc" ) )
    {
    return PhaseCorrelationRegistration< 2, signed char, signed char >( argc, argv );
    }
  else if( !strcmp( argv[1], "2ff" ) )
    {
    return PhaseCorrelationRegistration< 2, float, float >( argc, argv );
    }
  else if( !strcmp( argv[1], "2dd" ) )
    {
    return PhaseCorrelationRegistration< 2, double, double >( argc, argv );
    }
  else if( !strcmp( argv[1], "2cf" ) )
    {
    return PhaseCorrelationRegistration< 2, signed char, float >( argc, argv );
    }
  else if( !strcmp( argv[1], "2fd" ) )
    {
    return PhaseCorrelationRegistration< 2, float, double >( argc, argv );
    }
  else if( !strcmp( argv[1], "3cc" ) )
    {
    return PhaseCorrelationRegistration< 3, signed char, signed char >( argc, argv );
    }
  else if( !strcmp( argv[1], "3ff" ) )
    {
    return PhaseCorrelationRegistration< 3, float, float >( argc, argv );
    }
  else if( !strcmp( argv[1], "3dd" ) )
    {
    return PhaseCorrelationRegistration< 3, double, double >( argc, argv );
    }
  else if( !strcmp( argv[1], "3cf" ) )
    {
    return PhaseCorrelationRegistration< 3, signed char, float >( argc, argv );
    }
  else if( !strcmp( argv[1], "3fd" ) )
    {
    return PhaseCorrelationRegistration< 3, float, double >( argc, argv );
    }

  std::cerr << "Unexpected Dimension/FixedPixelType/MovingPixelType!" << std::endl;
  return EXIT_FAILURE;
}
