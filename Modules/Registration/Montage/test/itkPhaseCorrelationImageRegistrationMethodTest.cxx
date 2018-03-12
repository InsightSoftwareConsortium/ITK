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
#include "itkPhaseCorrelationImageRegistrationMethod.h"
#include "itkMaxPhaseCorrelationOptimizer.h"
#include "itkImageFileWriter.h"
#include "itkNumericTraits.h"

namespace itk
{

template< typename TPixel, unsigned int VDimension >
class HyperSphereImageSource: public itk::Object
{
public:
  typedef HyperSphereImageSource   Self;
  typedef Object                   Superclass;
  typedef SmartPointer<Self>       Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  typedef Array<double>            ParametersType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Image, Object);

  typedef itk::Image<TPixel, VDimension > ImageType;

  ImageType * GenerateImage()
  {
    m_Image = ImageType::New();

    typename ImageType::IndexType index;
    index.Fill(0);
    typename ImageType::RegionType region;
    region.SetSize( m_ImageSize );
    region.SetIndex( index );

    m_Image->SetLargestPossibleRegion( region );
    m_Image->SetBufferedRegion( region );
    m_Image->SetRequestedRegion( region );
    m_Image->Allocate();

    m_Image->SetSpacing( m_ImageSpacing );
    m_Image->SetOrigin( m_ImageOrigin );
    m_Image->SetDirection( m_ImageDirection );

    itk::Point<double, VDimension> p;
    TPixel value;
    typedef  itk::ImageRegionIteratorWithIndex<ImageType> ImageIteratorType;
    ImageIteratorType it(m_Image,region);
    while(!it.IsAtEnd())
      {
      m_Image->TransformIndexToPhysicalPoint(it.GetIndex(), p);
      if (m_SphereCenter.EuclideanDistanceTo(p) > m_SphereRadius)
        {
        value = itk::NumericTraits<TPixel>::ZeroValue();
        }
      else
        {
        value = value = itk::NumericTraits<TPixel>::OneValue();
        }
      it.Set( value );
      ++it;
      }

    return m_Image.GetPointer();
  }

protected:
  HyperSphereImageSource()
  {
    m_SphereRadius = 50.0;
    m_SphereCenter.Fill(50.0);
    m_ImageOrigin.Fill(0.0);
    m_ImageSize.Fill(100);
    m_ImageSpacing.Fill(1.0);
    m_ImageDirection.SetIdentity();
  }

private:
  typename ImageType::Pointer m_Image;

public:
  double                            m_SphereRadius;
  typename ImageType::PointType     m_SphereCenter;
  typename ImageType::PointType     m_ImageOrigin;
  typename ImageType::SizeType      m_ImageSize;
  typename ImageType::SpacingType   m_ImageSpacing;
  typename ImageType::DirectionType m_ImageDirection;
};

}


template < unsigned int VDimension, typename TFixedImagePixel, typename TMovingImagePixel >
int PhaseCorrelationRegistration( int argc, char* argv[] )
{
  if (argc < 3)
    {
    std::cerr << "Usage: " << argv[0] << " <<dimension><fixedTypeChar><movingTypeChar>> <phaseCorrelationFile>" << std::endl;
    return EXIT_FAILURE;
    }
  const char * phaseCorrelationFile = argv[2];

  bool pass = true;

  typedef itk::Image<TFixedImagePixel, VDimension>   FixedImageType;
  typedef itk::Image<TMovingImagePixel, VDimension>  MovingImageType;
  typedef typename MovingImageType::SizeType         SizeType;

  // test image sources
  typedef itk::HyperSphereImageSource<
      typename FixedImageType::PixelType,
      VDimension >                                   FixedImageSourceType;
  typedef itk::HyperSphereImageSource<
      typename MovingImageType::PixelType,
      VDimension >                                   MovingImageSourceType;

  typedef itk::PhaseCorrelationImageRegistrationMethod< FixedImageType,
                                                        MovingImageType >
                                                     PCMType;

  typedef itk::PhaseCorrelationOperator< typename itk::NumericTraits< TFixedImagePixel >::RealType, VDimension >
                                                     OperatorType;

  typedef itk::MaxPhaseCorrelationOptimizer<PCMType> OptimizerType;
  typedef typename PCMType::TransformType            TransformType;
  typedef typename TransformType::ParametersType     ParametersType;


  typename OperatorType::Pointer       pcmOperator   = OperatorType::New();
  typename OptimizerType::Pointer      pcmOptimizer  = OptimizerType::New();
  typename PCMType::Pointer            pcm           = PCMType::New();

  typename FixedImageSourceType::Pointer fixedImageSource = FixedImageSourceType::New();
  typename MovingImageSourceType::Pointer movingImageSource = MovingImageSourceType::New();

  SizeType size;
  double spacing[ VDimension ];
  double origin[ VDimension ];
  typename MovingImageType::SizeType   newMovingSize;
  typename MovingImageType::PointType  newMovingOrigin;
  for (unsigned int i = 0; i < VDimension; i++)
    {
    size[i] = 100;
    spacing[i] = 1.0;
    origin[i] = 0.0;
    newMovingSize[i] = 100;
    newMovingOrigin[i] = 0.0;
    }

  fixedImageSource->m_ImageSize = size;
  fixedImageSource->m_ImageSpacing = spacing;
  fixedImageSource->m_ImageOrigin = origin;
  typename FixedImageType::ConstPointer fixedImage = fixedImageSource->GenerateImage();

  ParametersType actualParameters(VDimension);
  for (unsigned int i = 0; i < VDimension; i++)
    {
    actualParameters[i] = 3 + i * 4;
    movingImageSource->m_SphereCenter[i] += actualParameters[i];
    }

  // increase the resolution and size and crop moving image in 1st dimension
  // this tests the ability of PCM to padd the images to the same real size
  // and to resample the images to the same pixel size and spacing
  //spacing[0] = 0.8;
  newMovingSize[0] = (unsigned long)( 100.0 / spacing[0] - 10 );
  newMovingSize[1] = (unsigned long)( 100.0 / spacing[1] + 10 );

  movingImageSource->m_ImageSize = newMovingSize;
  movingImageSource->m_ImageSpacing = spacing;

  // shift the origin of the moving image in 2nd dimension
  // this tests the ability of PCM to introduce between-image origin offset
  // into the transformation (so the final parameters can be directly used to
  // resample the two images into the same coordinate system)
  // ! supposing that the input images have all origin components == 0.0 !
  //newMovingOrigin[1] = 12.0;
  movingImageSource->m_ImageOrigin = newMovingOrigin;
  typename MovingImageType::ConstPointer movingImage = movingImageSource->GenerateImage();


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
  ParametersType finalParameters     = pcm->GetTransformParameters();
  ParametersType transformParameters = pcm->GetOutput()->Get()->GetParameters();

  const unsigned int numberOfParameters = actualParameters.Size();

  const double tolerance = 0.1;

  // Validate first two parameters (introduced by image source)
  for(unsigned int i=0; i<numberOfParameters; i++)
    {
    // the parameters are negated in order to get the inverse transformation.
    // this only works for comparing translation parameters....
    std::cout << finalParameters[i] << " == "
              << actualParameters[i] << " == "
              << transformParameters[i] << std::endl;

    if(  ( itk::Math::abs( finalParameters[i] - actualParameters[i] ) > tolerance ) ||
         ( itk::Math::abs( transformParameters[i] - actualParameters[i] ) > tolerance ) )
      {
      std::cout << "Tolerance exceeded at component " << i << std::endl;
      pass = false;
      }
    }

  // All other parameters must be 0
  for (unsigned int i=numberOfParameters; i<VDimension; i++)
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
