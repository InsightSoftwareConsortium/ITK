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

#include "itkVersorRigid3DTransform.h"
#include "itkCenteredTransformInitializer.h"
#include "itkImageRegionIterator.h"


namespace
{
const unsigned int Dimension = 3;

// This function assumes that the center of mass of both images is the
// geometrical center.
template< typename TFixedImage, typename TMovingImage >
bool RunTest(
  itk::SmartPointer< TFixedImage >  fixedImage,
  itk::SmartPointer< TMovingImage > movingImage
  )
  {
  typedef TFixedImage  FixedImageType;
  typedef TMovingImage MovingImageType;

  bool pass = true;

  // Transform Type
  typedef itk::VersorRigid3DTransform< double >     TransformType;

  // calculate image centers
  TransformType::InputPointType   fixedCenter;
  TransformType::InputPointType   movingCenter;

  typedef itk::ContinuousIndex< double, Dimension > ContinuousIndexType;

  const typename FixedImageType::RegionType & fixedRegion = fixedImage->GetLargestPossibleRegion();
  const typename FixedImageType::SizeType &   fixedSize   = fixedRegion.GetSize();
  const typename FixedImageType::IndexType &  fixedIndex  = fixedRegion.GetIndex();
  ContinuousIndexType                         fixedCenterIndex;
  for ( unsigned int i=0; i<Dimension; i++ )
    {
    assert( 0 < fixedSize[i] );
    fixedCenterIndex[i] = static_cast< double >( fixedIndex[i] ) +
      static_cast< double >( fixedSize[i] - 1 ) / 2.0;
    }
  fixedImage->TransformContinuousIndexToPhysicalPoint( fixedCenterIndex, fixedCenter );

  const typename MovingImageType::RegionType & movingRegion = movingImage->GetLargestPossibleRegion();
  const typename MovingImageType::SizeType &   movingSize   = movingRegion.GetSize();
  const typename MovingImageType::IndexType &  movingIndex  = movingRegion.GetIndex();
  ContinuousIndexType                          movingCenterIndex;
  for ( unsigned int i=0; i<Dimension; i++ )
    {
    assert( 0 < movingSize[i] );
    movingCenterIndex[i] = static_cast< double >( movingIndex[i] ) +
      static_cast< double >( movingSize[i] - 1 ) / 2.0;
    }
  movingImage->TransformContinuousIndexToPhysicalPoint( movingCenterIndex, movingCenter );

  TransformType::InputVectorType relativeCenter = movingCenter - fixedCenter;


  TransformType::Pointer transform = TransformType::New();

  typedef itk::CenteredTransformInitializer<
                                  TransformType,
                                  FixedImageType,
                                  MovingImageType >
                                            InitializerType;

  typename InitializerType::Pointer initializer = InitializerType::New();

  initializer->SetFixedImage( fixedImage );
  initializer->SetMovingImage( movingImage );
  initializer->SetTransform( transform );

  transform->SetIdentity();
  initializer->GeometryOn();
  initializer->InitializeTransform();

  std::cout << std::endl << std::endl;
  std::cout << "Testing Geometric Mode " << std::endl;
  //transform->Print( std::cout );

  const TransformType::InputPointType &   center1      = transform->GetCenter();
  const TransformType::OutputVectorType & translation1 = transform->GetTranslation();
  const TransformType::OffsetType &       offset1      = transform->GetOffset();
  const double tolerance = 1e-3;

  // Verfications for the Geometry Mode
  for(unsigned int k=0; k < Dimension; k++ )
    {
    if( std::fabs( center1[k] - fixedCenter[k] ) > tolerance )
      {
      std::cerr << "Center differs from expected value" << std::endl;
      std::cerr << "It should be " << fixedCenter << std::endl;
      std::cerr << "but it is    " << center1 << std::endl;
      pass = false;
      break;
      }
    if( std::fabs( translation1[k] - relativeCenter[k] ) > tolerance )
      {
      std::cerr << "Translation differs from expected value" << std::endl;
      std::cerr << "It should be " << relativeCenter << std::endl;
      std::cerr << "but it is    " << translation1 << std::endl;
      pass = false;
      break;
      }
    if( std::fabs( offset1[k] - relativeCenter[k] ) > tolerance )
      {
      std::cerr << "Offset differs from expected value" << std::endl;
      std::cerr << "It should be " << relativeCenter << std::endl;
      std::cerr << "but it is    " << offset1 << std::endl;
      pass = false;
      break;
      }
    }

  transform->SetIdentity();
  initializer->MomentsOn();
  initializer->InitializeTransform();

  std::cout << std::endl << std::endl;
  std::cout << "Testing Moments Mode " << std::endl;
  //transform->Print( std::cout );

  const TransformType::InputPointType &   center2      = transform->GetCenter();
  const TransformType::OutputVectorType & translation2 = transform->GetTranslation();
  const TransformType::OffsetType &       offset2      = transform->GetOffset();

  // Verfications for the Moments Mode
  for(unsigned int k=0; k < Dimension; k++ )
    {
    if( std::fabs( center2[k] - fixedCenter[k] ) > tolerance )
      {
      std::cerr << "Center differs from expected value" << std::endl;
      std::cerr << "It should be " << fixedCenter << std::endl;
      std::cerr << "but it is    " << center2 << std::endl;
      pass = false;
      break;
      }
    if( std::fabs( translation2[k] - relativeCenter[k] ) > tolerance )
      {
      std::cerr << "Translation differs from expected value" << std::endl;
      std::cerr << "It should be " << relativeCenter << std::endl;
      std::cerr << "but it is    " << translation2 << std::endl;
      pass = false;
      break;
      }
    if( std::fabs( offset2[k] - relativeCenter[k] ) > tolerance )
      {
      std::cerr << "Offset differs from expected value" << std::endl;
      std::cerr << "It should be " << relativeCenter << std::endl;
      std::cerr << "but it is    " << offset2 << std::endl;
      pass = false;
      break;
      }
    }

  return pass;
  }


template< typename TImage >
void PopulateImage( itk::SmartPointer< TImage > image )
  {
  image->Allocate();
  image->FillBuffer( 0 );

  typedef TImage                         ImageType;
  typedef typename ImageType::RegionType RegionType;
  typedef typename ImageType::SizeType   SizeType;
  typedef typename ImageType::IndexType  IndexType;

  const RegionType & region = image->GetLargestPossibleRegion();
  const SizeType &   size   = region.GetSize();
  const IndexType &  index  = region.GetIndex();

  RegionType internalRegion;
  SizeType   internalSize;
  IndexType  internalIndex;

  const unsigned int border = 20;

  assert( 2 * border < size[0] );
  assert( 2 * border < size[1] );
  assert( 2 * border < size[2] );

  internalIndex[0] = index[0] + border;
  internalIndex[1] = index[1] + border;
  internalIndex[2] = index[2] + border;

  internalSize[0]  = size[0] - 2 * border;
  internalSize[1]  = size[1] - 2 * border;
  internalSize[2]  = size[2] - 2 * border;


  internalRegion.SetSize(  internalSize  );
  internalRegion.SetIndex( internalIndex );

  typedef itk::ImageRegionIterator< ImageType > Iterator;
  Iterator it( image, internalRegion );

  it.GoToBegin();
  while( !it.IsAtEnd() )
    {
    it.Set( 200 );
    ++it;
    }
  }

} // namespace


/**
 *  This program tests the use of the CenteredTransformInitializer class
 *
 *
 */

int itkCenteredTransformInitializerTest(int , char* [] )
{

  bool pass = true;

  std::cout << std::endl << std::endl;
  std::cout << "Running tests with itk::Image" << std::endl;
  {
  // Create Images

  typedef itk::Image<unsigned char, Dimension>     FixedImageType;
  typedef itk::Image<unsigned char, Dimension>     MovingImageType;

  typedef FixedImageType::SizeType                 SizeType;
  typedef FixedImageType::SpacingType              SpacingType;
  typedef FixedImageType::PointType                PointType;
  typedef FixedImageType::IndexType                IndexType;
  typedef FixedImageType::RegionType               RegionType;

  SizeType size;
  size[0] = 100;
  size[1] = 100;
  size[2] =  60;

  PointType fixedOrigin;
  fixedOrigin[0] = 0.0;
  fixedOrigin[1] = 0.0;
  fixedOrigin[2] = 0.0;

  PointType movingOrigin;
  movingOrigin[0] = 29.0;
  movingOrigin[1] = 17.0;
  movingOrigin[2] = 13.0;

  SpacingType spacing;
  spacing[0] = 1.5;
  spacing[1] = 1.5;
  spacing[2] = 2.5;

  IndexType index;
  index[0] = 0;
  index[1] = 0;
  index[2] = 0;

  RegionType region;
  region.SetSize( size );
  region.SetIndex( index );


  FixedImageType::Pointer     fixedImage    = FixedImageType::New();
  MovingImageType::Pointer    movingImage   = MovingImageType::New();

  fixedImage->SetRegions( region );
  fixedImage->SetSpacing( spacing );
  fixedImage->SetOrigin(  fixedOrigin );

  movingImage->SetRegions( region );
  movingImage->SetSpacing( spacing );
  movingImage->SetOrigin(  movingOrigin );

  PopulateImage( fixedImage );
  PopulateImage( movingImage );

  pass &= RunTest( fixedImage, movingImage );
  }

  std::cout << std::endl << std::endl;
  std::cout << "Running tests with itk::Image" << std::endl;
  {
  // Create Images

  typedef itk::Image<unsigned char, Dimension>     FixedImageType;
  typedef itk::Image<unsigned char, Dimension>     MovingImageType;

  typedef FixedImageType::SizeType                 SizeType;
  typedef FixedImageType::SpacingType              SpacingType;
  typedef FixedImageType::PointType                PointType;
  typedef FixedImageType::IndexType                IndexType;
  typedef FixedImageType::RegionType               RegionType;
  typedef FixedImageType::DirectionType            DirectionType;

  SizeType size;
  size[0] = 100;
  size[1] = 100;
  size[2] =  60;

  PointType fixedOrigin;
  fixedOrigin[0] = 0.0;
  fixedOrigin[1] = 0.0;
  fixedOrigin[2] = 0.0;

  PointType movingOrigin;
  movingOrigin[0] = 29.0;
  movingOrigin[1] = 17.0;
  movingOrigin[2] = 13.0;

  SpacingType spacing;
  spacing[0] = 1.5;
  spacing[1] = 1.5;
  spacing[2] = 2.5;

  IndexType fixedIndex;
  fixedIndex[0] = 0;
  fixedIndex[1] = 0;
  fixedIndex[2] = 0;

  IndexType movingIndex;
  movingIndex[0] = 10;
  movingIndex[1] = 20;
  movingIndex[2] = 30;

  RegionType fixedRegion;
  fixedRegion.SetSize( size );
  fixedRegion.SetIndex( fixedIndex );

  RegionType movingRegion;
  movingRegion.SetSize( size );
  movingRegion.SetIndex( movingIndex );

  typedef itk::Versor< itk::SpacePrecisionType > VersorType;
  VersorType x; x.SetRotationAroundX( 0.5 );
  VersorType y; y.SetRotationAroundY( 1.0 );
  VersorType z; z.SetRotationAroundZ( 1.5 );

  DirectionType fixedDirection  = (x*y*z).GetMatrix();
  DirectionType movingDirection = (z*y*x).GetMatrix();


  FixedImageType::Pointer  fixedImage  = FixedImageType::New();
  MovingImageType::Pointer movingImage = MovingImageType::New();

  fixedImage->SetRegions(    fixedRegion     );
  fixedImage->SetSpacing(    spacing         );
  fixedImage->SetOrigin(     fixedOrigin     );
  fixedImage->SetDirection(  fixedDirection  );

  movingImage->SetRegions(   movingRegion    );
  movingImage->SetSpacing(   spacing         );
  movingImage->SetOrigin(    movingOrigin    );
  movingImage->SetDirection( movingDirection );

  PopulateImage( fixedImage );
  PopulateImage( movingImage );

  pass &= RunTest( fixedImage, movingImage );
  }

  if( !pass )
    {
    std::cout << "Test FAILED." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test PASSED." << std::endl;
  return EXIT_SUCCESS;


}
