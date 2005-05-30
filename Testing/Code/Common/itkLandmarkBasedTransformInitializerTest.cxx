#include "itkLandmarkBasedTransformInitializer.h"
#include "itkImage.h"
#include <math.h>
#include <iostream>

// not all versions of math.h seem to define M_PI:
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif


//
// The test specifies a bunch of fixed and moving landmarks and test if the 
// fixed landmarks after transform by the computed transform coincides
// with the moving landmarks....

int itkLandmarkBasedTransformInitializerTest(int, char * [])
{
  
  {
    // Test LandmarkBasedTransformInitializer for Rigid 3D landmark
    // based alignment
    std::cout << "Testing Landmark alignment with VersorRigid3DTransform" << std::endl;
    
    typedef  unsigned char  PixelType;
    const unsigned int Dimension = 3;
    
    typedef itk::Image< PixelType, Dimension >  FixedImageType;
    typedef itk::Image< PixelType, Dimension >  MovingImageType;

    FixedImageType::Pointer fixedImage   = FixedImageType::New();
    MovingImageType::Pointer movingImage = MovingImageType::New();

    // Create fixed and moving images of size 30 x 30 x 30
    // 
    FixedImageType::RegionType fRegion;
    FixedImageType::SizeType   fSize;
    FixedImageType::IndexType  fIndex;
    fSize.Fill(30);
    fIndex.Fill(0);
    fRegion.SetSize( fSize );
    fRegion.SetIndex( fIndex );
    MovingImageType::RegionType mRegion;
    MovingImageType::SizeType   mSize;
    MovingImageType::IndexType  mIndex;
    mSize.Fill(30);
    mIndex.Fill(0);
    mRegion.SetSize( mSize );
    mRegion.SetIndex( mIndex );
    fixedImage->SetLargestPossibleRegion( fRegion );
    fixedImage->SetBufferedRegion( fRegion );
    fixedImage->SetRequestedRegion( fRegion );
    fixedImage->Allocate();
    movingImage->SetLargestPossibleRegion( mRegion );
    movingImage->SetBufferedRegion( mRegion );
    movingImage->SetRequestedRegion( mRegion );
    movingImage->Allocate();
   
    // Set the transform type..
    typedef itk::VersorRigid3DTransform< double > TransformType;
    TransformType::Pointer transform = TransformType::New();
    typedef itk::LandmarkBasedTransformInitializer< TransformType, 
            FixedImageType, MovingImageType > TransformInitializerType;
    TransformInitializerType::Pointer initializer = TransformInitializerType::New();

    // Set fixed and moving landmarks
    TransformInitializerType::LandmarkPointContainer fixedLandmarks;
    TransformInitializerType::LandmarkPointContainer movingLandmarks;
    TransformInitializerType::LandmarkPointType point;
    TransformInitializerType::LandmarkPointType tmp;
    
    // Moving Landmarks = Fixed Landmarks rotated by 'angle' degrees and then
    //    translated by the 'translation'. Offset can be used to move the fixed 
    //    landmarks around.
    double angle = 10 * M_PI/180;
    TransformInitializerType::LandmarkPointType translation;
    translation[0] = 6;
    translation[1] = 10;
    translation[2] = 7;
    TransformInitializerType::LandmarkPointType offset;
    offset[0] = 10;
    offset[1] = 1;
    offset[2] = 5;
    
    point[0]=2 + offset[0];
    point[1]=2 + offset[1];
    point[2]=0 + offset[2];
    fixedLandmarks.push_back(point);
    tmp = point;
    point[0] = cos(angle)*point[0] - sin(angle)*point[1] + translation[0];
    point[1] = sin(angle)*tmp[0] + cos(angle)*point[1] + translation[1];
    point[2] = point[2] + translation[2];
    movingLandmarks.push_back(point);
    point[0]=2 + offset[0];
    point[1]=-2 + offset[1];
    point[2]=0 + offset[2];
    fixedLandmarks.push_back(point);
    tmp = point;
    point[0] = cos(angle)*point[0] - sin(angle)*point[1] + translation[0];
    point[1] = sin(angle)*tmp[0] + cos(angle)*point[1] + translation[1];
    point[2] = point[2] + translation[2];
    movingLandmarks.push_back(point);
    point[0]=-2 + offset[0];
    point[1]=2 + offset[1];
    point[2]=0 + offset[2];
    fixedLandmarks.push_back(point);
    tmp = point;
    point[0] = cos(angle)*point[0] - sin(angle)*point[1] + translation[0];
    point[1] = sin(angle)*tmp[0] + cos(angle)*point[1] + translation[1];
    point[2] = point[2] + translation[2];
    movingLandmarks.push_back(point);
    point[0]=-2 + offset[0];
    point[1]=-2 + offset[1];
    point[2]=0 + offset[2];
    fixedLandmarks.push_back(point);
    tmp = point;
    point[0] = cos(angle)*point[0] - sin(angle)*point[1] + translation[0];
    point[1] = sin(angle)*tmp[0] + cos(angle)*point[1] + translation[1];
    point[2] = point[2] + translation[2];
    movingLandmarks.push_back(point);
    
    initializer->SetFixedLandmarks(fixedLandmarks);
    initializer->SetMovingLandmarks(movingLandmarks);

    initializer->SetFixedImage( fixedImage );
    initializer->SetMovingImage( movingImage );
    initializer->SetTransform( transform );
    initializer->InitializeTransform();

    // Transform the landmarks now. For the given set of landmarks, since we computed the
    // moving landmarks explicitly from the rotation and translation specified, we should 
    // get a transform that does not give any mismatch. In other words, if the fixed
    // landmarks are transformed by the transform computed by the 
    // LandmarkBasedTransformInitializer, they should coincide exactly with the moving
    // landmarks. Note that we specified 4 landmarks, although three non-collinear
    // landmarks is sufficient to guarantee a solution.
    //
    TransformInitializerType::PointsContainerConstIterator 
      fitr = fixedLandmarks.begin();
    TransformInitializerType::PointsContainerConstIterator 
      mitr = movingLandmarks.begin();

    typedef TransformInitializerType::OutputVectorType  OutputVectorType;
    OutputVectorType error;
    OutputVectorType::RealValueType tolerance = 0.1;
    bool failed = false;
    
    while( mitr != movingLandmarks.end() )
      {
      std::cout << "  Fixed Landmark: " << *fitr << " Moving landmark " << *mitr 
        << " Transformed fixed Landmark : " << 
        transform->TransformPoint( *fitr ) << std::endl;
      
      error = *mitr - transform->TransformPoint( *fitr);
      if( error.GetNorm() > tolerance )
        {
        failed = true;
        }
      
      ++mitr;
      ++fitr;
      }
    
    if( failed )
      {
      // Hang heads in shame
      std::cout << "  Fixed landmarks transformed by the transform did not match closely "
        << " enough with the moving landmarks.  The transform computed was: ";
      transform->Print(std::cout);
      std::cout << "  [FAILED]" << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cout << "  Landmark alignment using Rigid3D transform [PASSED]" << std::endl;
      } 
  }

  {
    //Test landmark alignment using Rigid 2D transform in 2 dimensions
    std::cout << "Testing Landmark alignment with Rigid2DTransform" << std::endl;
    
    typedef  unsigned char  PixelType;
    const unsigned int Dimension = 2;
    
    typedef itk::Image< PixelType, Dimension >  FixedImageType;
    typedef itk::Image< PixelType, Dimension >  MovingImageType;

    FixedImageType::Pointer fixedImage   = FixedImageType::New();
    MovingImageType::Pointer movingImage = MovingImageType::New();

    // Create fixed and moving images of size 30 x 30
    // 
    FixedImageType::RegionType fRegion;
    FixedImageType::SizeType   fSize;
    FixedImageType::IndexType  fIndex;
    fSize.Fill(30);
    fIndex.Fill(0);
    fRegion.SetSize( fSize );
    fRegion.SetIndex( fIndex );
    MovingImageType::RegionType mRegion;
    MovingImageType::SizeType   mSize;
    MovingImageType::IndexType  mIndex;
    mSize.Fill(30);
    mIndex.Fill(0);
    mRegion.SetSize( mSize );
    mRegion.SetIndex( mIndex );
    fixedImage->SetLargestPossibleRegion( fRegion );
    fixedImage->SetBufferedRegion( fRegion );
    fixedImage->SetRequestedRegion( fRegion );
    fixedImage->Allocate();
    movingImage->SetLargestPossibleRegion( mRegion );
    movingImage->SetBufferedRegion( mRegion );
    movingImage->SetRequestedRegion( mRegion );
    movingImage->Allocate();
   
    // Set the transform type..
    typedef itk::Rigid2DTransform< double > TransformType;
    TransformType::Pointer transform = TransformType::New();
    typedef itk::LandmarkBasedTransformInitializer< TransformType, 
            FixedImageType, MovingImageType > TransformInitializerType;
    TransformInitializerType::Pointer initializer = TransformInitializerType::New();
    initializer->DebugOn();

    // Set fixed and moving landmarks
    TransformInitializerType::LandmarkPointContainer fixedLandmarks;
    TransformInitializerType::LandmarkPointContainer movingLandmarks;
    TransformInitializerType::LandmarkPointType point;
    TransformInitializerType::LandmarkPointType tmp;
    
    // Moving Landmarks = Fixed Landmarks rotated by 'angle' degrees and then
    //    translated by the 'translation'. Offset can be used to move the fixed 
    //    landmarks around.
    double angle = 10 * M_PI/180;
    TransformInitializerType::LandmarkPointType translation;
    translation[0] = 6;
    translation[1] = 10;
    TransformInitializerType::LandmarkPointType offset;
    offset[0] = 10;
    offset[1] = 1;
    
    point[0]=2 + offset[0];
    point[1]=2 + offset[1];
    fixedLandmarks.push_back(point);
    tmp = point;
    point[0] = cos(angle)*point[0] - sin(angle)*point[1] + translation[0];
    point[1] = sin(angle)*tmp[0] + cos(angle)*point[1] + translation[1];
    movingLandmarks.push_back(point);
    point[0]=2 + offset[0];
    point[1]=-2 + offset[1];
    fixedLandmarks.push_back(point);
    tmp = point;
    point[0] = cos(angle)*point[0] - sin(angle)*point[1] + translation[0];
    point[1] = sin(angle)*tmp[0] + cos(angle)*point[1] + translation[1];
    movingLandmarks.push_back(point);
    point[0]=-2 + offset[0];
    point[1]=2 + offset[1];
    fixedLandmarks.push_back(point);
    tmp = point;
    point[0] = cos(angle)*point[0] - sin(angle)*point[1] + translation[0];
    point[1] = sin(angle)*tmp[0] + cos(angle)*point[1] + translation[1];
    movingLandmarks.push_back(point);
    point[0]=-2 + offset[0];
    point[1]=-2 + offset[1];
    fixedLandmarks.push_back(point);
    tmp = point;
    point[0] = cos(angle)*point[0] - sin(angle)*point[1] + translation[0];
    point[1] = sin(angle)*tmp[0] + cos(angle)*point[1] + translation[1];
    movingLandmarks.push_back(point);
    
    initializer->SetFixedLandmarks(fixedLandmarks);
    initializer->SetMovingLandmarks(movingLandmarks);


    initializer->SetFixedImage( fixedImage );
    initializer->SetMovingImage( movingImage );
    initializer->SetTransform( transform );
    initializer->InitializeTransform();

    // Transform the landmarks now. For the given set of landmarks, since we computed the
    // moving landmarks explicitly from the rotation and translation specified, we should 
    // get a transform that does not give any mismatch. In other words, if the fixed
    // landmarks are transformed by the transform computed by the 
    // LandmarkBasedTransformInitializer, they should coincide exactly with the moving
    // landmarks. Note that we specified 4 landmarks, although two 
    // landmarks is sufficient to guarantee a solution.
    //
    TransformInitializerType::PointsContainerConstIterator 
      fitr = fixedLandmarks.begin();
    TransformInitializerType::PointsContainerConstIterator 
      mitr = movingLandmarks.begin();

    typedef TransformInitializerType::OutputVectorType  OutputVectorType;
    OutputVectorType error;
    OutputVectorType::RealValueType tolerance = 0.1;
    bool failed = false;
    
    while( mitr != movingLandmarks.end() )
      {
      std::cout << "  Fixed Landmark: " << *fitr << " Moving landmark " << *mitr 
        << " Transformed fixed Landmark : " << 
        transform->TransformPoint( *fitr ) << std::endl;
      
      error = *mitr - transform->TransformPoint( *fitr);
      if( error.GetNorm() > tolerance )
        {
        failed = true;
        }
      
      ++mitr;
      ++fitr;
      }
    
    if( failed )
      {
      // Hang heads in shame
      std::cout << "  Fixed landmarks transformed by the transform did not match closely "
        << " enough with the moving landmarks.  The transform computed was: ";
      transform->Print(std::cout);
      std::cout << "[FAILED]" << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cout << "  Landmark alignment using Rigid2D transform [PASSED]" << std::endl;
      } 
  }
    
  
  return EXIT_SUCCESS;
}

