
#ifndef __itkFlatStructuringElement_txx
#define __itkFlatStructuringElement_txx

#include "itkFlatStructuringElement.h"
#include <math.h>
#include <vector>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkFloodFilledSpatialFunctionConditionalIterator.h"
#include "itkEllipsoidInteriorExteriorSpatialFunction.h" 


namespace itk
{


template<unsigned int VDimension>
FlatStructuringElement<VDimension> FlatStructuringElement<VDimension>
::Box(RadiusType radius)
{
  // this should work for any number of dimensions
  FlatStructuringElement res = FlatStructuringElement();
  // res.m_Decomposable = true;
  res.SetRadius( radius );

//  res.ComputeBufferFromLines();
   Iterator kernel_it;
   for( kernel_it=res.Begin(); kernel_it != res.End(); ++kernel_it )
     {
     *kernel_it= true;
     }

  return(res);
}



template<unsigned int VDimension>
FlatStructuringElement<VDimension> FlatStructuringElement<VDimension>
::Ball(RadiusType radius)
{
  FlatStructuringElement res = FlatStructuringElement();
  res.SetRadius( radius );
  // res.m_Decomposable = false;

  unsigned int i;
  
  // Image typedef
  typedef Image<bool, VDimension> ImageType;

  // Create an image to hold the ellipsoid
  //
  typename ImageType::Pointer sourceImage = ImageType::New();
  typename ImageType::RegionType region;
  RadiusType size = radius;
  for( int i=0; i<VDimension; i++ )
    {
    size[i] = 2*size[i] + 1;
    }
  region.SetSize( size );

  sourceImage->SetRegions( region );
  sourceImage->Allocate();
  // sourceImage->Print( std::cout );

  // Set the background to be zero
  //
  ImageRegionIterator<ImageType> it(sourceImage, region);

  for(it.GoToBegin(); !it.IsAtEnd(); ++it)
    {
    it.Set(false);
    }

  
  // Create the ellipsoid
  //

  // Ellipsoid spatial function typedef
  typedef EllipsoidInteriorExteriorSpatialFunction<VDimension> EllipsoidType;
  
  // Create an ellipsoid spatial function for the source image
  typename EllipsoidType::Pointer spatialFunction = EllipsoidType::New();

  // Define and set the axes lengths for the ellipsoid
  typename EllipsoidType::InputType axes;
  for (i=0; i < VDimension; i++)
    {
    axes[i] = res.GetSize(i);
    }
  spatialFunction->SetAxes( axes );

  // Define and set the center of the ellipsoid in physical space
  typename EllipsoidType::InputType center;
  for (i=0; i < VDimension; i++)
    {
    // put the center of ellipse in the middle of the center pixel
    center[i] = res.GetRadius(i) + 0.5; 
    }
  spatialFunction->SetCenter( center );

  // Define the orientations of the ellipsoid axes, for now, we'll use
  // the identify matrix
  typename EllipsoidType::OrientationType orientations;
  orientations.fill( 0.0 );
  orientations.fill_diagonal( 1.0 );
  spatialFunction->SetOrientations(orientations);

  typename ImageType::IndexType seed;
  for (i=0; i < VDimension; i++)
    {
    seed[i] = res.GetRadius(i);
    }
  FloodFilledSpatialFunctionConditionalIterator<ImageType, EllipsoidType> 
    sfi = FloodFilledSpatialFunctionConditionalIterator<ImageType,
    EllipsoidType>(sourceImage, spatialFunction, seed);
  sfi.SetCenterInclusionStrategy();
  
  // Iterate through the entire image and set interior pixels to 1
  for(; !sfi.IsAtEnd(); ++sfi)
    {
    sfi.Set(true);
    }

  
  // Copy the ellipsoid into the kernel
  //
  Iterator kernel_it;
  for (it.GoToBegin(), kernel_it=res.Begin();!it.IsAtEnd();++it,++kernel_it)
    {
    *kernel_it = it.Get();
    }

  // Clean up
  //   ...temporary image should be cleaned up by SmartPointers automatically

  return res;
}


template<unsigned int VDimension>
template< class ImageType >
FlatStructuringElement<VDimension>
FlatStructuringElement<VDimension>::
FromImage(const typename ImageType::Pointer image, typename ImageType::PixelType foreground)
{
  image->Update();
  RadiusType size = image->GetLargestPossibleRegion().GetSize();
  Index< VDimension > centerIdx;
  for( int i=0; i<VDimension; i++ )
    {
    // TODO: throw an exception if size is not odd
    size[i] = size[i] / 2;
    centerIdx[i] = size[i];
    }
  FlatStructuringElement res = FlatStructuringElement();
  res.SetRadius( size );
//   res.m_Decomposable = false;

  for(int j=0; j<res.Size(); j++ )
    {
    res[j] = image->GetPixel( centerIdx + res.GetOffset( j ) );
    }

  return res;
}


template< unsigned int VDimension >
FlatStructuringElement<VDimension>
FlatStructuringElement<VDimension>::
FromImageUC(const typename Image<unsigned char, VDimension>::Pointer image, unsigned char foreground)
{
  return FromImage< Image<unsigned char, VDimension> >( image, foreground );
}


template< unsigned int VDimension >
FlatStructuringElement<VDimension>
FlatStructuringElement<VDimension>::
FromImageUC(const typename Image<unsigned char, VDimension>::Pointer image)
{
  return FromImage< Image<unsigned char, VDimension> >( image );
}



template<unsigned int VDimension>
template< class ImageType >
typename ImageType::Pointer
FlatStructuringElement<VDimension>::
GetImage(typename ImageType::PixelType foreground, typename ImageType::PixelType background)
{
  typename ImageType::Pointer image = ImageType::New();
  typename ImageType::RegionType region;
  RadiusType size = this->GetRadius();
  Index< VDimension > centerIdx;
  for( int i=0; i<VDimension; i++ )
    {
    centerIdx[i] = size[i];
    size[i] = 2*size[i] + 1;
    }
  region.SetSize( size );
  image->SetRegions( region );
  image->Allocate();

  // std::cout << this->GetRadius() << std::endl;
  // image->Print( std::cout );

  for(int j=0; j<this->Size(); j++ )
    {
    if( this->GetElement( j ) )
      {
      image->SetPixel( centerIdx+this->GetOffset( j ), foreground );
      }
    else
      {
      image->SetPixel( centerIdx+this->GetOffset( j ), background );
      }
    }

  return image;

}


template<unsigned int VDimension>
typename Image<unsigned char, VDimension>::Pointer
FlatStructuringElement<VDimension>::
GetImageUC( unsigned char foreground, unsigned char background)
{
  return GetImage< Image<unsigned char, VDimension> >( foreground, background );
}


template<unsigned int VDimension>
typename Image<unsigned char, VDimension>::Pointer
FlatStructuringElement<VDimension>::
GetImageUC()
{
  return GetImage< Image<unsigned char, VDimension> >();
}


}

#endif
