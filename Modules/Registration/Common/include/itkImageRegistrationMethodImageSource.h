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
#ifndef itkImageRegistrationMethodImageSource_h
#define itkImageRegistrationMethodImageSource_h
#include "itkImage.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkCommandIterationUpdate.h"
#include "itkOptimizerParameters.h"

/**
 *  This class generates two 2D-Gaussians (standard deviation RegionSize/2)
 *  One is shifted by 7,3 pixels from the other.
 *
 *  These two images are provided to the registration methods in this
 *  directory in order to obtain a uniform test input data.
 *
 *  Therefore the solution of the registration is |-7 -3|
 */
namespace itk
{

namespace testhelper
{

template< typename TFixedPixelType,
          typename TMovingPixelType,
          unsigned int NDimension     >
class ImageRegistrationMethodImageSource : public itk::Object
{
public:

  typedef ImageRegistrationMethodImageSource    Self;
  typedef Object                                Superclass;
  typedef SmartPointer<Self>                    Pointer;
  typedef SmartPointer<const Self>              ConstPointer;
  typedef OptimizerParameters<double>           ParametersType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(Image, Object);


  typedef itk::Image<TMovingPixelType,NDimension> MovingImageType;
  typedef itk::Image<TFixedPixelType,NDimension > FixedImageType;

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

  /* Fill images with a 2D gaussian*/
  typedef  itk::ImageRegionIteratorWithIndex<MovingImageType> MovingImageIteratorType;

  typedef  itk::ImageRegionIteratorWithIndex<FixedImageType> FixedImageIteratorType;


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
    const double value = 200.0 * std::exp( - ( x*x + y*y )/(s*s) );
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
    const double value = 200.0 * std::exp( - ( x*x + y*y )/(s*s) );
    ti.Set( static_cast<typename FixedImageType::PixelType>(value) );
    ++ti;
  }


}

protected:

ImageRegistrationMethodImageSource()
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

}  // end namespace testhelper

}  // end namespace itk
#endif
