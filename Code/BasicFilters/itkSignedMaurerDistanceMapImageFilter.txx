/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSignedMaurerDistanceMapImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSignedMaurerDistanceMapImageFilter_txx
#define __itkSignedMaurerDistanceMapImageFilter_txx

#include "itkSignedMaurerDistanceMapImageFilter.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkBinaryBallStructuringElement.h"
#include "itkBinaryDilateImageFilter.h"
#include "itkUnaryFunctorImageFilter.h"
#include "itkSubtractImageFilter.h"

#include "vnl/vnl_vector.h"
#include "vnl/vnl_math.h"

//Simple functor to invert an image for Outside Danielsson distance map
namespace itk
{
namespace Functor
{
template <class InputPixelType> 
class InvertBinaryIntensityFunctor
{
public:
  InputPixelType operator()( InputPixelType input )
    {
    if( input )
      {
      return NumericTraits<InputPixelType>::Zero;
      }
    else
      {
      return NumericTraits<InputPixelType>::One;
      }
    }
};
}
}


namespace itk
{


template<class TInputImage, class TOutputImage>
SignedMaurerDistanceMapImageFilter<TInputImage, TOutputImage>
::SignedMaurerDistanceMapImageFilter() : m_BackgroundValue( 0 ),
                                         m_InsideIsPositive( false ),
                                         m_UseImageSpacing( false ),
                                         m_SquaredDistance( true )
{
}


template<class TInputImage, class TOutputImage>
SignedMaurerDistanceMapImageFilter<TInputImage, TOutputImage>
::~SignedMaurerDistanceMapImageFilter()
{
}


template<class TInputImage, class TOutputImage>
void
SignedMaurerDistanceMapImageFilter<TInputImage, TOutputImage>
::GenerateData()
{

  this->GetOutput()->SetRegions(
                         this->GetInput()->GetRequestedRegion() );

  this->GetOutput()->Allocate();
  this->m_Spacing = this->GetOutput()->GetSpacing();

  typedef BinaryThresholdImageFilter<InputImageType, 
                                     InputImageType
                                        > BinaryFilterType;
  
  typename BinaryFilterType::Pointer binaryFilter = BinaryFilterType::New();

  binaryFilter->SetLowerThreshold( this->m_BackgroundValue );
  binaryFilter->SetUpperThreshold( this->m_BackgroundValue );
  binaryFilter->SetInsideValue( 0 );
  binaryFilter->SetOutsideValue( 1 );
  binaryFilter->SetInput( this->GetInput() );
  binaryFilter->Update();

  typedef Functor::InvertBinaryIntensityFunctor<InputPixelType>  FunctorType;

  typedef UnaryFunctorImageFilter< InputImageType,
                                   InputImageType,
                                   FunctorType >    InverterType;

  typename InverterType::Pointer inverter1 = InverterType::New();
  typename InverterType::Pointer inverter2 = InverterType::New();

  inverter1->SetInput( binaryFilter->GetOutput() );

  // Dilate the inverted image by 1 pixel to give it the same boundary
  // as the univerted this->GetInput().

  typedef BinaryBallStructuringElement<
                     InputPixelType,
                     InputImageDimension  > StructuringElementType;

  typedef BinaryDilateImageFilter<
                         InputImageType,
                         InputImageType,
                         StructuringElementType >     DilatorType;

  typename DilatorType::Pointer dilator = DilatorType::New();

  StructuringElementType structuringElement;
  structuringElement.SetRadius( 1 );
  structuringElement.CreateStructuringElement();
  dilator->SetKernel( structuringElement );
  dilator->SetDilateValue( 1 );
  dilator->SetInput( inverter1->GetOutput() );
  inverter2->SetInput( dilator->GetOutput() );

  typedef SubtractImageFilter<InputImageType,
                              InputImageType,
                              InputImageType > SubtracterType;

  typename SubtracterType::Pointer subtracter = SubtracterType::New();

  subtracter->SetInput1( binaryFilter->GetOutput() );
  subtracter->SetInput2( inverter2->GetOutput() );
  subtracter->Update();

  typedef ImageRegionConstIterator<InputImageType> InputIterator;

  InputIterator inIterator( subtracter->GetOutput(),
                            subtracter->GetOutput()->GetRequestedRegion() );

  typedef ImageRegionIterator<OutputImageType>  OutputIterator;

  OutputIterator outIterator( this->GetOutput(),
                              this->GetOutput()->GetRequestedRegion() );

  for (  inIterator.GoToBegin(), outIterator.GoToBegin();
        !inIterator.IsAtEnd();
        ++inIterator, ++outIterator )
    {
    if( inIterator.Get() )
      {
      outIterator.Set( NumericTraits< OutputPixelType >::Zero );
      }
    else
      {
      outIterator.Set( NumericTraits< OutputPixelType >::max() );
      }
    }

  vnl_vector<unsigned int> k(InputImageDimension-1);

  typedef typename InputImageType::RegionType   InputRegionType;
  typedef typename InputImageType::SizeType     InputSizeType;

  InputRegionType region = this->GetInput()->GetRequestedRegion();
  InputSizeType   size   = region.GetSize();


  for (unsigned int i = 0; i < InputImageDimension; i++)
    {
    OutputIndexType idx;
    unsigned int NumberOfRows = 1;
    for (unsigned int d = 0; d < InputImageDimension; d++)
      {
      idx[d] = 0;
      if( d != i )
        {
        NumberOfRows *= size[ d ];
        }
      }

    k[0] = 1;
    unsigned int count = 1;


    for (unsigned int d = i+2; d < i+InputImageDimension; d++)
      {
      k[ count ] = k[ count-1 ] * size[ d % InputImageDimension ];
      count++;
      }
    k.flip();

    unsigned int index;
    for (unsigned int n = 0; n < NumberOfRows;n++)
      {
      index = n;
      count = 0;
      for (unsigned int d = i+1; d < i+InputImageDimension; d++)
        {
        idx[ d % InputImageDimension ] =
             static_cast<unsigned int>(
                 static_cast<double>( index ) /
                 static_cast<double>( k[count] ) );

        index %= k[ count ];
        count++;
        }
      this->Voronoi(i, idx);
      }
    }

  if ( !this->m_SquaredDistance )
    {
    typedef ImageRegionIterator< OutputImageType > OutputIterator;
    typedef ImageRegionConstIterator< InputImageType  > InputIterator;

    typename OutputImageType::RegionType region =
                       this->GetOutput()->GetRequestedRegion();

    OutputIterator Ot( this->GetOutput(), region );
    InputIterator  It( this->GetInput(),  region );

    Ot.GoToBegin();
    It.GoToBegin();

    while( !Ot.IsAtEnd() )
      {

      const OutputPixelType outputValue =
        static_cast<OutputPixelType>(
          sqrt( vnl_math_abs( Ot.Get() ) ) );

      if( It.Get() != this->m_BackgroundValue )
        {
        if( this->GetInsideIsPositive() )
          {
          Ot.Set(  outputValue );
          }
        else
          {
          Ot.Set( -outputValue );
          }
        }
      else
        {
        if( this->GetInsideIsPositive() )
          {
          Ot.Set( -outputValue );
          }
        else
          {
          Ot.Set(  outputValue );
          }
        }

      ++Ot;
      ++It;
      }
    }

}


template<class TInputImage, class TOutputImage>
void
SignedMaurerDistanceMapImageFilter<TInputImage, TOutputImage>
::Voronoi(unsigned int d, OutputIndexType idx)
{
  typename OutputImageType::Pointer output(this->GetOutput());
  unsigned int nd = output->GetRequestedRegion().GetSize()[d];

  vnl_vector<OutputPixelType> g(nd);  g = 0;
  vnl_vector<OutputPixelType> h(nd);  h = 0;

  OutputPixelType di;

  int l = -1;

  for( unsigned int i = 0; i < nd; i++ )
    {
    idx[d] = i;

    di = output->GetPixel(idx);

    OutputPixelType iw;

    if( this->GetUseImageSpacing() )
      {
      iw = static_cast<OutputPixelType>( i * this->m_Spacing[d] );
      }
    else
      {
      iw  = static_cast<OutputPixelType>(i);
      }

    if( di != NumericTraits< OutputPixelType >::max() )
      {
      if( l < 1 )
        {
        l++;
        g(l) = di;
        h(l) = iw;
        }
      else
        {
        while( (l >= 1) &&
               this->Remove( g(l-1), g(l), di, h(l-1), h(l), iw) )
          {
          l--;
          }
        l++;
        g(l) = di;
        h(l) = iw;
        }
      }
    }

  if( l == -1 )
    {
    return;
    }

  int ns = l;

  l = 0;

  for( unsigned int i = 0; i < nd; i++ )
    {

    OutputPixelType iw;

    if( this->GetUseImageSpacing() )
      {
      iw = static_cast<OutputPixelType>( i * this->m_Spacing[d] );
      }
    else
      {
      iw = static_cast<OutputPixelType>( i );
      }

    OutputPixelType d1 = vnl_math_abs(g(l  )) + (h(l  )-iw)*(h(l  )-iw);
    OutputPixelType d2 = vnl_math_abs(g(l+1)) + (h(l+1)-iw)*(h(l+1)-iw);

    while( (l < ns) && (d1 > d2) )
      {
      l++;
      d1 = d2;
      d2 = vnl_math_abs(g(l+1)) + (h(l+1)-iw)*(h(l+1)-iw);
      }
    idx[d] = i;

    if ( this->GetInput()->GetPixel( idx ) != this->m_BackgroundValue )
      {
      if ( this->m_InsideIsPositive )
        {
        output->SetPixel( idx,  d1 );
        }
      else
        {
        output->SetPixel( idx, -d1 );
        }
      }
    else
      {
      if ( this->m_InsideIsPositive )
        {
        output->SetPixel( idx, -d1 );
        }
      else
        {
        output->SetPixel( idx,  d1 );
        }
      }
    }
}


template<class TInputImage, class TOutputImage>
bool
SignedMaurerDistanceMapImageFilter<TInputImage, TOutputImage>
::Remove( OutputPixelType d1, OutputPixelType d2, OutputPixelType df,
          OutputPixelType x1, OutputPixelType x2, OutputPixelType xf )
{
  OutputPixelType a = x2 - x1;
  OutputPixelType b = xf - x2;
  OutputPixelType c = xf - x1;

  return ( (   c * vnl_math_abs( d2 ) - b * vnl_math_abs( d1 )
             - a * vnl_math_abs( df ) - a * b * c ) > 0);
}


/**
 * Standard "PrintSelf" method
 */
template <class TInputImage, class TOutputImage>
void
SignedMaurerDistanceMapImageFilter<TInputImage, TOutputImage>
::PrintSelf( std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Background Value: "
     << this->m_BackgroundValue << std::endl;
  os << indent << "Spacing: "
     << this->m_Spacing << std::endl;
  os << indent << "Inside is positive: "
     << this->m_InsideIsPositive << std::endl;
  os << indent << "Use image spacing: "
     << this->m_UseImageSpacing << std::endl;
  os << indent << "Squared distance: "
     << this->m_SquaredDistance << std::endl;
}

} // end namespace itk

#endif
