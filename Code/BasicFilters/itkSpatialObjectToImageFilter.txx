/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkSpatialObjectToImageFilter.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkSpatialObjectToImageFilter_txx
#define _itkSpatialObjectToImageFilter_txx

#include "itkSpatialObjectToImageFilter.h"
#include <itkImageRegionIteratorWithIndex.h>

namespace itk
{

/** Constructor */
template <class TInputSpatialObject, class TOutputImage>
SpatialObjectToImageFilter<TInputSpatialObject,TOutputImage>
::SpatialObjectToImageFilter()
{
  this->SetNumberOfRequiredInputs(1);
  m_ChildrenDepth = 1;
  m_Size.Fill(0);
  
  m_Spacing[0] = 1.0;
  m_Spacing[1] = 1.0;
  m_Spacing[2] = 1.0;

  m_Origin[0] = 0;
  m_Origin[1] = 0;
  m_Origin[2] = 0;

  m_InsideValue = 0;
  m_OutsideValue = 0;
}

/** Destructor */
template <class TInputSpatialObject, class TOutputImage>
SpatialObjectToImageFilter<TInputSpatialObject,TOutputImage>
::~SpatialObjectToImageFilter()
{
}
  

/** Set the Input SpatialObject */
template <class TInputSpatialObject, class TOutputImage>
void 
SpatialObjectToImageFilter<TInputSpatialObject,TOutputImage>
::SetInput(const InputSpatialObjectType *input)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(0, 
                                   const_cast< InputSpatialObjectType * >( input ) );
}


/** Connect one of the operands  */
template <class TInputSpatialObject, class TOutputImage>
void
SpatialObjectToImageFilter<TInputSpatialObject,TOutputImage>
::SetInput( unsigned int index, const TInputSpatialObject * object ) 
{
  if( index+1 > this->GetNumberOfInputs() )
    {
    this->SetNumberOfRequiredInputs( index + 1 );
    }
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(index, 
                                   const_cast< TInputSpatialObject *>( object ) );
}



/** Get the input Spatial Object */
template <class TInputSpatialObject, class TOutputImage>
const typename SpatialObjectToImageFilter<TInputSpatialObject,TOutputImage>::InputSpatialObjectType *
SpatialObjectToImageFilter<TInputSpatialObject,TOutputImage>
::GetInput(void) 
{
  if (this->GetNumberOfInputs() < 1)
    {
    return 0;
    }
  
  return static_cast<const TInputSpatialObject * >
    (this->ProcessObject::GetInput(0) );
}
  
/** Get the input Spatial Object */
template <class TInputSpatialObject, class TOutputImage>
const typename SpatialObjectToImageFilter<TInputSpatialObject,TOutputImage>::InputSpatialObjectType *
SpatialObjectToImageFilter<TInputSpatialObject,TOutputImage>
::GetInput(unsigned int idx)
{
  return static_cast< const TInputSpatialObject * >
    (this->ProcessObject::GetInput(idx));
}

//----------------------------------------------------------------------------
template <class TInputSpatialObject, class TOutputImage>
void 
SpatialObjectToImageFilter<TInputSpatialObject,TOutputImage>
::SetSpacing(const double spacing[OutputImageDimension] )
{
  unsigned int i; 
  for (i=0; i<OutputImageDimension; i++)
    {
    if ( spacing[i] != m_Spacing[i] )
      {
      break;
      }
    } 
  if ( i < OutputImageDimension ) 
    { 
    for (i=0; i<OutputImageDimension; i++)
      {
      m_Spacing[i] = spacing[i];
      }
    }
}

template <class TInputSpatialObject, class TOutputImage>
void 
SpatialObjectToImageFilter<TInputSpatialObject,TOutputImage>
::SetSpacing(const float spacing[OutputImageDimension] )
{
  unsigned int i; 
  for (i=0; i<OutputImageDimension; i++)
    {
    if ( (double)spacing[i] != m_Spacing[i] )
      {
      break;
      }
    } 
  if ( i < OutputImageDimension ) 
    { 
    for (i=0; i<OutputImageDimension; i++)
      {
      m_Spacing[i] = spacing[i];
      }
    }
}

template <class TInputSpatialObject, class TOutputImage>
const double * 
SpatialObjectToImageFilter<TInputSpatialObject,TOutputImage>
::GetSpacing() const
{
  return m_Spacing;
}

//----------------------------------------------------------------------------
template <class TInputSpatialObject, class TOutputImage>
void 
SpatialObjectToImageFilter<TInputSpatialObject,TOutputImage>
::SetOrigin(const double origin[OutputImageDimension] )
{
  unsigned int i; 
  for (i=0; i<OutputImageDimension; i++)
    {
    if ( origin[i] != m_Origin[i] )
      {
      break;
      }
    } 
  if ( i < OutputImageDimension ) 
    { 
    for (i=0; i<OutputImageDimension; i++)
      {
      m_Origin[i] = origin[i];
      }
    }
}

template <class TInputSpatialObject, class TOutputImage>
void 
SpatialObjectToImageFilter<TInputSpatialObject,TOutputImage>
::SetOrigin(const float origin[OutputImageDimension] )
{
  unsigned int i; 
  for (i=0; i<OutputImageDimension; i++)
    {
    if ( (double)origin[i] != m_Origin[i] )
      {
      break;
      }
    } 
  if ( i < OutputImageDimension ) 
    { 
    for (i=0; i<OutputImageDimension; i++)
      {
      m_Origin[i] = origin[i];
      }
    }
}

template <class TInputSpatialObject, class TOutputImage>
const double * 
SpatialObjectToImageFilter<TInputSpatialObject,TOutputImage>
::GetOrigin() const
{
  return m_Origin;
}

//----------------------------------------------------------------------------

/** Update */
template <class TInputSpatialObject, class TOutputImage>
void
SpatialObjectToImageFilter<TInputSpatialObject,TOutputImage>
::GenerateData(void)
{
  itkDebugMacro(<< "SpatialObjectToImageFilter::Update() called");

  // Get the input and output pointers 
  const InputSpatialObjectType * InputObject  = this->GetInput();
  OutputImagePointer   OutputImage = this->GetOutput();

  // Generate the image
  double origin[ObjectDimension];
  SizeType size;

  for(unsigned int i=0;i<ObjectDimension;i++)
    {
    size[i] = (long unsigned int)(InputObject->GetBoundingBox()->GetMaximum()[i]
                                  - InputObject->GetBoundingBox()->GetMinimum()[i]);
    origin[i]=0;
    }
  
  typename OutputImageType::IndexType index;
  index.Fill(0);
  typename OutputImageType::RegionType region;
  
  // If the size of the output has been explicitly specified, the filter
  // will set the output size to the explicit size, otherwise the size from the spatial
  // object's bounding box will be used as default.

  if(   m_Size[0] != 0
        ||  m_Size[1] != 0
        ||  m_Size[2] != 0 )
    {
    region.SetSize( m_Size );
    }
  else
    {
    region.SetSize( size );
    }
  region.SetIndex( index );

  OutputImage->SetLargestPossibleRegion( region);     // 
  OutputImage->SetBufferedRegion( region );           // set the region 
  OutputImage->SetRequestedRegion( region );          //                                                                       
  
  // If the spacing has been explicitly specified, the filter
  // will set the output spacing to that explicit spacing, otherwise the spacing from
  // the spatial object is used as default.
  
  if(   m_Spacing[0] != 1.0
        ||  m_Spacing[1] != 1.0
        ||  m_Spacing[2] != 1.0 )
    {
    OutputImage->SetSpacing(this->m_Spacing);         // set spacing
    }
  else
    {
    OutputImage->SetSpacing(InputObject->GetIndexToObjectTransform()->GetScaleComponent());   // set spacing
    }
  OutputImage->SetOrigin(origin);   //   and origin
  OutputImage->Allocate();   // allocate the image                            

  typedef itk::ImageRegionIteratorWithIndex<OutputImageType> myIteratorType;

  myIteratorType it(OutputImage,region);

  itk::Point<double,ObjectDimension> point;

  while(!it.IsAtEnd())
    {
    for(unsigned int i=0;i<ObjectDimension;i++)
      {
      point[i]=it.GetIndex()[i];
      }
    double val =0;
    InputObject->ValueAt(point,val,99999);
    if(   m_InsideValue != 0 
          ||  m_OutsideValue != 0 )
      {
      if( val )
        {
        it.Set(m_InsideValue);
        }
      else
        {
        it.Set(m_OutsideValue);
        }
      }
    else
      {
      it.Set(static_cast<ValueType>(val));
      }
    ++it;
    }
  
  itkDebugMacro(<< "SpatialObjectToImageFilter::Update() finished");

} // end update function  


template<class TInputSpatialObject, class TOutputImage>
void 
SpatialObjectToImageFilter<TInputSpatialObject,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Size : " << m_Size << std::endl;
  os << indent << "Children depth : " << m_ChildrenDepth << std::endl;
  os << indent << "Inside Value : " << m_InsideValue << std::endl;
  os << indent << "Outside Value : " << m_OutsideValue << std::endl;
}



} // end namespace itk

#endif
