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
  
  if(m_Size[0] == 0)
  {
    region.SetSize( size );
  }
  else
  {
    region.SetSize( m_Size );
  }
  region.SetIndex( index );

  OutputImage->SetLargestPossibleRegion( region);// 
  OutputImage->SetBufferedRegion( region );  // set the region 
  OutputImage->SetRequestedRegion( region );         //                                                                       
  OutputImage->SetSpacing(InputObject->GetSpacing());   // set spacing
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
    it.Set(val);
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
}



} // end namespace itk

#endif
