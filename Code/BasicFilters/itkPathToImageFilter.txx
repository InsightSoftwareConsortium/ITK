/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkPathToImageFilter.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkPathToImageFilter_txx
#define __itkPathToImageFilter_txx

#include "itkPathToImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkPathIterator.h"

namespace itk
{

/** Constructor */
template <class TInputPath, class TOutputImage>
PathToImageFilter<TInputPath,TOutputImage>
::PathToImageFilter()
{
  this->SetNumberOfRequiredInputs(1);
  m_Size.Fill(0);
  
  for (unsigned int i = 0; i < OutputImageDimension; i++)
    {
    // Set an image spacing for the user
    m_Spacing[i] = 1.0;
    m_Origin[i] = 0;
    }

  m_PathValue = 0;
  m_BackgroundValue = 0;
}

/** Destructor */
template <class TInputPath, class TOutputImage>
PathToImageFilter<TInputPath,TOutputImage>
::~PathToImageFilter()
{
}
  

/** Set the Input SpatialObject */
template <class TInputPath, class TOutputImage>
void
PathToImageFilter<TInputPath,TOutputImage>
::SetInput(const InputPathType *input)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(0, const_cast< InputPathType * >( input ) );
}


/** Connect one of the operands  */
template <class TInputPath, class TOutputImage>
void
PathToImageFilter<TInputPath,TOutputImage>
::SetInput( unsigned int index, const InputPathType * path ) 
{
  if( index+1 > this->GetNumberOfInputs() )
    {
    this->SetNumberOfRequiredInputs( index + 1 );
    }
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(index, const_cast< InputPathType *>(path) );
}



/** Get the input Path */
template <class TInputPath, class TOutputImage>
const typename PathToImageFilter<TInputPath,TOutputImage>::InputPathType *
PathToImageFilter<TInputPath,TOutputImage>
::GetInput(void) 
{
  if (this->GetNumberOfInputs() < 1)
    {
    return 0;
    }
  
  return static_cast<const TInputPath * >
    (this->ProcessObject::GetInput(0) );
}
  
/** Get the input Path */
template <class TInputPath, class TOutputImage>
const typename PathToImageFilter<TInputPath,TOutputImage>::InputPathType *
PathToImageFilter<TInputPath,TOutputImage>
::GetInput(unsigned int idx)
{
  return static_cast< const TInputPath * >
    (this->ProcessObject::GetInput(idx));
}

//----------------------------------------------------------------------------
template <class TInputPath, class TOutputImage>
void
PathToImageFilter<TInputPath,TOutputImage>
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

template <class TInputPath, class TOutputImage>
void
PathToImageFilter<TInputPath,TOutputImage>
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

template <class TInputPath, class TOutputImage>
const double * 
PathToImageFilter<TInputPath,TOutputImage>
::GetSpacing() const
{
  return m_Spacing;
}

//----------------------------------------------------------------------------
template <class TInputPath, class TOutputImage>
void
PathToImageFilter<TInputPath,TOutputImage>
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

template <class TInputPath, class TOutputImage>
void
PathToImageFilter<TInputPath,TOutputImage>
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

template <class TInputPath, class TOutputImage>
const double * 
PathToImageFilter<TInputPath,TOutputImage>
::GetOrigin() const
{
  return m_Origin;
}

//----------------------------------------------------------------------------

template <class TInputPath, class TOutputImage>
void
PathToImageFilter<TInputPath,TOutputImage>
::GenerateData(void)
{
  unsigned int i;
  itkDebugMacro(<< "PathToImageFilter::GenerateData() called");

  // Get the input and output pointers 
  const InputPathType * InputPath   = this->GetInput();
  OutputImagePointer    OutputImage = this->GetOutput();

  // Generate the image
  double origin[OutputImageDimension];
  SizeType size;

  for(i=0;i<OutputImageDimension;i++)
    {
    // Set Image size to the size of the path's bounding box
    //size[i] = (long unsigned int)(InputObject->GetBoundingBox()->GetMaximum()[i]
    //                              - InputObject->GetBoundingBox()->GetMinimum()[i]);
    size[i]=0;
    origin[i]=0;
    }
  
  typename OutputImageType::IndexType index;
  index.Fill(0);
  typename OutputImageType::RegionType region;
  
  // If the size of the output has been explicitly specified, the filter
  // will set the output size to the explicit size, otherwise the size from the spatial
  // paths's bounding box will be used as default.

  bool specified = false;
  for (i = 0; i < OutputImageDimension; i++)
    {
    if (m_Size[i] != 0)
      {
      specified = true;
      break;
      }
    }

  if (specified)
    {
    region.SetSize( m_Size );
    }
  else
    {
    itkExceptionMacro( << "Currently, the user MUST specify an image size" )
    //region.SetSize( size );
    }
  region.SetIndex( index );

  OutputImage->SetLargestPossibleRegion( region);     // 
  OutputImage->SetBufferedRegion( region );           // set the region 
  OutputImage->SetRequestedRegion( region );          //                                                                       
  
  // If the spacing has been explicitly specified, the filter
  // will set the output spacing to that explicit spacing, otherwise the spacing from
  // the spatial object is used as default.
  
  specified = false;
  for (i = 0; i < OutputImageDimension; i++)
    {
    if (m_Spacing[i] != 0)
      {
      specified = true;
      break;
      }
    }

  if (specified)
    {
    OutputImage->SetSpacing(this->m_Spacing);         // set spacing
    }
  else
    {
    itkExceptionMacro( << "Currently, the user MUST specify an image spacing" )
    //OutputImage->SetSpacing(InputObject->GetIndexToObjectTransform()->GetScaleComponent());   // set spacing
    }
  OutputImage->SetOrigin(origin);   //   and origin
  OutputImage->Allocate();   // allocate the image                            

  ImageRegionIteratorWithIndex<OutputImageType> imageIt(OutputImage,region);
  for( imageIt.GoToBegin(); !imageIt.IsAtEnd(); ++imageIt )
    {
    imageIt.Set(m_BackgroundValue);
    }
  
  PathIterator<OutputImageType,InputPathType> pathIt(OutputImage,InputPath);
  for( pathIt.GoToBegin(); !pathIt.IsAtEnd(); ++pathIt )
    {
    pathIt.Set( m_PathValue );
    }
  
  itkDebugMacro(<< "PathToImageFilter::GenerateData() finished");

} // end update function  


template <class TInputPath, class TOutputImage>
void
PathToImageFilter<TInputPath,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Size : " << m_Size << std::endl;
  os << indent << "Path Value : " << m_PathValue << std::endl;
  os << indent << "Background Value : " << m_BackgroundValue << std::endl;
}



} // end namespace itk

#endif
