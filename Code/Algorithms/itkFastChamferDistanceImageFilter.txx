/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFastChamferDistanceImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkFastChamferDistanceImageFilter_txx
#define _itkFastChamferDistanceImageFilter_txx

#include <iostream>

#include "itkFastChamferDistanceImageFilter.h"
#include <itkNeighborhoodIterator.h>
#include <itkImageRegionIterator.h>
#include <itkImageRegionConstIterator.h>

namespace itk
{

template <class TInputImage,class TOutputImage>
FastChamferDistanceImageFilter<TInputImage,TOutputImage>
::FastChamferDistanceImageFilter()
{
  unsigned int i;
  unsigned int dim = ImageDimension;

  switch (ImageDimension)  
    {
    // Note the fall through the cases to set all the components
    case 3:
      m_Weights[--dim] = 1.65849;
    case 2:
      m_Weights[--dim] = 1.34065;
    case 1:
      m_Weights[--dim] = 0.92644;
      break;
    default:
      itkWarningMacro(<< "Dimension " << ImageDimension << " with Default weights ");
      for( i=1; i <= ImageDimension; i++ )
        {
        m_Weights[i-1] = vcl_sqrt((float)i);
        }
    }

  m_MaximumDistance = 10.0;
  m_NarrowBand = 0;
}

template <class TInputImage,class TOutputImage>
void FastChamferDistanceImageFilter<TInputImage,TOutputImage>
::GenerateDataND() 
{
  const int SIGN_MASK = 1;
  const int INNER_MASK = 2;
  
  typename NeighborhoodIterator<TInputImage>::RadiusType r;
  bool in_bounds;
  
  r.Fill(1);
  NeighborhoodIterator<TInputImage> it(r, this->GetOutput(), m_RegionToProcess);
  
  const unsigned int  center_voxel = it.Size()/2;
  int *neighbor_type;
  neighbor_type = new int[ it.Size() ];
  int i;
  unsigned int n;
  float val[ImageDimension];
  PixelType  center_value;
  int neighbor_start,neighbor_end;
  BandNodeType node;
  
  /** 1st Scan , using neighbors from center_voxel+1 to it.Size()-1 */

  /** Precomputing the neighbor types */ 
  neighbor_start = center_voxel + 1;
  neighbor_end   = it.Size() - 1;

  for ( i = neighbor_start; i <= neighbor_end; i++)
    {
      neighbor_type[i] = -1;
      for( n = 0; n < ImageDimension; n++ ) 
        {
        neighbor_type[i] += (it.GetOffset(i)[n] != 0);
        }
    }
  
  /** Scan the image */
  for ( it.GoToBegin(); ! it.IsAtEnd(); ++it )
    {
    center_value = it.GetPixel(center_voxel);
    if (center_value>= m_MaximumDistance)
      {
      continue;
      }
    if (center_value<= -m_MaximumDistance)
      {
      continue;
      }
    /** Update Positive Distance */
    if (center_value>-m_Weights[0])
      {
      for(n=0; n<ImageDimension; n++)
        {
        val[n]=center_value+m_Weights[n];
        }
      for (i = neighbor_start; i <= neighbor_end; i++)
        {
        // Experiment an InlineGetPixel()
        if (val[neighbor_type[i]]<it.GetPixel(i)) 
          {
          it.SetPixel(i,val[neighbor_type[i]],in_bounds);
          }
        }
      }
    /** Update Negative Distance */
    if (center_value<m_Weights[0])
      {
      for(n=0; n<ImageDimension; n++)
        {
        val[n]=center_value-m_Weights[n];
        }
      
      for (i = neighbor_start; i <= neighbor_end; i++)
        {
        // Experiment an InlineGetPixel()
        if (val[neighbor_type[i]]>it.GetPixel(i)) 
          {
          it.SetPixel(i,val[neighbor_type[i]],in_bounds);
          }
        }
      }
    }          
  
  /** 2nd Scan , using neighbors from 0 to center_voxel-1 */

  /*Clear the NarrowBand if it has been assigned */
  if (m_NarrowBand.IsNotNull()) 
    {
    m_NarrowBand->Clear();
    } 
  
  /** Precomputing the neighbor neighbor types */ 
  neighbor_start = 0;
  neighbor_end   = center_voxel-1;
  
  for( i = neighbor_start; i <= neighbor_end; i++ )
    {
    neighbor_type[i] = -1;
    for (n=0; n < ImageDimension; n++)
      {
      neighbor_type[i] += (it.GetOffset(i)[n] != 0);
      }
    }
  
  
  /** Scan the image */
  for (it.GoToEnd(), --it; ! it.IsAtBegin(); --it)
    {
    center_value = it.GetPixel(center_voxel);
    if (center_value>= m_MaximumDistance)
      {
      continue;
      }
    if (center_value<= -m_MaximumDistance)
      {
      continue;
      }
    
    // Update the narrow band 
    if (m_NarrowBand.IsNotNull()) {
    if (fabs((float)center_value) <= m_NarrowBand->GetTotalRadius()) 
      {
      node.m_Index = it.GetIndex();
      //Check node state.
      node.m_NodeState = 0;         
      if (center_value>0)
        {
        node.m_NodeState += SIGN_MASK;
        }
      if (fabs((float)center_value) < m_NarrowBand->GetInnerRadius())  
        {
        node.m_NodeState += INNER_MASK;
        }
      m_NarrowBand->PushBack(node);
      }
    }
    
    /** Update Positive Distance */
    if (center_value>-m_Weights[0])
      {
      for(n=0; n<ImageDimension; n++)
        {
        val[n]=center_value+m_Weights[n];
        }
      for (i = neighbor_start; i <= neighbor_end; i++)
        {
        // Experiment an InlineGetPixel()
        if (val[neighbor_type[i]]<it.GetPixel(i)) 
          {
          it.SetPixel(i,val[neighbor_type[i]],in_bounds);
          }
        }
      }
    
    /** Update Negative Distance */
    if (center_value<m_Weights[0])
      {
      for(n=0; n<ImageDimension; n++)
        {
        val[n]=center_value-m_Weights[n];
        }
      
      for (i = neighbor_start; i <= neighbor_end; i++)
        {
        // Experiment an InlineGetPixel()
        if ( val[neighbor_type[i]] > it.GetPixel(i) ) 
          {
          it.SetPixel(i,val[neighbor_type[i]],in_bounds);
          }
        }
      }
    }
  delete [] neighbor_type;
}


template <class TInputImage,class TOutputImage>
void 
FastChamferDistanceImageFilter<TInputImage,TOutputImage>
::GenerateData() 
{

  // Allocate the output image.
  typename TOutputImage::Pointer output = this->GetOutput();
  
  output->SetBufferedRegion(output->GetRequestedRegion());
  output->Allocate();
  
  ImageRegionIterator<TOutputImage>
    out(this->GetOutput(),this->GetInput()->GetRequestedRegion());
  ImageRegionConstIterator<TOutputImage>
    in( this->GetInput(), this->GetInput()->GetRequestedRegion());
  
  for(in.GoToBegin(),out.GoToBegin(); !in.IsAtEnd(); ++in,++out)
    {
    out.Set(in.Get());
    }
  
  m_RegionToProcess = this->GetInput()->GetRequestedRegion();

  //If the NarrowBand has been set, we update m_MaximumDistance using
  //narrowband TotalRadius plus a margin of 1 pixel.
  if ( m_NarrowBand.IsNotNull() )
    {
    m_MaximumDistance = m_NarrowBand->GetTotalRadius() + 1;
    }
  
  this->GenerateDataND();
  
} // end GenerateData()

template <class TInputImage,class TOutputImage>
void 
FastChamferDistanceImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  unsigned int i;
  Superclass::PrintSelf(os,indent);
  
  for ( i = 0; i < ImageDimension; i++ ) 
    {
    os << indent << "Chamfer weight " << i << ": " << m_Weights[i] << std::endl;
    }
  
  os << indent << "Maximal computed distance   : " << m_MaximumDistance << std::endl;
}

} // end namespace itk

#endif
