/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVoronoiPartitioningImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkVoronoiPartitioningImageFilter_txx
#define _itkVoronoiPartitioningImageFilter_txx
#include "itkVoronoiPartitioningImageFilter.h"

#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRegionConstIteratorWithIndex.h"


namespace itk
{

/* constructor: seting the default value of the parameters */
template <class TInputImage, class TOutputImage>
VoronoiPartitioningImageFilter <TInputImage,TOutputImage>
::VoronoiPartitioningImageFilter()
  : m_SigmaThreshold( 10 )
{
}

/* destructor */
template <class TInputImage, class TOutputImage>
VoronoiPartitioningImageFilter <TInputImage,TOutputImage>
::~VoronoiPartitioningImageFilter()
{
}

template <class TInputImage, class TOutputImage>
void
VoronoiPartitioningImageFilter<TInputImage,TOutputImage>
::ClassifyDiagram(void)
{
  PointIdIterator currPit;
  PointIdIterator currPitEnd;
  PointType currP;
  PointTypeDeque VertList;
  IndexList PixelPool;

  this->m_NumberOfBoundary = 0;
  for(int i=0;i<this->GetNumberOfSeeds();i++)
    {
    CellAutoPointer currCell;
    this->m_WorkingVD->GetCellId(i,currCell);
    currPitEnd = currCell->PointIdsEnd();
    VertList.clear();
    for(currPit=currCell->PointIdsBegin();currPit!=currPitEnd;++currPit)
      {
      this->m_WorkingVD->GetPoint((*currPit),&(currP));
      VertList.push_back(currP);
      }

    PixelPool.clear();
    this->GetPixelIndexFromPolygon(VertList,&PixelPool);
    this->m_NumberOfPixels[i] = PixelPool.size();
    this->m_Label[i] = this->TestHomogeneity(PixelPool);

    // when partitioning the NumberOfBoundary is the number of regions that
    // are not homogeneous
    if (!this->m_Label[i])
      {
      this->m_NumberOfBoundary++;
      }
    }
}

template <class TInputImage, class TOutputImage>
void
VoronoiPartitioningImageFilter<TInputImage,TOutputImage>
::GenerateAddingSeeds(void)
{
  EdgeIterator eit;
  EdgeIterator eitend = this->m_WorkingVD->EdgeEnd();  
  PointType adds;
  Point<int,2> seeds;

  // Walk the edges of the diagram, if a seed to either side is
  // no homogeneous, then place a new seed along the middle of the edge
  for(eit = this->m_WorkingVD->EdgeBegin();eit != eitend; ++eit)
    {
    seeds = this->m_WorkingVD->GetSeedsIDAroundEdge(&*eit);
    // if either seed is not homogeneous
    if( ( !this->m_Label[seeds[0]] || !this->m_Label[seeds[1]])
        && (this->m_NumberOfPixels[seeds[0]]>this->GetMinRegion())
        && (this->m_NumberOfPixels[seeds[1]]>this->GetMinRegion()) )
      {
      adds[0] = (eit->m_Left[0] + eit->m_Right[0])*0.5;
      adds[1] = (eit->m_Left[1] + eit->m_Right[1])*0.5;
      this->m_SeedsToAdded.push_back(adds);
      }
    }
}


template <class TInputImage, class TOutputImage>
void
VoronoiPartitioningImageFilter <TInputImage,TOutputImage>
::MakeSegmentBoundary(void)
{

  RegionType region = this->GetInput()->GetRequestedRegion(); 
  itk::ImageRegionIteratorWithIndex <OutputImageType> oit(this->GetOutput(), region); 
  while( !oit.IsAtEnd())
    {     
    oit.Set(0); 
    ++oit; 
    }

  NeighborIdIterator nit;
  NeighborIdIterator nitend;
  for(int i=0;i<this->GetNumberOfSeeds();i++)
    {
    nitend = this->m_WorkingVD->NeighborIdsEnd(i);
    for(nit=this->m_WorkingVD->NeighborIdsBegin(i);nit!=nitend;++nit)
      {
      if ((*nit)>i) 
        {
        drawLine(this->m_WorkingVD->GetSeed(i),this->m_WorkingVD->GetSeed(*nit));
        i=i;
        }
      }
    }
}

template <class TInputImage, class TOutputImage>
void
VoronoiPartitioningImageFilter<TInputImage,TOutputImage>
::MakeSegmentObject(void)
{
  RegionType region = this->GetInput()->GetRequestedRegion(); 
  itk::ImageRegionIteratorWithIndex <OutputImageType> oit(this->GetOutput(), region); 
  while( !oit.IsAtEnd())
    {     
    oit.Set(0); 
    ++oit; 
    }
  PointIdIterator currPit;
  PointIdIterator currPitEnd;
  PointType currP;
  PointTypeDeque VertList;
  for(int i=0;i<this->GetNumberOfSeeds();i++)
    {
    CellAutoPointer currCell; 
    this->m_WorkingVD->GetCellId(i, currCell);
    currPitEnd = currCell->PointIdsEnd();
    VertList.clear();
    for(currPit=currCell->PointIdsBegin();currPit!=currPitEnd;++currPit)
      {
      this->m_WorkingVD->GetPoint((*currPit),&(currP));
      VertList.push_back(currP);
      }
    // Need to fill with an segment identifier
    FillPolygon(VertList, static_cast<OutputPixelType>(i));
    }
}

template <class TInputImage, class TOutputImage>
bool
VoronoiPartitioningImageFilter <TInputImage,TOutputImage>
::TestHomogeneity(IndexList &Plist)
{
  int num=Plist.size();
  int i;
  double getp;
  double addp=0;
  double addpp=0;
  for(i=0;i<num;i++)
    {
    getp = (double)(this->GetInput()->GetPixel(Plist[i]));
    addp=addp+getp;
    addpp=addpp+getp*getp;
    }

  double savevar;
  if(num > 1)
    {
    savevar = vcl_sqrt((addpp - (addp*addp)/static_cast<double>(num) )
                   /(static_cast<double>(num)-1.0));
    }
  else
    {
    savevar = -1;
    }

  
  return (savevar >= 0 && vcl_sqrt(savevar) < m_SigmaThreshold);
}

template <class TInputImage, class TOutputImage>
void
VoronoiPartitioningImageFilter <TInputImage,TOutputImage>
::PrintSelf (std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "SigmaThreshold: " << m_SigmaThreshold << std::endl;
}

} //end namespace

#endif

