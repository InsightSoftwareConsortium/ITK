/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVoronoiPartitioningImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
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

  m_NumberOfBoundary = 0;
  for(int i=0;i<m_NumberOfSeeds;i++)
    {
    CellAutoPointer currCell;
    m_WorkingVD->GetCellId(i,currCell);
    currPitEnd = currCell->PointIdsEnd();
    VertList.clear();
    for(currPit=currCell->PointIdsBegin();currPit!=currPitEnd;++currPit)
      {
      m_WorkingVD->GetPoint((*currPit),&(currP));
      VertList.push_back(currP);
      }

    PixelPool.clear();
    this->GetPixelIndexFromPolygon(VertList,&PixelPool);
    m_NumberOfPixels[i] = PixelPool.size();
    m_Label[i] = this->TestHomogeneity(PixelPool);

    // when partitioning the NumberOfBoundary is the number of regions that
    // are not homogeneous
    if (!m_Label[i])
      {
      m_NumberOfBoundary++;
      }
    }
}

template <class TInputImage, class TOutputImage>
void
VoronoiPartitioningImageFilter<TInputImage,TOutputImage>
::GenerateAddingSeeds(void)
{
  EdgeIterator eit;
  EdgeIterator eitend = m_WorkingVD->EdgeEnd();  
  PointType adds;
  Point<int,2> seeds;

  for(eit = m_WorkingVD->EdgeBegin();eit != eitend; ++eit)
    {
    seeds = m_WorkingVD->GetSeedsIDAroundEdge(&*eit);
    // if either seed is not homogeneous
    if( ( !m_Label[seeds[0]] || !m_Label[seeds[1]])
         && (m_NumberOfPixels[seeds[0]]>m_MinRegion)
         && (m_NumberOfPixels[seeds[1]]>m_MinRegion) )
      {
      adds[0] = (eit->m_Left[0] + eit->m_Right[0])*0.5;
      adds[1] = (eit->m_Left[1] + eit->m_Right[1])*0.5;
      m_SeedsToAdded.push_back(adds);
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
  for(int i=0;i<m_NumberOfSeeds;i++)
    {
    nitend = m_WorkingVD->NeighborIdsEnd(i);
    for(nit=m_WorkingVD->NeighborIdsBegin(i);nit!=nitend;++nit)
      {
      if ((*nit)>i) 
        {
        drawLine(m_WorkingVD->GetSeed(i),m_WorkingVD->GetSeed(*nit));
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
  for(int i=0;i<m_NumberOfSeeds;i++)
    {
    CellAutoPointer currCell; 
    m_WorkingVD->GetCellId(i, currCell);
    currPitEnd = currCell->PointIdsEnd();
    VertList.clear();
    for(currPit=currCell->PointIdsBegin();currPit!=currPitEnd;++currPit)
      {
      m_WorkingVD->GetPoint((*currPit),&(currP));
      VertList.push_back(currP);
      }
    // Need to fill with an segment identifier
    FillPolygon(VertList);
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

  double savemean,savevar;
  if(num > 1)
    {
    savemean = addp/static_cast<double>(num);
    savevar = sqrt((addpp - (addp*addp)/static_cast<double>(num) )
                   /(static_cast<double>(num)-1.0));
    }
  else
    {
    savemean = 0;
    savevar = -1;
    }

//   // jvm - Mahalanobis distance
//   if (savevar > 0 && fabs(savemean - m_Mean) / m_Var < 2.5)
//     return true;
//   else
//     return false;
  
  return (savevar >= 0 && savevar < 50 );
}

} //end namespace

#endif

