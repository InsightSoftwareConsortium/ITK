/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhood.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkNeighborhood_txx
#define _itkNeighborhood_txx
namespace itk {
  
template<class TPixel, unsigned int VDimension, class TContainer>
void
Neighborhood<TPixel, VDimension, TContainer>
::ComputeNeighborhoodStrideTable()
{
  unsigned stride, accum;

  for (unsigned int dim = 0; dim < VDimension; ++dim)
    {
      stride = 0;
      accum = 1;

      for (unsigned int i=0; i<VDimension; ++i)
        {
          if (i == dim) stride = accum;
          accum *= m_Size[i];
        }

      m_StrideTable[dim] = stride;
    }
}

template<class TPixel, unsigned int VDimension, class TContainer>
void Neighborhood<TPixel, VDimension, TContainer>
::ComputeNeighborhoodOffsetTable()
{
  m_OffsetTable.reserve(this->Size());
  OffsetType o;
  unsigned int i, j;
  for (j = 0; j < VDimension; j++)
    {
      o[j] = -(static_cast<long>(this->GetRadius(j)));
    }

  for (i = 0; i < this->Size(); ++i)
    {
      m_OffsetTable.push_back(o);
      for (j= 0; j< VDimension; j++)
        {
          o[j] = o[j] + 1;
          if (o[j] > static_cast<long>(this->GetRadius(j)))
            {
              o[j] = -(static_cast<long>(this->GetRadius(j)));
            }
          else break;
        }
    }
}  
 
template<class TPixel, unsigned int VDimension, class TContainer>
void
Neighborhood<TPixel, VDimension, TContainer>
::SetRadius(const unsigned long s)
{
  SizeType k;
  for (unsigned int i = 0; i< VDimension; i++)
    {
      k[i] = s;
    }
  this->SetRadius(k);
}

template<class TPixel, unsigned int VDimension, class TContainer>
void
Neighborhood<TPixel, VDimension, TContainer>
::SetRadius(const SizeType &r)
{
  memcpy(m_Radius.m_Size, r.m_Size, sizeof(const unsigned long)*VDimension);
  this->SetSize();

  unsigned int cumul=1;
  for (unsigned int i = 0; i<VDimension; i++)
    {
      cumul*=m_Size[i];
    }

  this->Allocate(cumul);
  this->ComputeNeighborhoodStrideTable();
  this->ComputeNeighborhoodOffsetTable();
}

template<class TPixel, unsigned int VDimension, class TContainer>
Neighborhood<TPixel, VDimension, TContainer>
::Neighborhood(const Self &other)
{
  m_Radius     = other.m_Radius;
  m_Size       = other.m_Size;
  m_DataBuffer = other.m_DataBuffer;
  ::memcpy(m_StrideTable, other.m_StrideTable, sizeof(unsigned int) * VDimension);
  m_OffsetTable = other.m_OffsetTable;
}


template<class TPixel, unsigned int VDimension, class TContainer>
Neighborhood<TPixel, VDimension, TContainer> &
Neighborhood<TPixel, VDimension, TContainer>
::operator=(const Self &other)
{
    m_Radius     = other.m_Radius;
    m_Size       = other.m_Size;
    m_DataBuffer = other.m_DataBuffer;
    ::memcpy(m_StrideTable, other.m_StrideTable, sizeof(unsigned int) * VDimension);
    m_OffsetTable = other.m_OffsetTable;
    return *this;
}



template<class TPixel, unsigned int VDimension, class TContainer>
std::slice Neighborhood<TPixel, VDimension, TContainer>
::GetSlice(unsigned int d) const
{
  unsigned int n = this->Size()/2;
  n = n - ( static_cast<unsigned>(this->GetStride(d))
            * (static_cast<unsigned>(this->GetSize()[d] / 2) ) );

  return std::slice(static_cast<::size_t>(n),
                    static_cast<::size_t>(this->GetSize()[d]),
                    static_cast<::size_t>(this->GetStride(d)) );
}

template<class TPixel, unsigned int VDimension, class TContainer>
unsigned int Neighborhood<TPixel, VDimension, TContainer>
::GetNeighborhoodIndex(const OffsetType &o) const
{
  unsigned int idx = (this->Size()/2);
  for (unsigned i = 0; i < VDimension; ++i)
    {      idx+=o[i] * static_cast<long>(m_StrideTable[i]);    }
  return idx;
}


template<class TPixel, unsigned int VDimension, class TContainer>
void Neighborhood<TPixel, VDimension, TContainer>
::PrintSelf(std::ostream &os, Indent indent) const
{
  unsigned int i;

  os << indent << "m_Size: [ ";
  for (i=0; i<VDimension; ++i) os << m_Size[i] << " ";
  os << "]" << std::endl;
  
  os << indent << "m_Radius: [ ";
  for (i=0; i<VDimension; ++i) os << m_Radius[i] << " ";
  os << "]" << std::endl;

  os << indent << "m_StrideTable: [ ";
  for (i=0; i<VDimension; ++i) os << m_StrideTable[i] << " ";
  os << "]" << std::endl;

  os << indent << "m_OffsetTable: [ ";
  for (i=0; i< m_OffsetTable.size(); ++i) os << m_OffsetTable[i] << " ";
  os << "]" << std::endl;  
}

}  // namespace itk

#endif
