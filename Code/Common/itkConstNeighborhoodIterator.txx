/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConstNeighborhoodIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkConstNeighborhoodIterator_txx
#define _itkConstNeighborhoodIterator_txx
namespace itk {

template<class TImage>
ConstNeighborhoodIterator<TImage>::OffsetType
ConstNeighborhoodIterator<TImage>
::ComputeInternalIndex(unsigned int n) const
{
  OffsetType ans;
  long D = (long)Dimension;
  unsigned long r;
  r = (unsigned long)n;
  for (long i = D-1; i >= 0 ; --i)
    {
      ans[i] = r / m_StrideTable[i];
      r = r % m_StrideTable[i];
    }
  return ans;
}
 
template<class TImage>
ConstNeighborhoodIterator<TImage>
::ConstNeighborhoodIterator()
{
  IndexType zeroIndex;
  SizeType  zeroSize;
  zeroIndex.Fill(0);
  for (unsigned int j = 0; j < Dimension; ++j)
    {  zeroSize[j] = 0; }
  
  m_Bound.Fill(0);
  m_Begin = 0;
  m_BeginIndex.Fill(0);
  // m_ConstImage
  m_End   = 0;
  m_EndIndex.Fill(0);
  m_Loop.Fill(0);
  m_OutputBuffer = 0;
  m_OutputWrapOffsetModifier.Fill(0);
  m_Region.SetIndex(zeroIndex);
  m_Region.SetSize(zeroSize);
  
  for (unsigned int i = 0; i < Dimension; ++i)
    { m_StrideTable[i] = 0; }
  m_WrapOffset.Fill(0);
}

template<class TImage>
ConstNeighborhoodIterator<TImage>
::ConstNeighborhoodIterator(const Self& orig)
  : Neighborhood<InternalPixelType *, Dimension>(orig)
{
  m_Bound      = orig.m_Bound;
  m_Begin      = orig.m_Begin;
  m_BeginIndex = orig.m_BeginIndex;  
  m_ConstImage = orig.m_ConstImage;
  m_End        = orig.m_End;
  m_EndIndex   = orig.m_EndIndex;
  m_Loop       = orig.m_Loop;
  m_OutputBuffer = orig.m_OutputBuffer;
  m_OutputWrapOffsetModifier = orig.m_OutputWrapOffsetModifier;
  m_Region     = orig.m_Region;
  for (unsigned int i = 0; i < Dimension; ++i)
    { m_StrideTable[i] = orig.m_StrideTable[i]; }
  m_WrapOffset = orig.m_WrapOffset;
}

template<class TImage>
void
ConstNeighborhoodIterator<TImage>
::SetEndIndex()
{
  m_EndIndex = m_Region.GetIndex();
  m_EndIndex[Dimension-1] = m_Region.GetIndex()[Dimension-1] +
    m_Region.GetSize()[Dimension-1];        
}

template<class TImage> 
typename ConstNeighborhoodIterator<TImage>::NeighborhoodType
ConstNeighborhoodIterator<TImage>
::GetNeighborhood() const
{
  NeighborhoodType ans;
  typename NeighborhoodType::Iterator ans_it;
  ConstIterator this_it;
  const ConstIterator _end = Superclass::End();

  ans.SetRadius( this->GetRadius() );

  for (ans_it = ans.Begin(), this_it = Superclass::Begin();
       this_it < _end; ans_it++, this_it++)
    {
      *ans_it = **this_it;
    }

  return ans;
}

template<class TImage>
void
ConstNeighborhoodIterator<TImage>
::GoToBegin()
{
  this->SetLocation( m_BeginIndex );
}

template<class TImage>
void
ConstNeighborhoodIterator<TImage>
::GoToEnd()
{
  this->SetLocation( m_EndIndex );
}

template<class TImage>
void ConstNeighborhoodIterator<TImage>
::Initialize(const SizeType &radius, const ImageType *ptr,
             const RegionType &region)
{ 
  m_ConstImage = ptr;
  m_Region = region;
  m_OutputBuffer = 0;
  m_OutputWrapOffsetModifier.Fill(0);
  const IndexType regionIndex = region.GetIndex();

  this->SetRadius(radius);
  this->SetBeginIndex(region.GetIndex());
  this->SetLocation(region.GetIndex());
  this->SetBound(region.GetSize());
  this->SetEndIndex();
  
  m_Begin = ptr->GetBufferPointer() + ptr->ComputeOffset(regionIndex);

  m_End = ptr->GetBufferPointer() + ptr->ComputeOffset( m_EndIndex );
    
  this->ComputeStrideTable();
}

template<class TImage>
ConstNeighborhoodIterator<TImage> &
ConstNeighborhoodIterator<TImage>
::operator=(const Self& orig)
{
  Superclass::operator=(orig);

  m_Bound        = orig.m_Bound;
  m_Begin        = orig.m_Begin;
  m_ConstImage   = orig.m_ConstImage;
  m_End          = orig.m_End;
  m_EndIndex     = orig.m_EndIndex;
  m_Loop         = orig.m_Loop;
  m_OutputBuffer = orig.m_OutputBuffer;
  m_OutputWrapOffsetModifier = orig.m_OutputWrapOffsetModifier;
  m_Region       = orig.m_Region;
  m_BeginIndex = orig.m_BeginIndex;
  for (unsigned int i = 0; i < Dimension; ++i)
    m_StrideTable[i] = orig.m_StrideTable[i];
  m_WrapOffset = orig.m_WrapOffset;

  return *this;
}

template<class TImage>
const ConstNeighborhoodIterator<TImage> &
ConstNeighborhoodIterator<TImage>
::operator++()
{
  unsigned int i;
  Iterator it;
  const Iterator _end = Superclass::End();

  // Increment pointers.
  for (it = Superclass::Begin(); it < _end; ++it)
    {
      (*it)++;
    }
  if (m_OutputBuffer)
    {
      ++m_OutputBuffer;
    }
  
  // Check loop bounds, wrap & add pointer offsets if needed.
  for (i=0; i<Dimension; ++i)
    {
      m_Loop[i]++;
      if ( m_Loop[i] == m_Bound[i] )
        {
          m_Loop[i] = m_BeginIndex[i];
          for (it = Superclass::Begin(); it < _end; ++it)
            {
              (*it) += m_WrapOffset[i];
            }
          if (m_OutputBuffer)
            {
              m_OutputBuffer += m_WrapOffset[i]
                + m_OutputWrapOffsetModifier[i];
            }
        }        
      else break;
    }
  return *this;
}

template<class TImage>
const ConstNeighborhoodIterator<TImage> &
ConstNeighborhoodIterator<TImage>
::operator--()
{
  unsigned int i;
  Iterator it;
  const Iterator _end = Superclass::End();
  
  // Decrement pointers.
  for (it = Superclass::Begin(); it < _end; ++it)
    {
      (*it)--;
    }
  if (m_OutputBuffer)
    {
      --m_OutputBuffer;
    }
  
  // Check loop bounds, wrap & add pointer offsets if needed.
  for (i=0; i<Dimension; ++i)
    {
      if (m_Loop[i] == m_BeginIndex[i])
        {
          m_Loop[i]= m_Bound[i] - 1;
          for (it = Superclass::Begin(); it < _end; ++it)
            {
              (*it) -= m_WrapOffset[i];
            }
          if (m_OutputBuffer)
            {
              m_OutputBuffer -= m_WrapOffset[i]
                + m_OutputWrapOffsetModifier[i];
            }
        }        
      else
        {
          m_Loop[i]--;
          break;
        }
    }
  return *this;
}

template<class TImage>
void
ConstNeighborhoodIterator<TImage>
::PrintSelf(std::ostream &os, Indent indent) const
{
  unsigned int i;
  os << indent;
  os << "ConstNeighborhoodIterator {this= " << this;
  os << ", m_Region = { Start = {";
  for (i=0; i < Dimension; ++i) os << m_Region.GetIndex()[i] << " ";
  os << "}, Size = { ";
  for (i=0; i < Dimension; ++i) os << m_Region.GetSize()[i] << " ";
  os << "} }";
  os << ", m_BeginIndex = { ";
  for (i=0; i < Dimension; ++i) os << m_BeginIndex[i] << " ";
  os << "} , m_EndIndex = { ";
  for (i=0; i < Dimension; ++i) os << m_EndIndex[i] << " ";
  os << "} , m_Loop = { ";
  for (i=0; i < Dimension; ++i) os << m_Loop[i] << " ";
  os << "}, m_Bound = { ";
  for (i=0; i < Dimension; ++i) os << m_Bound[i] << " ";
  os << "}, m_WrapOffset = { ";
  for (i=0; i < Dimension; ++i) os << m_WrapOffset[i] << " ";
  os << "}, m_OutputWrapOffsetModifier = { ";
  for (i=0; i < Dimension; ++i) os << m_OutputWrapOffsetModifier[i] << " ";
  os << "}, m_OutputBuffer = " << m_OutputBuffer;

  os << ", m_Begin = " << m_Begin;
  os << ", m_End = " << m_End;
  os << "}"  << std::endl;
  Superclass::PrintSelf(os, indent.GetNextIndent());
}

template<class TImage>
void ConstNeighborhoodIterator<TImage>
::SetBound(const SizeType& size)
{
  const unsigned long *offset     = m_ConstImage->GetOffsetTable();
  SizeType bufferSize = m_ConstImage->GetBufferedRegion().GetSize();

  // Set the bounds and the wrapping offsets
  for (unsigned int i=0; i<Dimension; ++i)
    {
      m_Bound[i]      = m_BeginIndex[i]+size[i];
      m_WrapOffset[i] = (bufferSize[i] - (m_Bound[i] - m_BeginIndex[i]))
                        * offset[i];
    }
  m_WrapOffset[Dimension-1] = 0; // last offset is zero because there are no
                                 // higher dimensions
  
}

template<class TImage>
void 
ConstNeighborhoodIterator<TImage>
::SetOutputWrapOffsetModifier(const OffsetType &o)
{
  for (unsigned int i = 0; i < Dimension; ++i)
    {      m_OutputWrapOffsetModifier[i] = o[i];    }
}

template<class TImage>
void ConstNeighborhoodIterator<TImage>
::SetPixelPointers(const IndexType &pos)
{
  const Iterator _end = Superclass::End();
  InternalPixelType * Iit;
  ImageType *ptr = const_cast<ImageType *>(m_ConstImage.GetPointer());
  const SizeType size = this->GetSize();
  const unsigned long *OffsetTable = m_ConstImage->GetOffsetTable();
  const SizeType radius = this->GetRadius();

  unsigned int i;
  Iterator Nit;
  SizeType loop;
  for (i=0; i<Dimension; ++i) loop[i]=0;

  // Find first "upper-left-corner"  pixel address of neighborhood
  Iit = ptr->GetBufferPointer() + ptr->ComputeOffset(pos);

  for (i = 0; i<Dimension; ++i)
    {
      Iit -= radius[i] * OffsetTable[i];
    }

  // Compute the rest of the pixel addresses
  for (Nit = Superclass::Begin(); Nit != _end; ++Nit)
    {
      *Nit = Iit;
      ++Iit;
      for (i = 0; i <Dimension; ++i)
        {
          loop[i]++;
          if ( loop[i] == size[i] )
            {
              if (i==Dimension-1) break;
              Iit +=  OffsetTable[i+1] - OffsetTable[i] * size[i];
              loop[i]= 0;
            }
          else break;
        }
    }
}

} // namespace itk

#endif
