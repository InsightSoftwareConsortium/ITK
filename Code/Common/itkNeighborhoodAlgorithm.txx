/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodAlgorithm.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkImageRegionIterator.h"
#include "itkImageRegion.h"

namespace itk
{
  
namespace NeighborhoodAlgorithm
{
  
template<class TContainer, class TArray>
typename InnerProduct<TContainer, TArray>::ScalarType
InnerProduct<TContainer, TArray>
::operator()(TContainer &d, TArray &v) const
{
  ScalarType sum = NumericTraits<ScalarType>::Zero;
  InternalType *it;
  typename TContainer::Iterator this_it;
  const InternalType *_end = &(v[v.Size()]);
  
  for (it = &(v[0]), this_it = d.begin(); it < _end; ++it, ++this_it)
    {
      sum += *it * *this_it;
    }
  return sum;
}

template<class TContainer, class TArray>
typename InnerProduct<TContainer, TArray>::ScalarType
InnerProduct<TContainer, TArray>
::operator()(std::slice &s, TContainer &d, TArray &v) const
{
  ScalarType sum = NumericTraits<ScalarType>::Zero;
  InternalType *it;
  typename TContainer::SliceIteratorType slice_it(&d, s);

  slice_it = slice_it.Begin();;
  const InternalType *itEnd = &(v[v.Size()]);
  for (it = &(v[0]); it < itEnd; ++it, ++slice_it)
    {
      sum += *it * *slice_it;
    }

  return sum;
}

template<class TContainer, class TArray>
typename VectorComponentInnerProduct<TContainer, TArray>::ScalarType
VectorComponentInnerProduct<TContainer, TArray>
::operator()(TContainer &d, TArray &v) const
{
  ScalarType sum = NumericTraits<ScalarType>::Zero;
  ScalarType *it;
  typename TContainer::Iterator this_it;
  const ScalarType *_end = &(v[v.Size()]);
  
  for (it = &(v[0]), this_it = d.begin(); it < _end; ++it, ++this_it)
    {     
      sum += *it * (*this_it)[m_VisibleComponent];
    }
  return sum;
}


template<class TContainer, class TArray>
typename VectorComponentInnerProduct<TContainer, TArray>::ScalarType
VectorComponentInnerProduct<TContainer, TArray>
::operator()(std::slice &s, TContainer &d, TArray &v) const
{
  ScalarType sum = NumericTraits<ScalarType>::Zero;
  ScalarType *it;
  typename TContainer::SliceIteratorType slice_it(&d, s);

  slice_it = slice_it.Begin();;
  const ScalarType *itEnd = &(v[v.Size()]);
  for (it = &(v[0]); it < itEnd; ++it, ++slice_it)
    {
      sum += *it * (*slice_it)[m_VisibleComponent];
    }

  return sum;
}

template<class TIterator, class TArray>
typename IteratorInnerProduct<TIterator, TArray>::ScalarType
IteratorInnerProduct<TIterator, TArray>
::operator()(TIterator &d, TArray &v) const
{
  ScalarType sum = NumericTraits<ScalarType>::Zero;
  ScalarType *it;
  typename TIterator::Iterator this_it;
  const ScalarType *_end = &(v[v.Size()]);
  
  for (it = &(v[0]), this_it = d.begin(); it < _end; ++it, ++this_it)
    {
      sum += *it * **this_it;
    }
  return sum;
}

template<class TIterator, class TArray>
typename IteratorInnerProduct<TIterator, TArray>::ScalarType
IteratorInnerProduct<TIterator, TArray>
::operator()(std::slice &s, TIterator &d, TArray &v) const
{
  ScalarType sum = NumericTraits<ScalarType>::Zero;
  ScalarType *it;
  typename TIterator::SliceIteratorType slice_it(&d, s);

  slice_it = slice_it.Begin();;
  const ScalarType *itEnd = &(v[v.Size()]);
  for (it = &(v[0]); it < itEnd; ++it, ++slice_it)
    {
      sum += *it * **slice_it;
    }

  return sum;
}

template<class TIterator, class TArray>
typename BoundsCheckingIteratorInnerProduct<TIterator, TArray>::ScalarType
BoundsCheckingIteratorInnerProduct<TIterator, TArray>
::operator()(TIterator &d, TArray &v) const
{
  InnerProduct<Neighborhood<PixelType, Dimension>, TArray > IP;
  
  if ( d.InBounds() )
    {
      ScalarType sum = NumericTraits<ScalarType>::Zero;
      ScalarType *it;
      typename TIterator::Iterator this_it;
      const ScalarType *_end = &(v[v.Size()]);
      
      for (it = &(v[0]), this_it = d.begin(); it < _end; ++it, ++this_it)
        {
          sum += *it * **this_it;
        }
      return sum;
    }
  else
    {
      Neighborhood<PixelType, Dimension> N = d.GetNeighborhood();
      return IP(N, v);;
    }
}

template<class TIterator, class TArray>
typename BoundsCheckingIteratorInnerProduct<TIterator, TArray>::ScalarType
BoundsCheckingIteratorInnerProduct<TIterator, TArray>
::operator()(std::slice &s, TIterator &d, TArray &v) const
{
  InnerProduct<Neighborhood<PixelType, Dimension>, TArray > IP;
  ScalarType sum;
  ScalarType *it;
  typename TIterator::SliceIteratorType slice_it(&d, s);
  
  if ( d.InBounds() )
    {
      sum = NumericTraits<ScalarType>::Zero;
        
      slice_it = slice_it.Begin();;
      const ScalarType *itEnd = &(v[v.Size()]);
      for (it = &(v[0]); it < itEnd; ++it, ++slice_it)
        {
          sum += *it * **slice_it;
        }
      
      return sum;
    }
  else
    {
      Neighborhood<PixelType, Dimension> N = d.GetNeighborhood();
      return IP(s, N, v);
    }
}

template<class TIterator, class TArray>
typename VectorComponentIteratorInnerProduct<TIterator, TArray>::ScalarType
VectorComponentIteratorInnerProduct<TIterator, TArray>
::operator()(TIterator &d, TArray &v) const
{
  ScalarType sum = NumericTraits<ScalarType>::Zero;
  ScalarType *it;
  typename TIterator::Iterator this_it;
  const ScalarType *_end = &(v[v.Size()]);
  
  for (it = &(v[0]), this_it = d.begin(); it < _end; ++it, ++this_it)
    {
     
      sum += *it * (**this_it)[m_VisibleComponent];
    }
  return sum;
}

template<class TIterator, class TArray>
typename VectorComponentIteratorInnerProduct<TIterator, TArray>::ScalarType
VectorComponentIteratorInnerProduct<TIterator, TArray>
::operator()(std::slice &s, TIterator &d, TArray &v) const
{
  ScalarType sum = NumericTraits<ScalarType>::Zero;
  ScalarType *it;
  typename TIterator::SliceIteratorType slice_it(&d, s);

  slice_it = slice_it.Begin();;
  const ScalarType *itEnd = &(v[v.Size()]);
  for (it = &(v[0]); it < itEnd; ++it, ++slice_it)
    {
      sum += *it * (**slice_it)[m_VisibleComponent];
    }

  return sum;
}

template<class TIterator, class TArray>
typename BoundsCheckingVectorComponentIteratorInnerProduct<TIterator, TArray>
::ScalarType
BoundsCheckingVectorComponentIteratorInnerProduct<TIterator, TArray>
::operator()(TIterator &d, TArray &v) const
{
  VectorComponentInnerProduct<Neighborhood<VectorType, Dimension>, TArray > IP;
  ScalarType *it;
  typename TIterator::Iterator this_it;
  if ( d.InBounds() )
    { 
      ScalarType sum = NumericTraits<ScalarType>::Zero; 
      const ScalarType *_end = &(v[v.Size()]);
      
      for (it = &(v[0]), this_it = d.begin(); it < _end; ++it, ++this_it)
        {
          
          sum += *it * (**this_it)[m_VisibleComponent];
        }
      return sum;
    }
  else
    {
      Neighborhood<VectorType, Dimension> N = d.GetNeighborhood();
      return IP(N, v);
    }
}

template<class TIterator, class TArray>
typename BoundsCheckingVectorComponentIteratorInnerProduct<TIterator, TArray>
::ScalarType
BoundsCheckingVectorComponentIteratorInnerProduct<TIterator, TArray>
::operator()(std::slice &s, TIterator &d, TArray &v) const
{
  VectorComponentInnerProduct<Neighborhood<VectorType, Dimension>, TArray > IP;
  ScalarType *it;
  typename TIterator::SliceIteratorType slice_it(&d, s);
  if ( d.InBounds() )
    {
      ScalarType sum = NumericTraits<ScalarType>::Zero;
      slice_it = slice_it.Begin();;
      const ScalarType *itEnd = &(v[v.Size()]);
      for (it = &(v[0]); it < itEnd; ++it, ++slice_it)
        {
          sum += *it * (**slice_it)[m_VisibleComponent];
        }
      return sum;
    }
  else
    {
      Neighborhood<VectorType, Dimension> N = d.GetNeighborhood();
      return IP(s, N, v);
    }
}


template<class TOperation, class TIterator>
void ApplyOperatorToEach<TOperation, TIterator>
::operator()(ImageType *in, ImageType *out,
             Neighborhood<ScalarValueType, ImageDimension> &o)
  const
{
  TOperation OP;
  CalculateOutputWrapOffsetModifiers<ImageType> OM;

  // Create iterator.  The output buffer pointer of the iterator is set up
  // to account for any differences in the buffer sizes of the two images.
  long int *mod = new long int[ImageDimension];
  TIterator it(o.GetRadius(), in, in->GetRequestedRegion());
  it.SetOutputBuffer(out->GetBufferPointer() +
                     out->ComputeOffset(it.GetRegion().GetIndex()));
  OM(mod, in, out);
  it.SetOutputWrapOffsetModifier(mod);
  delete[] mod;
  
  // Process image to output.
  it = it.Begin();
  while( !it.IsAtEnd() )
    {
      *( it.GetOutputBuffer() ) = OP(it, o);
      ++it;
    }
}

template<class TImage>
void CalculateOutputWrapOffsetModifiers<TImage>
::operator()(long int *ans, TImage *input, TImage *output) const
{
  const Size<TImage::ImageDimension> isz =  input->GetBufferedRegion().GetSize();
  const Size<TImage::ImageDimension> osz = output->GetBufferedRegion().GetSize();

  for (int i=0; i<TImage::ImageDimension; ++i) ans[i] = osz[i] - isz[i];
}



template<class TPixel, int VDimension, class TAllocator>
Neighborhood<TPixel, VDimension, TAllocator>
ConvolveND(Neighborhood<TPixel, VDimension, TAllocator> &A,
         Neighborhood<TPixel, VDimension, TAllocator> &B, int Mode)
{
  typedef Neighborhood<TPixel, VDimension, TAllocator> NeighborhoodType;
  
  int iDim;
  unsigned long BOffset[VDimension];
  int iLoop[VDimension];
  int jLoop[VDimension];
  int overlap[VDimension];
  int overlapStart[VDimension];
  int End[VDimension];
  int Start[VDimension];
  int dist[VDimension];
  const int C = VDimension-1;
  bool iLoopNotDone;
  bool jLoopNotDone;
  int ChangedIdx;
  
  NeighborhoodType N;
  typename NeighborhoodType::Iterator Np;
  typename NeighborhoodType::Iterator Ap[VDimension];
  typename NeighborhoodType::Iterator Bp[VDimension];
  unsigned long radius[VDimension];
  
  int ARadius[VDimension];
  int BRadius[VDimension];
  int ASize[VDimension];
  int BSize[VDimension];
  unsigned long AStride[VDimension];
  unsigned long BStride[VDimension];
  const typename NeighborhoodType::Iterator ApBegin = A.Begin();
  const typename NeighborhoodType::Iterator BpEnd = B.End() - 1;

  for (iDim = 0; iDim < VDimension; ++iDim)
    {
      ARadius[iDim] = (int) A.GetRadius()[iDim];
      BRadius[iDim] = (int) B.GetRadius()[iDim];
      ASize[iDim]   = ARadius[iDim]*2 + 1;
      BSize[iDim]   = BRadius[iDim]*2 + 1;
      AStride[iDim] = A.GetStride(iDim);
      BStride[iDim] = B.GetStride(iDim);
      dist[iDim]    = ASize[iDim];
    }

  if (Mode==0) // Result radius is increased over A::Radius by B::Radius
    {
      for (iDim = 0; iDim < VDimension; ++iDim)
        {
          radius[iDim]       = ARadius[iDim] + BRadius[iDim];
          Start[iDim]        = -BSize[iDim] + 1;
          iLoop[iDim]        = Start[iDim]; // Initialize iLoop indicies
          overlapStart[iDim] = 0;
          End[iDim]          = ASize[iDim];
          overlap[iDim]      = overlapStart[iDim];
        }
      N.SetRadius(radius);
    }
  else  // Constrain result radius to A::Radius
    {
      for (iDim = 0; iDim < VDimension; ++iDim)
        {
          Start[iDim]        = -BRadius[iDim];
          iLoop[iDim]        = Start[iDim]; // Initialize iLoop indicies
          overlapStart[iDim] = BRadius[iDim];
          End[iDim]          = ASize[iDim] - BRadius[iDim];
          overlap[iDim]      = overlapStart[iDim];
        }
      N.SetRadius(A.GetRadius());
    }

  ChangedIdx = C;           // A value of C will initialize
                            // all run lengths and starting offsets.

  Np = N.Begin();
  iLoopNotDone = true;
  while (iLoopNotDone)
    {
      // Recalculate run lengths & starting offsets in each dimension
      // whose index has changed this iteration.
      for (iDim = ChangedIdx; iDim >= 0; --iDim)
        {
          if (iDim == C)
            {
              Ap[C] = ApBegin;
              Bp[C] = BpEnd;
            }
          else
            {
              Ap[iDim] = Ap[iDim+1];
              Bp[iDim] = Bp[iDim+1];
            }
          
          if ( iLoop[iDim] > 0 )
            {
              Ap[iDim] += iLoop[iDim] * AStride[iDim];
              --dist[iDim];
              overlap[iDim] = BSize[iDim];
            }
          else
            {
              Bp[iDim] += iLoop[iDim] * BStride[iDim];
              ++overlap[iDim];
            }
          
          if (overlap[iDim] > dist[iDim]) overlap[iDim] = dist[iDim];
          BOffset[iDim] = BSize[iDim] - overlap[iDim];
        }
      
      // Convolve
      // Initialize jLoop indicies
      memset(jLoop, 0, sizeof(int)*VDimension);
            
      jLoopNotDone = true;
      while( jLoopNotDone )
        {
          // Innermost jLoop (columns)
          for (jLoop[0] = 0; jLoop[0] < overlap[0]; ++jLoop[0], --Bp[0],
                 ++Ap[0])
            {
               *Np += *Ap[0] * *Bp[0];
            }
          Ap[0] += AStride[1] - overlap[0];
          Bp[0] -= BOffset[0];
          
          // Increment higher jLoop indicies.
          for (iDim = 1; iDim < VDimension; ++iDim)
            {
              ++jLoop[iDim];
              if ( jLoop[iDim] == overlap[iDim] )
                {
                  jLoop[iDim] = 0;
                  if (iDim==C)
                    {
                      jLoopNotDone=false;
                      break;
                    }
                  Ap[0] += AStride[iDim+1]-overlap[iDim]*AStride[iDim];
                  Bp[0] -= BOffset[iDim]*BStride[iDim];
                }
              else break;
            }
        }       
      
      // Increment iLoop indicies.
      for (iDim = 0, ChangedIdx = 0; iDim < VDimension; ++iDim)
        {
          ++iLoop[iDim];
          if ( iLoop[iDim] == End[iDim] )
            {
              ++ChangedIdx;
              iLoop[iDim] = Start[iDim];
              if (iDim==C) iLoopNotDone=false;
              overlap[iDim] = overlapStart[iDim];
              dist[iDim] = ASize[iDim];

            }
          else break;
        }
      
      // Increment output pointer
      ++Np;
      
    }
  return N;
}

template<class TPixel, unsigned int VDimension, class TAllocator>
Neighborhood<TPixel, VDimension, TAllocator>
Convolve3D(Neighborhood<TPixel, VDimension, TAllocator> &A,
           Neighborhood<TPixel, VDimension, TAllocator> &B, int Mode)
{
  enum {COL, ROW, SLI};

  typedef Neighborhood<TPixel, VDimension, TAllocator> NeighborhoodType;
  
  register  int is;
  register  int ir;
  register  int ic;
  unsigned long BOffsetC;
  unsigned long BOffsetR;
  register int s;
  register int r;
  register int c;
  int overlapS;
  int overlapR;
  int overlapC;
  int distS;
  int distR;
  int distC;
  int SStart;
  int RStart;
  int CStart;
  int SEnd;
  int REnd;
  int CEnd;
  int overlapSStart;
  int overlapRStart;
  int overlapCStart;

  NeighborhoodType N;
  unsigned long radius[VDimension];
  typename NeighborhoodType::Iterator Np;
  typename NeighborhoodType::Iterator Ap_0;
  typename NeighborhoodType::Iterator Bp_0;
  typename NeighborhoodType::Iterator Ap_1;
  typename NeighborhoodType::Iterator Bp_1;
  typename NeighborhoodType::Iterator Ap;
  typename NeighborhoodType::Iterator Bp;

  const int ARadiusS = (int) A.GetRadius()[SLI];
  const int ARadiusR = (int) A.GetRadius()[ROW];
  const int ARadiusC = (int) A.GetRadius()[COL];
  const int BRadiusS = (int) B.GetRadius()[SLI];
  const int BRadiusR = (int) B.GetRadius()[ROW];
  const int BRadiusC = (int) B.GetRadius()[COL];
  const int BSizeS   = BRadiusS*2 +1;
  const int ASizeS   = ARadiusS*2 +1;
  const int BSizeR   = BRadiusR*2 +1;
  const int ASizeR   = ARadiusR*2 +1;
  const int BSizeC   = BRadiusC*2 +1;
  const int ASizeC   = ARadiusC*2 +1;
  const unsigned long AStrideS = A.GetStride(SLI);
  const unsigned long BStrideS = B.GetStride(SLI);
  const unsigned long AStrideR = A.GetStride(ROW);
  const unsigned long BStrideR = B.GetStride(ROW);
  const typename NeighborhoodType::Iterator ApBegin = A.Begin();
  const typename NeighborhoodType::Iterator BpEnd = B.End() - 1;
  

  if (Mode == 0) // Result radius is increased over A::Radius by B::Radius
    {
      radius[SLI] = ARadiusS + BRadiusS;
      radius[ROW] = ARadiusR + BRadiusR;
      radius[COL] = ARadiusC + BRadiusC;
      N.SetRadius(radius);
      SStart = -BSizeS + 1;
      RStart = -BSizeR + 1;
      CStart = -BSizeC + 1;
      overlapSStart = 0;
      overlapRStart = 0;
      overlapCStart = 0;
      SEnd = ASizeS;
      REnd = ASizeR;
      CEnd = ASizeC;
    }
  else // Constrain result radius to A::Radius
    {
      N.SetRadius(A.GetRadius());
      SStart = -BRadiusS;
      RStart = -BRadiusR;
      CStart = -BRadiusC;
      overlapSStart = BRadiusS;
      overlapRStart = BRadiusR;
      overlapCStart = BRadiusC;
      SEnd = ASizeS - BRadiusS;
      REnd = ASizeR - BRadiusR;
      CEnd = ASizeC - BRadiusC;
    }
  
  for (overlapS = overlapSStart, distS = ASizeS, Np = N.Begin(), s = SStart;
       s < SEnd; ++s)
    {
      Ap_0 = ApBegin;
      Bp_0 = BpEnd;
      
      if (s > 0)
        {
          Ap_0 += s * AStrideS;
          --distS;
          overlapS = BSizeS;
        }
      else
        {
          Bp_0 += s * BStrideS;
          ++overlapS;
        }
      
      if (overlapS > distS) overlapS = distS;
      
      for (overlapR = overlapRStart, distR = ASizeR, r = RStart;
           r < REnd; ++r)
        {
          Ap_1 = Ap_0;
          Bp_1 = Bp_0;
          
          if (r > 0)
            {
              Ap_1 += r * AStrideR;
              --distR;
              overlapR = BSizeR;
            }
          else
            {
              Bp_1 += r * BStrideR;
              ++overlapR;
            }
          
          if (overlapR > distR) overlapR = distR;
          BOffsetR = BSizeR - overlapR;          

          for (overlapC = overlapCStart, distC = ASizeC, c = CStart; c < CEnd;
               ++c, ++Np)
            {
              Ap = Ap_1;
              Bp = Bp_1;
              
              if (c > 0)
                {
                  Ap += c;
                  --distC;
                  overlapC = BSizeC;
                }
              else
                {
                  Bp += c;
                  ++overlapC;
                }
              
              if (overlapC > distC) overlapC = distC;
              BOffsetC = BSizeC - overlapC;
              
              for (is = 0; is < overlapS; ++is)
                {
                  for (ir = 0; ir < overlapR; ++ir)
                    {
                      for (ic = 0; ic < overlapC; ++ic, --Bp, ++Ap)
                        {
                          *Np += *Ap * *Bp;
                        }
                      Ap += AStrideR - overlapC;
                      Bp -= BOffsetC;
                    }
                  Ap += AStrideS - overlapR*AStrideR;
                  Bp -= BOffsetR*BStrideR;
                }
            }
        }
    }
  return N;
}

template<class TPixel, unsigned int VDimension, class TAllocator>
Neighborhood<TPixel, VDimension, TAllocator>
Convolve2D(Neighborhood<TPixel, VDimension, TAllocator> &A,
           Neighborhood<TPixel, VDimension, TAllocator> &B, int Mode)
{
  enum {COL, ROW};

  typedef Neighborhood<TPixel, VDimension, TAllocator> NeighborhoodType;
  
  register int ir;
  register int ic;
  unsigned long BOffset;
  register int r;
  register int c;
  int overlapR;
  int overlapC;
  int distR;
  int distC;
  int RStart;
  int CStart;
  int REnd;
  int CEnd;
  int overlapRStart;
  int overlapCStart;
  unsigned long radius[VDimension];
  NeighborhoodType N;

  typename NeighborhoodType::Iterator Np;
  typename NeighborhoodType::Iterator Ap_1;
  typename NeighborhoodType::Iterator Bp_1;
  typename NeighborhoodType::Iterator Ap;
  typename NeighborhoodType::Iterator Bp;

  const int ARadiusR = (int) A.GetRadius()[ROW];
  const int ARadiusC = (int) A.GetRadius()[COL];
  const int BRadiusR = (int) B.GetRadius()[ROW];
  const int BRadiusC = (int) B.GetRadius()[COL];
  const int BSizeR    = BRadiusR*2 +1;
  const int ASizeR    = ARadiusR*2 +1;
  const int BSizeC    = BRadiusC*2 +1;
  const int ASizeC    = ARadiusC*2 +1;
  const unsigned long AStride = A.GetStride(ROW);
  const unsigned long BStride = B.GetStride(ROW);
  const typename NeighborhoodType::Iterator ApBegin = A.Begin();
  const typename NeighborhoodType::Iterator BpEnd = B.End() - 1;

  if (Mode == 0) // Result radius is increased over A::Radius by B::Radius
    {
      radius[ROW] = ARadiusR + BRadiusR;
      radius[COL] = ARadiusC + BRadiusC;
      N.SetRadius(radius);
      RStart = -BSizeR + 1;
      CStart = -BSizeC + 1;
      overlapRStart = 0;
      overlapCStart = 0;
      REnd = ASizeR;
      CEnd = ASizeC;
    }
  else // Constrain result radius to A::Radius
    {
      N.SetRadius(A.GetRadius());
      RStart = -BRadiusR;
      CStart = -BRadiusC;
      overlapRStart = BRadiusR;
      overlapCStart = BRadiusC;
      REnd = ASizeR - BRadiusR ;
      CEnd = ASizeC - BRadiusC ;
    }

   for (overlapR = overlapRStart, distR = ASizeR, Np = N.Begin(), r = RStart;
        r < REnd; ++r)
     {
      Ap_1 = ApBegin;
      Bp_1 = BpEnd;
   
      if (r > 0)
        {
          Ap_1 += r * AStride;
          --distR;
          overlapR = BSizeR;
        }
      else
        {
          Bp_1 += r * BStride;
          ++overlapR;
        }
      if (overlapR > distR) overlapR = distR;

      for (overlapC = overlapCStart, distC = ASizeC, c = CStart; c < CEnd;
           ++c, ++Np)
        {
          Ap = Ap_1;
          Bp = Bp_1;

          if (c > 0)
            {
              Ap += c;
              --distC;
              overlapC = BSizeC;
            }
          else
            {
              Bp += c;
              ++overlapC;
            }
          if (overlapC > distC) overlapC = distC;

          BOffset = BSizeC - overlapC;
          for (ir = 0; ir < overlapR; ++ir)
            {
              for (ic = 0; ic < overlapC; ++ic, --Bp, ++Ap)
                {
                   *Np += *Ap * *Bp;
                }
              Ap += AStride - overlapC;
              Bp -= BOffset;;
            }
        }
    }

  return N;
}

template<class TPixel, unsigned int VDimension, class TAllocator>
Neighborhood<TPixel, VDimension, TAllocator>
Convolve1D(Neighborhood<TPixel, VDimension, TAllocator> &A,
           Neighborhood<TPixel, VDimension, TAllocator> &B, int Mode)
{
  typedef Neighborhood<TPixel, VDimension, TAllocator> NeighborhoodType;
  
  NeighborhoodType N;
  typename NeighborhoodType::Iterator Ap;
  typename NeighborhoodType::Iterator Bp;
  typename NeighborhoodType::Iterator Np;

  int j;
  int overlap;
  int dist;
  int i;
  int Bsize;

  Bsize = (int)B.Size();
  if (Mode==0) // Result radius is increased over A::Radius by B::Radius..
    {
      N.SetRadius((A.Size() + B.Size() - 1)>>1);
      i = -Bsize+1;
      overlap =0;
    }
  else        // Constrain result radius to A::Radius.
    {
      N.SetRadius(A.GetRadius());
      i = - (int)(B.GetRadius()[0]);
      overlap = (int)(B.GetRadius()[0]);
    }

  for (Np = N.Begin(); Np < N.End(); ++Np, ++i)
    {
      Ap = A.Begin();
      Bp = B.End() - 1;
      if (i > 0)
        {
          Ap += i;
          overlap = Bsize;
        }
      else
        {
          Bp += i;
          ++overlap;
        }
      dist = A.End() - Ap;
      if (overlap > dist) overlap = dist;
      
      for (j = 0; j < overlap; ++j, ++Ap, --Bp)
        {
         *Np += *Ap * *Bp;
        }
    }


  return N;
}

} // end namespace NeighborhoodAlgorithm
} // end namespace itk
