/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkAdaptiveEqualizationHistogram_h
#define itkAdaptiveEqualizationHistogram_h

#include "itksys/hash_map.hxx"
#include "itkStructHashFunction.h"
#include "itkMath.h"
#include <cmath>
namespace itk
{
namespace Function
{

/* \class AdaptiveEqualizationHistogram
 *
 * Implements the function class for a moving histogram algorithm for
 * adaptive histogram equalization.
 *
 * \sa AdaptiveHistogramEqualizationImageFilter
 * \sa MovingHistogramImageFilter
 * \ingroup ITKImageStatistics
 */
template< class TInputPixel, class TOutputPixel >
class AdaptiveEqualizationHistogram
{
public:

  typedef float RealType;

  AdaptiveEqualizationHistogram()
    : m_BoundaryCount(0)
    {
    }

  // ~AdaptiveEqualizationHistogram()  {} default is ok

  void AddPixel(const TInputPixel & p)
  {
    m_Map[p]++;
  }

  void RemovePixel(const TInputPixel & p)
  {

    // insert new item if one doesn't exist
    typename MapType::iterator it = m_Map.find( p );

    itkAssertInDebugAndIgnoreInReleaseMacro( it != m_Map.end() );

    if ( --(it->second) == 0 )
      {
      m_Map.erase( it );
      }

  }

  TOutputPixel GetValue(const TInputPixel &pixel)
    {

      // Normalize input pixels to [-0.5 0.5] gray level.
      // AdaptiveHistogramEqualization compute kernel components with
      // float, but use double for accumulate and temporaries.
      const double iscale = (double)m_Maximum - m_Minimum;

      double sum = 0.0;
      typename MapType::iterator itMap = m_Map.begin();
      const RealType u = ( (double)pixel - m_Minimum ) / iscale - 0.5;
      while ( itMap != m_Map.end() )
        {
        const RealType v =  ( (double)itMap->first - m_Minimum ) / iscale - 0.5;
        const double ikernel =  m_KernelSize - m_BoundaryCount;
        sum += itMap->second * CumulativeFunction(u,v) / ikernel;

        ++itMap;
        }

      return (TOutputPixel)( iscale * ( sum + 0.5 ) + m_Minimum );
    }

  void AddBoundary() {++m_BoundaryCount;}

  void RemoveBoundary() {--m_BoundaryCount;}

  void SetAlpha( RealType alpha ) {m_Alpha=alpha;}
  void SetBeta( RealType beta ) {m_Beta=beta;}
  void SetKernelSize( RealType kernelSize ) {m_KernelSize=kernelSize;}

  void SetMinimum( TInputPixel minimum ) {m_Minimum=minimum;}
  void SetMaximum( TInputPixel maximum ) {m_Maximum=maximum;}

private:
  RealType m_Alpha;
  RealType m_Beta;
  RealType m_KernelSize;

  TInputPixel m_Minimum;
  TInputPixel m_Maximum;

  RealType CumulativeFunction(RealType u, RealType v)
  {
    // Calculate cumulative function
    const RealType s = itk::Math::sgn(u - v);
    const RealType ad = itk::Math::abs( 2.0 * ( u - v ) );

    return 0.5 * s * std::pow(ad, m_Alpha) - m_Beta * 0.5 * s * ad + m_Beta * u;
  }

private:
  typedef typename itksys::hash_map< TInputPixel,
                                     size_t,
                                     StructHashFunction< TInputPixel > > MapType;


  MapType       m_Map;
  size_t        m_BoundaryCount;

};

} // end namespace Function
} // end namespace itk

#endif  // itkAdaptiveHistogramHistogram_h
