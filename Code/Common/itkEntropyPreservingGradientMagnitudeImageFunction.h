/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEntropyPreservingGradientMagnitudeImageFunction.h
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
#ifndef _itkEntropyPreservingGradientMagnitudeImageFunction_h
#define _itkEntropyPreservingGradientMagnitudeImageFunction_h

#include "itkImageFunction.h"
#include "itkIndex.h"

namespace itk
{

/**
 * \class EntropyPreservingGradientMagnitudeImageFunction
 * \brief Calculate an entropy satisfying gradient magnitude.
 *
 * EntropyPreservingGradientMagnitudeImageFunction calculates an entropy satisfying image
 * gradient magnitude. This is class is templated over the
 * input image type.
 *
 * In level set methods, the propagating front can form corners
 * as it evolves. At these singularities, the front is no longer
 * differentiable and a weak solution must be constructed to
 * continue the solution.
 *
 * Viscosity or entropy solution can be formed by using upwind
 * finite differencing. Depending on the flow direction, either a forward
 * or backward differencing scheme is used. This is to ensure that only
 * grid points upstream in the flow is used to update the current point.
 *
 * In this function, the flow direction can be specified by the sign
 * of the speed value set via the SetSpeed() method.
 *
 * Reference:
 * "Level Set Methods and Fast Marching Methods", J.A. Sethian,
 * Cambridge Press, Chapter 6, Second edition, 1999.
 *
 * Possible improvements:
 * - the use of Neighborhood operators may improve efficiency.
 *
 */
template < class TInputImage >
class ITK_EXPORT EntropyPreservingGradientMagnitudeImageFunction :
  public ImageFunction< TInputImage, double >
{
public:
  /**
   * Standard "Self" typedef
   */
  typedef EntropyPreservingGradientMagnitudeImageFunction Self;

  /**
   * Standard "Superclass" typedef
   */
  typedef ImageFunction<TInputImage, double> Superclass;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /**
   * InputImageType typedef support.
   */
  typedef TInputImage InputImageType;

  /**
   * Dimension of the underlying image.
   */
  enum { ImageDimension = InputImageType::ImageDimension };

  /**
   * Index typedef support.
   */
  typedef typename Superclass::IndexType IndexType;

  /**
   * Point typedef support.
   */
  typedef typename Superclass::PointType PointType;

  /**
   * Set the input image.
   */
  virtual void SetInputImage( const InputImageType * ptr );

  /**
   * Set the speed parameter.
   */
  void SetSpeed( double value )
    { m_Speed = value; }

  /**
   * Get the speed parameter.
   */
  double GetSpeed() const
    { return m_Speed; }

  /**
   * Evalulate the function at specified index
   */
  virtual double Evaluate( const IndexType& index ) const;
  virtual double Evaluate( const PointType& point ) const
    { return this->Superclass::Evaluate( point ); }

  /**
   * Get the magnitude from last evaluation
   */
  double GetMagnitude() const
    { return m_Magnitude; }

protected:
  EntropyPreservingGradientMagnitudeImageFunction(){};
  EntropyPreservingGradientMagnitudeImageFunction( const Self& ){};
  ~EntropyPreservingGradientMagnitudeImageFunction(){};
  void operator=( const Self& ){};
  void PrintSelf(std::ostream& os, Indent indent);

private:
  signed long             m_ImageSize[ImageDimension];
  bool                    m_ImageSizeOK;

  double                  m_Speed;
  mutable double          m_Magnitude;

  mutable IndexType       m_NeighIndex;
  mutable double          m_CenterValue;
  mutable double          m_DiffValue;

};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkEntropyPreservingGradientMagnitudeImageFunction.txx"
#endif

#endif
