/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRT3DTransform.h
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

#ifndef __itkRT3DTransform_h
#define __itkRT3DTransform_h

#include "itkAffineTransform.h"
#include "vnl/vnl_math.h"



namespace itk
{


template < 
  class TScalarType=double,         // Data type for scalars (e.g. float or double)
  unsigned int NDimensions=3,       // Number of dimensions in the input space
  class TParameters = Point< double, NDimensions*(NDimensions+1) >,
  class TJacobianType = Matrix<double,NDimensions,NDimensions*(NDimensions+1) >
  > 
class RT3DTransform : public AffineTransform< TScalarType,
                                              NDimensions,
                                              TParameters,
                                              TJacobianType >
{

public:

    /**
     * Standard Self Typedef
     */
    typedef RT3DTransform  Self;

    /// Dimension of the domain space
    enum { SpaceDimension = NDimensions,
           ParametersDimension = NDimensions * (NDimensions+1) };


    /**
     * Standard "Superclass" typedef.
     */
    typedef AffineTransform<  TScalarType, NDimensions,
                              TParameters, TJacobianType >  Superclass;


    /** 
     * Smart pointer typedef support 
     */
    typedef SmartPointer<Self>        Pointer;
    typedef SmartPointer<const Self>  ConstPointer;


    /** 
     * Run-time type information (and related methods).
     */
    itkTypeMacro( RT3DTransform, AffineTransform);


    /** 
     * New macro for creation of through a Smart Pointer
     */
    itkNewMacro( Self );



    /**
     * Parameters Type
     */
    typedef typename Superclass::ParametersType  ParametersType;


    /**
     * Jacobian Type
     */
    typedef typename Superclass::JacobianType  JacobianType;


    /// Standard scalar type for this class
    typedef typename Superclass::ScalarType ScalarType;


    /**
     * Standard coordinate point type for this class
     */
//    typedef Point<TScalarType, SpaceDimension> InputPointType;
//    typedef Point<TScalarType, SpaceDimension> OutputPointType;
    typedef  typename Superclass::InputPointType     InputPointType;
    typedef  typename Superclass::OutputPointType    OutputPointType;

    
    /**
     * Standard matrix type for this class
     */
    typedef Matrix<TScalarType, SpaceDimension, SpaceDimension> MatrixType;


    /**
     * Set the transformation parameters
     *
     **/
    void SetRT3DParameters( const double sampleSize, 
                            const double blanking,
                            const long maxTheta, 
                            const long maxPhi, 
                            const double azimuthAngleSeparation,
                            const double elevationAngleSeparation);

    void SetRT3DParameters( const double sampleSize, 
                            const double blanking,
                            const long maxTheta, 
                            const long maxPhi);


    /**
     * Transform from azimuth-elevation to cartesian
     *
     **/
    OutputPointType     TransformPoint (const InputPointType  &point ) const;

    /**
     * Back transform from cartesian to azimuth-elevation
     *
     **/
    inline InputPointType      BackTransform(const OutputPointType  &point ) const;
    InputPointType  BackTransformPoint(const OutputPointType  &point) const;

    /**
     * Print contents of an AffineTransform
     **/
    void PrintSelf(std::ostream &s, Indent indent) const;
    
    void SetForwardIsIndexToPhysical();
    void SetForwardIsPhysicalToIndex();
    OutputPointType TransformAzElToCartesian(const InputPointType &point) const; 
    OutputPointType TransformCartesianToAzEl(const OutputPointType &point) const;

protected:

    /**
     * Create an RT3DTransform object
     **/
    RT3DTransform();      

    /**
     * Destroy an RT3DTransform object
     **/
    virtual ~RT3DTransform();


private:

      long m_MaxPhi;
      long m_MaxTheta;
      double m_RadiusSampleSize;
      double m_AzimuthAngularSeparation;
      double m_ElevationAngularSeparation;
      double m_FirstSampleDistance;
      double m_Blanking;
      double m_TransducerOffset;

      bool    m_ForwardIsIndexToPhysical;
}; //class RT3DTransform


}  // namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkRT3DTransform.txx"
#endif

#endif /* __itkRT3DTransform_h */


