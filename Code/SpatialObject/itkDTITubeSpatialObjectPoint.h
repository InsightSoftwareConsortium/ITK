/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDTITubeSpatialObjectPoint.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkDTITubeSpatialObjectPoint_h
#define __itkDTITubeSpatialObjectPoint_h

#include "itkTubeSpatialObjectPoint.h"
#include "itkCovariantVector.h"
#include "vnl/vnl_vector_fixed.h"

namespace itk 
{

/** \class DTITubeSpatialObjectPoint
* \brief Point used for a tube definition
*
* This class contains all the functions necessary to define a point
* that can be used to build tubes.
*
* \sa DTITubeSpatialObject 
*/ 
template < unsigned int TPointDimension = 3 >
class DTITubeSpatialObjectPoint 
  : public TubeSpatialObjectPoint<TPointDimension>
{

public:

  typedef DTITubeSpatialObjectPoint              Self;
  typedef SpatialObjectPoint<TPointDimension> Superclass;
  typedef Point< double, TPointDimension >    PointType;
  typedef Vector<double, TPointDimension >    VectorType;
  typedef CovariantVector<double, TPointDimension >    CovariantVectorType;
 
  /** Constructor. This one defines the # of dimensions in the 
   * DTITubeSpatialObjectPoint */
  DTITubeSpatialObjectPoint( void );

  /** Default destructor. */
  virtual ~DTITubeSpatialObjectPoint( void );

  /** Set/Get FA */
  void  SetFA(const float fa) {m_FA = fa;}
  float GetFA() const {return m_FA;}
 
  /** Set/Get ADC */
  void  SetADC(const float adc) {m_ADC = adc;}
  float GetADC() const {return m_ADC;}
  
  /** Set/Get GA */
  void  SetGA(const float ga) {m_GA = ga;}
  float GetGA() const {return m_GA;}

  /** Set/Get Lambda1 */
  void  SetLambda1(const float lambda1) {m_Lambda1 = lambda1;}
  float GetLambda1() const {return m_Lambda1;}

  /** Set/Get Lambda1 */
  void  SetLambda2(const float lambda2) {m_Lambda2 = lambda2;}
  float GetLambda2() const {return m_Lambda2;}

  /** Set/Get Lambda1 */
  void  SetLambda3(const float lambda3) {m_Lambda3 = lambda3;}
  float GetLambda3() const {return m_Lambda3;}

  /** Set/Get the minimum eigen value */
  void SetMinEigenValue(float* val)
    {
    for(unsigned int i=0;i<3;i++)
      {
      m_MinEV[i] = val[i];
      }
    }
  const float* GetMinEigenValue() const {return m_MinEV;}

  /** Set/Get the medium eigen value */
  void SetMedEigenValue(float* val)
    {
    for(unsigned int i=0;i<3;i++)
      {
      m_MedEV[i] = val[i];
      }
    }
  const float* GetMedEigenValue() const {return m_MedEV;}

  /** Set/Get the maximum eigen value */
  void SetMaxEigenValue(float* val)
    {
    for(unsigned int i=0;i<3;i++)
      {
      m_MaxEV[i] = val[i];
      }
    }
  const float* GetMaxEigenValue() const {return m_MaxEV;}

  /** Set/Get the MRI field */
  void SetMRI(float* mri)
    {
    for(unsigned int i=0;i<5;i++)
      {
      m_MRI[i] = mri[i];
      }
    }

  const float* GetMRI() const {return m_MRI;}

  /** Set/Get the tensor matrix */
  void SetTensorMatrix(float* matrix)
    {
    for(unsigned int i=0;i<6;i++)
      {
      m_TensorMatrix[i] = matrix[i];
      }
    }

  const float* GetTensorMatrix() const {return m_TensorMatrix;}

  /** Set/Get Interpolation */
  void  SetInterpolation(const int interp) {m_Interpolation = interp;}
  int GetInterpolation() const {return m_Interpolation;}

  /** Copy one DTITubeSpatialObjectPoint to another */
  Self & operator=(const DTITubeSpatialObjectPoint & rhs);

protected:

  float m_FA;
  float m_ADC;
  float m_GA;
  float m_Lambda1;
  float m_Lambda2;
  float m_Lambda3;
  float m_MinEV[3];
  float m_MedEV[3];
  float m_MaxEV[3];
  float m_MRI[5];
  float m_TensorMatrix[6];
  int   m_Interpolation;


  /** # of dimensions */
  unsigned short int m_NumDimensions;

  /** Print the object */
  void PrintSelf( std::ostream & os, Indent indent) const;
};

} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDTITubeSpatialObjectPoint.txx"
#endif

#endif // __itkDTITubeSpatialObjectPoint_h
