/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodSampler.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNeighborhoodSampler_h
#define __itkNeighborhoodSampler_h

#include "itkMacro.h"
#include "itkObject.h"
#include "itkSample.h"
#include "itkSubsample.h"
#include "itkFixedArray.h"
#include "itkSampleAlgorithmBase.h"

namespace itk{ 
namespace Statistics{

/** \class NeighborhoodSampler 
 *  \brief generates a Subsample that is sampled from the input sample
 *   using a spherical kernel.
 *
 *  The resulting Subsample has measurement vectors that falls in a
 * hyper-sphere that is defined by a center and a radius. To set
 * the center, use SetCenter method, and to set radius, use SetRadius
 * method. The distance metric is Euclidean one.
 */

template < class TSample >
class ITK_EXPORT NeighborhoodSampler : public SampleAlgorithmBase< TSample >
{
public:
  /** Standard class typedefs */
  typedef NeighborhoodSampler  Self ;
  typedef SampleAlgorithmBase< TSample > Superclass ;
  typedef SmartPointer< Self > Pointer ;
  typedef SmartPointer< const Self > ConstPointer ;

  /** Run-time type information (and related methods) */
  itkTypeMacro(NeighborhoodSampler, SampleAlgorithmBase);

  /** Method for creation through the object factory. */
  itkNewMacro(Self) ;

  /** MeasurementVector typedef support */ 
  typedef TSample SampleType ;

  /** Enums and typedefs from the TSample */
  itkStaticConstMacro(MeasurementVectorSize, unsigned int, 
                      TSample::MeasurementVectorSize) ;
  typedef typename TSample::MeasurementVectorType MeasurementVectorType ;
  typedef typename TSample::MeasurementType MeasurementType ;
  typedef typename TSample::FrequencyType FrequencyType ;
  typedef typename TSample::InstanceIdentifier InstanceIdentifier ;

  /** typedefs from the superclass */
  typedef typename Superclass::InputSampleType InputSampleType ;

  /** Type of the output subsample object */
  typedef Subsample< TSample > SubsampleType ;

  /** Type of the array of the radii */ 
  typedef double RadiusType ;

  /** Type of the array of the radii */ 
  typedef FixedArray< double, MeasurementVectorSize > CenterType ;

  /** Sets the center of the spherical kernel */
  void SetCenter(CenterType* center)
  {
    if ( m_Center != center )
      {
        m_Center = center ;
        this->Modified() ;
      }
  }

  /** Gets the center */
  CenterType* GetCenter() 
  { return m_Center ; }

  /** Sets the radius of the kernel */
  void SetRadius(RadiusType* radius)
  { 
    if ( m_Radius != radius )
      {
        m_Radius = radius ;
        this->Modified() ;
      }
  }

  /** Gets the radius */
  RadiusType* GetRadius()
  { return m_Radius ; } 

  /** Output of this algorithm */
  typedef SubsampleType OutputType ;

  /** Gets the Subsample */
  OutputType* GetOutput() ;

protected:
  NeighborhoodSampler() ;
  virtual ~NeighborhoodSampler() {}
  virtual void PrintSelf(std::ostream& os, Indent indent) const ;

  void GenerateData() ;

private:
  NeighborhoodSampler(const Self&) ; //purposely not implemented
  void operator=(const Self&) ; //purposely not implemented
  
  CenterType* m_Center ;
  RadiusType* m_Radius ;
  typename SubsampleType::Pointer m_Subsample ;
} ; // end of class

} // end of namespace Statistics 
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNeighborhoodSampler.txx"
#endif

#endif
