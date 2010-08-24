/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodSampler.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNeighborhoodSampler_h
#define __itkNeighborhoodSampler_h

#include "itkSampleToSubsampleFilter.h"
#include "itkSimpleDataObjectDecorator.h"

namespace itk
{
namespace Statistics
{
/** \class NeighborhoodSampler
 * \brief Generates a Subsample out of a Sample, based on a user-provided
 * distance to a MeasurementVector.
 *
 * This filter will take as input a Sample and produce as output a Subsample
 * containing the instances of the sample that are at a distance lower than
 * a user-provided threshold from a user-provided measurement vector.
 *
 * \sa Sample, Subsample
 *
 * \sa SampleToSubsampleFilter
 *
 */

template< class TSample >
class ITK_EXPORT NeighborhoodSampler:public SampleToSubsampleFilter< TSample >
{
public:
  /** Standard class typedefs. */
  typedef NeighborhoodSampler                Self;
  typedef SampleToSubsampleFilter< TSample > Superclass;
  typedef SmartPointer< Self >               Pointer;
  typedef SmartPointer< const Self >         ConstPointer;

  /** Standard macros */
  itkTypeMacro(NeighborhoodSampler, SampleToSubsampleFilter);
  itkNewMacro(Self);

  /** Typedefs for Measurement vector, measurement, Instance Identifier,
   * frequency, size, size element value from the template argument TSample */
  itkSuperclassTraitMacro(SampleType)
  itkSuperclassTraitMacro(MeasurementVectorType)
  itkSuperclassTraitMacro(MeasurementType)
  itkSuperclassTraitMacro(InstanceIdentifier)
  itkSuperclassTraitMacro(SubsampleType)
  itkSuperclassTraitMacro(OutputType)

  /** Type of the distance radius. */
  typedef double RadiusType;

  /** Type of DataObjects to use for distance radius input. */
  typedef SimpleDataObjectDecorator< RadiusType > InputRadiusObjectType;

  /** Method to set the input value of the Radius */
  itkSetDecoratedInputMacro(Radius, RadiusType, 1);
protected:
  NeighborhoodSampler();
  virtual ~NeighborhoodSampler();
  void PrintSelf(std::ostream & os, Indent indent) const;

  void GenerateData();

private:
  NeighborhoodSampler(const Self &); //purposely not implemented
  void operator=(const Self &);      //purposely not implemented
};                                   // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNeighborhoodSampler.txx"
#endif

#endif
