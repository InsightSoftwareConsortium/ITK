/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkProbesCollectorBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkProbesCollectorBase_h
#define __itkProbesCollectorBase_h

#include "itkConfigure.h"

#include "itkResourceProbe.h"
#include "itkMemoryUsageObserver.h"

namespace itk
{
  
/** \class ProbesCollectorBase
 *  \brief Class for aggregating a set of probes. 
 *
 *  This class defines a set of ResourceProbes and assign names to them.
 *  The user can start and stop each one of the probes by addressing them by name.
 *  
 *  \sa ResourceProbe
 *
 */
template <class TProbe>
class ITK_EXPORT ProbesCollectorBase 
{
public:
  typedef std::string               IdType;
  typedef std::map<IdType,TProbe>   MapType;
  
  /** destructor */
  virtual ~ProbesCollectorBase();

  /** Start a probe with a particular name. If the time probe does not
   * exist, it will be created */
  virtual void Start( const char * name );

  /** Stop a time probe identified with a name */
  virtual void Stop( const char * name );
  
  /** Report the summary of results from the probes */
  virtual void Report( std::ostream & os = std::cout ) const;

  /** Destroy the set of probes. New probes can be created after invoking this method. */
  virtual void Clear(void);

protected:
  MapType   m_Probes;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkProbesCollectorBase.txx"
#endif

#endif //__itkProbesCollectorBase_h
