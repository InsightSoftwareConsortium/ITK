/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLightProcessObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLightProcessObject_h
#define __itkLightProcessObject_h

#include "itkObject.h"
#include "itkObjectFactory.h"

namespace itk
{

/** \class LightProcessObject
 * \brief LightProcessObject is the base class for all process objects (source,
          filters, mappers) in the Insight data processing pipeline.
 *
 * LightProcessObject is an abstract object that specifies behavior and
 * interface of visualization network process objects (sources, filters,
 * mappers). Source objects are creators of visualization data; filters
 * input, process, and output visualization data; and mappers transform data
 * into another form (like rendering primitives or write data to a file).
 *
 * A major role of LightProcessObject is to define the inputs and outputs
 * of a filter. More than one input and/or output may exist for a given
 * filter. Some classes (e.g., source objects or mapper objects) will
 * not use inputs (the source) or outputs (mappers). In this case, the
 * inputs or outputs is just ignored.
 *
 * LightProcessObject invokes the following events: 
 * , Command::StartEvent, Command::EndEvent
 * These are convenience events you can use for any purpose
 * (e.g., debugging info, highlighting/notifying user interface, etc.) 
 * See Command and LightObject for information on using AddObserver.
 *
 * Another event Command::ProgressEvent can be observed. Some filters invoke
 * this event periodically during their execution (with the progress,
 * parameter, the fraction of work done). The use is similar to that of
 * StartEvent and EndEvent. Filters may also check their
 * AbortGenerateData flag to determine whether to prematurally end their
 * execution.
 *
 * An important feature of subclasses of LightProcessObject is that it is
 * possible to control the memory-management model (i.e., retain output
 * versus delete output data). If enabled the ReleaseDataFlag enables the
 * deletion of the output data once the downstream process object finishes
 * processing the data (please see text).
 *
 * Subclasses of LightProcessObject may override 4 of the methods of this class
 * to control how a given filter may interact with the pipeline (dataflow).
 * These methods are: GenerateOutputInformation(),
 * EnlargeOutputRequestedRegion(), GenerateInputRequestedRegion(), and
 * GenerateOutputRequestedRegion(). By overriding these methods, a filter
 * can deviate from the base assumptions of the pipeline execution model.
 *
 * \ingroup ITKSystemObjects
 * \ingroup DataProcessing
 */
class ITKCommon_EXPORT LightProcessObject : public Object
{
public:
  /** Standard class typedefs. */
  typedef LightProcessObject       Self;
  typedef Object  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(LightProcessObject,Object);

  /** Set the AbortGenerateData flag for the process object. Process objects
   *  may handle premature termination of execution in different ways.  */
  itkSetMacro(AbortGenerateData,bool);

  /** Get the AbortGenerateData flag for the process object. Process objects
   *  may handle premature termination of execution in different ways.  */
  itkGetConstReferenceMacro(AbortGenerateData,bool);
  
  /** Turn on and off the AbortGenerateData flag. */
  itkBooleanMacro(AbortGenerateData); 
  
  /** Set the execution progress of a process object. The progress is
   * a floating number between (0,1), 0 meaning no progress; 1 meaning
   * the filter has completed execution. */
  itkSetClampMacro(Progress,float,0.0,1.0);

  /** Get the execution progress of a process object. The progress is
   * a floating number between (0,1), 0 meaning no progress; 1 meaning
   * the filter has completed execution. */
  itkGetConstReferenceMacro(Progress,float);

  /** Update the progress of the process object. If a ProgressMethod exists,
   * executes it.  Then set the Progress ivar to amount. The parameter amount
   * should range between (0,1).  */
  void UpdateProgress(float amount);
  
  /** Actually generate new output.  */
  virtual void UpdateOutputData();

protected:
  LightProcessObject();
  ~LightProcessObject();
  void PrintSelf(std::ostream& os, Indent indent) const;
  
  /** This method causes the filter to generate its output. */
  virtual void GenerateData() {}


private:
  LightProcessObject(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /**
   * These support the progress method and aborting filter execution.
   */
  bool  m_AbortGenerateData;
  float m_Progress;
};

} // end namespace itk

#endif

