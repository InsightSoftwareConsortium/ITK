/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDataObject.h
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
#ifndef __itkDataObject_h
#define __itkDataObject_h

#include "itkObject.h"
#include "itkSmartPointerForwardReference.h"
#include "itkWeakPointer.h"
#include "itkExceptionObject.h"

namespace itk
{

class ProcessObject;
class DataObject;

  
/**
 * Exception object for DataObject exceptions
 */
class ITK_EXPORT DataObjectError : public ExceptionObject
{
 public:
  /**
   * Default constructor.  Needed to ensure the exception object can be
   * copied.
   */
  DataObjectError();
  
  /**
   * Constructor. Needed to ensure the exception object can be copied.
   */
  DataObjectError(const char *file, unsigned int lineNumber);

  /**
   * Constructor. Needed to ensure the exception object can be copied.
   */
  DataObjectError(const std::string& file, unsigned int lineNumber);

  /**
   * Copy constructor.  Needed to ensure the exception object can be copied.
   */
  DataObjectError(const DataObjectError &orig);

  /**
   * Operator=.  Needed to ensure the exception object can be copied.
   */
  DataObjectError& operator=( const DataObjectError& orig);

  /**
   * Standard type macro
   */
  itkTypeMacro(DataObjectError, ExceptionObject);

  /**
   * Set the data object that is throwing this exception.
   */
  void SetDataObject(DataObject *dobj);

  /**
   * Get the data object that is throwing this exception.
   */
  SmartPointer<DataObject> GetDataObject();

 protected:
  /**
   * Print exception information.  This method can be overridden by
   * specific exception subtypes.  The default is to print out the
   * location where the exception was first thrown and any description
   * provided by the ``thrower''.  
   */
  virtual void PrintSelf(std::ostream& os, Indent indent) const;
  
    
 private:
  SmartPointer<DataObject> m_DataObject;
};

  
/**
 * Exception object for invalid requested region
 */
class ITK_EXPORT InvalidRequestedRegionError : public DataObjectError
{
 public:
  /**
   * Default constructor. Needed to ensure the exception object can be copied.
   */
  InvalidRequestedRegionError();
  
  /**
   * Constructor. Needed to ensure the exception object can be copied.
   */
  InvalidRequestedRegionError(const char *file, unsigned int lineNumber);

  /**
   * Constructor. Needed to ensure the exception object can be copied.
   */
  InvalidRequestedRegionError(const std::string& file, unsigned int lineNumber);

  /**
   * Copy constructor.  Needed to ensure the exception object can be copied.
   */
  InvalidRequestedRegionError(const InvalidRequestedRegionError &orig);

  /**
   * Operator=.  Needed to ensure the exception object can be copied.
   */
  InvalidRequestedRegionError& operator=( const InvalidRequestedRegionError& orig);

  /**
   * Standard type macro
   */
  itkTypeMacro(InvalidRequestedRegionError, DataObjectError);

 protected:
  /**
   * Print exception information.  This method can be overridden by
   * specific exception subtypes.  The default is to print out the
   * location where the exception was first thrown and any description
   * provided by the ``thrower''.  
   */
  virtual void PrintSelf(std::ostream& os, Indent indent) const;
  
    
 private:
};
  

/** \class DataObject
 * This is the base class for all data objects in the Insight
 * data processing pipeline. A data object is an object that represents
 * and provides access to data. ProcessObjects (i.e., filters) operate 
 * on input data objects, producing new data objects as output. 
 * ProcessObject and DataObject are connected together into data flow 
 * pipelines.
 * 
 * \sa ProcessObject
 * \sa ImageBase
 * \sa Mesh
 * \ingroup DataRepresentation
 * \ingroup ITKSystemObjects
 */
class ITK_EXPORT DataObject : public Object
{
public:
  /** 
   * Standard "Self" typedef.
   */
  typedef DataObject          Self;

  /**
   * Standard "Superclass" typedef.
   */
  typedef Object  Superclass;

  /**
   * Standard smart pointer typedef.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(DataObject,Object);

  /**
   * Separate this data object from the pipeline.  This routine disconnects
   * a data object from the upstream pipeline. Hence an Update() from
   * downstream will not propagate back past this data object.  To completely
   * isolate this data object from the pipeline, the application must remove
   * this data object from any filters which it is connected as the input.
   */
  void DisconnectPipeline() const;
  
  /** 
   * Get the process object that generated this data object.
   * If there is no process object, then the data object has
   * been disconnected from the pipeline, or the data object
   * was created manually. (Note: we cannot use the GetObjectMacro()
   * defined in itkMacro because the mutual dependency of
   * DataObject and ProcessObject causes compile problems. Also,
   * a forward reference smart pointer is returned, not a smart pointer, 
   * because of the circular dependency between the process and data object.)
   *
   * GetSource() returns a SmartPointerForwardReference and not a WeakPointer
   * because it is assumed the code calling GetSource() wants to hold a
   * long term reference to the source.
   */
  SmartPointerForwardReference<ProcessObject> GetSource() const;

  /**
   * Which of the source's outputs corresponds to this data object?
   */
  unsigned int GetSourceOutputIndex() const;
    
  /** 
   * Restore the data object to its initial state. This means releasing
   * memory.
   */
  virtual void Initialize();

  /** 
   * Turn on/off a flag to control whether this object's data is released
   * after being used by a filter. 
   */
  itkSetMacro(ReleaseDataFlag,bool);
  itkGetMacro(ReleaseDataFlag,bool);
  itkBooleanMacro(ReleaseDataFlag);

  /** 
   * Turn on/off a flag to control whether every object releases its data
   * after being used by a filter. Being a global flag, it controls the
   * behavior of all DataObjects and ProcessObjects.
   */
  static void SetGlobalReleaseDataFlag(const bool val);
  static bool GetGlobalReleaseDataFlag();
  void GlobalReleaseDataFlagOn() 
    {this->SetGlobalReleaseDataFlag(true);}
  void GlobalReleaseDataFlagOff() 
    {this->SetGlobalReleaseDataFlag(false);}

  /** 
   * Release data back to system to conserve memory resource. Used during
   * pipeline execution.  Releasing this data does not make
   * down-stream data invalid, so it does not modify the MTime of this data
   * object.  
   */
  void ReleaseData();

  /** 
   * Return flag indicating whether data should be released after use  
   * by a filter. 
   */
  bool ShouldIReleaseData() const;

  /** 
   * Get the flag indicating the data has been released. 
   */
  bool GetDataReleased() const 
    {return m_DataReleased;}
  
  /**
   * Provides opportunity for the data object to insure internal
   * consistency before access. Also causes owning source/filter (if
   * any) to update itself. The Update() method is composed of
   * UpdateOutputInformation(), PropagateRequestedRegion(), and
   * UpdateOutputData(). This method may call methods that throw an
   * InvalidRequestedRegionError exception. This exception will leave
   * the pipeline in an inconsistent state.  You will need to call
   * ResetPipeline() on the last ProcessObject in your pipeline in
   * order to restore the pipeline to a state where you can call
   * Update() again.
   */
  virtual void Update();

  /**
   * Methods to update the pipeline.
   */
  virtual void UpdateOutputInformation() = 0;
  virtual void PropagateRequestedRegion() throw (InvalidRequestedRegionError);
  virtual void UpdateOutputData();

  /**
   * Reset the pipeline. If an exception is thrown during an Update(),
   * the pipeline may be in an inconsistent state.  This method clears
   * the internal state of the pipeline so Update() can be called.
   */
  virtual void ResetPipeline();

  /**
   * More internal methods to update the pipeline.
   */
  void SetPipelineMTime(unsigned long time) 
    {m_PipelineMTime = time;}
  itkGetMacro(PipelineMTime,unsigned long);

  virtual void PrepareForNewData() 
    {this->Initialize();}
  void DataHasBeenGenerated();
  void ComputeEstimatedPipelineMemorySize(unsigned long sizes[3]);
  unsigned long GetEstimatedPipelineMemorySize();
  virtual unsigned long GetEstimatedMemorySize();
  virtual unsigned long GetActualMemorySize();

  virtual void SetRequestedRegionToLargestPossibleRegion() = 0;
  virtual bool RequestedRegionIsOutsideOfTheBufferedRegion() = 0;
  virtual bool VerifyRequestedRegion() = 0;

  /**
   * Copy information from the specified data set.  This method is
   * part of the pipeline execution model. By default, a ProcessObject
   * will copy meta-data from the first input to all of its
   * outputs. See ProcessObject::GenerateOutputInformation().  Each
   * subclass of DataObject is responsible for being able to copy
   * whatever meta-data it needs from from another DataObject.
   * The default implementation of this method is empty. If a subclass
   * overrides this method, it should always call its superclass'
   * version.
   */
  virtual void CopyInformation(DataObject *data) {};
  
  /**
   * Set the requested region from this data object to match the requested
   * region of the data object passed in as a parameter.  This method is
   * implemented in the concrete subclasses of DataObject.
   */
  virtual void SetRequestedRegion(DataObject *data) = 0;

  
  
protected:
  DataObject();
  ~DataObject();
  DataObject(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /**
   * Propagate a call to ResetPipeline(). Called only from ProcessObject.
   */
  virtual void PropagateResetPipeline();
  
  // Was the update extent propagated down the pipeline?
  bool m_LastRequestedRegionWasOutsideOfTheBufferedRegion;

  // First update, the update region will be set to the largest possible
  // region.
  bool m_RequestedRegionInitialized;  

private:
  //Who generated this data?
  mutable WeakPointer<ProcessObject> m_Source; 
  mutable unsigned int m_SourceOutputIndex;
  
  //When was this data last generated?
  TimeStamp m_UpdateTime;  

  bool m_ReleaseDataFlag; //Data will release after use by a filter if on
  bool m_DataReleased; //Keep track of data release during pipeline execution

  // The Maximum MTime of all upstream filters and data objects.
  // This does not include the MTime of this data object.
  unsigned long m_PipelineMTime;

  // A guess at how much memory would be consumed by the data object
  // if the LargestPossibleRegion were updated.
  unsigned long m_EstimatedWholeMemorySize;

  // Static member that controls global data release after use by filter.
  //
  static bool m_GlobalReleaseDataFlag;

  /** 
   * Connect the specified process object to the data object. This
   * should only be called from a process object. 
   */
  void ConnectSource(ProcessObject *s, unsigned int idx) const;

  /** 
   * Disconnect the specified process object from the data object. This
   * should only be called from a process object. An application should
   * call DataObject::DisconnectPipeline() if it wants to disconnect a data 
   * object from a pipeline.
   */
  void DisconnectSource(ProcessObject *s, unsigned int idx) const;
  
  /**
   * Friends of DataObject
   */
  friend class ProcessObject;
  friend class DataObjectError;
};

} // end namespace itk
  
#endif
