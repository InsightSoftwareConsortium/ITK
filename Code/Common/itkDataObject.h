/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDataObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkDataObject_h
#define __itkDataObject_h

#include "itkObject.h"
#include "itkObjectFactory.h"

namespace itk
{

class ProcessObject;

/** \class DataObject
 * This is the base class for all data objects in the Insight
 * data processing pipeline. A data object is an object that represents
 * and provides access to data. ProcessObjects operate on data object,
 * producing new data objects as output. ProcessObject and DataObject
 * can be connected together into data flow pipelines.
 * 
 * \sa ProcessObject
 * \sa ImageBase
 * \sa Mesh
 */
class ITK_EXPORT DataObject : public Object
{
public:
  /** 
   * Smart pointer typedef support.
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

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(DataObject,Object);

  /** 
   * Set the source object creating this data object. 
   */
  void SetSource(ProcessObject *s);
  
  /** 
   * Get the source object creating this data object. 
   */
  itkGetObjectMacro(Source,ProcessObject);
  
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
    {this->SetGlobalReleaseDataFlag(true);};
  void GlobalReleaseDataFlagOff() 
    {this->SetGlobalReleaseDataFlag(false);};

  /** 
   * Release data back to system to conserve memory resource. Used during
   * visualization network execution.  Releasing this data does not make
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
   * consistency before access. Also causes owning source/filter 
   * (if any) to update itself. The Update() method is composed of 
   * UpdateOutputInformation(), PropagateRequestedRegion(), and UpdateOutputData().
   */
  virtual void Update();

  /**
   * Methods to update the pipeline.
   */
  virtual void UpdateOutputInformation() = 0;
  virtual void PropagateRequestedRegion();
  virtual void UpdateOutputData();

  /**
   * More internal methods to update the pipeline.
   */
  void SetPipelineMTime(unsigned long time) 
    {m_PipelineMTime = time; }
  itkGetMacro(PipelineMTime,unsigned long);

  virtual void PrepareForNewData() 
    {this->Initialize();};
  void DataHasBeenGenerated();
  void ComputeEstimatedPipelineMemorySize(unsigned long sizes[3]);
  unsigned long GetEstimatedPipelineMemorySize();
  virtual unsigned long GetEstimatedMemorySize();
  virtual unsigned long GetActualMemorySize();

  virtual void SetRequestedRegionToLargestPossibleRegion() = 0;
  virtual void CopyInformation(DataObject *data) = 0;
  virtual bool RequestedRegionIsOutsideOfTheBufferedRegion() = 0;
  virtual bool VerifyRequestedRegion() = 0;

  /** 
   * Handle the source/data loop. 
   */
  void UnRegister();

  /** 
   * Get the net reference count. That is the count minus
   * any self created loops. This is used in the Source/Data
   * registration to properly free the objects. 
   */
  virtual int GetNetReferenceCount() const
    {return this->GetReferenceCount();}

protected:
  DataObject();
  ~DataObject();
  DataObject(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent);

  // Was the update extent propagated down the pipeline
  bool m_LastRequestedRegionWasOutsideOfTheBufferedRegion;

  // First update, the update region will be set to the largest possible
  // region.
  bool m_RequestedRegionInitialized;  

private:
  ProcessObject *m_Source; ///Who generated this data as output?

  TimeStamp m_UpdateTime;  ///When was this data last generated?

  bool m_ReleaseDataFlag; ///Data will release after use by a filter if on
  bool m_DataReleased; ///Keep track of data release during network execution

  // The Maximum MTime of all upstream filters and data objects.
  // This does not include the MTime of this data object.
  unsigned long m_PipelineMTime;

  // A guess at how much memory would be consumed by the data object
  // if the LargestPossibleRegion were updated.
  unsigned long m_EstimatedWholeMemorySize;

  // How many upstream filters are local to the process.
  // This supports distributed processing (i.e., asynchronous updates).
  float m_Locality;  

  
  /**
   * Static member that controls global data release after use by filter
   */
  static bool m_GlobalReleaseDataFlag;
};

} // end namespace itk
  
#endif
