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
 * \sa MeshBase
 */
class ITK_EXPORT DataObject : public Object
{
public:
  /** 
   * Smart pointer typedef support.
   */
  typedef DataObject          Self;
  typedef SmartPointer<Self>  Pointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(DataObject,Object);

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /** 
   * Set the source object creating this data object. 
   */
  void SetSource(ProcessObject *s);
  
  /** 
   * Get the source object creating this data object. 
   */
  itkGetObjectMacro(Source,ProcessObject);
  
  /** 
   * Set the dimension (number of independent variables) of the data.
   * This method has the side effect of allocating memory related to
   * dimension size (e.g., WholeExtent).
   */
  virtual void SetDimension(unsigned int dim);

  /** 
   * Get the dimension of the data.
   */
  itkGetMacro(Dimension,unsigned int);

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
   * UpdateInformation(), PropagateUpdateExtent(), 
   * TriggerAsynchronousUpdate(), and UpdateData().
   */
  virtual void Update();

  /**
   * Methods to update the pipeline.
   */
  virtual void UpdateInformation();
  virtual void PropagateUpdateExtent();
  virtual void TriggerAsynchronousUpdate();
  virtual void UpdateData();

  /**
   * More internal methods to update the pipeline.
   */
  void SetPipelineMTime(unsigned long time) 
    {m_PipelineMTime = time; }
  itkGetMacro(PipelineMTime,unsigned long);

  /**
   * Enums used to describe the extent types.
   */
  typedef enum {ITK_UNSTRUCTURED_EXTENT,ITK_STRUCTURED_EXTENT} ExtentType;
  
  virtual int GetExtentType() 
    {return DataObject::ITK_UNSTRUCTURED_EXTENT;}
  void SetUpdateExtentToWholeExtent();
  virtual void PrepareForNewData() 
    {this->Initialize();};
  void DataHasBeenGenerated();
  void ComputeEstimatedPipelineMemorySize(unsigned long sizes[3]);
  unsigned long GetEstimatedPipelineMemorySize();
  virtual unsigned long GetEstimatedMemorySize();
  virtual unsigned long GetActualMemorySize();
  void CopyInformation(DataObject *data);
  virtual bool UpdateExtentIsOutsideOfTheExtent();
  virtual bool VerifyUpdateExtent();

  /**
   * Get the whole extent of this data object
   */
  const int *GetWholeExtent() const
    {return m_WholeExtent;}
      
  /**
   * Get the maximum number of pieces thaT this data can be
   * separated into.
   */
  int GetMaximumNumberOfPieces() const
    {return m_MaximumNumberOfPieces;}
      
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

private:
  ProcessObject *m_Source; ///Who generated this data as output?

  TimeStamp m_UpdateTime;  ///When was this data last generated?

  bool m_ReleaseDataFlag; ///Data will release after use by a filter if on
  bool m_DataReleased; ///Keep track of data release during network execution

  // The Maximum MTime of all upstream filters and data objects.
  // This does not include the MTime of this data object.
  unsigned long m_PipelineMTime;

  // A guess at how much memory would be consumed by the data object
  // if the WholeExtent were updated.
  unsigned long m_EstimatedWholeMemorySize;

  unsigned int m_Dimension; ///The dimension of the data (1, 2, 3, or n-D)

  // If the ExtentType is ITK_STRUCTURED_EXTENT, then these three extent 
  // variables represent the whole extent, the extent currently in memory, 
  // and the requested update extent. The extent is given as m_Dimension 
  // min/max pairs (i.e., the image is n-dimensional).
  int *m_WholeExtent;
  int *m_Extent;
  int *m_UpdateExtent;

  // If the ExtentType is ITK_UNSTRUCTURED_EXTENT, then these three variables
  // represent the maximum number of pieces that the data object can be
  // broken into, which piece out of how many is currently in the extent, and
  // the number of pieces and the specific piece requested for the
  // update. Data objects that do not support any division of the data can
  // simply leave the MaximumNumberOfPieces as 1. The NumberOfPieces and
  // Piece are similar to the Extent. The UpdateNumberOfPieces and
  // UpdatePiece are similar to the UpdateExtent. The WholeExtent is always
  // piece = 0 and number of pieces = 1;
  int m_MaximumNumberOfPieces;
  int m_NumberOfPieces;
  int m_Piece;
  int m_UpdateNumberOfPieces;
  int m_UpdatePiece;

  // How many upstream filters are local to the process.
  // This supports distributed processing (i.e., asynchronous updates).
  float m_Locality;  

  // Was the update extent propagated down the pipeline
  bool m_LastUpdateExtentWasOutsideOfTheExtent;

  
  /**
   * Static member that controls global data release after use by filter
   */
  static bool m_GlobalReleaseDataFlag;
};

} // end namespace itk
  
#endif
