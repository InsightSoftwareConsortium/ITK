/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkProcessObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkProcessObject_h
#define __itkProcessObject_h

#include "itkObject.h"
#include "itkDataObject.h"
#include "itkObjectFactory.h"
#include <vector>

namespace itk
{

/** \class ProcessObject
 * \brief ProcessObject is the base class for all process objects (source,
          filters, mappers) in the Insight data processing pipeline.
 *
 * ProcessObject is an abstract object that specifies behavior and
 * interface of visualization network process objects (sources, filters,
 * mappers). Source objects are creators of visualization data; filters
 * input, process, and output visualization data; and mappers transform data
 * into another form (like rendering primitives or write data to a file).
 *
 * A major role of ProcessObject is to define the inputs and outputs
 * of a filter. More than one input and/or output may exist for a given
 * filter. Some classes (e.g., source objects or mapper objects) will
 * not use inputs (the source) or outputs (mappers). In this case, the
 * inputs or outputs is just ignored.
 *
 * ProcessObject provides a mechanism for invoking the methods
 * StartMethod() and EndMethod() before and after object execution (via
 * GenerateData()). These are convenience methods you can use for any purpose
 * (e.g., debugging info, highlighting/notifying user interface, etc.) These
 * methods accept a single void* pointer that can be used to send data to the
 * methods. It is also possible to specify a function to delete the argument
 * via StartMethodArgDelete and EndMethodArgDelete.
 *
 * Another method, ProgressMethod() can be specified. Some filters invoke
 * this method periodically during their execution (with the progress,
 * parameter, the fraction of work done). The use is similar to that of
 * StartMethod() and EndMethod(). Filters may also check their
 * AbortGenerateData flag to determine whether to prematurally end their
 * execution.
 *
 * An important feature of subclasses of ProcessObject is that it is
 * possible to control the memory-management model (i.e., retain output
 * versus delete output data). If enabled the ReleaseDataFlag enables the
 * deletion of the output data once the downstream process object finishes
 * processing the data (please see text).
 *
 * Subclasses of ProcessObject may override 3 of the methods of this class
 * to control how a given filter may interact with the pipeline (dataflow).
 * These methods are: UpdateOutputInformation(),
 * EnlargeOutputRequestedRegion(), GenerateInputRequestedRegion(). By
 * overriding these methods, a filter deviate from the base assumptions
 * of the pipeline execution model.
 *       
 */
class ITK_EXPORT ProcessObject : public Object
{
public:
  /** 
   * Standard "Self" typedef.
   */
  typedef ProcessObject       Self;
  
  /**
   * Standard "Superclass" typedef.
   */
  typedef Object  Superclass;

  /** 
   * Smart pointer typedef support. 
   */
  typedef SmartPointer<Self>  Pointer;

  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(ProcessObject,Object);

  /** 
   * Smart Pointer type to a DataObject.
   */
  typedef DataObject::Pointer DataObjectPointer;
  /** 
   * STL Array of SmartPointers to DataObjects
   */
  typedef std::vector<DataObjectPointer> DataObjectPointerArray;

  /** 
   * Return an array with all the inputs of this process object.
   * This is useful for tracing back in the pipeline to construct
   * graphs etc. 
   */
  DataObjectPointerArray GetInputs() 
    {return m_Inputs;};
  int GetNumberOfInputs() const
    {return m_Inputs.size();}

  /** 
   * Return an array with all the inputs of this process object.
   * This is useful for tracing back in the pipeline to contruct
   * graphs etc. 
   */
  DataObjectPointerArray GetOutputs()
    { return m_Outputs; }
  int GetNumberOfOutputs() const
    {return m_Outputs.size();}
    
  /** 
   * Specify function to be called before object executes. 
   */
  void SetStartMethod(void (*f)(void *), void *arg);

  /** 
   * Specify function to be called to show progress of filter. 
   */
  void SetProgressMethod(void (*f)(void *), void *arg);

  /** 
   * Specify function to be called after object executes. 
   */
  void SetEndMethod(void (*f)(void *), void *arg);

  /** 
   * Set the arg delete method. This is used to free user memory. 
   */
  void SetStartMethodArgDelete(void (*f)(void *));

  /** 
   * Set the arg delete method. This is used to free user memory. 
   */
  void SetProgressMethodArgDelete(void (*f)(void *));

  /** 
   * Set the arg delete method. This is used to free user memory. 
   */
  void SetEndMethodArgDelete(void (*f)(void *));

  /** 
   * Set the AbortGenerateData flag for the process object. Process objects
   *  may handle premature termination of execution in different ways. 
   */
  itkSetMacro(AbortGenerateData,bool);

  /** 
   * Get the AbortGenerateData flag for the process object. Process objects
   *  may handle premature termination of execution in different ways. 
   */
  itkGetMacro(AbortGenerateData,bool);
  
  /**
   * Turn on and off the AbortGenerateData flag.
   */
  itkBooleanMacro(AbortGenerateData); 
  
  /** 
   * Set the execution progress of a process object. The progress is
   * a floating number between (0,1), 0 meaning no progress; 1 meaning
   * the filter has completed execution.
   */
  itkSetClampMacro(Progress,float,0.0,1.0);

  /** 
   * Get the execution progress of a process object. The progress is
   * a floating number between (0,1), 0 meaning no progress; 1 meaning
   * the filter has completed execution.
   */
  itkGetMacro(Progress,float);

  /** 
   * Update the progress of the process object. If a ProgressMethod exists,
   * executes it.  Then set the Progress ivar to amount. The parameter amount
   * should range between (0,1). 
   */
  void UpdateProgress(float amount);
  
  /** 
   * Bring this filter up-to-date. Update() checks modified times against
   * last execution times, and re-executes objects if necessary. A side
   * effect of this method is that the whole pipeline may execute
   * in order to bring this filter up-to-date.
   */
  virtual void Update();

  /** 
   * Like Update(), but make sure the requested region is the largest possible
   * region the output.
   */
  virtual void UpdateLargestPossibleRegion();

  /** 
   * Update the information decribing the output data. The default 
   * implementation of this method will copy information from the input to
   * the output.  A filter may override this method if its output will have
   * different information than its input.  For instance, a filter that 
   * shrinks an image will need to provide an implementation for this 
   * method that changes the spacing of the pixels. Such filters should call
   * their superclass' implementation of this method prior to changing the
   * information values they need (i.e. UpdateOutputInformation() should
   * call Superclass::UpdateOutputInformation() prior to changing the
   * information.
   */
  virtual void UpdateOutputInformation();

  /** 
   * Send the requested region information back up the pipeline (to the
   * filters that preceed this one).
   */
  virtual void PropagateRequestedRegion(DataObject *output);

  /** 
   * Actually generate new output 
   */
  virtual void UpdateOutputData(DataObject *output);

  /** 
   * Propagate the computation of the size of the pipeline. The first
   * size is the size of the pipeline after this source has finished
   * executing (and potentially freeing some input data). The second
   * size is the size of the specified output. The third size is the
   * maximum pipeline size encountered so far during this propagation.
   * All sizes are in kilobytes. 
   */
  void ComputeEstimatedPipelineMemorySize( DataObject *output,
					   unsigned long size[3] );

  /** 
   * The estimated size of the specified output after execution of
   * this source is stored in the first size entry. The second size
   * is the sum of all estimated output memory. The size of all inputs
   * is given to help this filter in the estimation.
   * All sizes are in kilobytes.
   */
  virtual void ComputeEstimatedOutputMemorySize( DataObject *output,
						 unsigned long *inputSize,
						 unsigned long size[2] );

  /** 
   * Give the process object a chance to indictate that it will produce more
   * output than it was requested to produce. For example, many imaging
   * filters must compute the entire output at once or can only produce output
   * in complete slices. Such filters cannot handle smaller requested regions.
   * These filters must provide an implementation of this method, setting
   * the output requested region to the size they will produce.  By default,
   * a process object does not modify the size of the output requested region.
   */
  virtual void EnlargeOutputRequestedRegion(DataObject *itkNotUsed(output)){};
  
  /** 
   * What is the input requested region that is required to produce the
   * output requested region? By default, the largest possible region is
   * always required but this is overridden in many subclasses. For instance,
   * for an image processing filter where an output pixel is a simple function
   * of an input pixel, the input requested region will be set to the output
   * requested region.  For an image processing filter where an output pixel
   * is a function of the pixels in a neighborhood of an input pixel, then
   * the input requested region will need to be larger than the output
   * requested region (to avoid introducing artificial boundary conditions).
   */
  virtual void GenerateInputRequestedRegion();

  /** 
   * Turn on/off flag to control whether this object's data is released
   * after being used by a source. 
   */
  virtual void SetReleaseDataFlag(bool flag);
  virtual bool GetReleaseDataFlag();
  void ReleaseDataFlagOn() {SetReleaseDataFlag(true);}
  void ReleaseDataFlagOff() {SetReleaseDataFlag(false);}

  /** 
   * Handle the source/data loop. 
   */
  void UnRegister() {};
  
  /** 
   * Test to see if this object is in a reference counting loop. 
   */
  virtual int InRegisterLoop(Object *) const {return 0;}

protected:
  ProcessObject();
  ~ProcessObject();
  ProcessObject(const ProcessObject&) {};
  void operator=(const ProcessObject&) {};
  void PrintSelf(std::ostream& os, Indent indent);
  
  /**
   * protected methods for setting inputs.
   */
  virtual void SetNthInput(unsigned int num, DataObject *input);
  virtual void AddInput(DataObject *input);
  virtual void RemoveInput(DataObject *input);
  itkSetMacro(NumberOfRequiredInputs,unsigned int);
  itkGetMacro(NumberOfRequiredInputs,unsigned int);

  /**
   * protected methods for setting outputs.
   */
  virtual void SetNthOutput(unsigned int num, DataObject *output);
  virtual void AddOutput(DataObject *output);
  virtual void RemoveOutput(DataObject *output);
  itkSetMacro(NumberOfRequiredOutputs,unsigned int);
  itkGetMacro(NumberOfRequiredOutputs,unsigned int);

  /**
   * GenerateData the algorithm
   */
  virtual void GenerateData() {};

  /**
   * Called to allocate the input array.  Copies old inputs.
   */
  void SetNumberOfInputs(unsigned int num);

  /**
   * method used internally for getting an input.
   */
  DataObjectPointer GetInput(unsigned int idx);

  /**
   * Called to allocate the output array.  Copies old outputs.
   */
  void SetNumberOfOutputs(unsigned int num);

  /**
   * method used internally for getting an output.
   */
  DataObjectPointer GetOutput(unsigned int idx);

  /**
   * By default, UpdateOutputInformation calls this method to copy information
   * unmodified from the input to the output.
   */
  virtual void GenerateOutputInformation();

  /**
   * Callbacks to be called during pipeline execution
   */
  void (*m_StartMethod)(void *);
  void (*m_StartMethodArgDelete)(void *);
  void *m_StartMethodArg;
  void (*m_ProgressMethod)(void *);
  void *m_ProgressMethodArg;
  void (*m_ProgressMethodArgDelete)(void *);
  void (*m_EndMethod)(void *);
  void (*m_EndMethodArgDelete)(void *);
  void *m_EndMethodArg;

private:
  /**
   * An Array of the inputs to the filter
   */
  std::vector<DataObjectPointer> m_Inputs;
  unsigned int m_NumberOfRequiredInputs;

  /**
   * An Array of the outputs to the filter
   */
  std::vector<DataObjectPointer> m_Outputs;
  unsigned int m_NumberOfRequiredOutputs;

  /**
   * This flag indicates when the pipeline is executing
   */
  bool m_Updating;

  /**
   * Time when GenerateOutputInformation was last called.
   */
  TimeStamp m_InformationTime;

  /**
   * These support the progress method and aborting filter execution
   */
  bool  m_AbortGenerateData;
  float m_Progress;

};

} // end namespace itk

#endif

