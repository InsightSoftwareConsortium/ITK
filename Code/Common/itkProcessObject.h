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
/**
 * itkProcessObject is the base class for all process objects (source,
 * filters, mappers) in the Insight data processing pipeline.
 * itkProcessObject is an abstract object that specifies behavior and
 * interface of visualization network process objects (sources, filters,
 * mappers). Source objects are creators of visualization data; filters
 * input, process, and output visualization data; and mappers transform data
 * into another form (like rendering primitives or write data to a file).
 *
 * A major role of itkProcessObject is to define the inputs and outputs
 * of a filter. More than one input and/or output may exist for a given
 * filter. Some classes (e.g., source objects or mapper objects) will
 * not use inputs (the source) or outputs (mappers). In this case, the
 * inputs or outputs is just ignored.
 *
 * itkProcessObject provides a mechanism for invoking the methods
 * StartMethod() and EndMethod() before and after object execution (via
 * Execute()). These are convenience methods you can use for any purpose
 * (e.g., debugging info, highlighting/notifying user interface, etc.) These
 * methods accept a single void* pointer that can be used to send data to the
 * methods. It is also possible to specify a function to delete the argument
 * via StartMethodArgDelete and EndMethodArgDelete.
 *
 * Another method, ProgressMethod() can be specified. Some filters invoke
 * this method periodically during their execution (with the progress,
 * parameter, the fraction of work done). The use is similar to that of
 * StartMethod() and EndMethod(). Filters may also check their AbortExecute
 * flag to determine whether to prematurally end their execution.
 *
 * An important feature of subclasses of itkProcessObject is that it is
 * possible to control the memory-management model (i.e., retain output
 * versus delete output data). If enabled the ReleaseDataFlag enables the
 * deletion of the output data once the downstream process object finishes
 * processing the data (please see text).  
 */

#ifndef __itkProcessObject_h
#define __itkProcessObject_h

#include "itkObject.h"

class itkDataObject;

class ITK_EXPORT itkProcessObject : public itkObject
{
public:
  /** 
   * Smart pointer typedef support. 
   */
  typedef itkSmartPointer<itkProcessObject> Pointer;

  /** 
   * Run-time type information (and related methods).
   */
  itkTypeMacro(itkProcessObject,itkObject);

  /** 
   * Instantiate object with no start, end, or progress methods. 
   */
  static Pointer New();

  /** 
   * Return an array with all the inputs of this process object.
   * This is useful for tracing back in the pipeline to construct
   * graphs etc. 
   */
  itkDataObject **GetInputs() 
    {return m_Inputs;};
  int GetNumberOfInputs() const
    {return m_NumberOfInputs;}

  /** 
   * Return an array with all the inputs of this process object.
   * This is useful for tracing back in the pipeline to contruct
   * graphs etc. 
   */
  itkDataObject **GetOutputs();
  int GetNumberOfOutputs() const
    {return m_NumberOfOutputs;}
    
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
   * Set/Get the AbortExecute flag for the process object. Process objects
   *  may handle premature termination of execution in different ways. 
   */
  itkSetMacro(AbortExecute,bool);
  itkGetMacro(AbortExecute,bool);
  itkBooleanMacro(AbortExecute); 
  
  /** 
   * Set/Get the execution progress of a process object. The progress is
   * a floating number between (0,1), 0 meaning no progress; 1 meaning
   * the filter has completed execution.
   */
  itkSetClampMacro(Progress,float,0.0,1.0);
  itkGetMacro(Progress,float);

  /** 
   * Update the progress of the process object. If a ProgressMethod exists,
   * executes it.  Then set the Progress ivar to amount. The parameter amount
   * should range between (0,1). 
   */
  void UpdateProgress(float amount);
  
  /** 
   * Bring this filter up-to-date before execution. Update() checks modified
   * time against last execution time, and re-executes object if necessary. 
   * A side effect of this method is that the whole pipeline may execute
   * in order to bring this filter up-to-date.
   */
  virtual void Update();

  /** 
   * Like update, but make sure the update extent is the whole extent in
   * the output.
   */
  virtual void UpdateWholeExtent();

  /** 
   * Updates any global information about the data 
   * (like spacing for images). */
  virtual void UpdateInformation();

  /** 
   * Send the update extent down the pipeline 
   */
  virtual void PropagateUpdateExtent(itkDataObject *output);

  /** 
   * Start any asynchronous processing, if any. 
   */
  virtual void TriggerAsynchronousUpdate();

  /** 
   * Actually generate new output 
   */
  virtual void UpdateData(itkDataObject *output);

  /** 
   * Propagate the computation of the size of the pipeline. The first
   * size is the size of the pipeline after this source has finished
   * executing (and potentially freeing some input data). The second
   * size is the size of the specified output. The third size is the
   * maximum pipeline size encountered so far during this propagation.
   * All sizes are in kilobytes. 
   */
  void ComputeEstimatedPipelineMemorySize( itkDataObject *output,
					   unsigned long size[3] );

  /** 
   * The estimated size of the specified output after execution of
   * this source is stored in the first size entry. The second size
   * is the sum of all estimated output memory. The size of all inputs
   * is given to help this filter in the estimation.
   * All sizes are in kilobytes.
   */
  virtual void ComputeEstimatedOutputMemorySize( itkDataObject *output,
						 unsigned long *inputSize,
						 unsigned long size[2] );

  /** 
   * Give the source a chance to say that it will produce more output
   * than it was asked to produce. For example, FFT always produces the
   * whole thing, and many imaging filters must produce the output in
   * whole slices (whole extent in two dimensions). By default we do not
   * modify the output update extent. 
   */
  virtual void EnlargeOutputUpdateExtents(itkDataObject *itkNotUsed(output)){};
  
  /** 
   * What is the input update extent that is required to produce the
   * desired output? By default, the whole input is always required but
   * this is overridden in many subclasses. 
   */
  virtual void ComputeInputUpdateExtents( itkDataObject *output );

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
  virtual int InRegisterLoop(itkObject *) const {return 0;}

protected:
  itkProcessObject();
  ~itkProcessObject();
  itkProcessObject(const itkProcessObject&) {};
  void operator=(const itkProcessObject&) {};
  void PrintSelf(std::ostream& os, itkIndent indent);
  
  // protected methods for setting inputs.
  virtual void SetNthInput(unsigned int num, itkDataObject *input);
  virtual void AddInput(itkDataObject *input);
  virtual void RemoveInput(itkDataObject *input);
  itkSetMacro(NumberOfRequiredInputs,int);
  itkGetMacro(NumberOfRequiredInputs,int);

  // protected methods for setting outputs.
  virtual void SetNthOutput(unsigned int num, itkDataObject *output);
  virtual void AddOutput(itkDataObject *output);
  virtual void RemoveOutput(itkDataObject *output);
  itkSetMacro(NumberOfRequiredOutputs,unsigned int);
  itkGetMacro(NumberOfRequiredOutputs,unsigned int);

  // Execute the algorithm
  virtual void Execute() {};

  // Called to allocate the input array.  Copies old inputs.
  void SetNumberOfInputs(int num);

  // method used internally for getting an input.
  itkDataObject *GetInput(unsigned int idx);

  // Called to allocate the output array.  Copies old outputs.
  void SetNumberOfOutputs(int num);

  // method used internally for getting an output.
  itkDataObject *GetOutput(unsigned int idx);

  // By default, UpdateInformation calls this method to copy information
  // unmodified from the input to the output.
  virtual void ExecuteInformation();

    // Callbacks to be called during pipeline execution
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

  itkDataObject **m_Inputs;     // An Array of the inputs to the filter
  int m_NumberOfInputs;
  unsigned int m_NumberOfRequiredInputs;

  itkDataObject **m_Outputs;   // An Array of the outputs to the filter
  int m_NumberOfOutputs;
  unsigned int m_NumberOfRequiredOutputs;

  bool m_Updating; // This flag indicates when the pipeline is executing

  // Time when ExecuteInformation was last called.
  itkTimeStamp m_InformationTime;

  // These support the progress method and aborting filter execution
  bool  m_AbortExecute;
  float m_Progress;

};

#endif

