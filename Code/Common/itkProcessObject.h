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
 * itkDataSet is the base class for all process objects (source, filters, 
 * mappers) in the Insight data processing pipeline.
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
 * Another method, ProgressMethod() can be specified. Some filters invoke this 
 * method periodically during their execution. The use is similar to that of 
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
   * Instantiate object with no start, end, or progress methods. 
   */
  static itkProcessObject::Pointer New();

  /** 
   * Return an array with all the inputs of this process object.
   * This is useful for tracing back in the pipeline to construct
   * graphs etc. 
   */
  itkDataObject **GetInputs() {return m_Inputs;};
  int GetNumberOfInputs() {return m_NumberOfInputs;}

  /** 
   * Return an array with all the inputs of this process object.
   * This is useful for tracing back in the pipeline to contruct
   * graphs etc. 
   */
  itkDataObject **GetOutputs();
  int GetNumberOfOutputs() {return m_NumberOfOutputs;}
    
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
  void SetAbortExecute(bool flag) {itkSetMacro(m_AbortExecute,flag);}
  bool GetAbortExecute(bool flag) {itkGteMacro;}
  void AbortExecuteOn() {this->SetAbortExecute(true);}
  void AbortExecuteOff() {this->SetAbortExecute(false);}
  
  /** 
   * Set/Get the execution progress of a process object. The progress is
   * a floating number between (0,1), 0 meaning no progress; 1 meaning
   * the filter has completed execution.
   */
  void SetProgress(float progress) 
    {itkSetClampMacro(m_Progress,progress,0.0,1.0);}
  float GetProgress() {itkGetMacro(m_Progress);}

  /** 
   * Update the progress of the process object. If a ProgressMethod exists,
   * executes it.  Then set the Progress ivar to amount. The parameter amount
   * should range between (0,1). 
   */
  void UpdateProgress(float amount);
  
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
  virtual const int InRegisterLoop(itkObject *) {return 0;}

protected:
  itkProcessObject();
  ~itkProcessObject();
  itkProcessObject(const itkProcessObject&) {};
  void operator=(const itkProcessObject&) {};
  void PrintSelf(std::ostream& os, itkIndent indent);
  
  // protected methods for setting inputs.
  virtual void SetNthInput(int num, itkDataObject *input);
  virtual void AddInput(itkDataObject *input);
  virtual void RemoveInput(itkDataObject *input);
  // Called to allocate the input array.  Copies old inputs.
  void SetNumberOfInputs(int num);

  // protected methods for setting outputs.
  virtual void SetNthOutput(int num, itkDataObject *output);
  virtual void AddOutput(itkDataObject *output);
  virtual void RemoveOutput(itkDataObject *output);

  // Execute the algorithm
  virtual void Execute() {};

  // Called to allocate the input array.  Copies old inputs.
  void SetNumberOfOutputs(int num);

  // method used internally for getting an output.
  itkDataObject *GetOutput(int idx);

private:
  itkDataObject **m_Inputs;     // An Array of the inputs to the filter
  int m_NumberOfInputs;
  int m_NumberOfRequiredInputs;

  itkDataObject **m_Outputs;   // An Array of the outputs to the filter
  int m_NumberOfOutputs;
  int m_Updating;

  // Time when ExecuteInformation was last called.
  itkTimeStamp m_InformationTime;

  void (*m_StartMethod)(void *);
  void (*m_StartMethodArgDelete)(void *);
  void *m_StartMethodArg;
  void (*m_ProgressMethod)(void *);
  void *m_ProgressMethodArg;
  void (*m_ProgressMethodArgDelete)(void *);
  void (*m_EndMethod)(void *);
  void (*m_EndMethodArgDelete)(void *);
  void *m_EndMethodArg;

  bool m_AbortExecute;
  float m_Progress;

};

#endif

