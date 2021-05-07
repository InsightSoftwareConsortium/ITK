# imports as they appear in __init__.py
from itkConfig import ITK_GLOBAL_VERSION_STRING as __version__
from itk.support.extras import *
from itk.support.init_helpers import *
from itk.support.types import itkCType, F, D, UC, US, UI, UL, SL, LD, ULL, SC, SS, SI, SLL, B

# additional imports
from itk.support.template_class import itkTemplate as _itkTemplate
from typing import Union

#**************************************************************************
# BASE TYPES
#**************************************************************************
class LightObject():
    """itk::LightObject\n
        Light weight base class for most itk classes.
        Create a new LightObject:
        'itk.LightObject.New()'
    """

    @staticmethod
    def New() -> LightObject:
        """Instantiate itk::LightObject"""
        ...

    def Clone(self) -> LightObject:
        """Creates and returns a clone of this object."""
        ...

    def CreateAnother(self) -> LightObject:
        """Create an object from an instance, potentially deferring to a factory.
            This method allows you to create an instance of an object that is exactly
            the same type as the referring object. This is useful in cases where an object
            has been cast back to a base class."""
        ...

    def GetNameOfClass(self) -> str:
        """Return the name of this class as a string.
            Used by the object factory (implemented in New()) to instantiate objects of a named type.
            Also used for debugging and other output information."""
        ...

    def GetReferenceCount(self) -> int:
        """Gets the reference count on this object."""
        ...

    #! NOT SURE HOW THIS WORKS IN PYTHON...
    # tried passing sys.stdout
    #def Print(self, ) -> :
    #    """"""
    #    ...

    # Choosing not to define __ methods

    @staticmethod
    def BreakOnError():
        """This method is called when itkExceptionMacro executes. It allows the debugger to break on error."""

    @staticmethod
    def cast(Object) -> LightObject:
        """Cast an object to itk.LightObject"""
        ...

class Object(LightObject):
    """itk::Object \n
        Base class for most ITK classes.
        Create a new Object:
        'itk.Object.New()'
    """

    @staticmethod
    def New() -> Object:
        """Instantiate itk::Object"""
        ...

    def AddObserver(self, Event: EventObject, Command) -> int:
        """Add an observer/command to this object invoked for event.
        :param Command: A method object to be executed."""
        ...

    def DebugOff(self) -> None:
        """Turn debugging output off."""
        ...

    def DebugOn(self) -> None:
        """Turn debugging output on."""
        ...

    def GetCommand(self, Tag: int) -> Command:
        """Get the command associated with the given tag."""
        ...

    def GetDebug(self) -> bool:
        """Get the value of the debug flag."""
        ...

    def GetMTime(self) -> int:
        """Return this object's modified time."""
        ...

    # I can only get this method to produce segfaults
    # not sure how it works or what it returns so this method is incomplete
    #! The import still does not give typehints
    def GetMetaDataDictionary(self) -> MetaDataDictionary:
        """Returns a constant reference to this objects MetaDataDictionary."""
        ...

    def  GetObjectName(self) -> str:
        """Get this object's name and other information associated with it."""
        #! The Doxygen documentation says "Enable/Disable debug messages." which seems wrong.
        ...

    def GetTimeStamp(self) -> TimeStamp:
        """Return this object's time stamp."""
        ...

    def HasObserver(self) -> bool:
        """Return true if an observer is registered for this event."""
        ...

    def InvokeEvent(self, Event: EventObject) -> None:
        """Call Execute on all the Commands observing this event id."""
        ...

    def RemoveAllObservers(self) -> None:
        """Remove all observers ."""
        ...

    def RemoveObserver(self, Tag: int) -> None:
        """Remove the observer with this tag value."""
        ...

    def SetDebug(self, DebugFlag: bool) -> None:
        """Set the value of the debug flag."""
        ...

    def SetMetaDataDictionary(self, MetaDataDictionary: MetaDataDictionary) -> None:
        """Set the MetaDataDictionary."""
        ...

    def SetObjectName(self, Name: str) -> None:
        """A facility to help application programmers set a human identifiable name for a given object.
        This has no inherent use in ITK, but is a convenience to allow developers to provide a name for this object."""
        ...

    #! TODO for Object, just static methods
    # and obviously fix the problems above
    @staticmethod
    def GetGlobalWarningDisplay() -> bool:
        """Get the value of the global warning display flag."""
        ...

    @staticmethod
    def GlobalWarningDisplayOff() -> None:
        """Turn off the global warning display flag"""
        ...

    @staticmethod
    def GlobalWarningDisplayOn() -> None:
        """Turn on the global warning display flag"""
        ...

    @staticmethod
    def SetGlobalWarningDisplay(flag: bool) -> None:
        """Set the global warning display flag to the given value"""
        ...

    @staticmethod
    def cast(Object) -> Object:
        """Cast an object to itk.Object"""
        ...

class ProcessObject(Object):
    """itk::ProcessObject \n
        The base class for all process objects (source, filters, mappers) in the Insight data processing pipeline.
        Create a new ProcessObject:
        'itk.ProcessObject.New()'"""
    #! Doesnt seem like this can be instantiated or shouldnt be
    # "Deprecated procedural interface function. Use snake case function instead."
    # "ITK’s Pythonic functions are anything that inherits from itk.ProjectObject,
    # i.e. mesh filters, point set filters, path filters, image filters …"
    # https://discourse.itk.org/t/pythonic-interface-to-itk-filters/1106/7
    # It seems like itk is transitioning so that all ProcessObjects should be accessed through
    # pythonic snake case functions and dont need typehints here...


class DataObject(Object):
    """itk::DataObject \n
        Base class for all data objects in ITK.
        Create a new DataObject:
        'itk.DataObject.New()'"""

    @staticmethod
    def New() -> DataObject:
        """Instantiate itk::DataObject"""
        ...

    def CopyInformation(self, other: DataObject) -> None:
        """Copy information from the specified data set.
            This method is part of the pipeline execution model.
            By default, a ProcessObject will copy meta-data from the first input to all of its outputs.
            See ProcessObject::GenerateOutputInformation(). Each subclass of DataObject is responsible
            for being able to copy whatever meta-data it needs from from another DataObject. The default
            implementation of this method is empty. If a subclass overrides this method, it should always
            call its superclass' version."""
        ...

    def DataHasBeenGenerated(self) -> None:
        """Inform the pipeline mechanism that data has been generated.
            This method is called by ProcessObject::UpdateOutputData() once the process object has finished
            generating its data. This essentially marks the DataObject as being updated and ready for use."""
        ...

    def DisconnectPipeline(self) -> None:
        """Separate this data object from the pipeline. This routine disconnects a data object from the upstream
            pipeline. Hence an Update() from downstream will not propagate back past this data object.
            To completely isolate this data object from the pipeline, the application must remove this data object
            from any filters which it is connected as the input."""
        ...

    def GetDataReleased(self) -> bool:
        """Get the flag indicating the data has been released."""
        ...

    def GetPipelineMTime(self) -> int:
        """Doxygen Documentation Incorrect.
            https://itk.org/Doxygen/html/classitk_1_1DataObject.html#a2ec89950dec13e634a0388cc761d108a"""
        ...

    def GetRealTimeStamp(self) -> RealTimeStamp:
        """Doxygen Documentation Incorrect.
            https://itk.org/Doxygen/html/classitk_1_1DataObject.html#afeae0d072f4e23c5c99fea82ff3a63d0"""
        ...

    def GetReleaseDataFlag(self) -> bool:
        """Doxygen Documentation Missing.
            https://itk.org/Doxygen/html/classitk_1_1DataObject.html#ac3225a0394cd5bbe0a873f6d2ff1c3b2"""
        ...

    def GetSource(self) -> ProcessObject:
        """Get the process object that generated this data object. If there is no process object,
            then the data object has been disconnected from the pipeline, or the data object was created manually.
            (Note: we cannot use the GetObjectMacro() defined in itkMacro because the mutual dependency of
            DataObject and ProcessObject causes compile problems. )"""
        ...

    def GetSourceOutputIndex(self) -> int:
        """Doxygen Documentation Missing.
            https://itk.org/Doxygen/html/classitk_1_1DataObject.html#a342b58ca4a79d1d713dae4c51aec8da9"""
        ...

    def GetSourceOutputName(self) -> str:
        """Doxygen Documentation Incorrect.
            https://itk.org/Doxygen/html/classitk_1_1DataObject.html#aa57e20e646c63cd427dc44115ac82806"""
        ...

    def GetUpdateMTime(self) -> int:
        """MTime for the last time this DataObject was generated."""
        ...

    def Graft(self, other: DataObject) -> None:
        """Method for grafting the content of one data object into another one.
            This method is intended to be overloaded by derived classes.
            Each one of them should use dynamic_casting in order to verify that the grafted object
            is actually of the same type as the class on which the Graft() method was invoked."""
        ...

    def Initialize(self) -> None:
        """Restore the data object to its initial state. This means releasing memory."""
        ...

    def PrepareForNewData(self) -> None:
        """Setup a DataObject to receive new data. This method is called by the pipeline mechanism
            on each output of filter that needs to execute. The default implementation is to return a
            DataObject to its initial state. This may involve releasing previously allocated bulk data.
            Subclasses of DataObject may want to override this method and/or the Initialize() method if
            they want a different default behavior (for instance a DataObject might want finer control
            over its bulk data memory management)."""
        ...

    def PropagateRequestedRegion(self) -> None:
        """Methods to update the pipeline. Called internally by the pipeline mechanism."""
        ...

    def ReleaseData(self) -> None:
        """Release data back to system to conserve memory resource.
            Used during pipeline execution. Releasing this data does not make down-stream data invalid,
            so it does not modify the MTime of this data object."""
        ...

    def ReleaseDataFlagOff(self) -> None:
        """Doxygen Documentation Missing.
            https://itk.org/Doxygen/html/classitk_1_1DataObject.html#a9a7f594b2d09852e14a4f96e0146345a"""
        ...

    def ReleaseDataFlagOn(self) -> None:
        """Doxygen Documentation Missing.
            https://itk.org/Doxygen/html/classitk_1_1DataObject.html#ae9bcbebb80a27e7c544dd82f854447a0"""
        ...

    def RequestedRegionIsOutsideOfTheBufferedRegion(self) -> bool:
        """Determine whether the RequestedRegion is outside of the BufferedRegion. This method returns
            true if the RequestedRegion is outside the BufferedRegion (true if at least one pixel is outside).
            This is used by the pipeline mechanism to determine whether a filter needs to re-execute in order
            to satisfy the current request. If the current RequestedRegion is already inside the BufferedRegion
            from the previous execution (and the current filter is up to date), then a given filter does not need
            to re-execute"""
        ...

    def ResetPipeline(self) -> None:
        """Reset the pipeline. If an exception is thrown during an Update(),
            the pipeline may be in an inconsistent state. This method clears the internal
            state of the pipeline so Update() can be called."""
        ...

    def SetPipelineMTime(self, time: int) -> None:
        """The maximum MTime of all upstream filters and data objects. This does not include the MTime of
            this data object."""
        ...

    def SetRealTimeStamp(self, TimeStamp: RealTimeStamp) -> None:
        """RealTime stamp for the last time this DataObject was generated. By default, the real time stamp
            is initialized to the origin of the Unix epoch. That is the time 00:00:00 UTC on 1 January 1970
            (or 1970-01-01T00:00:00Z ISO 8601)"""
        ...

    def SetReleaseDataFlag(self, flag: bool) -> None:
        """Turn on/off a flag to control whether this object's data is released after being used by a filter."""
        ...

    def SetRequestedRegion(self, other: DataObject) -> None:
        """Set the requested region from this data object to match the requested region of the data object
            passed in as a parameter. For DataObject's that do not support Regions, this method does nothing.
            Subclasses of DataObject that do support Regions, provide an alternative implementation."""
        ...

    def SetRequestedRegionToLargestPossibleRegion(self) -> None:
        """Set the RequestedRegion to the LargestPossibleRegion. This forces a filter to produce all of the
            output in one execution (i.e. not streaming) on the next call to Update()."""
        ...

    def ShouldIReleaseData(self) -> bool:
        """Return flag indicating whether data should be released after use by a filter."""
        ...

    def Update(self) -> None:
        """Provides opportunity for the data object to insure internal consistency before access.
            Also causes owning source/filter (if any) to update itself. The Update() method is composed
            of UpdateOutputInformation(), PropagateRequestedRegion(), and UpdateOutputData(). This method
            may call methods that throw an InvalidRequestedRegionError exception. This exception will
            leave the pipeline in an inconsistent state. You will need to call ResetPipeline() on the last
            ProcessObject in your pipeline in order to restore the pipeline to a state where you can call
            Update() again."""
        ...

    def UpdateOutputData(self) -> None:
        """Doxygen Documentation Missing.
                    https://itk.org/Doxygen/html/classitk_1_1DataObject.html#a93cbfee9b5c11fcd5d456ebf7a1fb755"""
        ...

    def UpdateOutputInformation(self) -> None:
        """Update the information for this DataObject so that it can be used as an output of a ProcessObject.
            This method is used in the pipeline mechanism to propagate information and initialize the meta
            data associated with a DataObject. Any implementation of this method in a derived class is
            assumed to call its source's ProcessObject::UpdateOutputInformation() which determines modified
            times, LargestPossibleRegions, and any extra meta data like spacing, origin, etc. Default
            implementation simply call's it's source's UpdateOutputInformation()."""
        ...

    def VerifyRequestedRegion(self) -> bool:
        """Verify that the RequestedRegion is within the LargestPossibleRegion."""
        ...

    # STATIC METHODS

    @staticmethod
    def GetGlobalReleaseDataFlag() -> bool:
        """Doxygen Documentation Missing.
            https://itk.org/Doxygen/html/classitk_1_1DataObject.html#ada5f72ff271140b4a24d5d54ac9eead6"""
        ...

    @staticmethod
    def GlobalReleaseDataFlagOff() -> None:
        """Doxygen Documentation Missing.
            https://itk.org/Doxygen/html/classitk_1_1DataObject.html#aa98c510f2dc5220ac0d2e9c913b4802d"""
        ...

    @staticmethod
    def GlobalReleaseDataFlagOn() -> None:
        """Doxygen Documentation Missing.
            https://itk.org/Doxygen/html/classitk_1_1DataObject.html#a4aa155cf9f1416d8a784a2c057d75818"""
        ...

    @staticmethod
    def SetGlobalReleaseDataFlag(flag: bool) -> None:
        """Turn on/off a flag to control whether every object releases its data after being used by a filter.
            Being a global flag, it controls the behavior of all DataObjects and ProcessObjects."""
        ...

    @staticmethod
    def cast(Object) -> DataObject:
        """Cast an object to itk.DataObject"""
        ...
