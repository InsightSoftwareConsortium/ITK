# imports as they appear in __init__.py
from itkConfig import ITK_GLOBAL_VERSION_STRING as __version__
from itk.support.extras import *
from itk.support.init_helpers import *
from itk.support.types import itkCType, F, D, UC, US, UI, UL, SL, LD, ULL, SC, SS, SI, SLL, B

# additional imports
from itk.support.template_class import itkTemplate as _itkTemplate
from typing import Union

#! Sample is an abstract class may want to hide it with _
class Sample(DataObject):
    """itk::Statistics::Sample \n
        Abstract class, do not instantiate.
        A collection of measurements for statistical analysis."""

    def GetFrequency(self, id: int) -> int:
        """Get the frequency of a measurement specified by instance identifier."""
        ...

    def GetMeasurementVector(self, id: int) -> Vector: #! Change to TypedVector once it is created
        """Get the measurement associated with a particular Instance Identifier."""
        ...

    def GetMeasurementVectorSize(self) -> int:
        """Get method for the length of the measurement vector"""
        ...

    def GetTotalFrequency(self) -> int:
        """Get the total frequency of the sample."""
        ...

    def SetMeasurementVectorSize(self, s: int) -> None:
        """Set method for the length of the measurement vector"""
        ...

    def Size(self) -> int:
        """Get the size of the sample (number of measurements)"""
        ...


# Used to provide proper typehints when using the dictionary type specification
class _ListSampleMeta():
    def __getitem__(self, parameters) -> ListSample:
        """Specifymeasurement vector type with:
           \t[MeasurementVectorType]
           :return filter: ListSample
           """
        ...


# Lies to the ide and says that itk.ListSample is a class
# when it is actually a dynamically created _itkTemplate object with dictionary access.
# The typed methods allow the ide to specify return types when
# either method is used.
# a 'TypedListSamplePROXY' object is returned.
class ListSample(_itkTemplate, metaclass=_ListSampleMeta):  #PROXY
    """Interface for instantiating itk::Statistics::ListSample< TMeasurementVector > \n
        Create a new DataObject (default Measurement Vector VF2):
            'itk.ListSample.New()'
        Supports type specification through dictionary access:
            'itk.ListSample[itkVectorType].New()'"""

    @staticmethod
    def New() -> TypedListSamplePROXY:  #! There may be keyword parms, unsure how to tell.
        """Instantiate itk::Statistics::ListSample< TMeasurementVector >"""
        ...


# This is a fake class that generically
# represents all forms of the ListSample object (itkListSampleVF2, itkListSampleVF3)
# methods and return types can then be specified generically in the ide.
# The user should not create this object
class TypedListSamplePROXY(Sample):
    """itk::Statistics::ListSample< TMeasurementVector > \n
        This class is the native implementation of the a Sample with an STL container.
        ListSample stores measurements in a list type structure (as opposed to a Histogram, etc.).
        ListSample allows duplicate measurements. ListSample is not sorted.
        ListSample does not allow the user to specify the frequency of a measurement directly.
        The GetFrequency() methods returns 1 if the measurement exists in the list, 0 otherwise.
            Create a new DataObject:
            'itk.ListSample.New()'"""

    @staticmethod
    def New() -> TypedListSamplePROXY:  #! There may be keyword parms, unsure how to tell.
        """Instantiate itk::Statistics::ListSample< TMeasurementVector >"""
        ...

    # Finish ListSample Implementation
    def Clear(self) -> None:
        """Removes all the elements in the Sample."""
        ...

    def Clone(self) -> TypedListSamplePROXY:
        """Creates and returns a clone of this object."""
        ...

    #! mv type should be TypedVectorPROXY once it exists as the actual type is unknown
    def PushBack(self, mv: Vector) -> None:
        """Inserts a measurement at the end of the list."""
        ...

    def Resize(self, newsize: int) -> None:
        """Resize the container. Using Resize() and then SetMeasurementVector() is about nine times faster
            than usign PushBack() continuously. Which means that whenever the total number of Measurement
            vectors is known, the users should prefer calling Resize() first and then set the values by
            calling SetMeasurementVector(). On the other hand, if the number of measurement vectors is not
            known from the beginning, then calling PushBack() sequentially is a convenient option."""
        ...

    def SetMeasurement(self, instanceId: int, dim: int, value: float) -> None:
        """Set a component a measurement to a particular value."""
        ...

    #! mv type should be TypedVectorPROXY once it exists as the actual type is unknown
    def SetMeasurementVector(self, instanceId: int, mv: Vector) -> None:
        """Replace a measurement with a different measurement."""
        ...

    @staticmethod
    def cast(Object) -> TypedListSamplePROXY:
        """Cast an object to itk::Statistics::ListSample< TMeasurementVector >"""
        ...


# Auto parser hints for the itk.Vector class
class _Vector():
    def __getitem__(self, parameters) -> Vector:
        """Specify class type with:
                [T, NVectorDimension]
            :return: Vector
            """
        ...


class Vector(_itkTemplate, metaclass=_Vector):
    """Interface for instantiating itk::Vector< T, NVectorDimension >
        Supports type specification through dictionary access:
            'itk.Vector[T, NVectorDimension]()"""

    @staticmethod
    def __init__() -> TypedVectorPROXY:
        """Instantiate itk::Vector< T, NVectorDimension >"""
        ...


class TypedVectorPROXY(FixedArray):
    def GetNorm(self) -> float:
        """Returns the Euclidean Norm of the vector"""
        ...

    def GetSquaredNorm(self) -> float:
        """Returns vector's Squared Euclidean Norm"""
        ...

    def GetVnlVector(self) -> Union[vnl_vector_refUC, vnl_vector_refF, vnl_vector_refD, vnl_vector_refSS]:
        """Get a vnl_vector_ref referencing the same memory block."""
        ...

    def GetVnlVector(self) -> Union[vnl_vectorUC, vnl_vectorF, vnl_vectorD, vnl_vectorSS]:
        """Get a vnl_vector_ref referencing the same memory block."""
        ...

    def Normalize(self) -> float:
        """Divides the vector components by the vector norm (when the norm is not null). The norm used is
        returned."""
        ...

    def SetNthComponent(self, c: int, v: Union[unsigned char const &, float, int]) -> None:
        """Returns the number of components in this vector type"""
        ...

    def SetVnlVector(self, arg0: Union[vnl_vectorUC const &, vnl_vectorF const &, vnl_vectorD const &, vnl_vectorSS const &]) -> None:
        """Set a vnl_vector_ref referencing the same memory block."""
        ...

    @staticmethod
    def GetNumberOfComponents() -> int:
        """Returns the number of components in this vector type"""
        ...

    @staticmethod
    def GetVectorDimension() -> int:
        """Get the dimension (size) of the vector."""
        ...
