%pythoncode %{
# Import the necessary modules for buffer protocol support
from itk.itkPyBufferPython import _get_formatstring
%}

// Apply buffer protocol support to all wrapped ImportImageContainer instances
%define APPLY_IMPORTIMAGECONTAINER_PYTHON_EXTENSIONS()
  DECL_PYTHON_IMPORTIMAGECONTAINER_CLASS(itk::ImportImageContainer)
%enddef
