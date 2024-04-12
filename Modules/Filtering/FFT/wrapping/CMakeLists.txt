itk_wrap_module("ITKFFT")
set(WRAPPER_SUBMODULE_ORDER
    itkInverse1DFFTImageFilter
    itkForward1DFFTImageFilter
    itkInverseFFTImageFilter
    itkForwardFFTImageFilter
    )
itk_auto_load_and_end_wrap_submodules()
