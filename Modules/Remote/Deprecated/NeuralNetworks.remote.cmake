if(NOT ITK_LEGACY_REMOVE AND ITKV4_COMPATIBILITY AND Module_ITKDeprecated)

itk_fetch_module(NeuralNetworks
"This deprecated remote module contains classes and support classes
for the calculation of artificial neural networks.

This can be used, for instance, for image classification.

This historical set of features is likely not appropriate for modern neural
network implementations due to performance issues."

  GIT_REPOSITORY ${git_protocol}://github.com/InsightSoftwareConsortium/ITKNeuralNetworks.git
  GIT_TAG c293e56699d6d102dcde567baf1cf5b704819c17
  )

  if(NOT ITK_LEGACY_SILENT AND Module_NeuralNetworks)
    message(WARNING "NeuralNetworks remote module is deprecated.
    Development of this module has ended, and it will be removed
    in future ITK versions.")
  endif()
endif()
