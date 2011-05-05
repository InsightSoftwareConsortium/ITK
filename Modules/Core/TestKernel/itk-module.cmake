set(DOCUMENTATION "This Module contains helper classes used for the Testing
infrastructure of ITK. As an application developer you should not normally need
any of these classes. However, they are essential for the quality control
system supporting ITK.")

itk_module(ITK-TestKernel DEPENDS ITK-IO-PNG ITK-IO-Meta ITK-IO-TIFF ITK-IO-BMP ITK-IO-VTK ITK-IO-JPEG ITK-IO-NRRD ITK-IO-GDCM ITK-IO-GIPL ITK-IO-NIFTI ITK-KWSys DESCRIPTION "${DOCUMENTATION}")
