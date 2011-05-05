set(DOCUMENTATION "Image Adaptors are classes that can take an ITK image and
present it as a different type of image. They are commonly used for trivial
pixel-wise operations that do not justify the memory duplication that a normal
ITK filter may imply. They are also very useful for implementing casting
operations on the fly.")

itk_module(ITK-ImageAdaptors DEPENDS ITK-Common TEST_DEPENDS ITK-TestKernel DESCRIPTION "${DOCUMENTATION}")
