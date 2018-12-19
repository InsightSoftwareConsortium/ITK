Introduction
============

This directory is a place-holder for all remote modules distributed outside
ITK's main repository. Remote modules share the same directory structure as
modules in the main repository. They will be picked up by the configuration
system and entered into the build after they are downloaded into this
directory.

Modules can easily be downloaded and made accessible to the community by
listing the module in the current directory. For more information on the
policy and procedures for adding a new module, please visit the
[Contributing with a Remote Module](https://itk.org/ITKSoftwareGuide/html/Book1/ITKSoftwareGuide-Book1ch9.html#x55-1640009.7)  in the [ITK Software Guide].

For more information on adding a new module to the list of new modules,
please visit the [ITKModuleTemplate](https://github.com/InsightSoftwareConsortium/ITKModuleTemplate) repository.

Note that in each `<remote module name>.remote.cmake` file, the first argument
of the function `itk_fetch_module()` is the name of the remote module, and it
has to be consistent with the module name defined in the correponding
`<remote module name>.remote.cmake` file.

To better distinguish the remote modules from the internal ITK modules, the names
of the remote modules should **not** contain the "ITK" string prefix in them.


[ITK Software Guide]: https://itk.org/ItkSoftwareGuide.pdf
