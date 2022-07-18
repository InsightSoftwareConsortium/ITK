"""
pyi_generator.py \
  Wrapping/Generators/Python/itk-pkl/itkFixedArray.index.txt \
  Wrapping/Generators/Python/itk-pkl/itkVector.index.txt
"""

import os
from os import remove
from argparse import ArgumentParser
from io import StringIO
from pathlib import Path, PurePath
import pickle
from typing import Union, List
import glob
import re
from collections import defaultdict


# The ITKClass is duplicated in igenerator.py
class ITKClass:
    def __init__(self, l_class_name):
        # Structure
        # { method_name: [
        #                  # Each entry of this list contains a description of the definition for the single method
        #                  # Each description is similar but can contain different types or defaults
        #                  [
        #                    # Describe each argument of the method
        #                    (
        #                      argument_name,
        #                      python_arg_type,
        #                      default_value # Use None if no default
        #                    ),
        #                    ...,
        #                    # The final entry is unique, this contains the information about the return value
        #                    (
        #                      None, # This indicates that it is a return type and not an argument
        #                      return_type, # acquired through the `get_arg_type` method
        #                      is_static # True if this is a static method
        #                    )
        #                  ],
        #
        #                  # Describe other overloads of the same method (method_name) here:
        #                  [ ... ],
        #                  ...
        #                ],
        #
        #   another_method_name: ...
        #   ...
        # }
        self.python_method_headers = {}
        self.has_new_method = False
        self.typed = False
        self.parent_class = []
        self.is_abstract = False
        self.class_name = l_class_name
        self.is_enum = False
        self.has_superclass = False
        self.enums = []
        self.submodule_name = ""

    def __eq__(self, other):
        if isinstance(other, ITKClass):
            return (
                self.python_method_headers == other.python_method_headers
                and self.has_new_method == other.has_new_method
                and self.typed == other.typed
                and self.parent_class == other.parent_class
                and self.is_abstract == other.is_abstract
                and self.class_name == other.class_name
                and self.is_enum == other.is_enum
                and self.enums == other.enums
                and self.submodule_name == other.submodule_name
            )

        return False


def generate_class_pyi_def(
    output_pyi_header_file, output_pyi_method_file, itk_class: ITKClass
):
    l_class_name = itk_class.class_name

    if itk_class.is_enum:
        output_pyi_header_file.write(
            f"itk.{l_class_name} =  _{l_class_name}Proxy\n\n\n"
        )
    elif not itk_class.is_abstract:
        output_pyi_header_file.write(
            generate_class_pyi_header(
                l_class_name, itk_class.has_new_method, itk_class.typed
            )
        )

    output_pyi_method_file.write(
        f"class {l_class_name}Proxy({', '.join(itk_class.parent_class)}):\n"  # if
    )

    if itk_class.is_enum and len(itk_class.enums) > 0:
        for enum in itk_class.enums:
            output_pyi_method_file.write(f"\t{enum} = ...\n")
        output_pyi_method_file.write(f"\n\n")
    else:
        if len(itk_class.python_method_headers.keys()) == 0:
            output_pyi_method_file.write(f"\t...\n\n")

        for method in itk_class.python_method_headers.keys():
            is_overloaded = len(itk_class.python_method_headers[method]) > 1
            for method_variation in itk_class.python_method_headers[method]:
                method_name = method
                attributes = method_variation
                params = ""
                return_type = attributes[len(attributes) - 1][1]

                is_a_static_method: bool = attributes[len(attributes) - 1][2]

                # remove special case containing the return type from the end of the attributes list
                attributes = attributes[:-1]
                if len(attributes) > 0:
                    params += ", "

                for attribute in attributes:
                    # name of the argument
                    params += attribute[0]

                    # type of the argument
                    if attribute[1] is None:
                        pass
                    else:
                        params += f": {attribute[1]}"

                    # default value of the argument (if any)
                    if attribute[2] is not None:
                        params += f" = {attribute[2]}, "
                    else:
                        params += ", "

                params = params[:-2]  # remove extra comma

                self_str = "self"
                if is_a_static_method:
                    output_pyi_method_file.write("\t@staticmethod\n")
                    self_str = ""
                    params = params[2:]  # remove comma from beginning

                if is_overloaded:
                    output_pyi_method_file.write("\t@overload\n")

                output_pyi_method_file.write(f"\tdef {method_name}({self_str}{params})")
                if return_type is not None:
                    output_pyi_method_file.write(f" -> {return_type}")
                output_pyi_method_file.write(f":\n" f'\t\t""""""\n' f"\t\t...\n\n")


not_callable = [
    "ImageToImageMetricv4",
    "NarrowBandLevelSetImageFilter",
    "ShapePriorSegmentationLevelSetImageFilter",
    "ImageToImageFilter",
    "ImageSource",
    "NarrowBandImageFilterBase",
    "NoiseBaseImageFilter",
    "ImageToMeshFilter",
    "ImageToPathFilter",
    "GenerateImageSource",
    "DenseFiniteDifferenceImageFilter",
    "InPlaceImageFilter",
    "AnisotropicDiffusionImageFilter",  # you must use snake_case method to properly use this class
    "ConvolutionImageFilterBase",
    "SparseFieldFourthOrderLevelSetImageFilter",
    "SegmentationLevelSetImageFilter",
    "TemporalProcessObject",
    "ProcessObject",
    "SegmentationLevelSetFunction",
    "LBFGSOptimizerBasev4",
]


def generate_class_pyi_header(l_class_name: str, has_new_method: bool, typed: bool):
    """Return a string containing the definition of a pyi class header.
    Supports both typed and non-typed classes."""
    if not typed:
        if has_new_method:
            class_header = (
                f"class _{l_class_name}Template(_itkTemplate):\n"
                f'    """Interface for instantiating itk::{l_class_name}'
                f"\n        Create a new {l_class_name} Object:\n"
                f"            'itk.{l_class_name}.New(**kwargs)"
                f'"""\n\n'
                f"    @staticmethod\n"
                f"    def New(**kwargs) -> _{l_class_name}Proxy:\n"
                f'        """Instantiate itk::{l_class_name}"""\n'
                f"        ...\n"
                f"\n"
                f"{l_class_name} = _{l_class_name}Template\n"
                f"\n"
                f"\n"
            )
        elif l_class_name not in not_callable:
            class_header = (
                f"class _{l_class_name}Template(_itkTemplate):\n"
                f'    """Interface for instantiating itk::{l_class_name}'
                f'"""\n\n'
                f"    def __new__(cls, *args: Any) -> _{l_class_name}Proxy:\n"
                f'        """Instantiate itk::{l_class_name}"""\n'
                f"        ...\n\n"
                f"    def __call__(self, *args: Any) -> _{l_class_name}Proxy:\n"
                f'        """Instantiate itk::{l_class_name}"""\n'
                f"        ...\n"
                f"\n"
                f"{l_class_name} = _{l_class_name}Template"
                f"\n"
                f"\n"
            )
        else:
            class_header = (
                f"class _{l_class_name}Template(_itkTemplate):\n"
                f'    """Interface for instantiating itk::{l_class_name}"""\n'
                f"    ...\n"
                f"\n"
                f"{l_class_name} = _{l_class_name}Template"
                f"\n"
                f"\n"
            )
        return class_header

    types = "INSERT_TYPE_NAMES_HERE"
    if has_new_method:
        class_header = (
            f"class _{l_class_name}TemplateGetter():\n"
            f"    def __getitem__(self, parameters) -> _{l_class_name}Template:\n"
            f'        """Specify class type with:\n'
            f"            \t[{types}]\n"
            f"            :return: {l_class_name}Template\n"
            f'            """\n'
            f"        ...\n"
            f"\n"
            f"\n"
            f"class _{l_class_name}Template(_itkTemplate, metaclass=_{l_class_name}TemplateGetter):\n"
            f'    """Interface for instantiating itk::{l_class_name}< {types} >\n'
            f"        Create a new {l_class_name} Object (of default type):\n"
            f"            'itk.{l_class_name}.New(**kwargs)\n"
            f"        Supports type specification through dictionary access:\n"
            f'            \'itk.{l_class_name}[{types}].New(**kwargs)"""\n'
            f"\n"
            f"    @staticmethod\n"
            f"    def New(**kwargs) -> _{l_class_name}Proxy:\n"
            f'        """Instantiate itk::{l_class_name}< {types} >"""\n'
            f"        ...\n"
            f"\n"
            f"{l_class_name} = _{l_class_name}Template"
            f"\n"
            f"\n"
        )
    elif l_class_name not in not_callable:
        class_header = (
            f"class _{l_class_name}TemplateGetter():\n"
            f"    def __getitem__(self, parameters) -> _{l_class_name}Template:\n"
            f'        """Specify class type with:\n'
            f"            \t[{types}]\n"
            f"            :return: {l_class_name}Template\n"
            f'            """\n'
            f"        ...\n"
            f"\n"
            f"\n"
            f"class _{l_class_name}Template(_itkTemplate, metaclass=_{l_class_name}TemplateGetter):\n"
            f'    """Interface for instantiating itk::{l_class_name}< {types} >\n'
            f"        Supports type specification through dictionary access:\n"
            f'            \'itk.{l_class_name}[{types}]()"""\n'
            f"\n"
            f"    def __new__(cls, *args: Any) -> _{l_class_name}Proxy:\n"
            f'        """Instantiate itk::{l_class_name}< {types} >"""\n'
            f"        ...\n\n"
            f"    def __call__(self, *args: Any) -> _{l_class_name}Proxy:\n"
            f'        """Instantiate itk::{l_class_name}< {types} >"""\n'
            f"        ...\n"
            f"\n"
            f"{l_class_name} = _{l_class_name}Template"
            f"\n"
            f"\n"
        )
    else:
        class_header = (
            f"class _{l_class_name}TemplateGetter():\n"
            f"    def __getitem__(self, parameters) -> _{l_class_name}Template:\n"
            f'        """Specify class type with:\n'
            f"            \t[{types}]\n"
            f"            :return: {l_class_name}Template\n"
            f'            """\n'
            f"        ...\n"
            f"\n"
            f"\n"
            f"class _{l_class_name}Template(_itkTemplate, metaclass=_{l_class_name}TemplateGetter):\n"
            f'    """Interface for instantiating itk::{l_class_name}< {types} >"""\n'
            f"    ...\n"
            f"\n"
            f"{l_class_name} = _{l_class_name}Template"
            f"\n"
            f"\n"
        )
    return class_header


def init_init_import_file() -> StringIO:
    init_import_file = StringIO()
    init_import_file.write(
        f"""# Python Header Interface File
# imports as they appear in __init__.py
from typing import Union, Any
from itkConfig import ITK_GLOBAL_VERSION_STRING as __version__

# Must import using relative paths so that symbols are imported successfully
from .support.extras import *
from .support.types import *

# Begin stub imports

\n"""
    )
    return init_import_file


def init_proxy_import_file() -> StringIO:
    proxy_import_file = StringIO()
    proxy_import_file.write(
        f"""# Python proxy import file
# This file allows the wrapped classes (both templates and proxies)
# to import proxy classes without exposing the Proxy classes to a user
# This also allows the imports to be done in other files without knowing the classes file name

\n"""
    )
    return proxy_import_file


def init_class_pyi_template_file(pyi_file: Path, submodule_name: str) -> None:
    with open(pyi_file, "w") as pyi_file:
        pyi_file.write(
            f"""# Interface for submodule: {submodule_name}
from typing import Union, Any
# additional imports
from .support.template_class import itkTemplate as _itkTemplate

\n"""
        )


def init_class_pyi_proxy_file(
    pyi_file: Path, submodule_name: str, parent_imports
) -> None:
    with open(pyi_file, "w") as pyi_file:
        pyi_file.write(
            f"""# Interface methods for submodule: {submodule_name}
from typing import Union, Any
# additional imports
from .support.template_class import itkTemplate as _itkTemplate
{f"from ._proxies import {', '.join(parent_imports)}" if len(parent_imports) > 0 else ""}

\n"""
        )


def write_class_template_pyi(
    pyi_file: Path, l_class_name: str, header_code: str
) -> None:
    # Write interface files to the stub directory to support editor autocompletion
    with open(pyi_file, "a+") as pyi_file:
        pyi_file.write(f"# Interface for class: {l_class_name}\n")
        pyi_file.write(
            f"from ._proxies import {l_class_name}Proxy as _{l_class_name}Proxy\n"
        )
        pyi_file.write(header_code)


def write_class_proxy_pyi(
    pyi_file: Path, l_class_name: str, interfaces_code: str
) -> None:
    # Write interface files to the stub directory to support editor autocompletion
    with open(pyi_file, "a+") as pyi_file:
        pyi_file.write(f"# Interface methods for class: {l_class_name}\n")
        pyi_file.write(interfaces_code)


def unpack(file_names: [str], save_dir: str) -> Union[str, None]:
    class_definitions = []

    for file_name in file_names:
        with open(file_name, "rb") as pickled_file:
            itk_class = pickle.load(pickled_file)
            class_definitions.append(itk_class)

    base = merge(class_definitions)

    if base is not None:
        init_class_pyi_template_file(
            Path(f"{save_dir}/{base.class_name}Template.pyi"), base.class_name
        )
        parent_imports = [f"{p[1:]} as {p}" for p in base.parent_class]
        init_class_pyi_proxy_file(
            Path(f"{save_dir}/{base.class_name}Proxy.pyi"),
            base.class_name,
            parent_imports,
        )

        output_pyi_header_file = StringIO()
        output_pyi_method_file = StringIO()

        generate_class_pyi_def(output_pyi_header_file, output_pyi_method_file, base)

        write_class_template_pyi(
            Path(f"{save_dir}/{base.class_name}Template.pyi"),
            base.class_name,
            output_pyi_header_file.getvalue(),
        )
        write_class_proxy_pyi(
            Path(f"{save_dir}/{base.class_name}Proxy.pyi"),
            base.class_name,
            output_pyi_method_file.getvalue(),
        )

        return base.class_name

    else:
        files = "\n\t".join(file_names)
        print(
            f"WARNING: The following files do not contain a class definition for Python stub wrapping:\n"
            f"\t{files}"
        )

        return None


def merge(class_definitions: []) -> Union[ITKClass, None]:
    # Merge all the class files together to return one complete class.
    if len(class_definitions) == 0:
        return None
    elif len(class_definitions) == 1:
        return class_definitions[0]
    else:
        base = class_definitions[0]
        for class_def in class_definitions[1:]:
            # Confirm base values match
            if (
                base.class_name != class_def.class_name
                or base.has_new_method != class_def.has_new_method
                or base.typed != class_def.typed
                or base.is_abstract != class_def.is_abstract
                or base.is_enum != class_def.is_enum
                or base.has_superclass != class_def.has_superclass
            ):
                raise AssertionError(
                    f"The basic values of two associated classes do not match.\n"
                    f"Item 1:"
                    f" class `{base.submodule_name}::{base.class_name}`"
                    f" has_new_method: `{base.has_new_method}`"
                    f" typed: `{base.typed}`"
                    f" is_abstract: `{base.is_abstract}`"
                    f" is_enum: `{base.is_enum}`"
                    f" has_superclass: `{base.has_superclass}`"
                    f"Item 2:"
                    f" class `{class_def.submodule_name}::{class_def.class_name}`"
                    f" has_new_method: `{class_def.has_new_method}`"
                    f" typed: `{class_def.typed}`"
                    f" is_abstract: `{class_def.is_abstract}`"
                    f" is_enum: `{class_def.is_enum}`"
                    f" has_superclass: `{class_def.has_superclass}`"
                )

            # Enums should not be defined in multiple submodules
            # If this is not the case, a combine function could be added here
            if base.is_enum:
                raise TypeError(
                    f"The enumeration class {base.class_name} is defined in multiple ITK submodules.\n"
                    f"If this is"
                )

            # In some instances, not all parent classes are represented in each overload
            # This merges them together but may provide some type dependent hints
            base.parent_class = list(set(base.parent_class + class_def.parent_class))

            # merge class methods together
            for (
                method_name,
                method_overloads,
            ) in class_def.python_method_headers.items():
                if method_name in base.python_method_headers:
                    # Add overloads that don't already exist in the base method's overload list
                    for overload in method_overloads:
                        if overload not in base.python_method_headers[method_name]:
                            base.python_method_headers[method_name].append(overload)
                else:
                    # In this case there is a method that is only defined for some overloads of the class
                    # The best we can do is always show it and let it error when the overload does not
                    # contain the method
                    base.python_method_headers[
                        method_name
                    ] = class_def.python_method_headers[method_name]
        return base


if __name__ == "__main__":
    cmdln_arg_parser = ArgumentParser()
    cmdln_arg_parser.add_argument(
        "--pyi_dir",
        action="store",
        dest="pyi_dir",
        default="",
        type=str,
        help="The directory for .pyi files to be generated",
    )
    cmdln_arg_parser.add_argument(
        "--pkl_dir",
        action="store",
        dest="pkl_dir",
        default="",
        type=str,
        help="The directory for .pkl files to be generated",
    )
    cmdln_arg_parser.add_argument(
        "--index_list_file",
        action="store",
        dest="index_list_file",
        default="",
        type=str,
        help="Configured file listing the index files containing pickle file references",
    )
    cmdln_arg_parser.add_argument(
        "-d",
        action="store_true",
        dest="debug_code",
        help="Enable debugging dode",
    )

    options = cmdln_arg_parser.parse_args()
    if not Path(options.pkl_dir).exists:
        except_comment = f"Invalid directory provided '{options.pkl_dir}'"
        raise Exception(except_comment)

    # Read index filepaths from configured file.
    # Passing this information through a file allows us to circumvent
    # command length constraints on Windows.
    index_files_txt = options.index_list_file.strip()
    with open(index_files_txt, 'r') as f:
        index_files = set(f.read().strip().split(';'))

    # All index files for python pickled pyi classes:
    existing_index_files = set([filepath.replace(os.sep, '/') for filepath in glob.glob(f"{options.pkl_dir}/*.index.txt")])

    invalid_index_files = existing_index_files - index_files
    missing_index_files = index_files - existing_index_files

    if options.debug_code:
        for invalid_index_file in invalid_index_files:
            print(
                f"WARNING: Outdated index file {invalid_index_file} has been removed"
            )
            remove(invalid_index_file)

    for missing_index_file in missing_index_files:
        # continue on without missing file, display warning
        print(
            f"WARNING: index file {missing_index_file} is missing,  "
            f"Python stub hints will not be generated for this file. "
        )
        index_files.remove(missing_index_file)

    if len(index_files) == 0:
        except_comment = f"No index files were found in directory '{options.pkl_dir}'"
        print(f"Exception: {except_comment}")
        raise Exception(except_comment)

    indexed_pickled_files = set()
    for index_file in sorted(index_files):
        with open(index_file, "r") as file:
            for line in file:
                indexed_pickled_files.add(line.strip())

    existing_pickled_files = set([filepath.replace(os.sep, '/') for filepath in glob.glob(f"{options.pkl_dir}/*.pkl")])

    invalid_pickled_files = existing_pickled_files - indexed_pickled_files
    missing_pickled_files = indexed_pickled_files - existing_pickled_files

    if options.debug_code:
        for invalid_pickle_file in invalid_pickled_files:
            print(
                f"WARNING: Outdated pickle file {invalid_pickle_file} has been removed"
            )
            remove(invalid_pickle_file)

    for missing_file in missing_pickled_files:
        # continue on without missing file, display warning
        print(
            f"WARNING: pickle file {missing_file} is missing, Python stub hints will not be generated for this file."
        )
        indexed_pickled_files.remove(missing_file)

    if len(indexed_pickled_files) == 0:
        raise Exception(f"No pickle files were found in directory {options.pkl_dir}")

    indexed_pickled_files = sorted(list(indexed_pickled_files))
    output_template_import_list: List[str] = []
    output_proxy_import_list: List[str] = []

    class_name_dict = defaultdict(list)
    for file in indexed_pickled_files:
        current_class_name = re.split(r"\.", PurePath(file).parts[-1])[0]
        class_name_dict[current_class_name].append(file)

    for current_class_name, class_files in class_name_dict.items():
        class_name = unpack(class_files, options.pyi_dir)
        if class_name is not None:
            output_template_import_list.append(f"from .{class_name}Template import *\n")
            output_proxy_import_list.append(f"from .{class_name}Proxy import *\n")

    output_init_import_file = init_init_import_file()
    output_proxy_import_file = init_proxy_import_file()

    with open(options.pyi_dir + "/__init__.pyi", "w") as init_file:
        init_file.write(output_init_import_file.getvalue())
        init_file.write("\n".join(output_template_import_list))

    with open(options.pyi_dir + "/_proxies.pyi", "w") as proxy_file:
        proxy_file.write(output_proxy_import_file.getvalue())
        proxy_file.write("\n".join(output_proxy_import_list))
