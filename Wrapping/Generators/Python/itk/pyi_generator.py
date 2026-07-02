"""
pyi_generator.py \
  Wrapping/Generators/Python/itk-pkl/itkFixedArray.index.txt \
  Wrapping/Generators/Python/itk-pkl/itkVector.index.txt
"""

import os
import sys
import sqlite3
from os import remove
from argparse import ArgumentParser
from io import StringIO
from pathlib import Path, PurePath
import pickle
import glob
from collections import defaultdict

sys.path.insert(0, str(Path(__file__).parents[2]))
from pkl_db import open_pkl_db  # noqa: E402


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


def unpack(keys: list[str], db_conn: sqlite3.Connection, save_dir: str) -> str | None:
    placeholders = ",".join("?" * len(keys))
    rows = {
        k: data
        for k, data in db_conn.execute(
            f"SELECT key, data FROM pkl_data WHERE key IN ({placeholders})", keys
        )
    }

    class_definitions = []
    for key in keys:
        if key not in rows:
            print(f"WARNING: pkl key {key!r} not found in database, skipping.")
            continue
        class_definitions.append(pickle.loads(rows[key]))

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
        files = "\n\t".join(keys)
        print(
            f"WARNING: The following keys do not contain a class definition for Python stub wrapping:\n"
            f"\t{files}"
        )

        return None


def merge(class_definitions: []) -> ITKClass | None:
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
                    base.python_method_headers[method_name] = (
                        class_def.python_method_headers[method_name]
                    )
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
        "--prune",
        action="store_true",
        dest="prune",
        help="Delete pkl DB rows whose keys are absent from the index manifests. "
        "Only safe when the manifests cover every module sharing the DB.",
    )
    cmdln_arg_parser.add_argument(
        "-d",
        action="store_true",
        dest="debug_code",
        help="Enable debugging dode",
    )

    options = cmdln_arg_parser.parse_args()
    if not Path(options.pkl_dir).exists():
        except_comment = f"Invalid directory provided '{options.pkl_dir}'"
        raise Exception(except_comment)

    # Read index filepaths from configured file.
    # Passing this information through a file allows us to circumvent
    # command length constraints on Windows.
    index_files_txt = options.index_list_file.strip()
    with open(index_files_txt) as f:
        index_files = set(f.read().strip().split(";"))

    # All index files for python pickled pyi classes:
    existing_index_files = {
        filepath.replace(os.sep, "/")
        for filepath in glob.glob(f"{options.pkl_dir}/*.index.txt")
    }

    invalid_index_files = existing_index_files - index_files
    missing_index_files = index_files - existing_index_files

    if options.debug_code:
        for invalid_index_file in invalid_index_files:
            print(f"WARNING: Outdated index file {invalid_index_file} has been removed")
            remove(invalid_index_file)

    for missing_index_file in missing_index_files:
        print(
            f"WARNING: index file {missing_index_file} is missing,  "
            f"Python stub hints will not be generated for this file. "
        )
        index_files.remove(missing_index_file)

    if len(index_files) == 0:
        except_comment = f"No index files were found in directory '{options.pkl_dir}'"
        print(f"Exception: {except_comment}")
        raise Exception(except_comment)

    # Collect DB keys from all .index.txt manifests (keys, not file paths).
    indexed_pkl_keys: set[str] = set()
    for index_file in sorted(index_files):
        with open(index_file) as file:
            for line in file:
                key = line.strip()
                if key:
                    indexed_pkl_keys.add(key)

    if len(indexed_pkl_keys) == 0:
        raise Exception(f"No pkl keys were found in index files in '{options.pkl_dir}'")

    indexed_pkl_keys_sorted = sorted(indexed_pkl_keys)
    output_template_import_list: list[str] = []
    output_proxy_import_list: list[str] = []

    class_name_dict: defaultdict[str, list[str]] = defaultdict(list)
    for key in indexed_pkl_keys_sorted:
        current_class_name = key.split(".")[0]
        class_name_dict[current_class_name].append(key)

    db_conn = open_pkl_db(options.pkl_dir)

    # Indexed keys absent from the DB mean it was deleted or damaged while the
    # stamp files stayed fresh, so ninja would never re-run igenerator; drop
    # the stamps and fail so the next build regenerates the database.
    present_keys: set[str] = set()
    for chunk_start in range(0, len(indexed_pkl_keys_sorted), 500):
        chunk = indexed_pkl_keys_sorted[chunk_start : chunk_start + 500]
        placeholders = ",".join("?" * len(chunk))
        present_keys.update(
            row[0]
            for row in db_conn.execute(
                f"SELECT key FROM pkl_data WHERE key IN ({placeholders})", chunk
            )
        )
    absent_keys = set(indexed_pkl_keys_sorted) - present_keys
    if absent_keys:
        db_conn.close()
        for stamp_file in glob.glob(f"{options.pkl_dir}/*.stamp"):
            remove(stamp_file)
        raise Exception(
            f"{len(absent_keys)} indexed pkl keys are missing from the pkl "
            f"database in '{options.pkl_dir}'. The igenerator stamp files "
            "were removed; re-run the build to regenerate the database."
        )

    for current_class_name, class_keys in class_name_dict.items():
        class_name = unpack(class_keys, db_conn, options.pyi_dir)
        if class_name is not None:
            output_template_import_list.append(f"from .{class_name}Template import *\n")
            output_proxy_import_list.append(f"from .{class_name}Proxy import *\n")

    if not output_template_import_list:
        raise Exception(
            f"No Python stub classes were generated from pkl keys in '{options.pkl_dir}'"
        )

    if options.prune:
        if missing_index_files:
            print(
                "WARNING: skipping pkl DB pruning: the index manifest set is "
                "incomplete, so live keys cannot be determined safely."
            )
        else:
            with db_conn:
                db_conn.execute("CREATE TEMP TABLE _live_keys (key TEXT PRIMARY KEY)")
                db_conn.executemany(
                    "INSERT OR IGNORE INTO _live_keys VALUES(?)",
                    [(k,) for k in indexed_pkl_keys_sorted],
                )
                db_conn.execute(
                    "DELETE FROM pkl_data WHERE key NOT IN (SELECT key FROM _live_keys)"
                )
                db_conn.execute("DROP TABLE _live_keys")

    db_conn.close()

    output_init_import_file = init_init_import_file()
    output_proxy_import_file = init_proxy_import_file()

    with open(options.pyi_dir + "/__init__.pyi", "w") as init_file:
        init_file.write(output_init_import_file.getvalue())
        init_file.write("\n".join(output_template_import_list))

    with open(options.pyi_dir + "/_proxies.pyi", "w") as proxy_file:
        proxy_file.write(output_proxy_import_file.getvalue())
        proxy_file.write("\n".join(output_proxy_import_list))
