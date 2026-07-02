"""Shared fixtures: load ITK wrapping scripts by path, build a fake castxml."""

import importlib.util
import os
import subprocess
import sys
from pathlib import Path

import pytest

# Default to the enclosing ITK source tree; ITK_SOURCE_DIR overrides to
# point the suite at a different checkout.
ITK_SOURCE_DIR = Path(
    os.environ.get("ITK_SOURCE_DIR", Path(__file__).resolve().parents[3])
)
CACHE_SCRIPT = ITK_SOURCE_DIR / "Wrapping/Generators/CastXML/itk-castxml-cache.py"
PKL_DB = ITK_SOURCE_DIR / "Wrapping/Generators/pkl_db.py"
PYI_GENERATOR = ITK_SOURCE_DIR / "Wrapping/Generators/Python/itk/pyi_generator.py"

FAKE_CASTXML = Path(__file__).parent / "fake_castxml.py"


def _load(name, path):
    spec = importlib.util.spec_from_file_location(name, path)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


@pytest.fixture(scope="session")
def cache_mod():
    return _load("itk_castxml_cache", CACHE_SCRIPT)


@pytest.fixture(scope="session")
def pkl_db_mod():
    return _load("pkl_db", PKL_DB)


class FakeCastxml:
    """A castxml stand-in whose invocations are observable.

    -E mode emits preprocessed output with `# N "path"` line markers naming
    the header fixture files; full mode writes XML derived from the header
    contents and FAKE_CASTXML_VERSION, so output provably tracks the inputs.
    """

    def __init__(self, tmp_path, monkeypatch):
        self.dir = tmp_path
        self.log = tmp_path / "invocations.log"
        self.cache = tmp_path / "cache"
        self.cache.mkdir()
        self.incdir_a = tmp_path / "incdirA"
        self.incdir_b = tmp_path / "incdirB"
        self.incdir_a.mkdir()
        self.incdir_b.mkdir()
        self.build = tmp_path / "build"
        self.build.mkdir()

        (self.incdir_a / "foo.h").write_text("content-A v1\n")
        (self.incdir_b / "bar.h").write_text("content-B v1\n")
        self.inc_file = self.build / "module.inc"
        self.inc_file.write_text(f'"-I{self.incdir_a}"\n"-I{self.incdir_b}"\n')
        self.cxx_file = self.build / "module.cxx"
        self.cxx_file.write_text("// module source\n")
        self.out_xml = self.build / "out.xml"

        if sys.platform == "win32":
            self.bin = tmp_path / "castxml.cmd"
            self.bin.write_text(
                f'@echo off\r\n"{sys.executable}" "{FAKE_CASTXML}" %*\r\n'
            )
        else:
            self.bin = tmp_path / "castxml"
            self.bin.write_text(
                f'#!/usr/bin/env bash\nexec "{sys.executable}" "{FAKE_CASTXML}" "$@"\n'
            )
            self.bin.chmod(0o755)

        monkeypatch.setenv("ITK_WRAP_CACHE", str(self.cache))
        monkeypatch.setenv("FAKE_LOG", str(self.log))
        monkeypatch.setenv(
            "FAKE_HDRS",
            os.pathsep.join(
                [str(self.incdir_a / "foo.h"), str(self.incdir_b / "bar.h")]
            ),
        )
        monkeypatch.delenv("ITK_WRAP_CACHE_FORMAT", raising=False)
        monkeypatch.delenv("ITK_WRAP_CACHE_BYPASS", raising=False)

    def invoke(self, extra_env=None, no_cache=False):
        cmd = [sys.executable, str(CACHE_SCRIPT)]
        if no_cache:
            cmd.append("--no-cache")
        cmd += [
            str(self.bin),
            "--castxml-cc-gnu",
            "g++",
            f"@{self.inc_file}",
            "-o",
            str(self.out_xml),
            str(self.cxx_file),
        ]
        env = dict(os.environ)
        if extra_env:
            env.update(extra_env)
        return subprocess.run(cmd, env=env, capture_output=True, text=True)

    def invocations(self):
        if not self.log.exists():
            return []
        return self.log.read_text().split()

    def reset_log(self):
        self.log.write_text("")

    def evict(self, days="13.9"):
        return subprocess.run(
            [
                sys.executable,
                str(CACHE_SCRIPT),
                "--evict",
                days,
                "--cache-dir",
                str(self.cache),
            ],
            capture_output=True,
            text=True,
        )


@pytest.fixture
def fake(tmp_path, monkeypatch):
    return FakeCastxml(tmp_path, monkeypatch)
