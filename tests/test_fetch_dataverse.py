"""Offline tests. No network requests or empirical research claims."""
import hashlib
import importlib.util
import io
from pathlib import Path
import tempfile
import unittest
import zipfile

spec = importlib.util.spec_from_file_location("fetch_dataverse", Path(__file__).resolve().parents[1] / "scripts/fetch_dataverse.py")
f = importlib.util.module_from_spec(spec)
spec.loader.exec_module(f)


class FetchTests(unittest.TestCase):
    def test_reject_unsafe_paths(self):
        for name in ("../escape", "/absolute", "a/../../b", "a\\b", "C:/a", ".", ""):
            with self.subTest(name=name), self.assertRaises(ValueError):
                f.safe_relative(name)
        self.assertEqual(f.safe_relative("original/code.sas"), Path("original/code.sas"))

    def test_catalog_preserves_paths_and_ids(self):
        catalog = {"status": "OK", "data": {"latestVersion": {"files": [
            {"directoryLabel": "original", "dataFile": {"id": 12, "filename": "code.sas", "filesize": 7}},
            {"directoryLabel": "original", "dataFile": {"id": 13, "filename": "code.sas", "filesize": 7}}
        ]}}}
        files = f.catalog_files(catalog)
        self.assertEqual(files[0]["path"], "originals/12/original/code.sas")
        self.assertNotEqual(files[0]["path"], files[1]["path"])
        catalog["data"]["latestVersion"]["files"][0]["directoryLabel"] = "../../outside"
        with self.assertRaises(ValueError):
            f.catalog_files(catalog)

    def item(self, content):
        return {"id": 1, "source_name": "source.zip", "path": "originals/1/source.zip",
                "size": len(content), "restricted": False,
                "checksum": {"type": "MD5", "value": hashlib.md5(content).hexdigest()},
                "url": "https://example.invalid/source"}

    def test_download_checksum_cache_and_corruption(self):
        content = b"original source bytes"
        item = self.item(content)
        calls = []
        def opener(url, timeout):
            calls.append(url)
            return io.BytesIO(content)
        with tempfile.TemporaryDirectory() as td:
            root = Path(td)
            first = f.download_one(item, root, opener=opener)
            second = f.download_one(item, root, first, opener=opener)
            self.assertEqual(len(calls), 1)
            self.assertEqual(first["sha256"], hashlib.sha256(content).hexdigest())
            self.assertEqual(first, second)
            (root / item["path"]).write_bytes(b"x" * len(content))
            f.download_one(item, root, second, opener=opener)
            self.assertEqual(len(calls), 2)
            self.assertEqual((root / item["path"]).read_bytes(), content)

    def test_corrupt_download_is_not_installed(self):
        item = self.item(b"correct")
        with tempfile.TemporaryDirectory() as td:
            root = Path(td)
            with self.assertRaisesRegex(ValueError, "Checksum mismatch"):
                f.download_one(item, root, opener=lambda *a, **kw: io.BytesIO(b"corrupt"))
            self.assertFalse((root / item["path"]).exists())
            self.assertFalse(list(root.rglob(".download-*")))

    def test_extract_regular_files_and_protect_existing(self):
        with tempfile.TemporaryDirectory() as td:
            root = Path(td)
            archive = root / "test.zip"
            with zipfile.ZipFile(archive, "w") as z:
                z.writestr("original/code.sas", "/* source */")
            dest = root / "extracted"
            f.safe_extract(archive, dest)
            self.assertEqual((dest / "original/code.sas").read_text(), "/* source */")
            with self.assertRaises(FileExistsError):
                f.safe_extract(archive, dest)

    def test_archive_traversal_link_and_size_rejected(self):
        with tempfile.TemporaryDirectory() as td:
            root = Path(td)
            for kind in ("traversal", "symlink", "size"):
                archive = root / f"{kind}.zip"
                with zipfile.ZipFile(archive, "w") as z:
                    if kind == "traversal":
                        z.writestr("../outside", "bad")
                    elif kind == "symlink":
                        info = zipfile.ZipInfo("link")
                        info.create_system = 3
                        info.external_attr = 0o120777 << 16
                        z.writestr(info, "/etc/passwd")
                    else:
                        z.writestr("large", "123456")
                with self.subTest(kind=kind), self.assertRaises(ValueError):
                    f.safe_extract(archive, root / kind, max_bytes=5)
                self.assertFalse((root / kind).exists())
                self.assertFalse(list(root.glob(".extract-*")))
            self.assertFalse((root / "outside").exists())


if __name__ == "__main__":
    unittest.main()
