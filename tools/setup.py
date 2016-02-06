from distutils.core import setup
from distutils.sysconfig import get_python_lib
import os.path
import py2exe


setup(
    version="1.0.2",
    console=['vcf2csv.py'],
    options={'py2exe': {
        'bundle_files': 1, 
        'compressed': True,
        'optimize': 2,
        'includes': ['vobject', 'jieba', 'pypinyin'],
        'dll_excludes': ['w9xpopen.exe'],
    }},
    zipfile=None,
    data_files=[('', [os.path.join(get_python_lib(), 'jieba', 'dict.txt')])],
)
