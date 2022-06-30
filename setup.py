from setuptools import find_packages, setup

install_requires = open('requirements.txt').read().strip().split('\n')

setup(
    name='bk',
    packages=find_packages(where='src'),
    package_dir={'': 'src'},
    install_requires=install_requires
)

# setup(
#     name=import_name,
#     version = version,
#     packages=find_packages(where='src'),
#     package_dir={'': 'src'},
#     package_data={
#         import_name : ['**/*.yml', '**/*.yaml']
#     },
#     include_package_data=True,
#     install_requires=install_requires,
#     extras_require=extras,
# )