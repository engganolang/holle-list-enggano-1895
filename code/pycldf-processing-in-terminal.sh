# go to the Enggano Holle List directory
cd /Users/Primahadi/Documents/enggano-AHRC/enggano-holle-list-2023-03-27

# create the Python virtual environment there
python3 -m venv myenv

# activate it
source myenv/bin/activate

# install the pycldf package
pip install pycldf

# try to validate the metadata.json file
cldf validate cldf/cldf-metadata.json

# there is an import error with 'urllib3 2.0' package
## ImportError: urllib3 v2.0 only supports OpenSSL 1.1.1+, currently the 'ssl' module is compiled with LibreSSL 2.8.3. See: https://github.com/urllib3/urllib3/issues/2168
## The WORKING solution here: https://stackoverflow.com/questions/76187256/importerror-urllib3-v2-0-only-supports-openssl-1-1-1-currently-the-ssl-modu
pip uninstall urllib3
pip install 'urllib3<2.0'

# try to validate the metadata.json file
cldf validate cldf/cldf-metadata.json
