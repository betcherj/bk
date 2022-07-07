import yaml

def read_yaml(path):
    file_handle = open(path, "r")
    res = yaml.safe_load(file_handle)
    file_handle.close()
    return res